library(shiny)
library(shinydashboard)
library(shinycssloaders)   # withSpinner()
library(leaflet)
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(DT)
library(plotly)
library(httr)
library(jsonlite)
library(fresh) 

# ══════════════════════════════════════════════════════════════
# 5. MAINTAINABILITY — central constants (one place to edit)
# ══════════════════════════════════════════════════════════════
DEFAULT_STANDORT <- "Renzstraße"

# ── ADFC-Markenfarben (eine Quelle der Wahrheit) ───────────────
ADFC_BLUE   <- "#004B7C"   # Dunkelblau – Primär
ADFC_ORANGE <- "#EE7400"   # Orange – Akzent / Highlight
ADFC_GREEN  <- "#7FC600"   # Grün – Sekundär
CANVAS      <- "#F4F6F9"   # App-Hintergrund
PAPER       <- "#FFFFFF"

# Plot-Farben leiten sich aus der Marke ab
COL_PRIMARY   <- ADFC_BLUE
COL_HIGHLIGHT <- ADFC_ORANGE
COL_ACCENT    <- ADFC_GREEN
COL_PLOT_BG   <- "#F5F7FA"
COL_PAPER_BG  <- PAPER

# ── Dashboard-Theme (fresh) ────────────────────────────────────
bike_theme <- create_theme(
  adminlte_sidebar(
    width            = "240px",
    dark_bg          = "#10243A",
    dark_hover_bg    = "#16344F",   # vorher ADFC_ORANGE → jetzt dezent
    dark_color       = "#CBD7E2",
    dark_hover_color = "#FFFFFF",
    dark_submenu_bg  = "#0B1B2D",
    dark_submenu_color = "#9FB2C4"
  ),
  adminlte_sidebar(
    width              = "240px",
    dark_bg            = "#10243A",   # tiefes Marineblau statt neutralem Slate
    dark_hover_bg      = ADFC_ORANGE, # Hover + aktiver Menüpunkt in Markenfarbe
    dark_color         = "#CBD7E2",
    dark_hover_color   = "#FFFFFF",
    dark_submenu_bg    = "#0B1B2D",
    dark_submenu_color = "#9FB2C4"
  ),
  adminlte_global(
    content_bg  = CANVAS,
    box_bg      = PAPER,
    info_box_bg = PAPER
  )
)

MONTH_STARTS  <- c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335)
MONTH_ABBR_DE <- c("Jan", "Feb", "Mär", "Apr", "Mai", "Jun",
                   "Jul", "Aug", "Sep", "Okt", "Nov", "Dez")
MONTH_FULL_DE <- c("Januar", "Februar", "März", "April", "Mai", "Juni",
                   "Juli", "August", "September", "Oktober", "November", "Dezember")

POLL_INTERVAL_S <- 5     # seconds between SQL status polls
POLL_DEADLINE_S <- 300   # hard deadline before giving up

# ── GCS snapshot location ─────────────────────────────────────
# The app reads its data from this object, which the daily refresh
# job (refresh_data.R) regenerates from Databricks each morning.
# GCS_BUCKET is provided as an env var on Cloud Run; the default is
# only a convenience for local runs.
GCS_BUCKET <- Sys.getenv("GCS_BUCKET", "adfc-483617-bike-counter")
GCS_OBJECT <- Sys.getenv("GCS_OBJECT", "bike_counter_data.rds")

# ── Helper: choose first valid choice, falling back to default ──
pick_default <- function(choices, preferred = DEFAULT_STANDORT) {
  if (length(choices) == 0) return(NULL)
  if (preferred %in% choices) preferred else choices[[1]]
}

# ── Helper: consistent plotly theme (5. Maintainability) ───────
style_plot <- function(fig) {
  fig %>%
    plotly::layout(plot_bgcolor = COL_PLOT_BG, paper_bgcolor = COL_PAPER_BG) %>%
    plotly::config(
      displayModeBar         = TRUE,
      modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d"),
      displaylogo            = FALSE
    )
}

# ── Helper: build cumulative-by-day-of-year frame (1. Architecture)
# Shared by all three cumulative tabs — single source of truth.
compute_cumulative <- function(df, max_doy = Inf) {
  df <- df[!is.na(df$counter), , drop = FALSE]
  if (nrow(df) == 0) return(NULL)
  df <- df[df$day_of_year <= max_doy, , drop = FALSE]
  if (nrow(df) == 0) return(NULL)

  daily <- aggregate(counter ~ year + day_of_year, data = df, FUN = sum, na.rm = TRUE)
  daily <- daily[order(daily$year, daily$day_of_year), ]
  # vectorised per-year cumulative sum (replaces the for-loop)
  daily$cumulative <- ave(daily$counter, daily$year, FUN = cumsum)
  daily
}

# ── Helper: build the 24-month wide monthly table (1. Architecture)
# Used by BOTH the DT output and the CSV download — no duplication.
build_monthly_wide <- function(df, months_back = 24) {
  cutoff_date <- Sys.Date() %m-% months(months_back)

  monthly_summary <- df %>%
    filter(date >= cutoff_date) %>%
    mutate(year_month = sprintf("%04d-%02d", year, month)) %>%
    group_by(Standort, year_month) %>%
    summarise(total = round(sum(counter, na.rm = TRUE), -3), .groups = "drop")

  wide_table <- monthly_summary %>%
    pivot_wider(names_from = year_month, values_from = total, values_fill = 0)

  month_cols        <- setdiff(names(wide_table), "Standort")
  month_cols_sorted <- sort(month_cols, decreasing = TRUE)
  wide_table %>% select(Standort, all_of(month_cols_sorted))
}

# ── Load .Renviron ────────────────────────────────────────────
if (file.exists(".Renviron")) {
  message("✓ .Renviron found at: ", normalizePath(".Renviron"))
  readRenviron(".Renviron")
} else {
  message("✗ .Renviron NOT found — working dir is: ", getwd())
}

# ── Load credentials ──────────────────────────────────────────
host         <- Sys.getenv("DATABRICKS_HOST")
token        <- Sys.getenv("DATABRICKS_TOKEN")
http_path    <- Sys.getenv("DATABRICKS_HTTP_PATH")
warehouse_id <- basename(http_path)

message("HOST: '", host, "'")
message("WAREHOUSE_ID: '", warehouse_id, "'")

if (any(nchar(c(host, token, http_path)) == 0)) {
  # NOTE: the app's main data now comes from the GCS snapshot, not from
  # Databricks. Credentials are only needed for (a) the live "Job-Status"
  # tab and (b) the emergency Databricks fallback in load_app_data(). So a
  # missing credential should warn, not kill the whole app at startup.
  warning("Databricks-Credentials unvollständig (DATABRICKS_HOST/TOKEN/HTTP_PATH). ",
          "Haupt-Daten kommen aus GCS; nur Job-Status-Tab und Notfall-Fallback betroffen.")
}

# ── Databricks Job Status ─────────────────────────────────────
job_id <- Sys.getenv("DATABRICKS_JOB_ID")

fetch_job_status <- function() {
  tryCatch({
    if (nchar(job_id) == 0) {
      return(list(error = "DATABRICKS_JOB_ID fehlt/leer."))
    }

    res <- httr::GET(
      url   = paste0("https://", host, "/api/2.1/jobs/runs/list"),
      httr::add_headers(Authorization = paste("Bearer", token)),
      query = list(job_id = job_id, limit = 1, expand_tasks = "false")
    )

    if (httr::status_code(res) != 200) {
      body_txt <- tryCatch(rawToChar(httr::content(res, as = "raw")), error = function(e) "")
      return(list(error = paste0("HTTP ", httr::status_code(res), " – ", body_txt)))
    }

    parsed <- httr::content(res, as = "parsed")
    if (is.null(parsed$runs) || length(parsed$runs) == 0) {
      return(list(error = "Keine runs zurückgegeben. Prüfe Job-ID & Berechtigungen."))
    }

    run <- parsed$runs[[1]]

    start_ms   <- run$start_time
    end_ms     <- run$end_time
    state      <- run$state$result_state
    life_state <- run$state$life_cycle_state

    start_dt <- as.POSIXct(as.numeric(start_ms) / 1000, origin = "1970-01-01", tz = "Europe/Berlin")
    end_dt <- if (!is.null(end_ms) && !is.na(end_ms) && as.numeric(end_ms) > 0)
      as.POSIXct(as.numeric(end_ms) / 1000, origin = "1970-01-01", tz = "Europe/Berlin")
    else NULL

    duration_str <- if (!is.null(end_dt)) {
      duration_s <- as.numeric(difftime(end_dt, start_dt, units = "secs"))
      sprintf("%d min %d s", floor(duration_s/60), round(duration_s %% 60))
    } else "läuft…"

    display_state <- if (!is.null(state)) state else life_state

    list(
      error      = NULL,
      status     = display_state,
      life_state = life_state,
      start_time = format(start_dt, "%d.%m.%Y %H:%M:%S"),
      end_time   = if (!is.null(end_dt)) format(end_dt, "%d.%m.%Y %H:%M:%S") else "—",
      duration   = duration_str,
      run_url    = run$run_page_url
    )
  }, error = function(e) {
    list(error = paste("Exception:", conditionMessage(e)))
  })
}

fetch_max_timestamp <- function() {
  tryCatch({
    res <- httr::POST(
      url    = paste0("https://", host, "/api/2.0/sql/statements"),
      httr::add_headers(Authorization = paste("Bearer", token)),
      httr::content_type_json(),
      body   = jsonlite::toJSON(list(
        warehouse_id = warehouse_id,
        statement    = "SELECT MAX(date) AS max_date FROM bike_counter_mobidatabw.eco_counter.v_eco_counter_mannheim",
        wait_timeout = "300s",
        format       = "JSON_ARRAY"
      ), auto_unbox = TRUE),
      httr::timeout(360)
    )
    result <- httr::content(res, as = "parsed")
    if (result$status$state != "SUCCEEDED") return(NA_character_)
    val <- result$result$data_array[[1]][[1]]
    if (is.null(val)) return(NA_character_) else as.character(val)
  }, error = function(e) NA_character_)
}

# ── Helper: submit SQL and poll until done (WITH DEADLINE) ─────
run_sql <- function(sql, deadline_s = POLL_DEADLINE_S) {

  # 1. Submit (returns immediately with statement_id)
  res <- POST(
    url    = paste0("https://", host, "/api/2.0/sql/statements"),
    add_headers(Authorization = paste("Bearer", token)),
    content_type_json(),
    body   = toJSON(list(
      warehouse_id = warehouse_id,
      statement    = sql,
      wait_timeout = "0s",       # async — don't block here
      format       = "JSON_ARRAY"
    ), auto_unbox = TRUE),
    httr::timeout(30)
  )

  if (status_code(res) != 200) {
    stop("Submit failed (HTTP ", status_code(res), "): ",
         rawToChar(content(res, as = "raw")))
  }

  result  <- content(res, as = "parsed")
  stmt_id <- result$statement_id
  message("Statement submitted: ", stmt_id)

  # 2. Poll until SUCCEEDED / FAILED / CANCELED — OR until deadline
  deadline <- Sys.time() + deadline_s
  repeat {
    if (Sys.time() > deadline) {
      # Best-effort cancel so we don't leak a running query
      tryCatch(
        POST(
          url = paste0("https://", host, "/api/2.0/sql/statements/", stmt_id, "/cancel"),
          add_headers(Authorization = paste("Bearer", token)),
          httr::timeout(15)
        ),
        error = function(e) NULL
      )
      stop("Query timed out after ", deadline_s,
           "s (statement ", stmt_id, " cancelled).")
    }

    Sys.sleep(POLL_INTERVAL_S)
    poll <- GET(
      url = paste0("https://", host, "/api/2.0/sql/statements/", stmt_id),
      add_headers(Authorization = paste("Bearer", token)),
      httr::timeout(30)
    )
    result <- content(poll, as = "parsed")
    state  <- result$status$state
    message("  state: ", state)
    if (state %in% c("SUCCEEDED", "FAILED", "CANCELED")) break
  }

  if (state != "SUCCEEDED") {
    stop("Query ", state, ": ", result$status$error$message)
  }

  result   # return full result for chunk extraction
}

fetch_last_job_run_log <- function() {
  tryCatch({
    res <- run_sql("
      SELECT
        run_id, job_id, job_name, life_state, result_state,
        start_time, end_time, duration_s, max_data_ts,
        rows_added, rows_total, error_message, written_at
      FROM counter.monitoring.job_run_log
      ORDER BY written_at DESC
      LIMIT 1
    ")

    cols <- sapply(res$manifest$schema$columns, `[[`, "name")
    row  <- res$result$data_array[[1]]
    if (is.null(row)) return(list(error = "job_run_log: keine Zeilen gefunden."))

    names(row) <- cols

    fmt_berlin <- function(ts) {
      if (is.null(ts) || is.na(ts) || ts == "" || ts == "NA") return("—")
      format(as.POSIXct(ts, tz = "UTC"), tz = "Europe/Berlin", "%d.%m.%Y %H:%M:%S")
    }

    list(
      error        = NULL,
      run_id       = row[["run_id"]],
      job_id       = row[["job_id"]],
      job_name     = row[["job_name"]],
      life_state   = row[["life_state"]],
      result_state = row[["result_state"]],
      start_time   = fmt_berlin(row[["start_time"]]),
      end_time     = fmt_berlin(row[["end_time"]]),
      duration_s   = row[["duration_s"]],
      max_data_ts  = fmt_berlin(row[["max_data_ts"]]),
      rows_added   = row[["rows_added"]],
      rows_total   = row[["rows_total"]],
      error_msg    = row[["error_message"]],
      written_at   = fmt_berlin(row[["written_at"]])
    )
  }, error = function(e) {
    list(error = paste("Exception:", conditionMessage(e)))
  })
}

# ══════════════════════════════════════════════════════════════
# GOOGLE CLOUD STORAGE HELPERS
# ──────────────────────────────────────────────────────────────
# Auth uses the Cloud Run metadata server (Application Default
# Credentials) — no key file. The service account that runs the
# container needs read (app) or write (refresh job) on the bucket.
# Only httr is required, so no extra package dependency.
# ══════════════════════════════════════════════════════════════
gcs_token <- function() {
  res <- httr::GET(
    "http://metadata.google.internal/computeMetadata/v1/instance/service-accounts/default/token",
    httr::add_headers(`Metadata-Flavor` = "Google"),
    httr::timeout(10)
  )
  if (httr::status_code(res) != 200) {
    stop("GCS-Token vom Metadata-Server fehlgeschlagen (HTTP ",
         httr::status_code(res), "). Läuft der Code auf Cloud Run?")
  }
  httr::content(res, as = "parsed")$access_token
}

# Download an object to a local path. Returns the path.
gcs_download <- function(dest, bucket = GCS_BUCKET, object = GCS_OBJECT) {
  url <- sprintf("https://storage.googleapis.com/storage/v1/b/%s/o/%s?alt=media",
                 bucket, utils::URLencode(object, reserved = TRUE))
  res <- httr::GET(
    url,
    httr::add_headers(Authorization = paste("Bearer", gcs_token())),
    httr::write_disk(dest, overwrite = TRUE),
    httr::timeout(120)
  )
  if (httr::status_code(res) != 200) {
    stop("GCS-Download fehlgeschlagen (HTTP ", httr::status_code(res),
         ") für gs://", bucket, "/", object)
  }
  dest
}

# Upload a local file to an object (simple media upload).
gcs_upload <- function(file, bucket = GCS_BUCKET, object = GCS_OBJECT) {
  url <- sprintf("https://storage.googleapis.com/upload/storage/v1/b/%s/o?uploadType=media&name=%s",
                 bucket, utils::URLencode(object, reserved = TRUE))
  res <- httr::POST(
    url,
    httr::add_headers(Authorization = paste("Bearer", gcs_token()),
                      `Content-Type` = "application/octet-stream"),
    body = httr::upload_file(file),
    httr::timeout(300)
  )
  if (!httr::status_code(res) %in% c(200L, 201L)) {
    stop("GCS-Upload fehlgeschlagen (HTTP ", httr::status_code(res), "): ",
         rawToChar(httr::content(res, as = "raw")))
  }
  invisible(TRUE)
}

# ══════════════════════════════════════════════════════════════
# DATABRICKS FETCH
# ──────────────────────────────────────────────────────────────
# The full station query + chunk assembly, factored out so BOTH the
# daily refresh job (refresh_data.R) and the emergency fallback below
# share one definition and can't drift. Returns a clean data.frame
# with columns: date (Date), counter, Standort, latitude, longitude.
# The app no longer calls this on the hot path — it reads the GCS
# snapshot instead — so the warehouse cold start can't freeze startup.
# ══════════════════════════════════════════════════════════════
fetch_bike_from_databricks <- function() {
  result <- run_sql("
    SELECT
      f.date,
      f.total        AS counter,
      s.counter_name AS Standort,
      s.latitude,
      s.longitude
    FROM bike_counter_mobidatabw.eco_counter.eco_counter_mannheim f
    JOIN bike_counter_mobidatabw.eco_counter.eco_counter_stations s
      ON f.counter_id = s.counter_id
  ")
  statement_id <- result$statement_id
  total_chunks <- result$manifest$total_chunk_count
  cols         <- sapply(result$manifest$schema$columns, `[[`, "name")

  message("Total chunks: ", total_chunks)
  message("Total rows expected: ", result$manifest$total_row_count)

  all_rows      <- list()
  all_rows[[1]] <- result$result$data_array

  if (total_chunks > 1) {
    for (chunk_index in 1:(total_chunks - 1)) {
      message("Fetching chunk ", chunk_index, " ...")
      chunk_res <- GET(
        url = paste0("https://", host, "/api/2.0/sql/statements/",
                     statement_id, "/result/chunks/", chunk_index),
        add_headers(Authorization = paste("Bearer", token)),
        httr::timeout(30)
      )
      all_rows[[chunk_index + 1]] <- content(chunk_res, as = "parsed")$data_array
    }
  }

  data <- do.call(c, all_rows)
  rows <- lapply(data, function(row) {
    vapply(row, function(v) if (is.null(v)) NA_character_ else as.character(v), character(1))
  })

  df           <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  colnames(df) <- cols
  df$date      <- as.Date(df$date)
  df$counter   <- as.numeric(df$counter)
  df$latitude  <- as.numeric(df$latitude)
  df$longitude <- as.numeric(df$longitude)

  message("✓ Databricks-Daten geladen. Zeilen: ", nrow(df))
  df
}

# ══════════════════════════════════════════════════════════════
# DATA LOAD — reads the precomputed GCS snapshot (fast, no warehouse)
# ──────────────────────────────────────────────────────────────
# load_app_data() is cached: the first visitor downloads the snapshot
# and publishes these globals; later visitors return instantly. All
# existing outputs keep referencing the same global names unchanged.
# If GCS is unavailable, it falls back to a live Databricks query so
# the app still works (slow, but flagged via data_source).
# ══════════════════════════════════════════════════════════════

# Globals are declared up front so they always exist (NULL until loaded).
app_data_loaded  <- FALSE
bike_counter     <- NULL
bike_by_standort <- NULL
sites_unique     <- NULL
STANDORT_CHOICES <- NULL
ym_choices       <- NULL

# Provenance / freshness — surfaced in the UI so stale data is visible.
data_source    <- NA_character_     # "gcs" | "databricks_fallback"
data_max_date  <- as.Date(NA)       # most recent date present in the data
data_loaded_at <- as.POSIXct(NA)    # when this process loaded it

# Pre-split lookup helper (unchanged behaviour; just reads the global).
get_standort <- function(name) {
  df <- bike_by_standort[[name]]
  if (is.null(df) || nrow(df) == 0) bike_counter[0, ] else df
}

load_app_data <- function() {
  if (isTRUE(app_data_loaded)) return(invisible(TRUE))
  on_cloud_run <- nzchar(Sys.getenv("K_SERVICE"))   # Cloud Run sets this; empty locally

  df_loaded <- tryCatch({
    if (!on_cloud_run) stop("lokal — GCS übersprungen")   # short-circuit, no 10s wait
    tmp <- tempfile(fileext = ".rds"); gcs_download(tmp)
    df <- readRDS(tmp); df$date <- as.Date(as.character(df$date))
    data_source <<- "gcs"; df
  }, error = function(e) {
    local_rds <- "data/bike_counter_data.rds"
    if (!on_cloud_run && file.exists(local_rds)) {        # fast local dev, no Databricks
      df <- readRDS(local_rds); df$date <- as.Date(as.character(df$date))
      data_source <<- "local_rds"; df
    } else {
      df <- fetch_bike_from_databricks()
      data_source <<- "databricks_fallback"; df
    }
  })

  # ── Pre-compute derived date columns ONCE (data is static after load) ──
  df_loaded$date        <- as.Date(df_loaded$date)
  df_loaded$year        <- lubridate::year(df_loaded$date)
  df_loaded$month       <- lubridate::month(df_loaded$date)
  df_loaded$day_of_year <- lubridate::yday(df_loaded$date)

  # ── Publish to globals (so every existing output works unchanged) ──
  bike_counter     <<- df_loaded
  bike_by_standort <<- split(df_loaded, df_loaded$Standort)
  sites_unique     <<- df_loaded %>%
    select(Standort, latitude, longitude) %>%
    distinct() %>%
    rename(name = Standort)
  STANDORT_CHOICES <<- sort(unique(df_loaded$Standort))
  ym_choices       <<- sort(
    unique(sprintf("%04d-%02d", df_loaded$year, df_loaded$month)),
    decreasing = TRUE
  )

  data_max_date  <<- suppressWarnings(max(df_loaded$date, na.rm = TRUE))
  data_loaded_at <<- Sys.time()
  app_data_loaded <<- TRUE

  message(">>> Zeilen: ", nrow(bike_counter),
          " | Quelle: ", data_source,
          " | Datenstand: ", format(data_max_date, "%d.%m.%Y"))
  message("✓ App bereit.")

  invisible(TRUE)
}

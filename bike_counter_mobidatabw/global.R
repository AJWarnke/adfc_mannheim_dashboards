library(shiny)
library(shinydashboard)
library(leaflet)
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(DT)
library(plotly)
library(httr)
library(jsonlite)

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
  stop("Missing Databricks credentials. Check .Renviron for DATABRICKS_HOST, DATABRICKS_TOKEN, DATABRICKS_HTTP_PATH.")
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

# ── Helper: submit SQL and poll until done ────────────────────
run_sql <- function(sql) {
  
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
  
  result      <- content(res, as = "parsed")
  stmt_id     <- result$statement_id
  message("Statement submitted: ", stmt_id)
  
  # 2. Poll until SUCCEEDED / FAILED / CANCELED
  repeat {
    Sys.sleep(5)
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

# ── Load bike counter data (live Databricks with RDS fallback) ─
# Now queries the denormalised Mannheim VIEW which joins fact + station dimension
bike_counter <- tryCatch({
  
  result <- run_sql("
    SELECT
      f.date,
      f.total        AS counter,
      s.counter_name AS Standort,
      s.latitude,
      s.longitude
    --FROM bike_counter_mobidatabw.eco_counter.eco_counter_csv_backfill_staging f   
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
  
  message("✓ Live data loaded. Rows: ", nrow(df))
  df
  
}, error = function(e) {
  message("FULL ERROR: ", conditionMessage(e))
  rds <- readRDS("data/bike_counter_data.rds")
  rds$date <- as.Date(as.character(rds$date))
  rds
})

message(">>> bike_counter rows: ", nrow(bike_counter))
message(">>> bike_counter Standorte: ", paste(unique(bike_counter$Standort), collapse = ", "))

# ── Site metadata now comes from the query itself ─────────────
# No more separate CSV — lat/lon are in bike_counter from the JOIN
sites_unique <- bike_counter %>%
  select(Standort, latitude, longitude) %>%
  distinct() %>%
  rename(name = Standort)

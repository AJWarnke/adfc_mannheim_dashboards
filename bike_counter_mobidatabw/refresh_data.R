#!/usr/bin/env Rscript
# ══════════════════════════════════════════════════════════════
# DAILY SNAPSHOT REFRESH
# ──────────────────────────────────────────────────────────────
# Runs as a Cloud Run *Job* (not the web service), triggered by
# Cloud Scheduler every morning at 07:00 Europe/Berlin — safely
# after the 02:00 Databricks update.
#
# It queries Databricks once, writes data/bike_counter_data.rds,
# and uploads it to gs://$GCS_BUCKET/$GCS_OBJECT. The Shiny app
# then reads that object at startup (see load_app_data() in
# global.R) — no warehouse cold start on the user's path.
#
# Reuses the SAME container image as the app; the job just
# overrides the entrypoint to `Rscript refresh_data.R`.
# ══════════════════════════════════════════════════════════════

# global.R provides: credentials, run_sql(), fetch_bike_from_databricks(),
# gcs_upload(), and the GCS_BUCKET / GCS_OBJECT constants.
source("global.R")

# ── Roll the Cloud Run *service* so it re-reads the fresh snapshot ──
# The app caches its data for the life of the R process, so a new
# snapshot in the bucket stays invisible until the service starts new
# instances. We bump a harmless template annotation via the Cloud Run
# Admin API; that creates a new revision (new instances → a fresh
# load_app_data() that reads the just-uploaded snapshot). updateMask is
# scoped to template.annotations, so env vars, secrets, image, and port
# are left untouched. Auth reuses the metadata-server token (gcs_token).
roll_app_service <- function(service = Sys.getenv("APP_SERVICE", "adfc-ma-bike-counter"),
                             project = Sys.getenv("GCP_PROJECT", "adfc-483617"),
                             region  = Sys.getenv("GCP_REGION",  "europe-west3")) {
  url <- sprintf(
    "https://run.googleapis.com/v2/projects/%s/locations/%s/services/%s?updateMask=template.annotations",
    project, region, service
  )
  body <- jsonlite::toJSON(list(template = list(annotations = list(
    `bike-refresh-ts` = as.character(as.integer(Sys.time()))
  ))), auto_unbox = TRUE)
  res <- httr::PATCH(
    url,
    httr::add_headers(Authorization = paste("Bearer", gcs_token()),
                      `Content-Type` = "application/json"),
    body = body,
    httr::timeout(90)
  )
  if (!httr::status_code(res) %in% c(200L, 201L)) {
    stop("HTTP ", httr::status_code(res), ": ",
         rawToChar(httr::content(res, as = "raw")))
  }
  invisible(TRUE)
}

started <- Sys.time()
message("=== Snapshot-Refresh gestartet: ",
        format(started, "%d.%m.%Y %H:%M:%S", tz = "Europe/Berlin"), " ===")

# 1. Pull the full dataset from Databricks.
df <- fetch_bike_from_databricks()

# 2. Sanity guard — never overwrite a good snapshot with an empty result.
if (is.null(df) || nrow(df) == 0) {
  stop("Abbruch: 0 Zeilen von Databricks erhalten — Snapshot NICHT überschrieben.")
}

# 3. Save locally, then upload to GCS.
tmp <- tempfile(fileext = ".rds")
saveRDS(df, tmp)
message("Snapshot lokal gespeichert (",
        round(file.size(tmp) / 1e6, 2), " MB). Lade nach GCS hoch …")

gcs_upload(tmp)   # -> gs://GCS_BUCKET/GCS_OBJECT

elapsed <- round(as.numeric(difftime(Sys.time(), started, units = "secs")), 1)
message("✓ Hochgeladen: gs://", GCS_BUCKET, "/", GCS_OBJECT,
        " | Zeilen: ", nrow(df),
        " | Datenstand: ", format(max(df$date, na.rm = TRUE), "%d.%m.%Y"),
        " | Dauer: ", elapsed, "s")

# 4. Roll the app so it picks up the new snapshot (best-effort).
#    If this fails, the snapshot is still uploaded and current — the app
#    just won't refresh until its next restart — so we warn, not fail.
message("Starte App-Service neu, damit er den neuen Snapshot liest …")
tryCatch({
  roll_app_service()
  message("✓ App-Service neu ausgerollt — neue Instanzen lesen den frischen Snapshot.")
}, error = function(e) {
  message("⚠ App-Redeploy übersprungen (Snapshot ist trotzdem aktuell): ",
          conditionMessage(e))
})

message("=== Refresh fertig ===")
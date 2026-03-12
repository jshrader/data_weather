#!/usr/bin/env Rscript
# Download PRISM daily grids via the new web service API.
# Source: PRISM Gridded Dataset Downloads via Web Service (updated 26 Mar 2025).

rm(list = ls())

# -----------------------------
# Configuration (edit as needed)
# -----------------------------
config <- list(
  variables    = c("ppt", "tmax", "tmin"), # ppt, tmin, tmax, tmean, tdmean, vpdmin, vpdmax
  region       = "us",                # us, ak, hi, pr (only us currently available)
  resolution   = "4km",               # 4km, 800m, 400m (400m not yet implemented)
  start_date   = "1981-01-01",
  end_date     = "2025-12-31",
  format       = NULL,                # NULL for COG default, or "nc", "asc", "bil"
  dataset_type = NULL,                # use "lt" for 800m monthly LT; not used for daily
  sleep_sec    = 1,
  download_method = "libcurl",        # "libcurl" recommended on servers
  max_retries  = 5,                   # number of retry attempts per file
  backoff_base = 2,                   # base seconds for exponential backoff
  backoff_max  = 60,                  # max sleep between retries
  skip_existing = TRUE,
  data_root    = Sys.getenv("PRISM_DATA_ROOT", unset = NA)
)

# -----------------------------
# Helpers
# -----------------------------
get_script_dir <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  match <- grep(file_arg, cmd_args)
  if (length(match) > 0) {
    return(dirname(sub(file_arg, "", cmd_args[match[1]])))
  }
  return(getwd())
}

get_project_root <- function() {
  normalizePath(file.path(get_script_dir(), ".."), mustWork = FALSE)
}

log_msg <- function(..., log_file) {
  ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  msg <- paste(..., collapse = "")
  cat(ts, msg, "\n", file = log_file, append = TRUE)
}

download_with_retry <- function(url, destfile, cfg, log_file) {
  attempt <- 0
  repeat {
    attempt <- attempt + 1
    status <- tryCatch(
      download.file(
        url,
        destfile = destfile,
        mode = "wb",
        quiet = TRUE,
        method = cfg$download_method
      ),
      error = function(e) {
        log_msg("ERROR ", url, " :: ", conditionMessage(e), log_file = log_file)
        NA_integer_
      },
      warning = function(w) {
        log_msg("WARN  ", url, " :: ", conditionMessage(w), log_file = log_file)
        invokeRestart("muffleWarning")
      }
    )

    if (!is.na(status) && status == 0) {
      return(TRUE)
    }

    if (attempt >= cfg$max_retries) {
      log_msg("FAIL  ", url, " after ", attempt, " attempts", log_file = log_file)
      return(FALSE)
    }

    backoff <- min(cfg$backoff_max, cfg$backoff_base * 2^(attempt - 1))
    jitter <- runif(1, min = 0, max = 0.5 * backoff)
    sleep_for <- backoff + jitter
    log_msg(
      "RETRY ", url, " attempt ", attempt + 1,
      " in ", sprintf("%.1f", sleep_for), "s",
      log_file = log_file
    )
    Sys.sleep(sleep_for)
  }
}
build_url <- function(var, date, cfg) {
  if (!inherits(date, "Date")) {
    date <- as.Date(date)
  }
  if (is.na(date)) {
    stop("Invalid date passed to build_url")
  }
  date_str <- format(date, "%Y%m%d")
  url <- sprintf("https://services.nacse.org/prism/data/get/%s/%s/%s/%s",
    cfg$region, cfg$resolution, var, date_str
  )
  if (!is.null(cfg$dataset_type)) {
    url <- paste0(url, "/", cfg$dataset_type)
  }
  if (!is.null(cfg$format)) {
    url <- paste0(url, "?format=", cfg$format)
  }
  url
}

build_filename <- function(var, date, cfg) {
  if (!inherits(date, "Date")) {
    date <- as.Date(date)
  }
  if (is.na(date)) {
    stop("Invalid date passed to build_filename")
  }
  date_str <- format(date, "%Y%m%d")
  sprintf("prism_%s_%s_%s_%s.zip", var, cfg$region, cfg$resolution, date_str)
}

# -----------------------------
# Validation & setup
# -----------------------------
if (is.na(config$data_root)) {
  config$data_root <- file.path(get_project_root(), "data", "prism", "remote")
}

if (!inherits(config$start_date, "Date")) {
  config$start_date <- as.Date(config$start_date)
}
if (!inherits(config$end_date, "Date")) {
  config$end_date <- as.Date(config$end_date)
}
if (is.na(config$start_date) || is.na(config$end_date)) {
  stop("start_date and end_date must be coercible to Date (e.g., \"YYYY-MM-DD\")")
}

if (config$end_date < config$start_date) {
  stop("end_date must be on or after start_date")
}

valid_vars <- c("ppt", "tmin", "tmax", "tmean", "tdmean", "vpdmin", "vpdmax")
if (!all(config$variables %in% valid_vars)) {
  stop("Invalid variable in config$variables. Valid: ", paste(valid_vars, collapse = ", "))
}

valid_regions <- c("us", "ak", "hi", "pr")
if (!(config$region %in% valid_regions)) {
  stop("Invalid region. Valid: ", paste(valid_regions, collapse = ", "))
}

valid_res <- c("4km", "800m", "400m")
if (!(config$resolution %in% valid_res)) {
  stop("Invalid resolution. Valid: ", paste(valid_res, collapse = ", "))
}

valid_formats <- c("nc", "asc", "bil")
if (!is.null(config$format) && !(config$format %in% valid_formats)) {
  stop("Invalid format. Valid: ", paste(valid_formats, collapse = ", "))
}

log_file <- file.path(get_project_root(), "code", "log", "PRISM0_download_daily_prism.log")
if (!dir.exists(dirname(log_file))) {
  dir.create(dirname(log_file), recursive = TRUE)
}

options(timeout = 600)

# -----------------------------
# Download loop
# -----------------------------
all_dates <- seq.Date(config$start_date, config$end_date, by = "day")
total_steps <- length(config$variables) * length(all_dates)
step_idx <- 0
pb <- txtProgressBar(min = 0, max = total_steps, style = 3)

for (var in config$variables) {
  print(paste0("Running code for ", var))
  out_dir <- file.path(config$data_root, "prism_daily", var, "zip")
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }

  for (d in all_dates) {
    step_idx <- step_idx + 1
    setTxtProgressBar(pb, step_idx)
    url <- build_url(var, d, config)
    fname <- build_filename(var, d, config)
    destfile <- file.path(out_dir, fname)

    if (config$skip_existing && file.exists(destfile)) {
      log_msg("SKIP ", destfile, log_file = log_file)
      next
    }

    log_msg("GET ", url, " -> ", destfile, log_file = log_file)

    ok <- download_with_retry(url, destfile, config, log_file)

    if (!ok && file.exists(destfile)) {
      file.remove(destfile)
    }

    Sys.sleep(config$sleep_sec)
  }
}

close(pb)

# EOF

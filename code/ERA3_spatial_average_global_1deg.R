suppressPackageStartupMessages({
  library(data.table)
  library(terra)
})

parse_args <- function(defaults = list(), flags = character()) {
  raw <- commandArgs(trailingOnly = TRUE)
  args <- defaults
  i <- 1L
  while (i <= length(raw)) {
    key <- raw[[i]]
    if (!startsWith(key, "--")) {
      stop(sprintf("Unexpected positional argument: %s", key), call. = FALSE)
    }
    key <- sub("^--", "", key)
    if (key %in% flags) {
      args[[key]] <- TRUE
      i <- i + 1L
      next
    }
    i <- i + 1L
    values <- character()
    while (i <= length(raw) && !startsWith(raw[[i]], "--")) {
      values <- c(values, raw[[i]])
      i <- i + 1L
    }
    args[[key]] <- if (length(values) == 0L) TRUE else values[[1]]
  }
  args
}

days_in_month_scalar <- function(year, month) {
  start <- as.Date(sprintf("%04d-%02d-01", year, month))
  next_month <- seq(start, by = "month", length.out = 2)[2]
  as.integer(format(next_month - 1, "%d"))
}

project_grid <- function() {
  rast(
    xmin = -180,
    xmax = 180,
    ymin = -90,
    ymax = 90,
    resolution = 1,
    crs = "EPSG:4326"
  )
}

zero_raster <- function(template) {
  init(template, fun = 0)
}

grid_frame <- function() {
  out <- as.data.table(expand.grid(
    lon = seq(-179.5, 179.5, by = 1),
    lat = seq(89.5, -89.5, by = -1)
  ))
  out[, `:=`(lon = round(lon, 6), lat = round(lat, 6))]
  out
}

write_note <- function(path, lines) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(lines, con = path, useBytes = TRUE)
}

write_csv_gz <- function(dt, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- sub("\\.gz$", "", path)
  fwrite(dt, tmp)
  if (nzchar(Sys.which("gzip"))) {
    system2(Sys.which("gzip"), c("-f", tmp))
  } else {
    stop("gzip is required to create .csv.gz outputs.", call. = FALSE)
  }
}

append_csv_chunk <- function(dt, path, append = FALSE) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  fwrite(dt, path, append = append, col.names = !append)
}

gzip_existing_csv <- function(csv_path) {
  if (!file.exists(csv_path)) {
    stop(sprintf("Cannot gzip missing file: %s", csv_path), call. = FALSE)
  }
  if (nzchar(Sys.which("gzip"))) {
    system2(Sys.which("gzip"), c("-f", csv_path))
  } else {
    stop("gzip is required to create .csv.gz outputs.", call. = FALSE)
  }
}

open_monthly_means <- function(path) {
  if (!file.exists(path)) {
    stop(sprintf("Missing NetCDF file: %s", path), call. = FALSE)
  }
  r <- rast(path)
  if (ext(r)[2] > 180) {
    r <- rotate(r)
  }
  r
}

infer_temp_to_celsius <- function(r) {
  vals <- global(r[[1]], "mean", na.rm = TRUE)[1, 1]
  if (is.na(vals)) {
    stop("Could not inspect temperature units.", call. = FALSE)
  }
  if (vals > 150) {
    return(r - 273.15)
  }
  r
}

monthly_precip_to_mm <- function(r, years, months, mode = c("mean_daily_m", "monthly_total_m")) {
  mode <- match.arg(mode)
  out <- r
  for (i in seq_len(nlyr(r))) {
    multiplier <- if (identical(mode, "mean_daily_m")) {
      days_in_month_scalar(years[[i]], months[[i]]) * 1000
    } else {
      1000
    }
    out[[i]] <- r[[i]] * multiplier
  }
  out
}

layer_calendar <- function(r) {
  layer_time <- time(r)
  if (is.null(layer_time) || all(is.na(layer_time))) {
    layer_depth <- depth(r)
    if (!is.null(layer_depth) && length(layer_depth) == nlyr(r) && all(!is.na(layer_depth))) {
      layer_time <- as.POSIXct(layer_depth, origin = "1970-01-01", tz = "UTC")
    } else {
      layer_names <- names(r)
      seconds_since_epoch <- as.numeric(sub(".*=", "", layer_names))
      if (length(seconds_since_epoch) != nlyr(r) || anyNA(seconds_since_epoch)) {
        stop("Input raster has no usable time or depth metadata.", call. = FALSE)
      }
      layer_time <- as.POSIXct(seconds_since_epoch, origin = "1970-01-01", tz = "UTC")
    }
  }
  layer_date <- as.Date(layer_time)
  data.table(
    layer = seq_len(nlyr(r)),
    year = as.integer(format(layer_date, "%Y")),
    month = as.integer(format(layer_date, "%m"))
  )
}

monthly_panel_chunk <- function(temp_layer, precip_layer, year, month) {
  dt <- as.data.table(as.data.frame(c(temp_layer, precip_layer), xy = TRUE, na.rm = FALSE))
  setnames(dt, c("x", "y"), c("lon", "lat"))
  setnames(dt, 3:4, c("era5_temp_mean_c", "era5_precip_total_mm"))
  dt[, `:=`(
    year = as.integer(year),
    month = as.integer(month),
    lat = round(lat, 6),
    lon = round(lon, 6)
  )]
  setcolorder(dt, c("year", "month", "lat", "lon", "era5_temp_mean_c", "era5_precip_total_mm"))
  dt
}

annual_panel_chunk <- function(temp_layer, precip_layer, year, hot_days_layer = NULL) {
  if (is.null(hot_days_layer)) {
    dt <- as.data.table(as.data.frame(c(temp_layer, precip_layer), xy = TRUE, na.rm = FALSE))
    setnames(dt, c("x", "y"), c("lon", "lat"))
    setnames(dt, 3:4, c("era5_temp_mean_c", "era5_precip_total_mm"))
  } else {
    dt <- as.data.table(as.data.frame(c(temp_layer, precip_layer, hot_days_layer), xy = TRUE, na.rm = FALSE))
    setnames(dt, c("x", "y"), c("lon", "lat"))
    setnames(dt, 3:5, c("era5_temp_mean_c", "era5_precip_total_mm", "era5_hot_days_30c"))
  }
  dt[, `:=`(
    year = as.integer(year),
    lat = round(lat, 6),
    lon = round(lon, 6),
    era5_temp_sq = era5_temp_mean_c^2,
    era5_precip_sq = era5_precip_total_mm^2
  )]
  base_cols <- c(
    "year", "lat", "lon",
    "era5_temp_mean_c", "era5_precip_total_mm",
    "era5_temp_sq", "era5_precip_sq"
  )
  if ("era5_hot_days_30c" %in% names(dt)) {
    base_cols <- c(base_cols, "era5_hot_days_30c")
  }
  setcolorder(dt, base_cols)
  dt
}

validate_panel <- function(dt, key_cols, expected_rows = NULL) {
  if (nrow(dt) == 0L) {
    stop("Output panel is empty.", call. = FALSE)
  }
  if (anyDuplicated(dt[, ..key_cols]) > 0L) {
    stop(sprintf("Duplicate rows detected for keys: %s", paste(key_cols, collapse = ", ")), call. = FALSE)
  }
  if (!is.null(expected_rows) && nrow(dt) != expected_rows) {
    stop(sprintf("Expected %s rows, found %s.", expected_rows, nrow(dt)), call. = FALSE)
  }
}

monthly_means_pipeline <- function(args) {
  target <- project_grid()
  expected_grid <- grid_frame()

  temp_native <- open_monthly_means(args[["monthly-temp-file"]])
  precip_native <- open_monthly_means(args[["monthly-precip-file"]])
  calendar <- layer_calendar(temp_native)

  if (nlyr(temp_native) != nlyr(precip_native)) {
    stop("Temperature and precipitation files have different numbers of layers.", call. = FALSE)
  }

  calendar <- calendar[year >= as.integer(args[["start-year"]]) & year <= as.integer(args[["end-year"]])]
  if (nrow(calendar) == 0L) {
    stop("No monthly layers remain after filtering the requested years.", call. = FALSE)
  }

  temp_native <- temp_native[[calendar$layer]]
  precip_native <- precip_native[[calendar$layer]]
  calendar[, layer := seq_len(.N)]

  temp_native <- infer_temp_to_celsius(temp_native)
  annual_years <- sort(unique(calendar$year))
  annual_temp_sum <- setNames(lapply(annual_years, function(...) zero_raster(target)), as.character(annual_years))
  annual_precip_sum <- setNames(lapply(annual_years, function(...) zero_raster(target)), as.character(annual_years))
  annual_weights <- setNames(as.list(rep(0, length(annual_years))), as.character(annual_years))

  if (nzchar(args[["output-monthly"]])) {
    monthly_csv <- sub("\\.gz$", "", args[["output-monthly"]])
    if (file.exists(monthly_csv)) {
      unlink(monthly_csv)
    }
    gz_monthly <- args[["output-monthly"]]
    if (file.exists(gz_monthly)) {
      unlink(gz_monthly)
    }
  }

  for (i in seq_len(nrow(calendar))) {
    year_value <- calendar$year[[i]]
    month_value <- calendar$month[[i]]
    year_key <- as.character(year_value)
    day_weight <- days_in_month_scalar(year_value, month_value)

    temp_layer <- resample(temp_native[[i]], target, method = "average")
    precip_multiplier <- if (identical(args[["monthly-precip-mode"]], "mean_daily_m")) {
      day_weight * 1000
    } else {
      1000
    }
    precip_layer <- resample(precip_native[[i]] * precip_multiplier, target, method = "average")

    annual_temp_sum[[year_key]] <- annual_temp_sum[[year_key]] + temp_layer * day_weight
    annual_precip_sum[[year_key]] <- annual_precip_sum[[year_key]] + precip_layer
    annual_weights[[year_key]] <- annual_weights[[year_key]] + day_weight

    if (nzchar(args[["output-monthly"]])) {
      monthly_chunk <- monthly_panel_chunk(
        temp_layer = temp_layer,
        precip_layer = precip_layer,
        year = year_value,
        month = month_value
      )
      append_csv_chunk(monthly_chunk, monthly_csv, append = i > 1L)
    }
  }

  if (nzchar(args[["output-monthly"]])) {
    gzip_existing_csv(monthly_csv)
  }

  annual_chunks <- vector("list", length = length(annual_years))
  for (i in seq_along(annual_years)) {
    year_value <- annual_years[[i]]
    year_key <- as.character(year_value)
    annual_temp <- annual_temp_sum[[year_key]] / annual_weights[[year_key]]
    annual_precip <- annual_precip_sum[[year_key]]
    annual_chunks[[i]] <- annual_panel_chunk(annual_temp, annual_precip, year_value)
  }

  annual_panel <- rbindlist(annual_chunks)
  validate_panel(
    annual_panel,
    key_cols = c("year", "lat", "lon"),
    expected_rows = length(annual_years) * nrow(expected_grid)
  )
  write_csv_gz(annual_panel, args[["output-annual"]])

  write_note(
    args[["output-note"]],
    c(
      "ERA5 1-degree weather controls for the encroachment project",
      "",
      sprintf("Source product: %s", args[["dataset-name"]]),
      "Source variables: 2m_temperature, total_precipitation",
      sprintf("Temporal coverage: %s-%s", min(annual_years), max(annual_years)),
      "Target grid: EPSG:4326, 1-degree cells, lon centers -179.5..179.5, lat centers 89.5..-89.5",
      "Spatial aggregation: terra::resample(..., method = \"average\") from the native 0.25-degree ERA5 grid to the 1-degree project grid",
      "Temperature aggregation: monthly ERA5 means -> spatial average to 1-degree -> annual mean weighted by days in month",
      sprintf(
        "Precipitation aggregation: monthly ERA5 mean daily precipitation (%s) -> convert to monthly totals in mm using calendar-day weights -> spatial average to 1-degree -> annual sum",
        args[["monthly-precip-mode"]]
      ),
      "Additional annual columns: era5_temp_sq, era5_precip_sq",
      "Missing coverage: none within the requested year range"
    )
  )
}

hourly_world_pipeline <- function(args) {
  target <- project_grid()
  expected_grid <- grid_frame()
  years <- as.integer(args[["start-year"]]):as.integer(args[["end-year"]])
  annual_chunks <- vector("list", length(years))
  monthly_chunks <- list()
  monthly_index <- 1L

  for (y in years) {
    temp_weighted_sum <- zero_raster(target)
    precip_year_sum <- zero_raster(target)
    hot_days_year_sum <- zero_raster(target)
    total_hours <- 0

    for (m in 1:12) {
      stamp <- sprintf("%04d%02d", y, m)
      temp_file <- file.path(args[["hourly-temp-dir"]], sprintf("era5_%s.grib", stamp))
      precip_file <- file.path(args[["hourly-precip-dir"]], sprintf("era5_%s.grib", stamp))
      if (!file.exists(temp_file) || !file.exists(precip_file)) {
        next
      }

      temp_native <- rast(temp_file)
      precip_native <- rast(precip_file)

      temp_month <- resample(mean(temp_native), target, method = "average")
      precip_month <- resample(sum(precip_native) * 1000, target, method = "average")

      if (nzchar(args[["output-monthly"]])) {
        monthly_chunks[[monthly_index]] <- monthly_panel_chunk(temp_month, precip_month, y, m)
        monthly_index <- monthly_index + 1L
      }

      temp_weighted_sum <- temp_weighted_sum + temp_month * nlyr(temp_native)
      precip_year_sum <- precip_year_sum + precip_month
      total_hours <- total_hours + nlyr(temp_native)

      if (isTRUE(args[["include-hot-days"]])) {
        daily_index <- as.character(as.Date(time(temp_native)))
        daily_max <- tapp(temp_native, daily_index, fun = max)
        hot_days_year_sum <- hot_days_year_sum + resample(sum(daily_max > 30), target, method = "average")
      }
    }

    annual_temp <- temp_weighted_sum / total_hours
    annual_chunks[[which(years == y)]] <- annual_panel_chunk(
      annual_temp,
      precip_year_sum,
      y,
      if (isTRUE(args[["include-hot-days"]])) hot_days_year_sum else NULL
    )
  }

  annual_panel <- rbindlist(annual_chunks, fill = TRUE)
  validate_panel(
    annual_panel,
    key_cols = c("year", "lat", "lon"),
    expected_rows = length(years) * nrow(expected_grid)
  )
  write_csv_gz(annual_panel, args[["output-annual"]])

  if (nzchar(args[["output-monthly"]]) && length(monthly_chunks) > 0L) {
    monthly_panel <- rbindlist(monthly_chunks)
    validate_panel(
      monthly_panel,
      key_cols = c("year", "month", "lat", "lon"),
      expected_rows = length(monthly_chunks) * nrow(expected_grid)
    )
    write_csv_gz(monthly_panel, args[["output-monthly"]])
  }

  missing_coverage <- if (length(years) > 0L && min(years) > 2005) {
    "none within the requested year range"
  } else {
    "local hourly world archive currently observed on this machine starts in 2005-01"
  }

  write_note(
    args[["output-note"]],
    c(
      "ERA5 1-degree weather controls for the encroachment project",
      "",
      "Source product: reanalysis-era5-single-levels (hourly reanalysis GRIB archive)",
      "Source variables: 2m_temperature, total_precipitation",
      sprintf("Temporal coverage: %s-%s", min(years), max(years)),
      "Target grid: EPSG:4326, 1-degree cells, lon centers -179.5..179.5, lat centers 89.5..-89.5",
      "Spatial aggregation: terra::resample(..., method = \"average\") from the native 0.25-degree ERA5 grid to the 1-degree project grid",
      "Temperature aggregation: hourly ERA5 -> monthly mean -> spatial average to 1-degree -> annual mean weighted by number of hourly layers",
      "Precipitation aggregation: hourly ERA5 total precipitation -> monthly sum at the native grid -> convert to mm -> spatial average to 1-degree -> annual sum",
      if (isTRUE(args[["include-hot-days"]])) {
        "Hot-day aggregation: hourly ERA5 -> daily maximum temperature at the native grid -> count days with Tmax > 30C -> spatial average to 1-degree -> annual sum"
      } else {
        "Additional annual columns: era5_temp_sq, era5_precip_sq"
      },
      sprintf("Missing coverage: %s", missing_coverage)
    )
  )
}

args <- parse_args(
  defaults = list(
    `source-mode` = "monthly-means",
    `dataset-name` = "reanalysis-era5-single-levels-monthly-means",
    `monthly-temp-file` = "/media/jgs/datadrive/data/weather/ecmwf/era5_monthly_means_world/era5_monthly_means_2m_temperature_1992_2020.nc",
    `monthly-precip-file` = "/media/jgs/datadrive/data/weather/ecmwf/era5_monthly_means_world/era5_monthly_means_total_precipitation_1992_2020.nc",
    `monthly-precip-mode` = "mean_daily_m",
    `hourly-temp-dir` = "/media/jgs/databackup/era5/2m_temperature_reanalysis_world",
    `hourly-precip-dir` = "/media/jgs/databackup/era5/total_precipitation_reanalysis_world",
    `start-year` = "1992",
    `end-year` = "2020",
    `output-annual` = "/home/jgs/Dropbox/research/projects/active/encroachment/data/interim/era5/era5_weather_annual_1deg.csv.gz",
    `output-monthly` = "/home/jgs/Dropbox/research/projects/active/encroachment/data/interim/era5/era5_weather_monthly_1deg.csv.gz",
    `output-note` = "/home/jgs/Dropbox/research/projects/active/encroachment/data/interim/era5/era5_weather_1deg_metadata.txt",
    `include-hot-days` = FALSE
  ),
  flags = c("include-hot-days")
)

if (!args[["source-mode"]] %in% c("monthly-means", "hourly-world")) {
  stop("--source-mode must be one of: monthly-means, hourly-world", call. = FALSE)
}

terraOptions(progress = 1)

if (identical(args[["source-mode"]], "monthly-means")) {
  monthly_means_pipeline(args)
} else {
  hourly_world_pipeline(args)
}

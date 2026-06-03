suppressPackageStartupMessages({
  library(data.table)
  library(terra)
})

parse_args <- function(defaults = list()) {
  raw <- commandArgs(trailingOnly = TRUE)
  args <- defaults
  i <- 1L
  while (i <= length(raw)) {
    key <- raw[[i]]
    if (!startsWith(key, "--")) {
      stop(sprintf("Unexpected positional argument: %s", key), call. = FALSE)
    }
    key <- sub("^--", "", key)
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

args <- parse_args(
  defaults = list(
    `output-grid` = "output/era5/project_grid_1deg.csv.gz",
    `output-raster` = "output/era5/project_grid_1deg.tif"
  )
)

target <- rast(
  xmin = -180,
  xmax = 180,
  ymin = -90,
  ymax = 90,
  resolution = 1,
  crs = "EPSG:4326"
)
values(target) <- NA_real_

grid <- as.data.table(crds(target, df = TRUE, na.rm = FALSE))
setnames(grid, c("x", "y"), c("lon", "lat"))
grid[, `:=`(lon = round(lon, 6), lat = round(lat, 6))]

expected <- as.data.table(expand.grid(
  lon = seq(-179.5, 179.5, by = 1),
  lat = seq(89.5, -89.5, by = -1)
))
expected[, `:=`(lon = round(lon, 6), lat = round(lat, 6))]

if (!setequal(grid, expected) || nrow(grid) != nrow(expected)) {
  stop("Generated grid does not match the expected 1-degree project grid.", call. = FALSE)
}

dir.create(dirname(args[["output-grid"]]), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(args[["output-raster"]]), recursive = TRUE, showWarnings = FALSE)

fwrite(grid, args[["output-grid"]])
writeRaster(target, args[["output-raster"]], overwrite = TRUE)

cat(sprintf("Wrote %s\n", normalizePath(args[["output-grid"]], winslash = "/", mustWork = FALSE)))
cat(sprintf("Wrote %s\n", normalizePath(args[["output-raster"]], winslash = "/", mustWork = FALSE)))

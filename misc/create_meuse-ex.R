library(rgeedim)
library(terra)

# augments classic `meuse` datasets from {sp} package
data(meuse, package = "sp")
data(meuse.grid, package = "sp")

predictors <- rast(meuse.grid, crs = "EPSG:28992")
samples <- vect(meuse, geom = c("x", "y"), crs = "EPSG:28992")
samples <- samples[, names(samples)[!names(samples) %in% names(predictors)]]

gd_initialize()

r <- terra::as.polygons(predictors, ext = TRUE)
x <- "CGIAR/SRTM90_V4" |>
  gd_image_from_id() |>
  gd_download(filename = "dem.tif",
              region = project(r, "OGC:CRS84"),
              scale = 90,
              crs = "EPSG:28992") |>
  rast() |>
  subset(1)|>
  project(predictors) |>
  crop(predictors)
pr2 <- terrain(x, c("slope"))#, "TPI", "TRI", "roughness", "flowdir"))
predictors <- c(predictors, x, pr2)

samples <- cbind(samples, extract(predictors, samples))
writeVector(samples, "inst/extdata/meuse.gpkg", overwrite = TRUE)
writeRaster(predictors, "inst/extdata/meuse.grid.ex.tif", overwrite = TRUE)

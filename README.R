## ---- include = FALSE----------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "man/figures/README-",
  out.width = "100%",
  eval = requireNamespace("sp", quietly = TRUE)
)


## ----example-------------------------------------------------------------------------------------------------------------
library(grundo)
library(terra)

samples <- vect(system.file("extdata", "meuse.gpkg", package = "grundo"))
predictors <- rast(system.file("extdata", "meuse.grid.ex.tif", package = "grundo"))

## to use classic `meuse` datasets from {sp} package
# data(meuse, package = "sp")
# data(meuse.grid, package = "sp")
# predictors <- rast(meuse.grid, crs = "EPSG:28992")
# samples <- vect(meuse, geom = c("x", "y"), crs = "EPSG:28992")


## ----dependent-var-------------------------------------------------------------------------------------------------------
variable <- "om"


## ----predictors-and-samples1---------------------------------------------------------------------------------------------
# remove observations that are missing organic matter %
samples <- samples[!is.na(samples[[variable]][[1]]), ]

# visual inspection
plot(predictors[[3]])
points(samples, pch = "+", cex = 1)


## ----predictors-and-samples2---------------------------------------------------------------------------------------------
names(predictors)

names(samples)


## ----cubist-meuse--------------------------------------------------------------------------------------------------------
# cubist with {Cubist} + OK residuals
x <- grundo(
  predictors = predictors,
  samples = samples,
  variable = variable,
  model = "cubist",
  vgm_model = "Exp"
)

plot(x$result)
plot(sum(x$result[[1:2]]))


## ----ranger-meuse--------------------------------------------------------------------------------------------------------
# random forest with {ranger} + OK residuals
y <- grundo(
  predictors = predictors,
  samples = samples,
  variable = variable,
  model = "ranger",
  vgm_model = "Exp"
)

plot(y$result)
plot(sum(y$result[[1:2]]))


## ----meuse-ensemble------------------------------------------------------------------------------------------------------
plot(mean(c(sum(x$result[[1:2]]), sum(y$result[[1:2]]))))


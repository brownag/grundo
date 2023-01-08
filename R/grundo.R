#' grundo
#'
#' grundo mundo
#'
#' @param predictors `SpatRaster` (or coercible to one)
#' @param samples `SpatVector` (or coercible to one)
#' @param variable `character`. Observed variable name (in `samples`)
#' @param model Primary model selection choice. Includes: `"cubist"` (default), `"ranger"` ...
#' @param vgm_model Variogram model selection choice for ordinary kriging of primary model residuals, see [gstat::vgm()] for details
#'
#' @return a `list` object of class `"grundo"`
#' @export
#' @importFrom terra rast vect crs extract interpolate crds
#' @importFrom gstat krige vgm variogram fit.variogram
#' @importFrom stats as.formula complete.cases model.frame na.pass
grundo <- function(predictors, samples, variable,
                   model = "cubist",
                   vgm_model = vgm_models()) {

  if (!requireNamespace("sf")) {
    stop("Package 'sf' is required.", call. = FALSE)
  }

  if (!requireNamespace("stars")) {
    stop("Package 'sf' is required.", call. = FALSE)
  }

  if (!inherits(predictors, 'SpatRaster')) {
    predictors <- terra::rast(predictors)
  }

  if (!inherits(samples, 'SpatVector')) {
    samples <- terra::vect(samples)
  }

  # assume samples are in same CRS as predictors if undefined
  if (terra::crs(samples) == "") {
    terra::crs(samples) <- terra::crs(predictors)
  }

  # empty result template and var names
  result <- rast(predictors[[1]])
  var_pred <- paste0(variable, "_pred")
  var_resid <- paste0(variable, "_resid")

  # extract predictor variables
  x0 <- terra::extract(predictors, samples)[-1]

  # model and ordinary kriging formulas
  fm <- as.formula(paste0(variable, " ~ ", paste(colnames(x0), collapse = " + ")))
  rfm <- as.formula(paste0(var_resid, " ~ 1"))

  # construct model frame data.frame(y, x1, x2, ...)
  mf <- model.frame(fm, cbind(samples, x0), na.action = na.pass)

  # fit a model
  m0 <- switch(tolower(trimws(model)),
                 "cubist" = gr_cubist(mf, variable),
                 "ranger" = gr_ranger(mf, variable)
               )

  # interpolate() calls <package>::predict() on SpatRaster by default
  result[[var_pred]] <- terra::interpolate(predictors, m0, na.rm = TRUE)[[1]]

  # construct calibration data.frame
  calibration <- mf[variable]

  # extract prediction
  calibration[[var_pred]] <- terra::extract(result[[var_pred]], samples)[[var_pred]]

  # calculate residuals
  calibration[[var_resid]] <- (calibration[[variable]] - calibration[[var_pred]])

  # combine calibration results from model with coordinates of samples for kriging
  calvect <- cbind(as.data.frame(calibration), terra::crds(samples))
  krige_cal_data <- sf::st_as_sf(calvect[complete.cases(calvect),],
                                 coords = c("x","y"),
                                 crs = terra::crs(predictors))
  # create XY grid for kriging
  krige_grid <- stars::st_as_stars(predictors[[1]])

  # fit variogram
  var_vgm <- gstat::fit.variogram(gstat::variogram(rfm, data = krige_cal_data),
                                  gstat::vgm(vgm_model))
  var_krig <- gstat::krige(
    rfm,
    locations = krige_cal_data,
    newdata = krige_grid,
    model = var_vgm
  )

  # kriging residuals
  result[[var_resid]] <- terra::rast(var_krig)[[1]]

  res <- list(
    result = result,
    calibration = calibration,
    model = m0,
    variogram = var_vgm,
    kriging = var_krig
  )
  class(res) <- c('grundo', 'list')
  res
}

#' @export
#' @importFrom methods show
print.grundo <- function(x, ...) {
  show(x$result)
}

#' Variogram Model Types
#'
#' Get choices for variogram model types for use with `grundo()` `vgm_model` argument.
#' @seealso [grundo()]
#' @return character. Vector of variogram model type abbreviations from [gstat::vgm()]
#' @export
#' @importFrom gstat vgm
vgm_models <- function() {
  gstat::vgm()
}

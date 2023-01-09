#' Support Vector Machine/Regression Model (`e1071`)
#'
#' Wrapper method for fitting a `e1071::svm()` model on standard model frame.
#'
#' @param mf `data.frame` containing predictors and Observation Column
#' @param variable `character` Observation Column name
#' @param predvars Optional: subset of predictor columns from `mf` to use in model fitting
#'
#' @return an object of class `"svm.formula"`, `"svm"` with attribute `"grundo_model"` `"e1071::svm"`
#' @export
gr_svm <- function(mf, variable, predvars = NULL) {
  if (!requireNamespace("e1071")) {
    stop("Package 'e1071' is required.", call. = FALSE)
  }

  if (length(predvars) == 0) {
    cn <- colnames(mf)
    predvars <- cn[!cn %in% variable]
  }

  if (length(predvars) > 1)
    message("NOTE: only first predictor is used in SVM")
  fm <- as.formula(paste0(variable, " ~ ", predvars[1]))
  res <- e1071::svm(fm, data = mf)
  attr(res, 'grundo_model') <- "e1071::svm"
  return(res)
}

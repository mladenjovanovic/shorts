shorts_model_fit <- function(model = NULL, observed, predicted, na.rm = FALSE) {
  resid <- predicted - observed

  # This is needed for the LOOCV option which doesn't return model
  # thus, RSE cannot be computed
  if(is.null(model)) {
    RSE <- NA
  } else {
    RSE = summary(model)$sigma
  }

  list(
    RSE = RSE,
    R_squared = stats::cor(observed, predicted)^2,
    minErr = min(resid, na.rm = na.rm),
    maxErr = max(resid, na.rm = na.rm),
    maxAbsErr = max(abs(resid), na.rm = na.rm),
    RMSE = sqrt(mean(resid^2, na.rm = na.rm)),
    MAE = mean(abs(resid), na.rm = na.rm),
    MAPE = 100 * mean(abs(resid / observed), na.rm = na.rm)
  )
}

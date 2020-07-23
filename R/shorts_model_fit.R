shorts_model_fit <- function(model, observed, predicted, na.rm = FALSE) {
  resid <- predicted - observed

  list(
    RSE = summary(model)$sigma,
    R_squared = stats::cor(observed, predicted)^2,
    minErr = min(resid, na.rm = na.rm),
    maxErr = max(resid, na.rm = na.rm),
    maxAbsErr = max(abs(resid), na.rm = na.rm),
    RMSE = sqrt(mean(resid^2, na.rm = na.rm)),
    MAE = mean(abs(resid), na.rm = na.rm),
    MAPE = 100 * mean(abs(resid / observed), na.rm = na.rm)
  )
}

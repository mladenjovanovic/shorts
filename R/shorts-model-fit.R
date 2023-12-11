shorts_model_fit <- function(observed, predicted, na.rm = FALSE) {
  resid <- observed - predicted
  resid_perc <- 100 * resid / observed

  list(
    R2 = stats::cor(observed, predicted)^2,
    meanErr = mean(resid, na.rm = na.rm),
    meanErr_perc = mean(resid_perc, na.rm = na.rm),
    minErr = min(resid, na.rm = na.rm),
    minErr_perc = min(resid_perc, na.rm = na.rm),
    maxErr = max(resid, na.rm = na.rm),
    maxErr_perc = max(resid_perc, na.rm = na.rm),
    maxAbsErr = max(abs(resid), na.rm = na.rm),
    maxAbsErr_perc = max(abs(resid_perc), na.rm = na.rm),
    RMSE = sqrt(mean(resid^2, na.rm = na.rm)),
    RMSE_perc = sqrt(mean(resid_perc^2, na.rm = na.rm)),
    MAE = mean(abs(resid), na.rm = na.rm),
    MAE_perc = mean(abs(resid_perc), na.rm = na.rm)
  )
}

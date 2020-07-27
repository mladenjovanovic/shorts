new_shorts_model <- function(parameters, model_fit, model, data, LOOCV = NULL) {
  shorts_model_object <- list(
    parameters = parameters,
    model_fit = model_fit,
    model = model,
    data = data,
    LOOCV = LOOCV
  )

  class(shorts_model_object) <- "shorts_model"
  return(shorts_model_object)
}


new_shorts_mixed_model <- function(parameters, model_fit, model, data, LOOCV = NULL) {
  shorts_mixed_model_object <- list(
    parameters = parameters,
    model_fit = model_fit,
    model = model,
    data = data,
    LOOCV = LOOCV
  )

  class(shorts_mixed_model_object) <- "shorts_mixed_model"
  return(shorts_mixed_model_object)
}

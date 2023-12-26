new_shorts_model <- function(data, model_info, model, parameters, corrections, predictions, model_fit, CV) {
  shorts_model_object <- list(
    data = data,
    model_info = model_info,
    model = model,
    parameters = parameters,
    corrections = corrections,
    predictions = predictions,
    model_fit = model_fit,
    CV = CV
  )

  class(shorts_model_object) <- "shorts_model"
  return(shorts_model_object)
}

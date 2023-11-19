new_shorts_model <- function(parameters, model_fit, model, data, CV = NULL) {
  shorts_model_object <- list(
    data = data,
    model = model,
    parameters = parameters,
    model_fit = model_fit,
    CV = CV
  )

  class(shorts_model_object) <- "shorts_model"
  return(shorts_model_object)
}

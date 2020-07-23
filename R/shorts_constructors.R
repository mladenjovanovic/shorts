new_shorts_model <- function(parameters, model_fit, model, data) {
  shorts_model_object <- list(
    parameters = parameters,
    model_fit = model_fit,
    model = model,
    data = data
  )

  class(shorts_model_object) <- "shorts_model"
  return(shorts_model_object)
}


new_shorts_mixed_model <- function(parameters, model_fit, model, data) {
  shorts_mixed_model_object <- list(
    parameters = parameters,
    model_fit = model_fit,
    model = model,
    data = data
  )

  class(shorts_mixed_model_object) <- "shorts_mixed_model"
  return(shorts_mixed_model_object)
}

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

new_fv_profile <- function(bodymass,
                           F0,
                           F0_rel,
                           V0,
                           Pmax,
                           Pmax_relative,
                           FV_slope,
                           RFmax_cutoff,
                           RFmax,
                           Drf,
                           RSE_FV,
                           RSE_Drf,
                           data) {
  fv_profile <- list(
    bodymass = bodymass,
    F0 = F0,
    F0_rel = F0_rel,
    V0 = V0,
    Pmax = Pmax,
    Pmax_relative = Pmax_relative,
    FV_slope = FV_slope,
    RFmax_cutoff = RFmax_cutoff,
    RFmax = RFmax,
    Drf = Drf,
    RSE_FV = RSE_FV,
    RSE_Drf = RSE_Drf,
    data = data
  )

  class(fv_profile) <- "shorts_fv_profile"
  return(fv_profile)
}

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
                           Pmax_rel,
                           FV_slope,
                           RFmax_cutoff,
                           RFmax,
                           Drf,
                           RSE_FV,
                           RSE_Drf,
                           F0_poly,
                           F0_poly_rel,
                           V0_poly,
                           Pmax_poly,
                           Pmax_poly_rel,
                           FV_slope_poly,
                           data) {
  fv_profile <- list(
    bodymass = bodymass,
    F0 = F0,
    F0_rel = F0_rel,
    V0 = V0,
    Pmax = Pmax,
    Pmax_rel = Pmax_rel,
    FV_slope = FV_slope,
    RFmax_cutoff = RFmax_cutoff,
    RFmax = RFmax,
    Drf = Drf,
    RSE_FV = RSE_FV,
    RSE_Drf = RSE_Drf,
    F0_poly = F0_poly,
    F0_poly_rel = F0_poly_rel,
    V0_poly = V0_poly,
    Pmax_poly = Pmax_poly,
    Pmax_poly_rel = Pmax_poly_rel,
    FV_slope_poly = FV_slope_poly,
    data = data
  )

  class(fv_profile) <- "shorts_fv_profile"
  return(fv_profile)
}

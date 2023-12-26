#' @rdname model_functions
#'
#' @description \code{model_tether} estimates short sprint parameters using distance-velocity trace
#'     (e.g., tether devices).
#' @export
#' @examples
#'
#' # Model Tether
#' df <- create_sprint_trace(MSS = 8, MAC = 6, time = seq(0, 6, 0.5))
#' m1 <- model_tether(distance = df$distance, velocity = df$velocity)
#' m1
#' plot(m1)
#'
model_tether <- function(distance,
                         velocity,
                         weights = 1,
                         CV = NULL,
                         use_observed_MSS = FALSE,
                         na.rm = FALSE,
                         ...) {

  # Estimation function
  model_func <- function(train, test, use_observed_MSS, ...) {
    param_start <- list(MSS = 7, MAC = 7)
    param_lower <- c(MSS = 0, MAC = 0)
    param_upper <- c(MSS = Inf, MAC = Inf)

    if (use_observed_MSS == TRUE) {
      observed_MSS <- max(train$velocity)

      param_start <- list(MSS = observed_MSS, MAC = 7)
      param_lower <- c(MSS = observed_MSS, MAC = 0)
      param_upper <- c(MSS = observed_MSS, MAC = Inf)
    }

    # Non-linear model
    model <- minpack.lm::nlsLM(
      velocity ~ predict_velocity_at_distance(distance, MSS, MAC),
      data = train,
      start = param_start,
      lower = param_lower,
      upper = param_upper,
      weights = train$weight,
      ...
    )

    # Parameters
    MSS <- stats::coef(model)[["MSS"]]
    MAC <- stats::coef(model)[["MAC"]]
    TAU <- MSS / MAC
    PMAX <- (MSS * MAC) / 4

    # Model fit
    pred_velocity <- stats::predict(model, newdata = data.frame(distance = test$distance))
    resid_velocity <- test$velocity - pred_velocity

    return(list(
      data = train,
      model_info = list(
        predictor = "distance",
        target = "velocity",
        use_observed_MSS = use_observed_MSS
      ),
      model = model,
      parameters = list(
        MSS = MSS,
        MAC = MAC,
        TAU = TAU,
        PMAX = PMAX
      ),
      corrections = NULL,
      predictions = list(
        .predictor = test$distance,
        .observed = test$velocity,
        .predicted = pred_velocity,
        .residual = resid_velocity
      )
    ))
  }

  model_sprint(
    df = data.frame(
      distance = distance,
      velocity = velocity,
      weight = weights
    ),
    use_observed_MSS = use_observed_MSS,
    CV = CV,
    na.rm = na.rm,
    model_func = model_func,
    ...
  )
}

#' @rdname model_functions
#' @description \code{model_tether_DC} estimates short sprint parameters using distance-velocity trace
#'     (e.g., tether devices) with additional distance correction \code{DC} parameter
#' @export
#' @examples
#'
#' # Model Tether with Distance Correction (DC)
#' df <- create_sprint_trace(MSS = 8, MAC = 6, time = seq(0.001, 6, 0.5), DC = 5)
#' m1 <- model_tether_DC(distance = df$distance, velocity = df$velocity)
#' m1
#' plot(m1)
#'
model_tether_DC <- function(distance,
                            velocity,
                            weights = 1,
                            CV = NULL,
                            use_observed_MSS = FALSE,
                            na.rm = FALSE,
                            ...) {

  # Estimation function
  model_func <- function(train, test, use_observed_MSS, ...) {
    param_start <- list(MSS = 7, MAC = 7, DC = 0)
    param_lower <- c(MSS = 0, MAC = 0, DC = -Inf)
    param_upper <- c(MSS = Inf, MAC = Inf, DC = Inf)

    if (use_observed_MSS == TRUE) {
      observed_MSS <- max(train$velocity)

      param_start <- list(MSS = observed_MSS, MAC = 7, DC = 0)
      param_lower <- c(MSS = observed_MSS, MAC = 0, DC = -Inf)
      param_upper <- c(MSS = observed_MSS, MAC = Inf, DC = Inf)
    }

    # Non-linear model
    model <- minpack.lm::nlsLM(
      velocity ~ predict_velocity_at_distance(distance - DC, MSS, MAC),
      data = train,
      start = param_start,
      lower = param_lower,
      upper = param_upper,
      weights = train$weight,
      ...
    )

    # Parameters
    MSS <- stats::coef(model)[["MSS"]]
    MAC <- stats::coef(model)[["MAC"]]
    TAU <- MSS / MAC
    PMAX <- (MSS * MAC) / 4

    # Correction
    DC <- stats::coef(model)[["DC"]]

    # Model fit
    pred_velocity <- stats::predict(model, newdata = data.frame(distance = test$distance))
    resid_velocity <- test$velocity - pred_velocity

    return(list(
      data = train,
      model_info = list(
        predictor = "distance",
        target = "velocity",
        use_observed_MSS = use_observed_MSS
      ),
      model = model,
      parameters = list(
        MSS = MSS,
        MAC = MAC,
        TAU = TAU,
        PMAX = PMAX
      ),
      corrections = list(
        DC = DC
      ),
      predictions = list(
        .predictor = test$distance,
        .observed = test$velocity,
        .predicted = pred_velocity,
        .residual = resid_velocity
      )
    ))
  }

  model_sprint(
    df = data.frame(
      distance = distance,
      velocity = velocity,
      weight = weights
    ),
    use_observed_MSS = use_observed_MSS,
    CV = CV,
    na.rm = na.rm,
    model_func = model_func,
    ...
  )
}

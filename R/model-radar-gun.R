#' @rdname model_functions
#' @description \code{model_radar_gun} estimates short sprint parameters using time-velocity trace,
#'     with additional parameter \code{TC} serving as intercept
#' @export
#' @examples
#'
#' # Model Radar Gun (includes Time Correction)
#' df <- create_sprint_trace(MSS = 8, MAC = 6, time = seq(0, 6, 0.1))
#' m1 <- model_radar_gun(time = df$time, velocity = df$velocity)
#' m1
#' plot(m1)
#'
model_radar_gun <- function(time,
                            velocity,
                            weights = 1,
                            CV = NULL,
                            use_observed_MSS = FALSE,
                            na.rm = FALSE,
                            ...) {


  # Estimation function
  model_func <- function(train, test, use_observed_MSS, ...) {
    param_start <- list(MSS = 7, MAC = 7, TC = 0)
    param_lower <- c(MSS = 0, MAC = 0, TC = -Inf)
    param_upper <- c(MSS = Inf, MAC = Inf, TC = Inf)

    if (use_observed_MSS == TRUE) {
      observed_MSS <- max(train$velocity)

      param_start <- list(MSS = observed_MSS, MAC = 7, TC = 0)
      param_lower <- c(MSS = observed_MSS, MAC = 0, TC = -Inf)
      param_upper <- c(MSS = observed_MSS, MAC = Inf, TC = Inf)
    }

    # Non-linear model
    model <- minpack.lm::nlsLM(
      velocity ~ predict_velocity_at_time(time - TC, MSS, MAC),
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
    TC <- stats::coef(model)[["TC"]]

    # Model fit
    pred_velocity <- stats::predict(model, newdata = data.frame(time = test$time))
    resid_velocity <- test$velocity - pred_velocity

    return(list(
      data = train,
      model_info = list(
        predictor = "time",
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
        TC = TC
      ),
      predictions = list(
        .predictor = test$time,
        .observed = test$velocity,
        .predicted = pred_velocity,
        .residual = resid_velocity
      )
    ))
  }

  model_sprint(
    df = data.frame(
      time = time,
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
#' @description \code{model_laser_gun} alias for \code{\link{model_radar_gun}}
#' @export
#' @examples
#'
#' # Model Laser Gun (includes Time Correction)
#' df <- create_sprint_trace(MSS = 8, MAC = 6, time = seq(0, 6, 0.1))
#' m1 <- model_laser_gun(time = df$time, velocity = df$velocity)
#' m1
#' plot(m1)
#'
model_laser_gun <- model_radar_gun

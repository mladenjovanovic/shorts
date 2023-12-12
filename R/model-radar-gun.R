#' @rdname model_functions
#' @description \code{model_radar_gun} estimates short sprint parameters using time-velocity trace,
#'     with additional parameter \code{TC} serving as intercept
#' @param use_observed_MSS Should observed peak \code{velocity} be used as \code{MSS} parameter? Default
#'     is \code{FALSE}
#' @export
#' @examples
#' instant_velocity <- data.frame(
#'   time = c(0, 1, 2, 3, 4, 5, 6),
#'   velocity = c(0.00, 4.99, 6.43, 6.84, 6.95, 6.99, 7.00)
#' )
#'
#' sprint_model <- with(
#'   instant_velocity,
#'   model_radar_gun(time, velocity)
#' )
#'
#' print(sprint_model)
#' coef(sprint_model)
#' plot(sprint_model)
model_radar_gun <- function(time,
                            velocity,
                            weights = 1,
                            CV = NULL,
                            use_observed_MSS = FALSE,
                            na.rm = FALSE,
                            ...) {


  # Estimation function
  model_func <- function(train, test, use_observed_MSS, ...) {
    param_start <- list(MSS = 7, TAU = 0.8, TC = 0)
    param_lower <- NULL
    param_upper <- NULL

    if (use_observed_MSS == TRUE) {
      observed_MSS <- max(train$velocity)

      param_start <- list(MSS = observed_MSS, TAU = 0.8, TC = 0)
      param_lower <- c(MSS = observed_MSS, TAU = -Inf, TC = -Inf)
      param_upper <- c(MSS = observed_MSS, TAU = Inf, TC = Inf)
    }

    # Non-linear model
    model <- minpack.lm::nlsLM(
      velocity ~ MSS * (1 - exp(1)^(-(time + TC) / TAU)),
      data = train,
      start = param_start,
      lower = param_lower,
      upper = param_upper,
      weights = train$weight,
      ...
    )

    # Maximal Sprinting Speed
    MSS <- stats::coef(model)[[1]]
    TAU <- stats::coef(model)[[2]]
    TC <- stats::coef(model)[[3]]

    # Maximal acceleration
    MAC <- MSS / TAU

    # Maximal Power (relative)
    PMAX <- (MSS * MAC) / 4

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
        TAU = TAU,
        MAC = MAC,
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
#' @param use_observed_MSS Should observed peak \code{velocity} be used as \code{MSS} parameter? Default
#'     is \code{FALSE}
#' @export
#' @examples
#' instant_velocity <- data.frame(
#'   time = c(0, 1, 2, 3, 4, 5, 6),
#'   velocity = c(0.00, 4.99, 6.43, 6.84, 6.95, 6.99, 7.00)
#' )
#'
#' sprint_model <- with(
#'   instant_velocity,
#'   model_laser_gun(time, velocity)
#' )
#'
#' print(sprint_model)
#' coef(sprint_model)
#' plot(sprint_model)
model_laser_gun <- model_radar_gun

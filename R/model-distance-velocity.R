#' @rdname model_functions
#' @description \code{model_distance_velocity} estimates short sprint parameters using distance-velocity trace
#'     (e.g., tether devices)
#' @param use_observed_MSS Should observed peak \code{velocity} be used as \code{MSS} parameter? Default
#'     is \code{FALSE}
#' @export
#' @examples
#' # example code
#'
#' distance <- c(5, 10, 20, 30, 40)
#'
#' velocity <- predict_velocity_at_distance(distance, MSS = 10, MAC = 8)
#'
#' m1 <- model_distance_velocity(distance = distance, velocity = velocity)
#'
#' m1
#'
#' plot(m1)
model_distance_velocity <- function(distance,
                                    velocity,
                                    weights = 1,
                                    CV = NULL,
                                    use_observed_MSS = FALSE,
                                    na.rm = FALSE,
                                    ...) {

  # Estimation function
  model_func <- function(train, test, use_observed_MSS, ...) {
    param_start <- list(MSS = 7, TAU = 0.8)
    param_lower <- NULL
    param_upper <- NULL

    if (use_observed_MSS == TRUE) {
      observed_MSS <- max(train$velocity)

      param_start <- list(MSS = observed_MSS, TAU = 0.8)
      param_lower <- c(MSS = observed_MSS, TAU = -Inf)
      param_upper <- c(MSS = observed_MSS, TAU = Inf)
    }

    # Non-linear model
    model <- minpack.lm::nlsLM(
      velocity ~ MSS * (1 - exp(1)^(-((TAU * I(LambertW::W(-exp(1)^(-(distance) / (MSS * TAU) - 1))) + (distance) / MSS + TAU) / TAU))),
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

    # Maximal acceleration
    MAC <- MSS / TAU

    # Maximal Power (relative)
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
        TAU = TAU,
        MAC = MAC,
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
#' @description \code{model_distance_velocity_DC} estimates short sprint parameters using distance-velocity trace
#'     (e.g., tether devices) with additional distance correction \code{DC} parameter
#' @param use_observed_MSS Should observed peak \code{velocity} be used as \code{MSS} parameter? Default
#'     is \code{FALSE}
#' @export
#' @examples
#' distance <- c(5, 10, 20, 30, 40)
#'
#' velocity <- predict_velocity_at_distance(distance - 0.5, MSS = 10, MAC = 8)
#'
#' m1 <- model_distance_velocity_DC(distance = distance, velocity = velocity)
#'
#' m1
#'
#' plot(m1)
#'
model_distance_velocity_DC <- function(distance,
                                    velocity,
                                    weights = 1,
                                    CV = NULL,
                                    use_observed_MSS = FALSE,
                                    na.rm = FALSE,
                                    ...) {

  # Estimation function
  model_func <- function(train, test, use_observed_MSS, ...) {
    param_start <- list(MSS = 7, TAU = 0.8, DC = 0)
    param_lower <- NULL
    param_upper <- NULL

    if (use_observed_MSS == TRUE) {
      observed_MSS <- max(train$velocity)

      param_start <- list(MSS = observed_MSS, TAU = 0.8)
      param_lower <- c(MSS = observed_MSS, TAU = -Inf, DC = -Inf)
      param_upper <- c(MSS = observed_MSS, TAU = Inf, DC = Inf)
    }

    # Non-linear model
    model <- minpack.lm::nlsLM(
      velocity ~ MSS * (1 - exp(1)^(-((TAU * I(LambertW::W(-exp(1)^(-(distance + DC) / (MSS * TAU) - 1))) + (distance + DC) / MSS + TAU) / TAU))),
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

    # Maximal acceleration
    MAC <- MSS / TAU

    # Maximal Power (relative)
    PMAX <- (MSS * MAC) / 4

    # Corrections
    DC <- stats::coef(model)[[3]]

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
        TAU = TAU,
        MAC = MAC,
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


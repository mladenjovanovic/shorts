#' @rdname model_functions
#' @description \code{model_timing_gates} estimates short sprint parameters using distance-time trace
#'      (e.g., timing gates/photo cells)
#' @examples
#'
#' # Model Timing Gates (simple, without corrections)
#' df <- create_sprint_trace(MSS = 8, MAC = 6, distance = c(5, 10, 20, 30, 40))
#' m1 <- model_timing_gates(distance = df$distance, time = df$time)
#' m1
#' plot(m1)
#'
#' @export
model_timing_gates <- function(distance,
                               time,
                               weights = 1,
                               CV = NULL,
                               na.rm = FALSE,
                               ...) {


  # Estimation function
  model_func <- function(train, test, ...) {
    param_start <- list(MSS = 7, MAC = 7)
    param_lower <- c(MSS = 0, MAC = 0)
    param_upper <- c(MSS = Inf, MAC = Inf)

    # Non-linear model
    model <- minpack.lm::nlsLM(
      time ~ predict_time_at_distance(distance, MSS, MAC),
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
    pred_time <- stats::predict(model, newdata = data.frame(distance = test$distance))
    resid_time <- test$time - pred_time

    return(list(
      data = train,
      model_info = list(
        predictor = "distance",
        target = "time"
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
        .observed = test$time,
        .predicted = pred_time,
        .residual = resid_time
      )
    ))
  }

  model_sprint(
    df = data.frame(
      distance = distance,
      time = time,
      weight = weights
    ),
    CV = CV,
    na.rm = na.rm,
    model_func = model_func,
    ...
  )
}

#' @rdname model_functions
#' @description \code{model_timing_gates_TC} estimates short sprint parameters using distance-time trace
#'      (e.g., timing gates/photo cells), with additional time correction parameter \code{TC}
#' @examples
#'
#' # Model Timing Gates (with Time Correction)
#' df <- create_sprint_trace(MSS = 8, MAC = 6, distance = c(5, 10, 20, 30, 40), TC = 0.2)
#' m1 <- model_timing_gates_TC(distance = df$distance, time = df$time)
#' m1
#' plot(m1)
#'
#' @export
model_timing_gates_TC <- function(distance,
                                  time,
                                  weights = 1,
                                  CV = NULL,
                                  na.rm = FALSE,
                                  ...) {


  # Estimation function
  model_func <- function(train, test, ...) {
    param_start <- list(MSS = 7, MAC = 7, TC = 0)
    param_lower <- c(MSS = 0, MAC = 0, TC = -Inf)
    param_upper <- c(MSS = Inf, MAC = Inf, TC = Inf)

    # Non-linear model
    model <- minpack.lm::nlsLM(
      time ~ predict_time_at_distance(distance, MSS, MAC) + TC,
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
    pred_time <- stats::predict(model, newdata = data.frame(distance = test$distance))
    resid_time <- test$time - pred_time

    return(list(
      data = train,
      model_info = list(
        predictor = "distance",
        target = "time"
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
        .predictor = test$distance,
        .observed = test$time,
        .predicted = pred_time,
        .residual = resid_time
      )
    ))
  }

  model_sprint(
    df = data.frame(
      distance = distance,
      time = time,
      weight = weights
    ),
    CV = CV,
    na.rm = na.rm,
    model_func = model_func,
    ...
  )
}


#' @rdname model_functions
#' @description \code{model_timing_gates_FD} estimates short sprint parameters using distance-time trace
#'      (e.g., timing gates/photo cells), with additional flying distance correction parameter \code{FD}
#' @param FD Use this parameter if you do not want the \code{FD} parameter to be estimated, but rather take the
#'     provided value
#' @examples
#'
#' # Model Timing Gates (with Flying Distance Correction)
#' df <- create_sprint_trace(MSS = 8, MAC = 6, distance = c(5, 10, 20, 30, 40), FD = 0.5)
#' m1 <- model_timing_gates_FD(distance = df$distance, time = df$time)
#' m1
#' plot(m1)
#'
#' @export
model_timing_gates_FD <- function(distance,
                                  time,
                                  weights = 1,
                                  FD = NULL,
                                  CV = NULL,
                                  na.rm = FALSE,
                                  ...) {


  # Estimation function
  model_func <- function(train, test, ...) {
    param_start <- list(MSS = 7, MAC = 7, FD = 0)
    param_lower <- c(MSS = 0, MAC = 0, FD = 0)
    param_upper <- c(MSS = Inf, MAC = Inf, FD = Inf)

    user_FD <- NULL

    # If FD is provided, use that
    if (is.null(FD) == FALSE) {
      user_FD <- FD
      param_start <- list(MSS = 7, MAC = 7, FD = FD)
      param_lower <- c(MSS = 0, MAC = 0, FD = FD)
      param_upper <- c(MSS = Inf, MAC = Inf, FD = FD)
    }

    # Non-linear model
    model <- minpack.lm::nlsLM(
      time ~ predict_time_at_distance(distance + FD, MSS, MAC) - predict_time_at_distance(FD, MSS, MAC),
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
    FD <- stats::coef(model)[["FD"]]

    # Model fit
    pred_time <- stats::predict(model, newdata = data.frame(distance = test$distance))
    resid_time <- test$time - pred_time

    return(list(
      data = train,
      model_info = list(
        predictor = "distance",
        target = "time",
        user_FD = user_FD
      ),
      model = model,
      parameters = list(
        MSS = MSS,
        MAC = MAC,
        TAU = TAU,
        PMAX = PMAX
      ),
      corrections = list(
        FD = FD
      ),
      predictions = list(
        .predictor = test$distance,
        .observed = test$time,
        .predicted = pred_time,
        .residual = resid_time
      )
    ))
  }

  model_sprint(
    df = data.frame(
      distance = distance,
      time = time,
      weight = weights
    ),
    CV = CV,
    na.rm = na.rm,
    model_func = model_func,
    ...
  )
}

#' @rdname model_functions
#' @description \code{model_timing_gates_DC} estimates short sprint parameters using distance-time trace
#'      (e.g., timing gates/photo cells), with additional distance correction parameter \code{DC}
#' @examples
#'
#' # Model Timing Gates (with Distance Correction)
#' df <- create_sprint_trace(MSS = 8, MAC = 6, distance = c(5, 10, 20, 30, 40), DC = 1.5)
#' m1 <- model_timing_gates_DC(distance = df$distance, time = df$time)
#' m1
#' plot(m1)
#'
#' @export
model_timing_gates_DC <- function(distance,
                                  time,
                                  weights = 1,
                                  CV = NULL,
                                  na.rm = FALSE,
                                  ...) {


  # Estimation function
  model_func <- function(train, test, ...) {
    param_start <- list(MSS = 7, MAC = 7, DC = 0)
    param_lower <- c(MSS = 0, MAC = 0, DC = -Inf)
    param_upper <- c(MSS = Inf, MAC = Inf, DC = Inf)

    # Non-linear model
    model <- minpack.lm::nlsLM(
      time ~ predict_time_at_distance(distance - DC, MSS, MAC),
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
    pred_time <- stats::predict(model, newdata = data.frame(distance = test$distance))
    resid_time <- test$time - pred_time

    return(list(
      data = train,
      model_info = list(
        predictor = "distance",
        target = "time"
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
        .observed = test$time,
        .predicted = pred_time,
        .residual = resid_time
      )
    ))
  }

  model_sprint(
    df = data.frame(
      distance = distance,
      time = time,
      weight = weights
    ),
    CV = CV,
    na.rm = na.rm,
    model_func = model_func,
    ...
  )
}


#' @rdname model_functions
#' @description \code{model_timing_gates_TC_DC} estimates short sprint parameters using distance-time trace
#'      (e.g., timing gates/photo cells), with additional time correction \code{TC} and
#'      distance correction \code{DC} parameters
#' @examples
#'
#' # Model Timing Gates (with Time and Distance Corrections)
#' df <- create_sprint_trace(MSS = 8, MAC = 6, distance = c(5, 10, 20, 30, 40), TC = 0.25, DC = 1.5)
#' m1 <- model_timing_gates_TC_DC(distance = df$distance, time = df$time)
#' m1
#' plot(m1)
#'
#' @export
model_timing_gates_TC_DC <- function(distance,
                                     time,
                                     weights = 1,
                                     CV = NULL,
                                     na.rm = FALSE,
                                     ...) {


  # Estimation function
  model_func <- function(train, test, ...) {
    param_start <- list(MSS = 7, MAC = 7, TC = 0, DC = 0)
    param_lower <- c(MSS = 0, MAC = 0, TC = -Inf, DC = -Inf)
    param_upper <- c(MSS = Inf, MAC = Inf, TC = Inf, DC = Inf)

    # Non-linear model
    model <- minpack.lm::nlsLM(
      time ~ predict_time_at_distance(distance - DC, MSS, MAC) + TC,
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
    DC <- stats::coef(model)[["DC"]]

    # Model fit
    pred_time <- stats::predict(model, newdata = data.frame(distance = test$distance))
    resid_time <- test$time - pred_time

    return(list(
      data = train,
      model_info = list(
        predictor = "distance",
        target = "time"
      ),
      model = model,
      parameters = list(
        MSS = MSS,
        MAC = MAC,
        TAU = TAU,
        PMAX = PMAX
      ),
      corrections = list(
        TC = TC,
        DC = DC
      ),
      predictions = list(
        .predictor = test$distance,
        .observed = test$time,
        .predicted = pred_time,
        .residual = resid_time
      )
    ))
  }

  model_sprint(
    df = data.frame(
      distance = distance,
      time = time,
      weight = weights
    ),
    CV = CV,
    na.rm = na.rm,
    model_func = model_func,
    ...
  )
}

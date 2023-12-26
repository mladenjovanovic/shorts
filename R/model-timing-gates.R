#' @rdname model_functions
#' @description \code{model_timing_gates} estimates short sprint parameters using distance-time trace
#'      (e.g., timing gates/photo cells)
#' @examples
#' split_distances <- c(10, 20, 30, 40, 50)
#' split_times <- create_timing_gates_splits(
#'   gates = split_distances,
#'   MSS = 10,
#'   MAC = 9,
#'   FD = 0,
#'   TC = 0
#' )
#'
#' # Simple model
#' simple_model <- model_timing_gates(split_distances, split_times)
#'
#' print(simple_model)
#' coef(simple_model)
#' plot(simple_model)
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

    # Non-linear model
    model <- minpack.lm::nlsLM(
      time ~ predict_time_at_distance(distance, MSS, MAC),
      data = train,
      start = param_start,
      weights = train$weight,
      ...
    )

    # Parameters
    MSS <- stats::coef(model)[[1]]
    MAC <- stats::coef(model)[[2]]
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
#' split_distances <- c(10, 20, 30, 40, 50)
#' split_times <- create_timing_gates_splits(
#'   gates = split_distances,
#'   MSS = 10,
#'   MAC = 9,
#'   FD = 0,
#'   TC = 0.2,
#'   noise = 0.001
#' )
#'
#' # TC model
#' TC_model <- model_timing_gates_TC(split_distances, split_times)
#'
#' print(TC_model)
#' coef(TC_model)
#' plot(TC_model)
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

    # Non-linear model
    model <- minpack.lm::nlsLM(
      time ~ predict_time_at_distance(distance, MSS, MAC) + TC,
      data = train,
      start = param_start,
      weights = train$weight,
      ...
    )

    # Parameters
    MSS <- stats::coef(model)[[1]]
    MAC <- stats::coef(model)[[2]]
    TAU <- MSS / MAC
    PMAX <- (MSS * MAC) / 4

    # Correction
    TC <- stats::coef(model)[[3]]

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
#' split_distances <- c(10, 20, 30, 40, 50)
#' split_times <- create_timing_gates_splits(
#'   gates = split_distances,
#'   MSS = 10,
#'   MAC = 9,
#'   FD = 0.5,
#'   TC = 0,
#'   noise = 0.001
#' )
#'
#' # FD model
#' FD_model <- model_timing_gates_FD(split_distances, split_times)
#'
#' print(FD_model)
#' coef(FD_model)
#' plot(FD_model)
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
    param_lower <- NULL
    param_upper <- NULL

    user_FD <- NULL

    # If FD is provided, use that
    if (is.null(FD) == FALSE) {
      user_FD <- FD
      param_start <- list(MSS = 7, MAC = 7, FD = FD)
      param_lower <- c(MSS = -Inf, MAC = -Inf, FD = FD)
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
    MSS <- stats::coef(model)[[1]]
    MAC <- stats::coef(model)[[2]]
    TAU <- MSS / MAC
    PMAX <- (MSS * MAC) / 4

    # Correction
    FD <- stats::coef(model)[[3]]

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
#' @description \code{model_timing_gates_FD_TC} estimates short sprint parameters using distance-time trace
#'      (e.g., timing gates/photo cells), with additional flying distance correction parameter \code{FD} and
#'      time correction parameter \code{TC}
#' @param FD Use this parameter if you do not want the \code{FD} parameter to be estimated, but rather take the
#'     provided value
#' @examples
#' split_distances <- c(10, 20, 30, 40, 50)
#' split_times <- create_timing_gates_splits(
#'   gates = split_distances,
#'   MSS = 10,
#'   MAC = 9,
#'   FD = 0.5,
#'   TC = 0.1,
#'   noise = 0.001
#' )
#'
#' # FD TC model
#' FD_TC_model <- model_timing_gates_FD_TC(split_distances, split_times)
#'
#' print(FD_TC_model)
#' coef(FD_TC_model)
#' plot(FD_TC_model)
#' @export
model_timing_gates_FD_TC <- function(distance,
                                   time,
                                   weights = 1,
                                   FD = NULL,
                                   CV = NULL,
                                   na.rm = FALSE,
                                   ...) {


  # Estimation function
  model_func <- function(train, test, ...) {
    param_start <- list(MSS = 7, MAC = 7, FD = 0, TC = 0)
    param_lower <- NULL
    param_upper <- NULL

    user_FD <- NULL

    # If FD is provided, use that
    if (is.null(FD) == FALSE) {
      user_FD <- FD
      param_start <- list(MSS = 7, MAC = 7, FD = FD, TC = 0)
      param_lower <- c(MSS = -Inf, MAC = -Inf, FD = FD, TC = -Inf)
      param_upper <- c(MSS = Inf, MAC = Inf, FD = FD, TC = Inf)
    }

    # Non-linear model
    model <- minpack.lm::nlsLM(
      time ~ predict_time_at_distance(distance + FD, MSS, MAC) - predict_time_at_distance(FD, MSS, MAC) + TC,
      data = train,
      start = param_start,
      lower = param_lower,
      upper = param_upper,
      weights = train$weight,
      ...
    )

    # Parameters
    MSS <- stats::coef(model)[[1]]
    MAC <- stats::coef(model)[[2]]
    TAU <- MSS / MAC
    PMAX <- (MSS * MAC) / 4

    # Corrections
    FD <- stats::coef(model)[[3]]
    TC <- stats::coef(model)[[4]]

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
        FD = FD,
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


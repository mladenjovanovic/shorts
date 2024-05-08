#' @rdname model_functions
#' @description \code{model_time_distance} estimates short sprint parameters using time distance trace
#' @examples
#'
#' # Model Time-Distance trace (simple, without corrections)
#' df <- create_sprint_trace(MSS = 8, MAC = 6, time = seq(0, 5, by = 0.5))
#' m1 <- model_time_distance(time = df$time, distance = df$distance)
#' m1
#' plot(m1)
#'
#' @export
model_time_distance <- function(time,
                                distance,
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
      distance ~ predict_distance_at_time(time, MSS, MAC),
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
    pred_distance <- stats::predict(model, newdata = data.frame(time = test$time))
    resid_distance <- test$distance - pred_distance

    return(list(
      data = train,
      model_info = list(
        predictor = "time",
        target = "distance"
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
        .predictor = test$time,
        .observed = test$distance,
        .predicted = pred_distance,
        .residual = resid_distance
      )
    ))
  }

  model_sprint(
    df = data.frame(
      time = time,
      distance = distance,
      weight = weights
    ),
    CV = CV,
    na.rm = na.rm,
    model_func = model_func,
    ...
  )
}


#' @rdname model_functions
#' @description \code{model_time_distance_FD} estimates short sprint parameters using time-distance trace
#'      with additional estimated flying distance correction parameter \code{FD}
#' @examples
#'
#' # Model Time-Distance trace (with Flying Distance Correction)
#' df <- create_sprint_trace(MSS = 8, MAC = 6, time = seq(0, 5, by = 0.5), FD = 0.5)
#' m1 <- model_time_distance_FD(time = df$time, distance = df$distance)
#' m1
#' plot(m1)
#'
#' @export
model_time_distance_FD <- function(time,
                                   distance,
                                   weights = 1,
                                   CV = NULL,
                                   na.rm = FALSE,
                                   ...) {


  # Estimation function
  model_func <- function(train, test, ...) {
    param_start <- list(MSS = 7, MAC = 7, FD = 0)
    param_lower <- c(MSS = 0, MAC = 0, FD = -Inf)
    param_upper <- c(MSS = Inf, MAC = Inf, FD = Inf)

    # Non-linear model
    model <- minpack.lm::nlsLM(
      distance ~ predict_distance_at_time(time + predict_time_at_distance(FD, MSS, MAC), MSS, MAC) - FD,
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
    pred_distance <- stats::predict(model, newdata = data.frame(time = test$time))
    resid_distance <- test$distance - pred_distance

    return(list(
      data = train,
      model_info = list(
        predictor = "time",
        target = "distance"
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
        .predictor = test$time,
        .observed = test$distance,
        .predicted = pred_distance,
        .residual = resid_distance
      )
    ))
  }

  model_sprint(
    df = data.frame(
      time = time,
      distance = distance,
      weight = weights
    ),
    CV = CV,
    na.rm = na.rm,
    model_func = model_func,
    ...
  )
}

#' @rdname model_functions
#' @description \code{model_time_distance_FD_fixed} estimates short sprint parameters using time-distance trace
#'      with additional flying distance correction parameter \code{FD} which
#'      is fixed by the user
#' @param FD Flying distance parameter. Default is 0
#' @examples
#'
#' # Model Time-Distance trace (with Flying Distance Correction fixed)
#' df <- create_sprint_trace(MSS = 8, MAC = 6, time = seq(0, 5, by = 0.5), FD = 0.5)
#' m1 <- model_time_distance_FD_fixed(time = df$time, distance = df$distance, FD = 0.5)
#' m1
#' plot(m1)
#'
#' @export
model_time_distance_FD_fixed <- function(time,
                                   distance,
                                   weights = 1,
                                   FD = 0,
                                   CV = NULL,
                                   na.rm = FALSE,
                                   ...) {


  # Estimation function
  model_func <- function(train, test, ...) {
    param_start <- list(MSS = 7, MAC = 7)
    param_lower <- c(MSS = 0, MAC = 0)
    param_upper <- c(MSS = Inf, MAC = Inf)

    # Add FD to train and test
    train$FD <- FD
    test$FD <- FD

    # Non-linear model
    model <- minpack.lm::nlsLM(
      distance ~ predict_distance_at_time(time + predict_time_at_distance(FD, MSS, MAC), MSS, MAC) - FD,
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
    pred_distance <- stats::predict(model, newdata = data.frame(time = test$time))
    resid_distance <- test$distance - pred_distance

    return(list(
      data = train,
      model_info = list(
        predictor = "time",
        target = "distance",
        user_FD = FD
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
        .predictor = test$time,
        .observed = test$distance,
        .predicted = pred_distance,
        .residual = resid_distance
      )
    ))
  }

  model_sprint(
    df = data.frame(
      time = time,
      distance = distance,
      weight = weights
    ),
    CV = CV,
    na.rm = na.rm,
    model_func = model_func,
    ...
  )
}


#' @rdname model_functions
#' @description \code{model_time_distance} estimates short sprint parameters using time distance trace
#'     with additional time correction parameter \code{TC}
#' @examples
#'
#' # Model Time-Distance trace (with Time Correction)
#' df <- create_sprint_trace(MSS = 8, MAC = 6, time = seq(0, 5, by = 0.5), TC = 1.5)
#' m1 <- model_time_distance_TC(time = df$time, distance = df$distance)
#' m1
#' plot(m1)
#'
#' @export
model_time_distance_TC <- function(time,
                                   distance,
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
      distance ~ predict_distance_at_time(time - TC, MSS, MAC),
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
    pred_distance <- stats::predict(model, newdata = data.frame(time = test$time))
    resid_distance <- test$distance - pred_distance

    return(list(
      data = train,
      model_info = list(
        predictor = "time",
        target = "distance"
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
        .observed = test$distance,
        .predicted = pred_distance,
        .residual = resid_distance
      )
    ))
  }

  model_sprint(
    df = data.frame(
      time = time,
      distance = distance,
      weight = weights
    ),
    CV = CV,
    na.rm = na.rm,
    model_func = model_func,
    ...
  )
}


#' @rdname model_functions
#' @description \code{model_time_distance} estimates short sprint parameters using time distance trace
#'     with additional distance correction parameter \code{DC}
#' @examples
#'
#' # Model Time-Distance trace (with Distance Correction)
#' df <- create_sprint_trace(MSS = 8, MAC = 6, time = seq(0, 5, by = 0.5), DC = -5)
#' m1 <- model_time_distance_DC(time = df$time, distance = df$distance)
#' m1
#' plot(m1)
#'
#' @export
model_time_distance_DC <- function(time,
                                   distance,
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
      distance ~ predict_distance_at_time(time, MSS, MAC) + DC,
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
    pred_distance <- stats::predict(model, newdata = data.frame(time = test$time))
    resid_distance <- test$distance - pred_distance

    return(list(
      data = train,
      model_info = list(
        predictor = "time",
        target = "distance"
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
        .predictor = test$time,
        .observed = test$distance,
        .predicted = pred_distance,
        .residual = resid_distance
      )
    ))
  }

  model_sprint(
    df = data.frame(
      time = time,
      distance = distance,
      weight = weights
    ),
    CV = CV,
    na.rm = na.rm,
    model_func = model_func,
    ...
  )
}

#' @rdname model_functions
#' @description \code{model_time_distance} estimates short sprint parameters using time distance trace
#'     with additional time correction \code{TC} and distance correction \code{TC} parameters
#' @examples
#'
#' # Model Time-Distance trace (with Time and Distance Corrections)
#' df <- create_sprint_trace(MSS = 8, MAC = 6, time = seq(0, 5, by = 0.5), TC = -1.3, DC = 5)
#' m1 <- model_time_distance_TC_DC(time = df$time, distance = df$distance)
#' m1
#' plot(m1)
#'
#' @export
model_time_distance_TC_DC <- function(time,
                                      distance,
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
      distance ~ predict_distance_at_time(time - TC, MSS, MAC) + DC,
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

    # Corrections
    TC <- stats::coef(model)[["TC"]]
    DC <- stats::coef(model)[["DC"]]

    # Model fit
    pred_distance <- stats::predict(model, newdata = data.frame(time = test$time))
    resid_distance <- test$distance - pred_distance

    return(list(
      data = train,
      model_info = list(
        predictor = "time",
        target = "distance"
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
        .predictor = test$time,
        .observed = test$distance,
        .predicted = pred_distance,
        .residual = resid_distance
      )
    ))
  }

  model_sprint(
    df = data.frame(
      time = time,
      distance = distance,
      weight = weights
    ),
    CV = CV,
    na.rm = na.rm,
    model_func = model_func,
    ...
  )
}

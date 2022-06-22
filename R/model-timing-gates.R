#' Models Using Timing Gates Split Times
#'
#' These functions model the sprint split times using mono-exponential equation, where \code{time}
#'     is used as target or outcome variable, and \code{distance} as predictor.
#'     \itemize{
#'     \item{\code{\link{model_timing_gates}}}{ Provides the simplest model with estimated \code{MSS}
#'      and \code{MAC} parameters}
#'     \item{\code{\link{model_timing_gates_TC}}}{ Besides estimating \code{MSS} and \code{MAC}
#'     parameters, this function estimates additional parameter \code{TC} or time correction}
#'     \item{\code{\link{model_timing_gates_FD}}}{ In addition to estimating \code{MSS} and
#'      \code{MAC} parameters, this function estimates \code{FD} or flying distance}
#'     \item{\code{\link{model_timing_gates_FD_TC}}}{ Combines the approach of the \code{\link{model_timing_gates_FD}} with
#'     that one of \code{\link{model_timing_gates_TC}}. In other words, it add extra parameter \code{TC} to be estimated in
#'     the \code{\link{model_timing_gates_FD}} model}
#'     }
#'
#' @param distance,time Numeric vector. Indicates the position of the timing gates and time measured
#' @param weights Numeric vector. Default is vector of 1.
#'     This is used to give more weight to particular observations. For example, use \code{1\\distance} to give
#'     more weight to observations from shorter distances.
#' @param LOOCV Should Leave-one-out cross-validation be used to estimate model fit? Default is \code{FALSE}
#' @param control Control object forwarded to \code{\link[minpack.lm]{nlsLM}}. Default is \code{minpack.lm::nls.lm.control(maxiter = 1000)}
#' @param na.rm Logical. Default is FALSE
#' @param ... Extra parameters forwarded to \code{\link[minpack.lm]{nlsLM}} function
#' @return List object with the following elements:
#'     \describe{
#'         \item{data}{Data frame used to estimate the sprint parameters, consisting of \code{distance},
#'             \code{time}, \code{weights}, and \code{pred_time} columns}
#'         \item{model}{Model returned by the \code{\link[minpack.lm]{nlsLM}} function}
#'         \item{parameters}{List with the estimated parameters, of which the following
#'         are always returned: \code{MSS}, \code{TAU}, \code{MAC}, and \code{PMAX}}
#'         \item{model_fit}{List with the following components:
#'             \code{RSE}, \code{R_squared}, \code{minErr}, \code{maxErr}, and \code{RMSE}}
#'         }
#' @references
#'     Haugen TA, Tønnessen E, Seiler SK. 2012. The Difference Is in the Start: Impact of Timing and Start
#'         Procedure on Sprint Running Performance: Journal of Strength and Conditioning Research 26:473–479.
#'         DOI: 10.1519/JSC.0b013e318226030b.
#'
#'     Jovanović, M., Vescovi, J.D. (2020). shorts: An R Package for Modeling Short Sprints. Preprint
#'         available at SportRxiv. https://doi.org/10.31236/osf.io/4jw62
#' @examples
#' split_distances <- c(10, 20, 30, 40, 50)
#' split_times <- create_timing_gates_splits(
#'   gates = split_distances,
#'   MSS = 10,
#'   MAC = 9,
#'   FD = 0.25,
#'   TC = 0
#' )
#'
#' # Simple model
#' simple_model <- model_timing_gates(split_distances, split_times)
#'
#' print(simple_model)
#' coef(simple_model)
#' plot(simple_model)
#'
#' # Model with correction of 0.3s
#' model_with_correction <- model_timing_gates(split_distances, split_times + 0.3)
#'
#' print(model_with_correction)
#' plot(model_with_correction)
#'
#' # Model with time_correction estimation
#' model_with_TC <- model_timing_gates_TC(split_distances, split_times)
#'
#' print(model_with_TC)
#' plot(model_with_TC)
#'
#' # Model with flying distance estimations
#' model_with_FD <- model_timing_gates_FD(split_distances, split_times)
#'
#' print(model_with_FD)
#' plot(model_with_FD)
#'
#' # Model with flying distance estimations and time correction
#' model_with_FD_TC <- model_timing_gates_FD_TC(split_distances, split_times)
#'
#' print(model_with_FD_TC)
#' plot(model_with_FD_TC)
#' @name model_timing_gates
NULL


# =====================================================================================================================================
# This is generic function or a wrapper
model_timing_gates_generic <- function(distance,
                                       time,
                                       weights = 1,
                                       LOOCV = FALSE,
                                       na.rm = FALSE,
                                       model_func = function(train, test, ...) {
                                         list(model = NULL, coefs = NULL, pred_time = NULL)
                                       },
                                       ...) {

  # ========================================
  # Put data into data frame
  df <- data.frame(
    distance = distance,
    time = time,
    weights = weights
  )

  # Remove NAs
  if (na.rm) {
    df <- stats::na.omit(df)
  }

  # Run model
  training_model <- model_func(
    train = df,
    test = df,
    ...
  )

  # Get the predicted time
  training_pred_time <- training_model$pred_time

  training_model_fit <- shorts_model_fit(
    model = training_model$model,
    observed = df$time,
    predicted = training_pred_time,
    na.rm = na.rm
  )

  # LOOCV
  LOOCV_data <- NULL

  if (LOOCV) {
    cv_folds <- data.frame(index = seq_len(nrow(df)))
    cv_folds <- split(cv_folds, cv_folds$index)

    testing <- lapply(cv_folds, function(fold) {
      train_data <- df[-fold$index, ]
      test_data <- df[fold$index, ]

      model <- model_func(
        train = train_data,
        test = test_data,
        ...
      )

      return(model)
    })

    # Extract predicted time
    testing_pred_time <- sapply(testing, function(data) data$pred_time)

    testing_model_fit <- shorts_model_fit(
      observed = df$time,
      predicted = testing_pred_time,
      na.rm = na.rm
    )

    # Extract model parameters
    testing_parameters <- purrr::map_dfr(testing, function(x) {
      x$coefs
    })

    # Testing df
    testing_df <- df
    testing_df$pred_time <- testing_pred_time

    # Save everything in the object
    LOOCV_data <- list(
      parameters = testing_parameters,
      model_fit = testing_model_fit,
      data = testing_df
    )
  }

  # Add predicted time to df
  df <- data.frame(
    distance = distance,
    time = time,
    weights = weights,
    pred_time = training_pred_time
  )

  # Return object
  return(new_shorts_model(
    data = df,
    model = training_model$model,
    parameters = training_model$coefs,
    model_fit = training_model_fit,
    CV = LOOCV_data
  ))
}

# =====================================================================================================================================
#' @rdname model_timing_gates
#' @export
model_timing_gates <- function(distance,
                               time,
                               weights = 1,
                               LOOCV = FALSE,
                               control = minpack.lm::nls.lm.control(maxiter = 1000),
                               na.rm = FALSE,
                               ...) {

  # Estimation function
  model_func <- function(train, test, ...) {

    # Non-linear model
    speed_mod <- minpack.lm::nlsLM(
      time ~ TAU * I(LambertW::W(-exp(1)^(-distance / (MSS * TAU) - 1))) + distance / MSS + TAU,
      data = train,
      start = list(MSS = 7, TAU = 0.8),
      weights = train$weights,
      ...
    )

    # Maximal Sprinting Speed
    MSS <- stats::coef(speed_mod)[[1]]
    TAU <- stats::coef(speed_mod)[[2]]

    # Maximal acceleration
    MAC <- MSS / TAU

    # Maximal Power (relative)
    PMAX <- (MSS * MAC) / 4

    # Model fit
    pred_time <- stats::predict(speed_mod, newdata = data.frame(distance = test$distance))

    return(list(
      model = speed_mod,
      coefs = list(
        MSS = MSS,
        TAU = TAU,
        MAC = MAC,
        PMAX = PMAX
      ),
      pred_time = pred_time
    ))
  }

  model_timing_gates_generic(
    distance = distance,
    time = time,
    weights = weights,
    LOOCV = LOOCV,
    na.rm = na.rm,
    model_func = model_func,
    control = control,
    ...
  )
}



# =====================================================================================================================================
#' @rdname model_timing_gates
#' @export
model_timing_gates_TC <- function(distance,
                                  time,
                                  weights = 1,
                                  LOOCV = FALSE,
                                  control = minpack.lm::nls.lm.control(maxiter = 1000),
                                  na.rm = FALSE,
                                  ...) {

  # Estimation function
  model_func <- function(train, test, ...) {

    # Non-linear model
    speed_mod <- minpack.lm::nlsLM(
      time ~ TAU * I(LambertW::W(-exp(1)^(-distance / (MSS * TAU) - 1))) + distance / MSS + TAU - TC,
      data = train,
      start = list(MSS = 7, TAU = 0.8, TC = 0),
      weights = train$weights,
      ...
    )

    # Maximal Sprinting Speed
    MSS <- stats::coef(speed_mod)[[1]]
    TAU <- stats::coef(speed_mod)[[2]]
    TC <- stats::coef(speed_mod)[[3]]

    # Maximal acceleration
    MAC <- MSS / TAU

    # Maximal Power (relative)
    PMAX <- (MSS * MAC) / 4

    # Model fit
    pred_time <- stats::predict(speed_mod, newdata = data.frame(distance = test$distance))

    return(list(
      model = speed_mod,
      coefs = list(
        MSS = MSS,
        TAU = TAU,
        MAC = MAC,
        PMAX = PMAX,
        TC = TC
      ),
      pred_time = pred_time
    ))
  }

  model_timing_gates_generic(
    distance = distance,
    time = time,
    weights = weights,
    LOOCV = LOOCV,
    na.rm = na.rm,
    model_func = model_func,
    control = control,
    ...
  )
}

# =====================================================================================================================================
#' @rdname model_timing_gates
#' @export
model_timing_gates_FD <- function(distance,
                                  time,
                                  weights = 1,
                                  LOOCV = FALSE,
                                  control = minpack.lm::nls.lm.control(maxiter = 1000),
                                  na.rm = FALSE,
                                  ...) {

  # Estimation function
  model_func <- function(train, test, ...) {

    # Non-linear model
    speed_mod <- minpack.lm::nlsLM(
      time ~ (TAU * I(LambertW::W(-exp(1)^(-(distance + FD) / (MSS * TAU) - 1))) + (distance + FD) / MSS + TAU) -
        (TAU * I(LambertW::W(-exp(1)^(-FD / (MSS * TAU) - 1))) + FD / MSS + TAU),
      data = train,
      start = list(MSS = 7, TAU = 0.8, FD = 0),
      weights = train$weights,
      ...
    )

    # Maximal Sprinting Speed
    MSS <- stats::coef(speed_mod)[[1]]
    TAU <- stats::coef(speed_mod)[[2]]
    FD <- stats::coef(speed_mod)[[3]]

    # Maximal acceleration
    MAC <- MSS / TAU

    # Maximal Power (relative)
    PMAX <- (MSS * MAC) / 4

    # Model fit
    pred_time <- stats::predict(speed_mod, newdata = data.frame(distance = test$distance))

    return(list(
      model = speed_mod,
      coefs = list(
        MSS = MSS,
        TAU = TAU,
        MAC = MAC,
        PMAX = PMAX,
        FD = FD
      ),
      pred_time = pred_time
    ))
  }

  model_timing_gates_generic(
    distance = distance,
    time = time,
    weights = weights,
    LOOCV = LOOCV,
    na.rm = na.rm,
    model_func = model_func,
    control = control,
    ...
  )
}


# =====================================================================================================================================
#' @rdname model_timing_gates
#' @export
model_timing_gates_FD_TC <- function(distance,
                                     time,
                                     weights = 1,
                                     LOOCV = FALSE,
                                     control = minpack.lm::nls.lm.control(maxiter = 1000),
                                     na.rm = FALSE,
                                     ...) {

  # Estimation function
  model_func <- function(train, test, ...) {

    # Non-linear model
    speed_mod <- minpack.lm::nlsLM(
      time ~ (TAU * I(LambertW::W(-exp(1)^(-(distance + FD) / (MSS * TAU) - 1))) + (distance + FD) / MSS + TAU) -
        (TAU * I(LambertW::W(-exp(1)^(-FD / (MSS * TAU) - 1))) + FD / MSS + TAU) - TC,
      data = train,
      start = list(MSS = 7, TAU = 0.8, FD = 0, TC = 0),
      weights = train$weights,
      ...
    )

    # Maximal Sprinting Speed
    MSS <- stats::coef(speed_mod)[[1]]
    TAU <- stats::coef(speed_mod)[[2]]
    FD <- stats::coef(speed_mod)[[3]]
    TC <- stats::coef(speed_mod)[[4]]

    # Maximal acceleration
    MAC <- MSS / TAU

    # Maximal Power (relative)
    PMAX <- (MSS * MAC) / 4

    # Model fit
    pred_time <- stats::predict(speed_mod, newdata = data.frame(distance = test$distance))

    return(list(
      model = speed_mod,
      coefs = list(
        MSS = MSS,
        TAU = TAU,
        MAC = MAC,
        PMAX = PMAX,
        FD = FD,
        TC = TC
      ),
      pred_time = pred_time
    ))
  }

  model_timing_gates_generic(
    distance = distance,
    time = time,
    weights = weights,
    LOOCV = LOOCV,
    na.rm = na.rm,
    model_func = model_func,
    control = control,
    ...
  )
}

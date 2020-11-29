#' Models Using Split Times
#'
#' These functions model the sprint split times using mono-exponential equation, where \code{time}
#'  is used as target or outcome variable, and \code{distance} as predictor. Function
#'  \code{\link{model_using_splits}} provides the simplest model with estimated \code{MSS} and \code{TAU}
#'  parameters. Time correction using heuristic rule of thumbs (e.g., adding 0.3s to split times) can be
#'  implemented using \code{time_correction} function parameter. Function
#'  \code{\link{model_using_splits_with_time_correction}}, besides estimating \code{MSS} and \code{TAU},
#'  estimates additional parameter \code{time_correction}.  Function \code{\link{model_using_splits_with_corrections}},
#'  besides estimating \code{MSS}, \code{TAU} and \code{time_correction}, estimates additional parameter
#'  \code{distance_correction}. For more information about these function please refer to accompanying vignettes in
#'  this package.
#'
#' @param distance,time Numeric vector. Indicates the position of the timing gates and time measured
#' @param time_correction Numeric vector. Used to correct for different starting techniques. This correction is
#'     done by adding \code{time_correction} to \code{time}. Default is 0. See more in Haugen et al. (2018)
#' @param weights Numeric vector. Default is vector of 1.
#'     This is used to give more weight to particular observations. For example, use \code{1\\distance} to give
#'     more weight to observations from shorter distances.
#' @param LOOCV Should Leave-one-out cross-validation be used to estimate model fit? Default is \code{FALSE}
#' @param na.rm Logical. Default is FALSE
#' @param ... Forwarded to \code{\link[stats]{nls}} function
#' @return List object with the following elements:
#'     \describe{
#'         \item{parameters}{List with the following estimated parameters:
#'             \code{MSS}, \code{TAU}, \code{MAC}, \code{PMAX}, \code{time_correction}, and
#'             \code{distance_correction}}
#'         \item{model_fit}{List with the following components:
#'             \code{RSE}, \code{R_squared}, \code{minErr}, \code{maxErr}, and \code{RMSE}}
#'         \item{model}{Model returned by the \code{\link[stats]{nls}} function}
#'         \item{data}{Data frame used to estimate the sprint parameters, consisting of \code{distance},
#'             \code{time}, \code{weights}, and \code{pred_time} columns}
#'         }
#' @references
#'     Haugen TA, Tønnessen E, Seiler SK. 2012. The Difference Is in the Start: Impact of Timing and Start
#'         Procedure on Sprint Running Performance: Journal of Strength and Conditioning Research 26:473–479.
#'         DOI: 10.1519/JSC.0b013e318226030b.
#' @examples
#' split_times <- data.frame(
#'   distance = c(5, 10, 20, 30, 35),
#'   time = c(1.20, 1.96, 3.36, 4.71, 5.35)
#' )
#'
#' # Simple model
#' simple_model <- with(
#'   split_times,
#'   model_using_splits(distance, time)
#' )
#'
#' print(simple_model)
#' coef(simple_model)
#' plot(simple_model)
#'
#' # Model with correction of 0.3s
#' model_with_correction <- with(
#'   split_times,
#'   model_using_splits(distance, time, time_correction = 0.3)
#' )
#'
#' print(model_with_correction)
#' coef(model_with_correction)
#' plot(model_with_correction)
#'
#' # Model with time_correction estimation
#' model_with_time_correction_estimation <- with(
#'   split_times,
#'   model_using_splits_with_time_correction(distance, time)
#' )
#'
#' print(model_with_time_correction_estimation)
#' coef(model_with_time_correction_estimation)
#' plot(model_with_time_correction_estimation)
#'
#' # Model with time and distance correction estimation
#' model_with_time_distance_correction_estimation <- with(
#'   split_times,
#'   model_using_splits_with_corrections(distance, time)
#' )
#'
#' print(model_with_time_distance_correction_estimation)
#' coef(model_with_time_distance_correction_estimation)
#' plot(model_with_time_distance_correction_estimation)
#' @name model_split_times
NULL

# =====================================================================================================================================
#' @rdname model_split_times
#' @export
model_using_splits <- function(distance,
                               time,
                               time_correction = 0,
                               weights = 1,
                               LOOCV = FALSE,
                               na.rm = FALSE,
                               ...) {
  run_model <- function(train, test, ...) {
    # Non-linear model
    speed_mod <- stats::nls(
      corrected_time ~ TAU * I(LambertW::W(-exp(1)^(-distance / (MSS * TAU) - 1))) + distance / MSS + TAU,
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
    pred_time <- TAU * I(LambertW::W(-exp(1)^(-test$distance / (MSS * TAU) - 1))) + test$distance / MSS + TAU
    pred_time <- pred_time - time_correction


    return(list(
      model = speed_mod,
      MSS = MSS,
      TAU = TAU,
      MAC = MAC,
      PMAX = PMAX,
      pred_time = pred_time
    ))
  }

  # ========================================
  # Put data into data frame
  df <- data.frame(
    distance = distance,
    time = time,
    time_correction = time_correction,
    # Correct the time
    corrected_time = time + time_correction,
    weights = weights
  )

  # Remove NAs
  if (na.rm) {
    df <- stats::na.omit(df)
  }

  # Run model
  training_model <- run_model(
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
    cv_folds <- data.frame(index = seq_len(nrow(df)), df)
    cv_folds <- split(cv_folds, cv_folds$index)

    testing <- lapply(cv_folds, function(fold) {
      train_data <- df[-fold$index, ]
      test_data <- df[fold$index, ]

      model <- run_model(
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
    testing_parameters <- as.data.frame(
      t(sapply(testing, function(data) {
        c(data$MSS, data$TAU, data$MAC, data$PMAX)
      }))
    )

    colnames(testing_parameters) <- c("MSS", "TAU", "MAC", "PMAX")
    testing_parameters$time_correction <- time_correction
    testing_parameters$distance_correction <- 0

    # Modify df
    testing_df <- data.frame(
      distance = distance,
      time = time,
      weights = weights,
      pred_time = testing_pred_time
    )

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
    parameters = list(
      MSS = training_model$MSS,
      TAU = training_model$TAU,
      MAC = training_model$MAC,
      PMAX = training_model$PMAX,
      time_correction = time_correction,
      distance_correction = 0
    ),
    model_fit = training_model_fit,
    model = training_model$model,
    data = df,
    LOOCV = LOOCV_data
  ))
}

# =====================================================================================================================================
#' @rdname model_split_times
#' @export
model_using_splits_with_time_correction <- function(distance,
                                                    time,
                                                    weights = 1,
                                                    LOOCV = FALSE,
                                                    na.rm = FALSE,
                                                    ...) {
  run_model <- function(train, test, ...) {

    # Non-linear model
    speed_mod <- stats::nls(
      time ~ TAU * I(LambertW::W(-exp(1)^(-distance / (MSS * TAU) - 1))) + distance / MSS + TAU - time_correction,
      data = train,
      start = list(MSS = 7, TAU = 0.8, time_correction = 0),
      weights = train$weights,
      ...
    )

    # Maximal Sprinting Speed
    MSS <- stats::coef(speed_mod)[[1]]
    TAU <- stats::coef(speed_mod)[[2]]
    time_correction <- stats::coef(speed_mod)[[3]]

    # Maximal acceleration
    MAC <- MSS / TAU

    # Maximal Power (relative)
    PMAX <- (MSS * MAC) / 4

    # Model fit
    pred_time <- TAU * I(LambertW::W(-exp(1)^(-test$distance / (MSS * TAU) - 1))) + test$distance / MSS + TAU - time_correction

    return(list(
      model = speed_mod,
      MSS = MSS,
      TAU = TAU,
      MAC = MAC,
      PMAX = PMAX,
      time_correction = time_correction,
      pred_time = pred_time
    ))
  }

  # =========================
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
  training_model <- run_model(
    train = df,
    test = df,
    ...
  )

  training_model_fit <- shorts_model_fit(
    model = training_model$model,
    observed = df$time,
    predicted = training_model$pred_time,
    na.rm = na.rm
  )

  # LOOCV
  LOOCV_data <- NULL

  if (LOOCV) {
    cv_folds <- data.frame(index = seq_len(nrow(df)), df)
    cv_folds <- split(cv_folds, cv_folds$index)

    testing <- lapply(cv_folds, function(fold) {
      train_data <- df[-fold$index, ]
      test_data <- df[fold$index, ]

      model <- run_model(
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
    testing_parameters <- as.data.frame(
      t(sapply(testing, function(data) {
        c(data$MSS, data$TAU, data$MAC, data$PMAX, data$time_correction)
      }))
    )

    colnames(testing_parameters) <- c("MSS", "TAU", "MAC", "PMAX", "time_correction")
    testing_parameters$distance_correction <- 0

    # Modify df
    testing_df <- data.frame(
      distance = distance,
      time = time,
      weights = weights,
      pred_time = testing_pred_time
    )

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
    pred_time = training_model$pred_time
  )

  # Return object
  return(new_shorts_model(
    parameters = list(
      MSS = training_model$MSS,
      TAU = training_model$TAU,
      MAC = training_model$MAC,
      PMAX = training_model$PMAX,
      time_correction = training_model$time_correction,
      distance_correction = 0
    ),
    model_fit = training_model_fit,
    model = training_model$model,
    data = df,
    LOOCV = LOOCV_data
  ))
}

# =====================================================================================================================================
#' @rdname model_split_times
#' @export
model_using_splits_with_corrections <- function(distance,
                                                time,
                                                weights = 1,
                                                LOOCV = FALSE,
                                                na.rm = FALSE,
                                                ...) {

  run_model <- function(train, test, ...) {

    # Non-linear model
    speed_mod <- stats::nls(
      time ~ TAU * I(LambertW::W(-exp(1)^(-(distance + distance_correction) / (MSS * TAU) - 1))) + (distance + distance_correction) / MSS + TAU - time_correction,
      data = train,
      start = list(MSS = 7, TAU = 0.8, time_correction = 0, distance_correction = 0),
      weights = train$weights,
      ...
    )

    # Maximal Sprinting Speed
    MSS <- stats::coef(speed_mod)[[1]]
    TAU <- stats::coef(speed_mod)[[2]]
    time_correction <- stats::coef(speed_mod)[[3]]
    distance_correction <- stats::coef(speed_mod)[[4]]

    # Maximal acceleration
    MAC <- MSS / TAU

    # Maximal Power (relative)
    PMAX <- (MSS * MAC) / 4

    # Model fit
    pred_time <- TAU * I(LambertW::W(-exp(1)^(-(test$distance + distance_correction) / (MSS * TAU) - 1))) + (test$distance + distance_correction) / MSS + TAU - time_correction

    return(list(
      model = speed_mod,
      MSS = MSS,
      TAU = TAU,
      MAC = MAC,
      PMAX = PMAX,
      time_correction = time_correction,
      distance_correction = distance_correction,
      pred_time = pred_time
    ))
  }


  # ==========================
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
  training_model <- run_model(
    train = df,
    test = df,
    ...
  )

  training_model_fit <- shorts_model_fit(
    model = training_model$model,
    observed = df$time,
    predicted = training_model$pred_time,
    na.rm = na.rm
  )

  # LOOCV
  LOOCV_data <- NULL

  if (LOOCV) {
    cv_folds <- data.frame(index = seq_len(nrow(df)), df)
    cv_folds <- split(cv_folds, cv_folds$index)

    testing <- lapply(cv_folds, function(fold) {
      train_data <- df[-fold$index, ]
      test_data <- df[fold$index, ]

      model <- run_model(
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
    testing_parameters <- as.data.frame(
      t(sapply(testing, function(data) {
        c(data$MSS, data$TAU, data$MAC, data$PMAX, data$time_correction, data$distance_correction)
      }))
    )

    colnames(testing_parameters) <- c("MSS", "TAU", "MAC", "PMAX", "time_correction", "distance_correction")

    # Modify df
    testing_df <- data.frame(
      distance = distance,
      time = time,
      weights = weights,
      pred_time = testing_pred_time
    )

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
    pred_time = training_model$pred_time
  )

  # Return object
  return(new_shorts_model(
    parameters = list(
      MSS = training_model$MSS,
      TAU = training_model$TAU,
      MAC = training_model$MAC,
      PMAX = training_model$PMAX,
      time_correction = training_model$time_correction,
      distance_correction = training_model$distance_correction
    ),
    model_fit = training_model_fit,
    model = training_model$model,
    data = df,
    LOOCV = LOOCV_data
  ))
}

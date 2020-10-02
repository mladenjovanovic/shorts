#' Model Using Instantaneous Velocity or Radar Gun
#'
#' This function models the sprint instantaneous velocity using mono-exponential equation that estimates
#'     maximum sprinting speed (\code{MSS}) and relative acceleration (\code{TAU}). \code{velocity} is used as target or outcome
#'     variable, and \code{time} as predictor.
#'
#' @param time Numeric vector
#' @param velocity Numeric vector
#' @param time_correction Numeric vector. Used to filter out noisy data from the radar gun. This correction is
#'     done by adding \code{time_correction} to \code{time}. Default is 0. See more in Samozino (2018)
#' @param weights Numeric vector. Default is 1
#' @param LOOCV Should Leave-one-out cross-validation be used to estimate model fit? Default is \code{FALSE}
#' @param na.rm Logical. Default is FALSE
#' @param ... Forwarded to \code{\link[nlme]{nlme}} function
#' @return List object with the following elements:
#'     \describe{
#'         \item{parameters}{List with the following estimated parameters:
#'             \code{MSS}, \code{TAU}, \code{MAC}, and \code{PMAX}}
#'         \item{model_fit}{List with the following components:
#'             \code{RSE}, \code{R_squared}, \code{minErr}, \code{maxErr}, and \code{RMSE}}
#'         \item{model}{Model returned by the \code{\link[stats]{nls}} function}
#'         \item{data}{Data frame used to estimate the sprint parameters, consisting of \code{time},
#'            \code{velocity}, \code{weights}, and \code{pred_velocity} columns}
#'         }
#' @references
#'     Samozino P. 2018. A Simple Method for Measuring Force, Velocity and Power Capabilities and Mechanical
#'         Effectiveness During Sprint Running. In: Morin J-B, Samozino P eds. Biomechanics of Training and Testing.
#'        Cham: Springer International Publishing, 237–267. DOI: 10.1007/978-3-319-05633-3_11.
#'
#' @examples
#' instant_velocity <- data.frame(
#'   time = c(0, 1, 2, 3, 4, 5, 6),
#'   velocity = c(0.00, 4.99, 6.43, 6.84, 6.95, 6.99, 7.00)
#' )
#'
#' sprint_model <- with(
#'   instant_velocity,
#'   model_using_radar(time, velocity)
#' )
#'
#' # sprint_model$parameters
#' coef(sprint_model)
#' @name model_radar
NULL

# =====================================================================================================================================
#' @rdname model_radar
#' @export
model_using_radar <- function(time,
                              velocity,
                              time_correction = 0,
                              weights = 1,
                              LOOCV = FALSE,
                              na.rm = FALSE,
                              ...) {

  run_model <- function(train, test, ...) {
    # Non-linear model
    speed_mod <- stats::nls(
      velocity ~ MSS * (1 - exp(1)^(-(corrected_time) / TAU)),
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
    pred_velocity <- MSS * (1 - exp(1)^(-(test$corrected_time) / TAU))

    return(list(
      model = speed_mod,
      MSS = MSS,
      TAU = TAU,
      MAC = MAC,
      PMAX = PMAX,
      pred_velocity = pred_velocity
    ))
  }

  # ==================================
  # Put data into data frame
  df <- data.frame(
    time = time,
    time_correction = time_correction,
    corrected_time = time + time_correction,
    velocity = velocity,
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
    observed = df$velocity,
    predicted = training_model$pred_velocity,
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
    testing_pred_velocity <- sapply(testing, function(data) data$pred_velocity)

    testing_model_fit <- shorts_model_fit(
      observed = df$velocity,
      predicted = testing_pred_velocity,
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
      time = time,
      velocity = velocity,
      weights = weights,
      pred_velocity = testing_pred_velocity
    )

    # Save everything in the object
    LOOCV_data <- list(
      parameters = testing_parameters,
      model_fit = testing_model_fit,
      data = testing_df
    )
  }

  # Add predicted velocity to df
  df <- data.frame(
    time = time,
    velocity = velocity,
    weights = weights,
    pred_velocity = training_model$pred_velocity
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
#' @rdname model_radar
#' @export
model_using_radar_with_time_correction <- function(time,
                              velocity,
                              weights = 1,
                              LOOCV = FALSE,
                              na.rm = FALSE,
                              ...) {

  run_model <- function(train, test, ...) {
    # Non-linear model
    speed_mod <- stats::nls(
      velocity ~ MSS * (1 - exp(1)^(-(time + time_correction) / TAU)),
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
    pred_velocity <- MSS * (1 - exp(1)^(-(test$time + time_correction) / TAU))

    return(list(
      model = speed_mod,
      MSS = MSS,
      TAU = TAU,
      MAC = MAC,
      PMAX = PMAX,
      time_correction = time_correction,
      pred_velocity = pred_velocity
    ))
  }

  # ==================================
  # Put data into data frame
  df <- data.frame(
    time = time,
    velocity = velocity,
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
    observed = df$velocity,
    predicted = training_model$pred_velocity,
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
    testing_pred_velocity <- sapply(testing, function(data) data$pred_velocity)

    testing_model_fit <- shorts_model_fit(
      observed = df$velocity,
      predicted = testing_pred_velocity,
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
      time = time,
      velocity = velocity,
      weights = weights,
      pred_velocity = testing_pred_velocity
    )

    # Save everything in the object
    LOOCV_data <- list(
      parameters = testing_parameters,
      model_fit = testing_model_fit,
      data = testing_df
    )
  }

  # Add predicted velocity to df
  df <- data.frame(
    time = time,
    velocity = velocity,
    weights = weights,
    pred_velocity = training_model$pred_velocity
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


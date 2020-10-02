#' Mixed Model Using Instantaneous Velocity
#'
#' This function models the sprint instantaneous velocity using mono-exponential equation and non-linear
#'     mixed model using \code{\link[nlme]{nlme}} to estimate fixed and random maximum sprinting speed (\code{MSS})
#'     and relative acceleration (\code{TAU}) parameters. In mixed model, fixed and random effects are estimated for
#'     \code{MSS} and \code{TAU} parameters using \code{athlete} as levels. \code{velocity} is used as target or outcome
#'     variable, and \code{time} as predictor.
#' @param data Data frame
#' @param time Character string. Name of the column in \code{data}
#' @param velocity Character string. Name of the column in \code{data}
#' @param athlete Character string. Name of the column in \code{data}. Used as levels in the \code{\link[nlme]{nlme}}
#' @param time_correction Numeric vector. Used to filter out noisy data from the radar gun.
#'     This correction is done by adding \code{time_correction} to \code{time}. Default is 0. See more in Samozino (2018)
#' @param random Formula forwarded to \code{\link[nlme]{nlme}} to set random effects. Default is \code{MSS + TAU ~ 1}
#' @param LOOCV Should Leave-one-out cross-validation be used to estimate model fit? Default is \code{FALSE}.
#'     This can be very slow process due high level of samples in the radar data
#' @param na.rm Logical. Default is FALSE
#' @param ... Forwarded to \code{\link[nlme]{nlme}} function
#' @return List object with the following elements:
#'     \describe{
#'         \item{parameters}{List with two data frames: \code{fixed} and \code{random} containing the following
#'             estimated parameters: \code{MSS}, \code{TAU}, \code{MAC}, and \code{PMAX}}
#'         \item{model_fit}{List with the following components:
#'             \code{RSE}, \code{R_squared}, \code{minErr}, \code{maxErr}, and \code{RMSE}}
#'         \item{model}{Model returned by the \code{\link[nlme]{nlme}} function}
#'         \item{data}{Data frame used to estimate the sprint parameters, consisting of \code{athlete}, \code{time},
#'           \code{velocity}, and \code{pred_velocity} columns}
#'         }
#' @references
#'     Samozino P. 2018. A Simple Method for Measuring Force, Velocity and Power Capabilities and Mechanical
#'         Effectiveness During Sprint Running. In: Morin J-B, Samozino P eds. Biomechanics of Training and Testing.
#'        Cham: Springer International Publishing, 237–267. DOI: 10.1007/978-3-319-05633-3_11.
#'
#' @examples
#' data("radar_gun_data")
#' mixed_model <- mixed_model_using_radar(radar_gun_data, "time", "velocity", "athlete")
#'
#' # mixed_model$parameters
#' coef(mixed_model)
#' @name mixed_model_radar
NULL


# =====================================================================================================================================
#' @rdname mixed_model_radar
#' @export
mixed_model_using_radar <- function(data,
                                    time,
                                    velocity,
                                    athlete,
                                    time_correction = 0,
                                    random = MSS + TAU ~ 1,
                                    LOOCV = FALSE,
                                    # weights = rep(1, nrow(data)),
                                    na.rm = FALSE,
                                    ...) {


  run_model <- function(train, test, ...) {

    # Create mixed model
    mixed_model <- nlme::nlme(
      velocity ~ MSS * (1 - exp(1)^(-(corrected_time) / TAU)),
      data = train,
      fixed = MSS + TAU ~ 1,
      random = random,
      groups = ~athlete,
      # weights = ~weights,
      start = c(MSS = 7, TAU = 0.8),
      ...
    )

    # Pull estimates
    fixed_effects <- nlme::fixed.effects(mixed_model)
    random_effects <- nlme::random.effects(mixed_model)
    athlete_list <- row.names(random_effects)

    empty_df <- matrix(
      0,
      nrow = nrow(random_effects),
      ncol = length(fixed_effects)
    )
    colnames(empty_df) <- names(fixed_effects)
    empty_df <- as.data.frame(empty_df)

    random_effects <- as.data.frame(random_effects)
    effect_names <- colnames(random_effects)[colnames(random_effects) %in% colnames(empty_df)]

    empty_df[, effect_names] <- random_effects[, effect_names]

    fixed_effects_athlete <- matrix(
      fixed_effects,
      nrow = nrow(random_effects),
      ncol = length(fixed_effects),
      byrow = TRUE
    )

    random_effects <- empty_df + fixed_effects_athlete
    random_effects <- data.frame(
      athlete = athlete_list,
      random_effects
    )

    fixed_effects <- data.frame(t(fixed_effects))
    fixed_effects$MAC <- fixed_effects$MSS / fixed_effects$TAU
    fixed_effects$PMAX <- (fixed_effects$MSS * fixed_effects$MAC) / 4
    fixed_effects$time_correction <- time_correction
    fixed_effects$distance_correction <- 0

    random_effects$MAC <- random_effects$MSS / random_effects$TAU
    random_effects$PMAX <- (random_effects$MSS * random_effects$MAC) / 4
    random_effects$time_correction <- time_correction
    random_effects$distance_correction <- 0

    # Model fit
    pred_velocity <- stats::predict(mixed_model, newdata = test)

    return(list(
      model = mixed_model,
      fixed_effects = fixed_effects,
      random_effects = random_effects,
      pred_velocity = as.numeric(pred_velocity)
    ))
  }

  # ================================================
  # Combine to DF

  df <- data.frame(
    athlete = data[[athlete]],
    time = data[[time]],
    time_correction = time_correction,
    corrected_time = data[[time]] + time_correction,
    velocity = data[[velocity]] # ,
    # weights = weights
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
    testing_fixed_parameters <- as.data.frame(
      t(sapply(testing, function(data) {
        c(
          data$fixed_effects$MSS,
          data$fixed_effects$TAU,
          data$fixed_effects$MAC,
          data$fixed_effects$PMAX,
          data$fixed_effects$time_correction,
          data$fixed_effects$distance_correction
        )
      }))
    )
    colnames(testing_fixed_parameters) <- c(
      "MSS",
      "TAU",
      "MAC",
      "PMAX",
      "time_correction",
      "distance_correction"
    )

    testing_random_parameters <- lapply(testing, function(data) data$random_effects)
    testing_random_parameters <- do.call(rbind, testing_random_parameters)

    # Modify df
    testing_df <- data.frame(
      athlete = data[[athlete]],
      time = data[[time]],
      velocity = data[[velocity]],
      pred_velocity = testing_pred_velocity # ,
      # weights = weights
    )

    # Save everything in the object
    LOOCV_data <- list(
      parameters = list(
        fixed = testing_fixed_parameters,
        random = testing_random_parameters
      ),
      model_fit = testing_model_fit,
      data = testing_df
    )
  }

  # Add predicted velocity to df
  df <- data.frame(
    athlete = data[[athlete]],
    time = data[[time]],
    velocity = data[[velocity]],
    pred_velocity = training_model$pred_velocity # ,
    # weights = weights
  )

  return(new_shorts_mixed_model(
    parameters = list(
      fixed = training_model$fixed_effects,
      random = training_model$random_effects
    ),
    model_fit = training_model_fit,
    model = training_model$model,
    data = df,
    LOOCV = LOOCV_data
  ))
}


# =====================================================================================================================================
#' @rdname mixed_model_radar
#' @export
mixed_model_using_radar_with_time_correction <- function(data,
                                    time,
                                    velocity,
                                    athlete,
                                    random = MSS + TAU ~ 1,
                                    LOOCV = FALSE,
                                    # weights = rep(1, nrow(data)),
                                    na.rm = FALSE,
                                    ...) {


  run_model <- function(train, test, ...) {
    # Create mixed model
    mixed_model <- nlme::nlme(
      velocity ~ MSS * (1 - exp(1)^(-(time + time_correction) / TAU)),
      data = train,
      fixed = MSS + TAU + time_correction ~ 1,
      random = random,
      groups = ~athlete,
      # weights = ~weights,
      start = c(MSS = 7, TAU = 0.8, time_correction = 0),
      ...
    )

    # Pull estimates
    fixed_effects <- nlme::fixed.effects(mixed_model)
    random_effects <- nlme::random.effects(mixed_model)
    athlete_list <- row.names(random_effects)

    empty_df <- matrix(
      0,
      nrow = nrow(random_effects),
      ncol = length(fixed_effects)
    )
    colnames(empty_df) <- names(fixed_effects)
    empty_df <- as.data.frame(empty_df)

    random_effects <- as.data.frame(random_effects)
    effect_names <- colnames(random_effects)[colnames(random_effects) %in% colnames(empty_df)]

    empty_df[, effect_names] <- random_effects[, effect_names]

    fixed_effects_athlete <- matrix(
      fixed_effects,
      nrow = nrow(random_effects),
      ncol = length(fixed_effects),
      byrow = TRUE
    )

    random_effects <- empty_df + fixed_effects_athlete
    random_effects <- data.frame(
      athlete = athlete_list,
      random_effects
    )

    fixed_effects <- data.frame(t(fixed_effects))
    fixed_effects$MAC <- fixed_effects$MSS / fixed_effects$TAU
    fixed_effects$PMAX <- (fixed_effects$MSS * fixed_effects$MAC) / 4
    fixed_effects$distance_correction <- 0

    random_effects$MAC <- random_effects$MSS / random_effects$TAU
    random_effects$PMAX <- (random_effects$MSS * random_effects$MAC) / 4
    random_effects$distance_correction <- 0

    # Reorganized the order
    fixed_effects <- fixed_effects[c("MSS", "TAU", "MAC", "PMAX", "time_correction", "distance_correction")]
    random_effects <- random_effects[c("athlete", "MSS", "TAU", "MAC", "PMAX", "time_correction", "distance_correction")]

    # Model fit
    pred_velocity <- stats::predict(mixed_model, newdata = test)

    return(list(
      model = mixed_model,
      fixed_effects = fixed_effects,
      random_effects = random_effects,
      pred_velocity = as.numeric(pred_velocity)
    ))
  }

  # ================================================
  # Combine to DF

  df <- data.frame(
    athlete = data[[athlete]],
    time = data[[time]],
    velocity = data[[velocity]] # ,
    # weights = weights
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
    testing_fixed_parameters <- as.data.frame(
      t(sapply(testing, function(data) {
        c(
          data$fixed_effects$MSS,
          data$fixed_effects$TAU,
          data$fixed_effects$MAC,
          data$fixed_effects$PMAX,
          data$fixed_effects$time_correction,
          data$fixed_effects$distance_correction
        )
      }))
    )
    colnames(testing_fixed_parameters) <- c(
      "MSS",
      "TAU",
      "MAC",
      "PMAX",
      "time_correction",
      "distance_correction"
    )

    testing_random_parameters <- lapply(testing, function(data) data$random_effects)
    testing_random_parameters <- do.call(rbind, testing_random_parameters)

    # Modify df
    testing_df <- data.frame(
      athlete = data[[athlete]],
      time = data[[time]],
      velocity = data[[velocity]],
      pred_velocity = testing_pred_velocity # ,
      # weights = weights
    )

    # Save everything in the object
    LOOCV_data <- list(
      parameters = list(
        fixed = testing_fixed_parameters,
        random = testing_random_parameters
      ),
      model_fit = testing_model_fit,
      data = testing_df
    )
  }

  # Add predicted velocity to df
  df <- data.frame(
    athlete = data[[athlete]],
    time = data[[time]],
    velocity = data[[velocity]],
    pred_velocity = training_model$pred_velocity # ,
    # weights = weights
  )

  return(new_shorts_mixed_model(
    parameters = list(
      fixed = training_model$fixed_effects,
      random = training_model$random_effects
    ),
    model_fit = training_model_fit,
    model = training_model$model,
    data = df,
    LOOCV = LOOCV_data
  ))
}

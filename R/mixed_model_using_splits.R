#' Mixed Models Using Split Times
#'
#' These functions model the sprint split times using mono-exponential equation, where \code{time}
#'  is used as target or outcome variable, and \code{distance} as predictor. Function
#'  \code{\link{mixed_model_using_splits}} provides the simplest model with estimated \code{MSS} and \code{TAU}
#'  parameters. Time correction using heuristic rule of thumbs (e.g., adding 0.3s to split times) can be
#'  implemented using \code{time_correction} function parameter. Function
#'  \code{\link{mixed_model_using_splits_with_time_correction}}, besides estimating \code{MSS} and \code{TAU},
#'  estimates additional parameter \code{time_correction}.  Function
#'  \code{\link{mixed_model_using_splits_with_distance_correction}}, besides estimating \code{MSS} and \code{TAU},
#'  estimates additional parameter \code{distance_correction}. Function \code{\link{mixed_model_using_splits_with_corrections}},
#'  besides estimating \code{MSS}, \code{TAU} and \code{time_correction}, estimates additional parameter
#'  \code{distance_correction}. For more information about these function please refer to accompanying vignettes in
#'  this package.
#'
#' @param data Data frame
#' @param distance Character string. Name of the column in \code{data}
#' @param time Character string. Name of the column in \code{data}
#' @param time_correction Numeric vector. Used to correct for different starting techniques.  This correction is
#'     done by adding \code{time_correction} to \code{time}. Default is 0. See more in Haugen et al. (2018)
#' @param athlete Character string. Name of the column in \code{data}. Used as levels in the \code{\link[nlme]{nlme}}
#' @param random Formula forwarded to \code{\link[nlme]{nlme}} to set random effects. Default is \code{MSS + TAU ~ 1}
#' @param LOOCV Should Leave-one-out cross-validation be used to estimate model fit? Default is \code{FALSE}
#' @param na.rm Logical. Default is FALSE
#' @param ... Forwarded to \code{\link[nlme]{nlme}} function
#' @return List object with the following elements:
#'     \describe{
#'         \item{parameters}{List with two data frames: \code{fixed} and \code{random} containing the following
#'             estimated parameters: \code{MSS}, \code{TAU}, \code{time_correction}, \code{distance_correction},
#'             \code{MAC}, and \code{PMAX}}
#'         \item{model_fit}{List with the following components:
#'             \code{RSE}, \code{R_squared}, \code{minErr}, \code{maxErr}, and \code{RMSE}}
#'         \item{model}{Model returned by the \code{\link[nlme]{nlme}} function}
#'         \item{data}{Data frame used to estimate the sprint parameters, consisting of \code{athlete}, \code{distance},
#'             \code{time}, and \code{pred_time} columns}
#'         }
#' @references
#'     Haugen TA, Tønnessen E, Seiler SK. 2012. The Difference Is in the Start: Impact of Timing and Start
#'         Procedure on Sprint Running Performance: Journal of Strength and Conditioning Research 26:473–479.
#'         DOI: 10.1519/JSC.0b013e318226030b.
#' @examples
#' data("split_times")
#'
#' mixed_model <- mixed_model_using_splits(
#'   data = split_times,
#'   distance = "distance",
#'   time = "time",
#'   athlete = "athlete"
#' )
#'
#' print(mixed_model)
#' coef(mixed_model)
#' plot(mixed_model)
#'
#' mixed_model <- mixed_model_using_splits_with_time_correction(
#'   data = split_times,
#'   distance = "distance",
#'   time = "time",
#'   athlete = "athlete"
#' )
#'
#' print(mixed_model)
#' coef(mixed_model)
#' plot(mixed_model)
#'
#' mixed_model <- mixed_model_using_splits_with_distance_correction(
#'   data = split_times,
#'   distance = "distance",
#'   time = "time",
#'   athlete = "athlete"
#' )
#'
#' print(mixed_model)
#' coef(mixed_model)
#' plot(mixed_model)
#'
#' mixed_model <- mixed_model_using_splits_with_corrections(
#'   data = split_times,
#'   distance = "distance",
#'   time = "time",
#'   athlete = "athlete"
#' )
#'
#' print(mixed_model)
#' coef(mixed_model)
#' plot(mixed_model)
#' @name mixed_model_split_times
NULL

# =====================================================================================================================================
#' @rdname mixed_model_split_times
#' @export
mixed_model_using_splits <- function(data,
                                     distance,
                                     time,
                                     athlete,
                                     time_correction = 0,
                                     random = MSS + TAU ~ 1,
                                     # weights = rep(1, nrow(data)),
                                     LOOCV = FALSE,
                                     na.rm = FALSE,
                                     ...) {
  run_model <- function(train, test, ...) {
    # Create mixed model
    mixed_model <- nlme::nlme(
      corrected_time ~ TAU * I(LambertW::W(-exp(1)^(-distance / (MSS * TAU) - 1))) + distance / MSS + TAU,
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
    pred_time <- stats::predict(mixed_model, newdata = test)
    pred_time <- pred_time - time_correction

    return(list(
      model = mixed_model,
      fixed_effects = fixed_effects,
      random_effects = random_effects,
      pred_time = as.numeric(pred_time)
    ))
  }

  # Combine to DF
  df <- data.frame(
    athlete = data[[athlete]],
    distance = data[[distance]],
    time = data[[time]],
    time_correction = time_correction,
    corrected_time = data[[time]] + time_correction # ,
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
    testing_pred_time <- testing_pred_time - time_correction

    testing_model_fit <- shorts_model_fit(
      observed = df$time,
      predicted = testing_pred_time,
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
      distance = data[[distance]],
      time = data[[time]],
      pred_time = testing_pred_time # ,
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

  # Add predicted time to df
  # Combine to DF
  df <- data.frame(
    athlete = data[[athlete]],
    distance = data[[distance]],
    time = data[[time]],
    pred_time = training_model$pred_time # ,
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
#' @rdname mixed_model_split_times
#' @export
mixed_model_using_splits_with_time_correction <- function(data,
                                                          distance,
                                                          time,
                                                          athlete,
                                                          random = MSS + TAU ~ 1,
                                                          LOOCV = FALSE,
                                                          # weights = rep(1, nrow(data)),
                                                          na.rm = FALSE,
                                                          ...) {
  run_model <- function(train, test, ...) {
    # Create mixed model
    mixed_model <- nlme::nlme(
      time ~ TAU * I(LambertW::W(-exp(1)^(-distance / (MSS * TAU) - 1))) + distance / MSS + TAU - time_correction,
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

    # Model fit
    pred_time <- stats::predict(mixed_model, newdata = test)

    return(list(
      model = mixed_model,
      fixed_effects = fixed_effects[c("MSS", "TAU", "MAC", "PMAX", "time_correction", "distance_correction")],
      random_effects = random_effects[c("athlete", "MSS", "TAU", "MAC", "PMAX", "time_correction", "distance_correction")],
      pred_time = as.numeric(pred_time)
    ))
  }


  # Combine to DF
  df <- data.frame(
    athlete = data[[athlete]],
    distance = data[[distance]],
    time = data[[time]]
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

  # Model fit
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
      distance = data[[distance]],
      time = data[[time]],
      pred_time = testing_pred_time # ,
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
  # Add predicted time to df
  # Combine to DF
  df <- data.frame(
    athlete = data[[athlete]],
    distance = data[[distance]],
    time = data[[time]],
    pred_time = training_model$pred_time # ,
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
#' @rdname mixed_model_split_times
#' @export
mixed_model_using_splits_with_distance_correction <- function(data,
                                                              distance,
                                                              time,
                                                              athlete,
                                                              random = MSS + TAU ~ 1,
                                                              LOOCV = FALSE,
                                                              # weights = rep(1, nrow(data)),
                                                              na.rm = FALSE,
                                                              ...) {
  run_model <- function(train, test, ...) {
    # Create mixed model
    mixed_model <- nlme::nlme(
      time ~ (TAU * I(LambertW::W(-exp(1)^(-(distance + distance_correction) / (MSS * TAU) - 1))) +
        (distance + distance_correction) / MSS + TAU) -
        (TAU * I(LambertW::W(-exp(1)^(-distance_correction / (MSS * TAU) - 1))) +
          distance_correction / MSS + TAU),
      data = train,
      fixed = MSS + TAU + distance_correction ~ 1,
      random = random,
      groups = ~athlete,
      # weights = ~weights,
      start = c(MSS = 7, TAU = 0.8, distance_correction = 0),
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
    fixed_effects$time_correction <- 0

    random_effects$MAC <- random_effects$MSS / random_effects$TAU
    random_effects$PMAX <- (random_effects$MSS * random_effects$MAC) / 4
    random_effects$time_correction <- 0

    # Model fit
    pred_time <- stats::predict(mixed_model, newdata = test)

    return(list(
      model = mixed_model,
      fixed_effects = fixed_effects[c("MSS", "TAU", "MAC", "PMAX", "time_correction", "distance_correction")],
      random_effects = random_effects[c("athlete", "MSS", "TAU", "MAC", "PMAX", "time_correction", "distance_correction")],
      pred_time = as.numeric(pred_time)
    ))
  }


  # Combine to DF
  df <- data.frame(
    athlete = data[[athlete]],
    distance = data[[distance]],
    time = data[[time]]
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

  # Model fit
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
      distance = data[[distance]],
      time = data[[time]],
      pred_time = testing_pred_time # ,
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
  # Add predicted time to df
  # Combine to DF
  df <- data.frame(
    athlete = data[[athlete]],
    distance = data[[distance]],
    time = data[[time]],
    pred_time = training_model$pred_time # ,
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
#' @rdname mixed_model_split_times
#' @export
mixed_model_using_splits_with_corrections <- function(data,
                                                      distance,
                                                      time,
                                                      athlete,
                                                      random = MSS + TAU ~ 1,
                                                      LOOCV = FALSE,
                                                      # weights = rep(1, nrow(data)),
                                                      na.rm = FALSE,
                                                      ...) {
  run_model <- function(train, test, ...) {
    # Create mixed model
    mixed_model <- nlme::nlme(
      time ~ TAU * I(LambertW::W(-exp(1)^(-(distance + distance_correction) / (MSS * TAU) - 1))) + (distance + distance_correction) / MSS + TAU - time_correction,
      data = train,
      fixed = MSS + TAU + time_correction + distance_correction ~ 1,
      random = random,
      groups = ~athlete,
      # weights = ~weights,
      start = c(MSS = 7, TAU = 0.8, time_correction = 0, distance_correction = 0),
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

    random_effects$MAC <- random_effects$MSS / random_effects$TAU
    random_effects$PMAX <- (random_effects$MSS * random_effects$MAC) / 4

    # Model fit
    pred_time <- stats::predict(mixed_model, newdata = test)

    return(list(
      model = mixed_model,
      fixed_effects = fixed_effects[c("MSS", "TAU", "MAC", "PMAX", "time_correction", "distance_correction")],
      random_effects = random_effects[c("athlete", "MSS", "TAU", "MAC", "PMAX", "time_correction", "distance_correction")],
      pred_time = as.numeric(pred_time)
    ))
  }


  # Combine to DF
  df <- data.frame(
    athlete = data[[athlete]],
    distance = data[[distance]],
    time = data[[time]]
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

  # Model fit
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
      distance = data[[distance]],
      time = data[[time]],
      pred_time = testing_pred_time # ,
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
  # Add predicted time to df
  # Combine to DF
  df <- data.frame(
    athlete = data[[athlete]],
    distance = data[[distance]],
    time = data[[time]],
    pred_time = training_model$pred_time # ,
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

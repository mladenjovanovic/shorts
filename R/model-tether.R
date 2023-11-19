#' Model Using Instantaneous Tether device
#'
#' This function models the sprint instantaneous velocity using mono-exponential equation that estimates
#'     maximum sprinting speed (\code{MSS}) and relative acceleration (\code{TAU}). \code{velocity} is used as target or outcome
#'     variable, and \code{distance} as predictor.
#'
#' @param distance Numeric vector
#' @param velocity Numeric vector
#' @param weights Numeric vector. Default is 1
#' @param CV Should cross-validation be used to estimate model fit? Default is \code{NULL}. Otherwise use integer
#'     indicating number of folds. See Example for more information
#' @param use_observed_MSS Should \code{MSS} be estimated from the observed \code{velocity}? Default is \code{FALSE}
#' @param control Control object forwarded to \code{\link[minpack.lm]{nlsLM}}. Default is \code{minpack.lm::nls.lm.control(maxiter = 1000)}
#' @param na.rm Logical. Default is FALSE
#' @param ... Forwarded to \code{\link[minpack.lm]{nlsLM}} function
#' @return List object with the following elements:
#'     \describe{
#'         \item{parameters}{List with the following estimated parameters:
#'             \code{MSS}, \code{TAU}, \code{MAC}, and \code{PMAX}}
#'         \item{model_fit}{List with the following components:
#'             \code{RSE}, \code{R_squared}, \code{minErr}, \code{maxErr}, and \code{RMSE}}
#'         \item{model}{Model returned by the \code{\link[minpack.lm]{nlsLM}} function}
#'         \item{data}{Data frame used to estimate the sprint parameters, consisting of \code{distance},
#'            \code{velocity}, \code{weights}, and \code{pred_velocity} columns}
#'         }
#' @examples
#' distance <- c(5, 10, 20, 30, 40)
#'
#' velocity <- predict_velocity_at_distance(distance, MSS = 10, MAC = 8)
#'
#' m1 <- model_tether(distance = distance, velocity = velocity)
#'
#' m1
#'
#' plot(m1)
#' @export
model_tether <- function(distance,
                         velocity,
                         weights = 1,
                         CV = NULL,
                         use_observed_MSS = FALSE,
                         control = minpack.lm::nls.lm.control(maxiter = 1000),
                         na.rm = FALSE,
                         ...) {
  run_model <- function(train, test, use_observed_MSS, ...) {
    param_start <- list(MSS = 7, TAU = 0.8)
    param_lower <- NULL
    param_upper <- NULL

    if (use_observed_MSS == TRUE) {
      observed_MSS <- max(velocity)

      param_start <- list(MSS = observed_MSS, TAU = 0.8)
      param_lower <- c(MSS = observed_MSS, TAU = -Inf)
      param_upper <- c(MSS = observed_MSS, TAU = Inf)
    }

    # Non-linear model
    speed_mod <- minpack.lm::nlsLM(
      velocity ~ MSS * (1 - exp(1)^(-((TAU * I(LambertW::W(-exp(1)^(-(distance) / (MSS * TAU) - 1))) + (distance) / MSS + TAU) / TAU))),
      data = train,
      start = param_start,
      lower = param_lower,
      upper = param_upper,
      weights = train$weights,
      control = control,
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
    pred_velocity <- stats::predict(speed_mod, newdata = data.frame(distance = test$distance))

    return(list(
      model = speed_mod,
      coefs = list(
        MSS = MSS,
        TAU = TAU,
        MAC = MAC,
        PMAX = PMAX
      ),
      pred_velocity = pred_velocity
    ))
  }

  # ==================================
  # Put data into data frame
  df <- data.frame(
    distance = distance,
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
    use_observed_MSS = use_observed_MSS,
    ...
  )

  training_model_fit <- shorts_model_fit(
    model = training_model$model,
    observed = df$velocity,
    predicted = training_model$pred_velocity,
    na.rm = na.rm
  )

  # Cross validation
  CV_data <- NULL

  if (!is.null(CV)) {
    # Shuffle data
    cv_df <- df[sample(nrow(df)), ]

    cv_folds <- data.frame(
      fold = cut(seq(1, nrow(cv_df)), breaks = CV, labels = FALSE),
      index = seq(1, nrow(cv_df))
    )

    cv_folds <- split(cv_folds, cv_folds$fold)

    testing <- lapply(cv_folds, function(fold) {
      train_data <- cv_df[-fold$index, ]
      test_data <- cv_df[fold$index, ]

      model <- run_model(
        train = train_data,
        test = test_data,
        use_observed_MSS = use_observed_MSS,
        ...
      )

      return(model)
    })

    # Extract predicted time

    testing_pred_velocity <- unlist(lapply(testing, function(data) data$pred_velocity))

    testing_model_fit <- shorts_model_fit(
      observed = cv_df$velocity,
      predicted = testing_pred_velocity,
      na.rm = na.rm
    )
    # Extract model parameters

    testing_parameters <- purrr::map_dfr(testing, function(x) {
      x$coefs
    })

    # Testing df
    testing_df <- cv_df
    testing_df$pred_velocity <- testing_pred_velocity

    # Save everything in the object
    CV_data <- list(
      parameters = testing_parameters,
      model_fit = testing_model_fit,
      data = testing_df
    )
  }

  # Add predicted velocity to df
  df <- data.frame(
    distance = distance,
    velocity = velocity,
    weights = weights,
    pred_velocity = training_model$pred_velocity
  )

  # Return object
  return(new_shorts_model(
    data = df,
    model = training_model$model,
    parameters = training_model$coefs,
    model_fit = training_model_fit,
    CV = CV_data
  ))
}


#' Model Using Instantaneous Tether device and Distance Correction
#'
#' This function models the sprint instantaneous velocity using mono-exponential equation that estimates
#'     maximum sprinting speed (\code{MSS}) and relative acceleration (\code{TAU}). \code{velocity} is used as target or outcome
#'     variable, and \code{distance} as predictor. Additional parameter \code{DC}, serving the intercept function, is
#'     estimated
#'
#' @param distance Numeric vector
#' @param velocity Numeric vector
#' @param weights Numeric vector. Default is 1
#' @param CV Should cross-validation be used to estimate model fit? Default is \code{NULL}. Otherwise use integer
#'     indicating number of folds. See Example for more information
#' @param use_observed_MSS Should \code{MSS} be estimated from the observed \code{velocity}? Default is \code{FALSE}
#' @param control Control object forwarded to \code{\link[minpack.lm]{nlsLM}}. Default is \code{minpack.lm::nls.lm.control(maxiter = 1000)}
#' @param na.rm Logical. Default is FALSE
#' @param ... Forwarded to \code{\link[minpack.lm]{nlsLM}} function
#' @return List object with the following elements:
#'     \describe{
#'         \item{parameters}{List with the following estimated parameters:
#'             \code{MSS}, \code{TAU}, \code{MAC}, \code{PMAX}, and \code{DC}}
#'         \item{model_fit}{List with the following components:
#'             \code{RSE}, \code{R_squared}, \code{minErr}, \code{maxErr}, and \code{RMSE}}
#'         \item{model}{Model returned by the \code{\link[minpack.lm]{nlsLM}} function}
#'         \item{data}{Data frame used to estimate the sprint parameters, consisting of \code{distance},
#'            \code{velocity}, \code{weights}, and \code{pred_velocity} columns}
#'         }
#' @examples
#' distance <- c(5, 10, 20, 30, 40)
#'
#' velocity <- predict_velocity_at_distance(distance - 0.5, MSS = 10, MAC = 8)
#'
#' m1 <- model_tether_DC(distance = distance, velocity = velocity)
#'
#' m1
#'
#' plot(m1)
#' @export
model_tether_DC <- function(distance,
                         velocity,
                         weights = 1,
                         CV = NULL,
                         use_observed_MSS = FALSE,
                         control = minpack.lm::nls.lm.control(maxiter = 1000),
                         na.rm = FALSE,
                         ...) {
  run_model <- function(train, test, use_observed_MSS, ...) {
    param_start <- list(MSS = 7, TAU = 0.8, DC = 0)
    param_lower <- NULL
    param_upper <- NULL

    if (use_observed_MSS == TRUE) {
      observed_MSS <- max(velocity)

      param_start <- list(MSS = observed_MSS, TAU = 0.8, DC = 0)
      param_lower <- c(MSS = observed_MSS, TAU = -Inf, DC = -Inf)
      param_upper <- c(MSS = observed_MSS, TAU = Inf, DC = Inf)
    }

    # Non-linear model
    speed_mod <- minpack.lm::nlsLM(
      velocity ~ MSS * (1 - exp(1)^(-((TAU * I(LambertW::W(-exp(1)^(-(distance + DC) / (MSS * TAU) - 1))) + (distance + DC) / MSS + TAU) / TAU))),
      data = train,
      start = param_start,
      lower = param_lower,
      upper = param_upper,
      weights = train$weights,
      control = control,
      ...
    )

    # Maximal Sprinting Speed
    MSS <- stats::coef(speed_mod)[[1]]
    TAU <- stats::coef(speed_mod)[[2]]

    # Maximal acceleration
    MAC <- MSS / TAU

    # Maximal Power (relative)
    PMAX <- (MSS * MAC) / 4

    # Distance correction
    DC <- stats::coef(speed_mod)[[3]]

    # Model fit
    pred_velocity <- stats::predict(speed_mod, newdata = data.frame(distance = test$distance))

    return(list(
      model = speed_mod,
      coefs = list(
        MSS = MSS,
        TAU = TAU,
        MAC = MAC,
        PMAX = PMAX,
        DC = DC
      ),
      pred_velocity = pred_velocity
    ))
  }

  # ==================================
  # Put data into data frame
  df <- data.frame(
    distance = distance,
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
    use_observed_MSS = use_observed_MSS,
    ...
  )

  training_model_fit <- shorts_model_fit(
    model = training_model$model,
    observed = df$velocity,
    predicted = training_model$pred_velocity,
    na.rm = na.rm
  )

  # Cross validation
  CV_data <- NULL

  if (!is.null(CV)) {
    # Shuffle data
    cv_df <- df[sample(nrow(df)), ]

    cv_folds <- data.frame(
      fold = cut(seq(1, nrow(cv_df)), breaks = CV, labels = FALSE),
      index = seq(1, nrow(cv_df))
    )

    cv_folds <- split(cv_folds, cv_folds$fold)

    testing <- lapply(cv_folds, function(fold) {
      train_data <- cv_df[-fold$index, ]
      test_data <- cv_df[fold$index, ]

      model <- run_model(
        train = train_data,
        test = test_data,
        use_observed_MSS = use_observed_MSS,
        ...
      )

      return(model)
    })

    # Extract predicted time

    testing_pred_velocity <- unlist(lapply(testing, function(data) data$pred_velocity))

    testing_model_fit <- shorts_model_fit(
      observed = cv_df$velocity,
      predicted = testing_pred_velocity,
      na.rm = na.rm
    )
    # Extract model parameters

    testing_parameters <- purrr::map_dfr(testing, function(x) {
      x$coefs
    })

    # Testing df
    testing_df <- cv_df
    testing_df$pred_velocity <- testing_pred_velocity

    # Save everything in the object
    CV_data <- list(
      parameters = testing_parameters,
      model_fit = testing_model_fit,
      data = testing_df
    )
  }

  # Add predicted velocity to df
  df <- data.frame(
    distance = distance,
    velocity = velocity,
    weights = weights,
    pred_velocity = training_model$pred_velocity
  )

  # Return object
  return(new_shorts_model(
    data = df,
    model = training_model$model,
    parameters = training_model$coefs,
    model_fit = training_model_fit,
    CV = CV_data
  ))
}

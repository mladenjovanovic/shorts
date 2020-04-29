#' Model Using Split Times
#'
#' This function models the sprint split times using mono-exponential equation that estimates
#'     maximum sprinting speed (\code{MSS}) and relative acceleration (\code{TAU}). \code{time} is used as target or outcome
#'     variable, and \code{distance} as predictor.
#' @param distance Numeric vector
#' @param time Numeric vector
#' @param time_correction Numeric vector. Used to correct for different starting techniques. This correction is
#'     done by adding \code{time_correction} to \code{time}. Default is 0. See more in Haugen et al. (2018)
#' @param weights Numeric vector. Default is vector of 1
#'     This is used to give more weight to particular observations. For example, use \code{1\\distance} to give
#'     more weight to observations from shorter distances.
#' @param na.rm Logical. Default is FALSE
#' @param ... Forwarded to \code{\link[stats]{nls}} function
#' @return List object with the following elements:
#'     \describe{
#'         \item{parameters}{List with the following estimated parameters:
#'             \code{MSS}, \code{TAU}, \code{MAC}, and \code{PMAX}}
#'         \item{model_fit}{List with the following components:
#'             \code{RSE}, \code{R_squared}, \code{minErr}, \code{maxErr}, and \code{RMSE}}
#'         \item{model}{Model returned by the \code{\link[stats]{nls}} function}
#'         \item{data}{Data frame used to estimate the sprint parameters, consisting of \code{distance},
#'             \code{time}, \code{time_correction}, \code{corrected_time}, \code{weights},and \code{pred_time} columns}
#'         }
#' @export
#' @references
#'     Haugen TA, Tønnessen E, Seiler SK. 2012. The Difference Is in the Start: Impact of Timing and Start
#'         Procedure on Sprint Running Performance: Journal of Strength and Conditioning Research 26:473–479.
#'         DOI: 10.1519/JSC.0b013e318226030b.
#' @examples
#' split_times <- data.frame(
#' distance = c(5, 10, 20, 30, 35),
#' time = c(1.21, 1.99, 3.38, 4.71, 5.30)
#' )
#'
#' sprint_model <- with(
#'   split_times,
#'   model_using_split_times(distance, time)
#' )
#'
#' sprint_model$parameters
model_using_split_times <- function(distance,
                                    time,
                                    time_correction = 0,
                                    weights = 1,
                                    na.rm = FALSE,
                                    ...) {
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

  # Non-linear model
  speed_mod <- stats::nls(
    corrected_time ~ TAU * I(LambertW::W(-exp(1)^(-distance / (MSS * TAU) - 1))) + distance / MSS + TAU,
    data = df,
    start = list(MSS = 7, TAU = 0.8),
    weights = df$weights,
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
  pred_time <- TAU * I(LambertW::W(-exp(1)^(-df$distance / (MSS * TAU) - 1))) + df$distance / MSS + TAU

  # Adjust predicted time to be comparable to original time
  pred_time <- pred_time - time_correction

  RSE <- summary(speed_mod)$sigma
  R_squared <- stats::cor(df$time, pred_time)^2
  minErr <- min(pred_time - df$time)
  maxErr <- max(pred_time - df$time)
  RMSE <- sqrt(mean((pred_time - df$time)^2))

  # Add predicted time to df
  df$pred_time <- pred_time

  # Return object
  return(list(
    parameters = list(
      MSS = MSS,
      TAU = TAU,
      MAC = MAC,
      PMAX = PMAX),
    model_fit = list(
      RSE = RSE,
      R_squared = R_squared,
      minErr = minErr,
      maxErr = maxErr,
      RMSE = RMSE
    ),
    model = speed_mod,
    data = df
  ))
}


#' Model Using Split Times with Time Correction
#'
#' This function models the sprint split times using mono-exponential equation that estimates
#'     maximum sprinting speed (\code{MSS}), relative acceleration (\code{TAU}) and \code{time_correction}.
#'    \code{time} is used as target or outcome variable, and \code{distance} as predictor.
#' @param distance Numeric vector
#' @param time Numeric vector
#' @param weights Numeric vector. Default is vector of 1
#'     This is used to give more weight to particular observations. For example, use \code{1\\distance} to give
#'     more weight to observations from shorter distances.
#' @param na.rm Logical. Default is FALSE
#' @param ... Forwarded to \code{\link[stats]{nls}} function
#' @return List object with the following elements:
#'     \describe{
#'         \item{parameters}{List with the following estimated parameters:
#'             \code{MSS}, \code{TAU}, \code{MAC}, \code{PMAX}, and \code{time_correction}}
#'         \item{model_fit}{List with the following components:
#'             \code{RSE}, \code{R_squared}, \code{minErr}, \code{maxErr}, and \code{RMSE}}
#'         \item{model}{Model returned by the \code{\link[stats]{nls}} function}
#'         \item{data}{Data frame used to estimate the sprint parameters, consisting of \code{distance},
#'             \code{time}, \code{weights},and \code{pred_time} columns}
#'         }
#' @export
#' @examples
#' split_times <- data.frame(
#' distance = c(5, 10, 20, 30, 35),
#' time = c(1.21, 1.99, 3.38, 4.71, 5.30)
#' )
#'
#' sprint_model <- with(
#'   split_times,
#'   model_using_split_times_with_time_correction(distance, time)
#' )
#'
#' sprint_model$parameters
model_using_split_times_with_time_correction <- function(distance,
                                    time,
                                    weights = 1,
                                    na.rm = FALSE,
                                    ...) {
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

  # Non-linear model
  speed_mod <- stats::nls(
    time ~ TAU * I(LambertW::W(-exp(1)^(-distance / (MSS * TAU) - 1))) + distance / MSS + TAU  - time_correction,
    data = df,
    start = list(MSS = 7, TAU = 0.8, time_correction = 0),
    weights = df$weights,
    ...
  )

  # Maximal Sprinting Speed
  MSS <- stats::coef(speed_mod)[[1]]
  TAU <- stats::coef(speed_mod)[[2]]
  time_correction <-  stats::coef(speed_mod)[[3]]

  # Maximal acceleration
  MAC <- MSS / TAU

  # Maximal Power (relative)
  PMAX <- (MSS * MAC) / 4

  # Model fit
  pred_time <- TAU * I(LambertW::W(-exp(1)^(-df$distance / (MSS * TAU) - 1))) + df$distance / MSS + TAU - time_correction

  RSE <- summary(speed_mod)$sigma
  R_squared <- stats::cor(df$time, pred_time)^2
  minErr <- min(pred_time - df$time)
  maxErr <- max(pred_time - df$time)
  RMSE <- sqrt(mean((pred_time - df$time)^2))

  # Add predicted time to df
  df$pred_time <- pred_time

  # Return object
  return(list(
    parameters = list(
      MSS = MSS,
      TAU = TAU,
      MAC = MAC,
      PMAX = PMAX,
      time_correction = time_correction),
    model_fit = list(
      RSE = RSE,
      R_squared = R_squared,
      minErr = minErr,
      maxErr = maxErr,
      RMSE = RMSE
    ),
    model = speed_mod,
    data = df
  ))
}

#' Model Using Split Times with Corrections
#'
#' This function models the sprint split times using mono-exponential equation that estimates
#'     maximum sprinting speed (\code{MSS}), relative acceleration (\code{TAU}), \code{time_correction},
#'     and \code{distance_correcion}. \code{time} is used as target or outcome variable, and \code{distance}
#'     as predictor.
#' @param distance Numeric vector
#' @param time Numeric vector
#' @param weights Numeric vector. Default is vector of 1
#'     This is used to give more weight to particular observations. For example, use \code{1\\distance} to give
#'     more weight to observations from shorter distances.
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
#'             \code{time}, \code{weights},and \code{pred_time} columns}
#'         }
#' @export
#' @examples
#' split_times <- data.frame(
#' distance = c(5, 10, 20, 30, 35),
#' time = c(1.20, 1.96, 3.36, 4.71, 5.35)
#' )
#'
#' sprint_model <- with(
#'   split_times,
#'   model_using_split_times_with_corrections(distance, time)
#' )
#'
#' sprint_model$parameters
model_using_split_times_with_corrections <- function(distance,
                                                         time,
                                                         weights = 1,
                                                         na.rm = FALSE,
                                                     ...) {

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
  # Non-linear model
  speed_mod <- stats::nls(
    time ~ TAU * I(LambertW::W(-exp(1)^(-(distance + distance_correction) / (MSS * TAU) - 1))) + (distance + distance_correction) / MSS + TAU  - time_correction,
    data = df,
    start = list(MSS = 7, TAU = 0.8, time_correction = 0, distance_correction = 0),
    weights = df$weights,
    ...
  )

  # Maximal Sprinting Speed
  MSS <- stats::coef(speed_mod)[[1]]
  TAU <- stats::coef(speed_mod)[[2]]
  time_correction <-  stats::coef(speed_mod)[[3]]
  distance_correction <- stats::coef(speed_mod)[[4]]

  # Maximal acceleration
  MAC <- MSS / TAU

  # Maximal Power (relative)
  PMAX <- (MSS * MAC) / 4

  # Model fit
  pred_time <- TAU * I(LambertW::W(-exp(1)^(-(distance + distance_correction) / (MSS * TAU) - 1))) + (distance + distance_correction) / MSS + TAU  - time_correction

  RSE <- summary(speed_mod)$sigma
  R_squared <- stats::cor(df$time, pred_time)^2
  minErr <- min(pred_time - df$time)
  maxErr <- max(pred_time - df$time)
  RMSE <- sqrt(mean((pred_time - df$time)^2))

  # Add predicted time to df
  df$pred_time <- pred_time

  # Return object
  return(list(
    parameters = list(
      MSS = MSS,
      TAU = TAU,
      MAC = MAC,
      PMAX = PMAX,
      time_correction = time_correction,
      distance_correction = distance_correction),
    model_fit = list(
      RSE = RSE,
      R_squared = R_squared,
      minErr = minErr,
      maxErr = maxErr,
      RMSE = RMSE
    ),
    model = speed_mod,
    data = df
  ))
}

#' Mixed Model Using Split Times
#'
#' This function models the sprint split times using mono-exponential equation and non-linear
#'     mixed model using \code{\link[nlme]{nlme}} to estimate fixed and random maximum sprinting speed (\code{MSS})
#'     and relative acceleration (\code{TAU}) parameters. In mixed model, fixed and random effects are estimated for
#'     \code{MSS} and \code{TAU} parameters using \code{athlete} as levels. \code{time} is used as target or outcome
#'     variable, and \code{distance} as predictor.
#' @param data Data frame
#' @param distance Character string. Name of the column in \code{data}
#' @param time Character string. Name of the column in \code{data}
#' @param time_correction Numeric vector. Used to correct for different starting techniques.  This correction is
#'     done by adding \code{time_correction} to \code{time}. Default is 0. See more in Haugen et al. (2018)
#' @param athlete Character string. Name of the column in \code{data}. Used as levels in the \code{\link[nlme]{nlme}}
#' @param na.rm Logical. Default is FALSE
#' @param ... Forwarded to \code{\link[nlme]{nlme}} function
#' @return List object with the following elements:
#'     \describe{
#'         \item{parameters}{List with two data frames: \code{fixed} and \code{random} containing the following
#'             estimated parameters: \code{MSS}, \code{TAU}, \code{MAC}, and \code{PMAX}}
#'         \item{model_fit}{List with the following components:
#'             \code{RSE}, \code{R_squared}, \code{minErr}, \code{maxErr}, and \code{RMSE}}
#'         \item{model}{Model returned by the \code{\link[nlme]{nlme}} function}
#'         \item{data}{Data frame used to estimate the sprint parameters, consisting of \code{athlete}, \code{distance},
#'             \code{time}, and \code{time_correction}, \code{corrected_time}, and \code{pred_time} columns}
#'         }
#' @export
#' @references
#'     Haugen TA, Tønnessen E, Seiler SK. 2012. The Difference Is in the Start: Impact of Timing and Start
#'         Procedure on Sprint Running Performance: Journal of Strength and Conditioning Research 26:473–479.
#'         DOI: 10.1519/JSC.0b013e318226030b.
#' @examples
#' data("split_times")
#'
#' mixed_model <- mixed_model_using_split_times(split_times, "distance", "time", "athlete")
#' mixed_model$parameters
mixed_model_using_split_times <- function(data,
                                          distance,
                                          time,
                                          athlete,
                                          time_correction = 0,
                                    # weights = rep(1, nrow(data)),
                                    na.rm = FALSE,
                                    ...) {

  # Combine to DF
  df <- data.frame(
    athlete = data[[athlete]],
    distance = data[[distance]],
    time = data[[time]],
    time_correction = time_correction,
    corrected_time = data[[time]] + time_correction#,
    #weights = weights
  )

  # Remove NAs
  if (na.rm) {
    df <- stats::na.omit(df)
  }

  # Create mixed model
  mixed_model <- nlme::nlme(
    corrected_time~TAU*I(LambertW::W(-exp(1)^(-distance/(MSS*TAU)-1))) + distance / MSS + TAU,
    data = df,
    fixed = MSS + TAU~1,
    random = MSS + TAU~1,
    groups = ~athlete,
    # weights = ~weights,
    start = c(MSS=7,TAU=0.8),
    ...
  )

  # Pull estimates
  fixed_effects <- nlme::fixed.effects(mixed_model)
  random_effects <- nlme::random.effects(mixed_model)

  # Fixed effects
  fixed_effects <- data.frame(t(fixed_effects))
  fixed_effects$MAC <- fixed_effects$MSS / fixed_effects$TAU
  fixed_effects$PMAX <- (fixed_effects$MSS * fixed_effects$MAC) / 4


  random_effects$athlete <- rownames(random_effects)
  random_effects$MSS <- random_effects$MSS + fixed_effects$MSS
  random_effects$TAU <- random_effects$TAU + fixed_effects$TAU
  rownames(random_effects) <- NULL
  random_effects <- random_effects[c("athlete", "MSS", "TAU")]
  random_effects$MAC <- random_effects$MSS / random_effects$TAU
  random_effects$PMAX <- (random_effects$MSS * random_effects$MAC) / 4

   # Model fit

  pred_time <- stats::predict(mixed_model, newdata = df)
  pred_time <- pred_time - time_correction

  RSE <- summary(mixed_model)$sigma
  R_squared <- stats::cor(df$time, pred_time)^2
  minErr <- min(pred_time - df$time)
  maxErr <- max(pred_time - df$time)
  RMSE <- sqrt(mean((pred_time -df$time)^2))

  # Add predicted time to df
  df$pred_time <- pred_time

 return(list(
   parameters = list(
     fixed = fixed_effects,
     random = random_effects),
   model_fit = list(
     RSE = RSE,
     R_squared = R_squared,
     minErr = minErr,
     maxErr = maxErr,
     RMSE = RMSE
   ),
   model = mixed_model,
   data = df
 ))

}

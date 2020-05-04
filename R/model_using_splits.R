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
#' unlist(simple_model$parameters)
#'
#' # Model with correction of 0.3s
#' model_with_correction <- with(
#'   split_times,
#'   model_using_splits(distance, time, time_correction = 0.3)
#' )
#' unlist(model_with_correction$parameters)
#'
#' # Model with time_correction estimation
#' model_with_time_correction_estimation <- with(
#'   split_times,
#'   model_using_splits_with_time_correction(distance, time)
#' )
#' unlist(model_with_time_correction_estimation$parameters)
#'
#' # Model with time and distance correction estimation
#' model_with_time_distance_correction_estimation <- with(
#'   split_times,
#'   model_using_splits_with_corrections(distance, time)
#' )
#' unlist(model_with_time_distance_correction_estimation$parameters)
#' @name model_split_times
NULL

# =====================================================================================================================================
#' @rdname model_split_times
#' @export
model_using_splits <- function(distance,
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
  # Add predicted time to df
  df <- data.frame(
    distance = distance,
    time = time,
    weights = weights,
    pred_time = pred_time
  )

  # Return object
  return(list(
    parameters = list(
      MSS = MSS,
      TAU = TAU,
      MAC = MAC,
      PMAX = PMAX
    ),
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

# =====================================================================================================================================
#' @rdname model_split_times
#' @export
model_using_splits_with_time_correction <- function(distance,
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
    time ~ TAU * I(LambertW::W(-exp(1)^(-distance / (MSS * TAU) - 1))) + distance / MSS + TAU - time_correction,
    data = df,
    start = list(MSS = 7, TAU = 0.8, time_correction = 0),
    weights = df$weights,
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
  pred_time <- TAU * I(LambertW::W(-exp(1)^(-df$distance / (MSS * TAU) - 1))) + df$distance / MSS + TAU - time_correction

  RSE <- summary(speed_mod)$sigma
  R_squared <- stats::cor(df$time, pred_time)^2
  minErr <- min(pred_time - df$time)
  maxErr <- max(pred_time - df$time)
  RMSE <- sqrt(mean((pred_time - df$time)^2))

  # Add predicted time to df
  df <- data.frame(
    distance = distance,
    time = time,
    weights = weights,
    pred_time = pred_time
  )

  # Return object
  return(list(
    parameters = list(
      MSS = MSS,
      TAU = TAU,
      MAC = MAC,
      PMAX = PMAX,
      time_correction = time_correction
    ),
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

# =====================================================================================================================================
#' @rdname model_split_times
#' @export
model_using_splits_with_corrections <- function(distance,
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
    time ~ TAU * I(LambertW::W(-exp(1)^(-(distance + distance_correction) / (MSS * TAU) - 1))) + (distance + distance_correction) / MSS + TAU - time_correction,
    data = df,
    start = list(MSS = 7, TAU = 0.8, time_correction = 0, distance_correction = 0),
    weights = df$weights,
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
  pred_time <- TAU * I(LambertW::W(-exp(1)^(-(distance + distance_correction) / (MSS * TAU) - 1))) + (distance + distance_correction) / MSS + TAU - time_correction

  RSE <- summary(speed_mod)$sigma
  R_squared <- stats::cor(df$time, pred_time)^2
  minErr <- min(pred_time - df$time)
  maxErr <- max(pred_time - df$time)
  RMSE <- sqrt(mean((pred_time - df$time)^2))

  # Add predicted time to df
  # Add predicted time to df
  df <- data.frame(
    distance = distance,
    time = time,
    weights = weights,
    pred_time = pred_time
  )

  # Return object
  return(list(
    parameters = list(
      MSS = MSS,
      TAU = TAU,
      MAC = MAC,
      PMAX = PMAX,
      time_correction = time_correction,
      distance_correction = distance_correction
    ),
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

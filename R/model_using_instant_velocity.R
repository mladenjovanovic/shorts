#' Model Using Instantaneous Velocity
#'
#' This function models the sprint instantaneous velocity using mono-exponential equation that estimates
#'     maximum sprinting speed (\code{MSS}) and relative acceleration (\code{TAU})
#' @param time Numeric vector
#' @param velocity Numeric vector
#' @param time_delay Numeric vector. Used to filter out noisy data from the radar gun. Default is 0. See more
#'     in Samozino (2018)
#' @param weights Numeric vector. Default is vector of 1s in lengths of \code{time} parameter
#' @param na.rm Logical. Default is FALSE
#' @return List object with the following elements:
#'     \describe{
#'         \item{parameters}{List with the following estimated parameters:
#'             \code{MSS}, \code{TAU}, \code{MAC}, and \code{PMAX}}
#'         \item{model_fit}{List with the following components:
#'             \code{RSE}, \code{R_squared}, \code{minErr}, \code{maxErr},
#'             \code{maxAbsErr}, and \code{RMSE}}
#'         \item{model}{Model returned by the \code{\link[stats]{nls}} function}
#'         \item{data}{Data frame used to estimate the sprint parameters, consisting of \code{distance}
#'             and \code{time} columns}
#'         }
#' @export
#' @references
#'     Samozino P. 2018. A Simple Method for Measuring Force, Velocity and Power Capabilities and Mechanical
#'         Effectiveness During Sprint Running. In: Morin J-B, Samozino P eds. Biomechanics of Training and Testing.
#'        Cham: Springer International Publishing, 237–267. DOI: 10.1007/978-3-319-05633-3_11.
#'
#' @examples
#' instant_velocity <- data.frame(
#' time = c(0, 1, 2, 3, 4, 5, 6),
#' velocity = c(0.00, 4.99, 6.43, 6.84, 6.95, 6.99, 7.00)
#' )
#'
#' sprint_model <- with(
#'   instant_velocity,
#'   model_using_instant_velocity(time, velocity)
#' )
#'
#' sprint_model$parameters
model_using_instant_velocity <- function(time,
                                    velocity,
                                    time_delay = 0,
                                    weights = rep(1, length(time)),
                                    na.rm = FALSE) {

  # Remove NAs pairwise
  if (na.rm) {
    na.idx <- is.na(time) | is.na(velocity)
    time <- time[!na.idx]
    velocity <- velocity[!na.idx]
  }

  # Put data into data frame
  df <- data.frame(time = time, velocity = velocity, time_delay = time_delay)

  # Non-linear model
  speed_mod <- stats::nls(
    velocity ~ MSS * (1 - exp(1)^(-(time - time_delay)/TAU)),
    data = df,
    start = list(MSS = 7, TAU = 0.8),
    weights = weights
  )

  # Maximal Sprinting Speed
  MSS <- stats::coef(speed_mod)[[1]]
  TAU <- stats::coef(speed_mod)[[2]]

  # Maximal acceleration
  MAC <- MSS / TAU

  # Maximal Power (relative)
  PMAX <- (MSS * MAC) / 4

  # Model fit
  RSE <- summary(speed_mod)$sigma
  R_squared <- stats::cor(time, stats::fitted(speed_mod))^2
  minErr <- min(stats::fitted(speed_mod) - velocity)
  maxErr <- max(stats::fitted(speed_mod) - velocity)
  maxAbsErr <- max(abs(stats::fitted(speed_mod) - velocity))
  RMSE <- sqrt(mean((stats::fitted(speed_mod) - velocity)^2))

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
      maxAbsErr = maxAbsErr,
      RMSE = RMSE
    ),
    model = speed_mod,
    data = df
  ))
}

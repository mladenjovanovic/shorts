#' Model Using Split Times
#'
#' This function models the sprint split times using mono-exponential equation that estimates
#'     maximum sprinting speed (\code{MSS}) and relative acceleration (\code{TAU})
#' @param distance Numeric vector
#' @param time Numeric vector
#' @param weights Numeric vector. Default is vector of 1s in lengths of \code{distance} parameter
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
                                    weights = rep(1, length(distance)),
                                    na.rm = FALSE) {

  # Remove NAs pairwise
  if (na.rm) {
    na.idx <- is.na(distance) | is.na(time)
    distance <- distance[!na.idx]
    time <- time[!na.idx]
  }

  # Put data into data frame
  df <- data.frame(distance = distance, time = time)

  # Non-linear model
  speed_mod <- stats::nls(
    time ~ TAU * I(LambertW::W(-exp(1)^(-distance / (MSS * TAU) - 1))) + distance / MSS + TAU,
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
  minErr <- min(stats::fitted(speed_mod) - time)
  maxErr <- max(stats::fitted(speed_mod) - time)
  maxAbsErr <- max(abs(stats::fitted(speed_mod) - time))
  RMSE <- sqrt(mean((stats::fitted(speed_mod) - time)^2))

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

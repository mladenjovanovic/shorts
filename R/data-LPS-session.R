#' LPS Basketball Session Dataset
#'
#'
#' @details
#' This dataset represents a sample data provided by Local Positioning System (LPS) on a single individual
#'     performing a single basketball practice session (aprox. 90min). Sampling frequency is 20Hz.
#'
#' @format Data frame with 5 variables and 91,099 observations:
#' \describe{
#'    \item{time}{Time in seconds from the start of the session}
#'    \item{x}{x-coordinate in meters provided by the LPS}
#'    \item{y}{y-coordinate in meters provided by the LPS}
#'    \item{velocity}{Velocity provided by LPS in m/s}
#'    \item{acceleration}{Acceleration provided by LPS in m/s}
#' }
#' @usage data(LPS_session)
"LPS_session"

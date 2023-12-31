#' Create Sprint Trace
#'
#' This function creates sprint trace either using \code{time} or \code{distance} vectors
#' @param MSS,MAC  Numeric vector. Model parameters
#' @param time Numeric vector.
#' @param distance Numeric vector.
#' @param TC Numeric vector. Time-shift added to sprint times. Default is 0
#' @param DC Numeric vector. Distance-shift added to sprint distance. Default is 0
#' @param FD Numeric vector. Flying start distance. Default is 0
#' @param remove_leading Should trace leading to sprint be removed? Default is \code{FALSE}
#' @return Data-frame with following 6 columns
#'     \describe{
#'        \item{time}{Measurement-scale time vector in seconds. Equal to parameter \code{time}}
#'        \item{distance}{Measurement-scale distance vector in meters. Equal to parameter \code{distance}}
#'        \item{velocity}{Velocity vector in m/s}
#'        \item{acceleration}{Acceleration vector in m/s/s}
#'        \item{sprint_time}{Sprint scale time vector in seconds. Sprint always start at t=0s}
#'        \item{sprint_distance}{Sprint scale distance vector in meters. Sprint always start at d=0m}
#'     }
#' @export
#' @seealso \code{\link{create_timing_gates_splits}}
#' @examples
#'
#' df <- create_sprint_trace(8, 7, time = seq(0, 6, by = 0.01))
#' df <- create_sprint_trace(8, 7, distance = seq(0, 40, by = 1))
#'
#' @export
create_sprint_trace <- function(MSS,
                                MAC,
                                time = NULL,
                                distance = NULL,
                                TC = 0,
                                DC = 0,
                                FD = 0,
                                remove_leading = FALSE) {
  if (!is.null(time) & !is.null(distance)) {
    stop("Please use either time or distance vector, not both.", call. = FALSE)
  }

  if (FD < 0) {
    stop("Flying start distance (FD) cannot be negative number.", call. = FALSE)
  }

  # TRUE sprint performance data, with the start at d=0 and t=0
  FD_time <- predict_time_at_distance(FD, MSS, MAC)

  if (!is.null(time)) {
    df <- data.frame(time = time)
    df$sprint_time <- df$time + FD_time - TC
    df$sprint_time <- ifelse(df$sprint_time < 0, 0, df$sprint_time)

    df$sprint_distance <- predict_distance_at_time(
      time = df$sprint_time,
      MSS = MSS,
      MAC = MAC
    )

    df$distance <- df$sprint_distance + DC - FD

    if (remove_leading == TRUE) {
      df <- df[df$time >= TC, ]
    }
  } else if (!is.null(distance)) {
    df <- data.frame(distance = distance)
    df$sprint_distance <- df$distance + FD - DC
    df$sprint_distance <- ifelse(df$sprint_distance < 0, 0, df$sprint_distance)

    df$sprint_time <- predict_time_at_distance(
      distance = df$sprint_distance,
      MSS = MSS,
      MAC = MAC
    )

    df$time <- df$sprint_time + TC - FD_time

    if (remove_leading == TRUE) {
      df <- df[df$distance >= DC, ]
    }
  }

  # Add velocity and acceleration
  df$velocity <- predict_velocity_at_time(
    time = df$sprint_time,
    MSS = MSS,
    MAC = MAC
  )

  df$velocity <- ifelse(df$velocity < 0, 0, df$velocity)

  df$acceleration <- predict_acceleration_at_time(
    time = df$sprint_time,
    MSS = MSS,
    MAC = MAC
  )

  df$acceleration <- ifelse(df$time < TC, 0, df$acceleration)

  data.frame(
    time = df$time,
    distance = df$distance,
    velocity = df$velocity,
    acceleration = df$acceleration,
    sprint_time = df$sprint_time,
    sprint_distance = df$sprint_distance
  )
}

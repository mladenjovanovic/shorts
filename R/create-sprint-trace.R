#' Create Sprint Trace
#'
#' This function creates sprint trace either using `time` or `distance` vectors
#' @param MSS,MAC  Numeric vector. Model parameters
#' @param time Numeric vector.
#' @param distance Numeric vector.
#' @param TC Numeric vector. Time-shift added to sprint times. Default is 0
#' @param DC Numeric vector. Distance-shift added to sprint distance. Default is 0
#' @param FD Numeric vector. Flying start distance. Default is 0
#' @return Data-frame with following 6 columns
#'     \describe{
#'        \item{time}{Measurement-scale time vector in seconds}
#'        \item{distance}{Measurement-scale distance vector in meters}
#'        \item{velocity}{Velocity vector in m/s}
#'        \item{acceleration}{Acceleration vector in m/s/s}
#'        \item{sprint_time}{Sprint scale time vector in seconds. Sprint always start at t=0s}
#'        \item{sprint_distance}{Sprint scale distance vector in meters. Sprint always start at d=0m}
#'     }
#' @export
#' @examples
#' df <- create_sprint_trace(8, 7, time = seq(0, 6, by = 0.01))
#'
#' df <- create_sprint_trace(8, 7, distance = seq(0, 40, by = 1))
#'
#' @export
create_sprint_trace <- function(MSS,
                                MAC,
                                time = NULL,
                                distance = NULL,
                                TC = 0,
                                DC = 0,
                                FD = 0) {
  if (!is.null(time) & !is.null(distance)) {
    stop("Please use either time or distance vector, not both.", call. = FALSE)
  }

  # TRUE sprint performance data, with the start at d=0 and t=0
  if (!is.null(time)) {
    df <- data.frame(sprint_time = time)

    df$sprint_distance <- predict_distance_at_time(
      time = df$sprint_time,
      MSS = MSS,
      MAC = MAC
    )
  } else if (!is.null(distance)) {
    df <- data.frame(sprint_distance = distance)

    df$sprint_time <- predict_time_at_distance(
      distance = df$sprint_distance,
      MSS = MSS,
      MAC = MAC
    )
  }

  # Add velocity and acceleration
  df$velocity <- predict_velocity_at_time(
    time = df$sprint_time,
    MSS = MSS,
    MAC = MAC
  )

  df$acceleration <- predict_acceleration_at_time(
    time = df$sprint_time,
    MSS = MSS,
    MAC = MAC
  )

  # Filter-out everything below flying-distance FD since this is not seen
  # by the measurement device
  df <- df[df$sprint_distance >= FD, ]

  # Now, set this to be t=0 and d=0
  df$time <- df$sprint_time - df$sprint_time[1]
  df$distance <- df$sprint_distance - df$sprint_distance[1]

  # Now add TC and DC
  df$time <- df$time + TC
  df$distance <- df$distance + DC

  data.frame(
    time = df$time,
    distance = df$distance,
    velocity = df$velocity,
    acceleration = df$acceleration,
    sprint_time = df$sprint_time,
    sprint_distance = df$sprint_distance
  )
}

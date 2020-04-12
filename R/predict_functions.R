#' Kinematics prediction functions
#'
#' Predicts kinematic from known \code{MSS} and \code{TAU} parameters
#' @param time,distance,velocity Numeric vectors
#' @param MSS,TAU Numeric vectors. Model parameters
#' @examples
#' require(tidyverse)
#'
#' MSS <- 8
#' TAU <- 0.7
#'
#' df <- tibble(
#'   time = seq(0, 6, length.out = 10),
#'   distance_at_time = predict_distance_at_time(time, MSS, TAU),
#'   velocity_at_time = predict_velocity_at_time(time, MSS, TAU),
#'   acceleration_at_time = predict_acceleration_at_time(time, MSS, TAU),
#'   time_at_distance = predict_time_at_distance( distance_at_time, MSS, TAU),
#'   velocity_at_distance = predict_velocity_at_distance(distance_at_time, MSS, TAU),
#'   acceleration_at_distance = predict_acceleration_at_distance(distance_at_time, MSS, TAU),
#'   acceleration_at_velocity = predict_acceleration_at_velocity(velocity_at_time, MSS, TAU)
#' )
#'
#' df
#' @name predict_kinematics
NULL

#' @rdname predict_kinematics
#' @export
predict_velocity_at_time <- function(time, MSS, TAU) {
  MSS * (1 - exp(1)^(-(time/TAU)))
}

#' @rdname predict_kinematics
#' @export
predict_distance_at_time <- function(time, MSS, TAU) {
  MSS * (time + TAU * exp(1)^(-time/TAU)) - MSS * TAU
}

#' @rdname predict_kinematics
#' @export
predict_acceleration_at_time <- function(time, MSS, TAU) {
  MSS / TAU * exp(1)^(-time/TAU)
}

#' @rdname predict_kinematics
#' @export
predict_time_at_distance <- function(distance, MSS, TAU) {
  TAU*I(LambertW::W(-exp(1)^(-distance/(MSS*TAU)-1))) + distance / MSS + TAU
}

#' @rdname predict_kinematics
#' @export
predict_velocity_at_distance <- function(distance, MSS, TAU) {
  time_at_distance <- predict_time_at_distance(distance, MSS, TAU)

  predict_velocity_at_time(time_at_distance, MSS, TAU)
}

#' @rdname predict_kinematics
#' @export
predict_acceleration_at_distance <- function(distance, MSS, TAU) {
  time_at_distance <- predict_time_at_distance(distance, MSS, TAU)

  predict_acceleration_at_time(time_at_distance, MSS, TAU)
}

#' @rdname predict_kinematics
#' @export
predict_acceleration_at_velocity <- function(velocity, MSS, TAU) {
    MAC <- MSS / TAU

    slope <- MAC / MSS

    ifelse(
      velocity < 0,
      MAC,
      ifelse(
        velocity > MSS,
        0,
        MAC - velocity * slope
      )
    )
}

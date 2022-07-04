#' Kinematics prediction functions
#'
#' Predicts kinematic from known \code{MSS} and \code{MAC} parameters
#'
#'
#' @param time,distance,velocity Numeric vectors
#' @param MSS,MAC Numeric vectors. Model parameters
#' @param bodymass Body mass in kg. Used to calculate relative power and forwarded to \code{\link{get_air_resistance}}
#' @param ... Forwarded to \code{\link{get_air_resistance}} for the purpose of calculation of air resistance and power
#' @return Numeric vector
#' @references
#' Haugen TA, Tønnessen E, Seiler SK. 2012. The Difference Is in the Start: Impact of Timing and Start
#'         Procedure on Sprint Running Performance: Journal of Strength and Conditioning Research 26:473–479.
#'         DOI: 10.1519/JSC.0b013e318226030b.
#'
#' Jovanović, M., Vescovi, J.D. (2020). shorts: An R Package for Modeling Short Sprints. Preprint
#'         available at SportRxiv. https://doi.org/10.31236/osf.io/4jw62
#'
#' Samozino P. 2018. A Simple Method for Measuring Force, Velocity and Power Capabilities and Mechanical
#'         Effectiveness During Sprint Running. In: Morin J-B, Samozino P eds. Biomechanics of Training and Testing.
#'        Cham: Springer International Publishing, 237–267. DOI: 10.1007/978-3-319-05633-3_11.
#' @examples
#' MSS <- 8
#' MAC <- 9
#'
#' time_seq <- seq(0, 6, length.out = 10)
#'
#' df <- data.frame(
#'   time = time_seq,
#'   distance_at_time = predict_distance_at_time(time_seq, MSS, MAC),
#'   velocity_at_time = predict_velocity_at_time(time_seq, MSS, MAC),
#'   acceleration_at_time = predict_acceleration_at_time(time_seq, MSS, MAC)
#' )
#'
#' df$time_at_distance <- predict_time_at_distance(df$distance_at_time, MSS, MAC)
#' df$velocity_at_distance <- predict_velocity_at_distance(df$distance_at_time, MSS, MAC)
#' df$acceleration_at_distance <- predict_acceleration_at_distance(df$distance_at_time, MSS, MAC)
#' df$acceleration_at_velocity <- predict_acceleration_at_velocity(df$velocity_at_time, MSS, MAC)
#'
#' # Power calculation uses shorts::get_air_resistance function and its defaults
#' # values to calculate power. Use the ... to setup your own parameters for power
#' # calculations
#' df$power_at_time <- predict_power_at_time(
#'   time = df$time, MSS = MSS, MAC = MAC,
#'   # Check shorts::get_air_resistance for available params
#'   bodymass = 100, bodyheight = 1.85
#' )
#'
#' df
#' @name predict_kinematics
NULL

#' @rdname predict_kinematics
#' @export
predict_velocity_at_time <- function(time, MSS, MAC) {
  TAU <- MSS / MAC
  MSS * (1 - exp(1)^(-(time / TAU)))
}

#' @rdname predict_kinematics
#' @export
predict_distance_at_time <- function(time, MSS, MAC) {
  TAU <- MSS / MAC
  (MSS * (time + TAU * exp(1)^(-time / TAU)) - MSS * TAU)
}

#' @rdname predict_kinematics
#' @export
predict_acceleration_at_time <- function(time, MSS, MAC) {
  TAU <- MSS / MAC
  MSS / TAU * exp(1)^(-time / TAU)
}

#' @rdname predict_kinematics
#' @export
predict_time_at_distance <- function(distance, MSS, MAC) {
  TAU <- MSS / MAC
  TAU * I(LambertW::W(-exp(1)^(-(distance) / (MSS * TAU) - 1))) + (distance) / MSS + TAU
}

#' @rdname predict_kinematics
#' @export
predict_velocity_at_distance <- function(distance, MSS, MAC) {
  time_at_distance <- predict_time_at_distance(distance, MSS, MAC)

  predict_velocity_at_time(time_at_distance, MSS, MAC)
}

#' @rdname predict_kinematics
#' @export
predict_acceleration_at_distance <- function(distance, MSS, MAC) {
  time_at_distance <- predict_time_at_distance(distance, MSS, MAC)

  predict_acceleration_at_time(time_at_distance, MSS, MAC)
}

#' @rdname predict_kinematics
#' @export
predict_acceleration_at_velocity <- function(velocity, MSS, MAC) {
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

#' @rdname predict_kinematics
#' @export
predict_air_resistance_at_time <- function(time, MSS, MAC, ...) {
  vel <- predict_velocity_at_time(time, MSS, MAC)

  get_air_resistance(velocity = vel, ...)
}

#' @rdname predict_kinematics
#' @export
predict_air_resistance_at_distance <- function(distance, MSS, MAC, ...) {
  vel <- predict_velocity_at_distance(distance, MSS, MAC)

  get_air_resistance(velocity = vel, ...)
}

#' @rdname predict_kinematics
#' @export
predict_force_at_time <- function(time, MSS, MAC, bodymass = 75, ...) {
  acc <- predict_acceleration_at_time(time, MSS, MAC)
  vel <- predict_velocity_at_time(time, MSS, MAC)
  air_res <- get_air_resistance(velocity = vel, bodymass = bodymass, ...)

  bodymass * acc + air_res
}

#' @rdname predict_kinematics
#' @export
predict_force_at_distance <- function(distance, MSS, MAC, bodymass = 75, ...) {
  acc <- predict_acceleration_at_distance(distance, MSS, MAC)
  vel <- predict_velocity_at_distance(distance, MSS, MAC)
  air_res <- get_air_resistance(velocity = vel, bodymass = bodymass, ...)

  bodymass * acc + air_res
}

#' @rdname predict_kinematics
#' @export
predict_power_at_distance <- function(distance, MSS, MAC, bodymass = 75, ...) {
  acc <- predict_acceleration_at_distance(distance, MSS, MAC)
  vel <- predict_velocity_at_distance(distance, MSS, MAC)
  air_res <- get_air_resistance(velocity = vel, bodymass = bodymass, ...)

  (bodymass * acc + air_res) * vel
}


#' @rdname predict_kinematics
#' @export
predict_power_at_time <- function(time, MSS, MAC, bodymass = 75, ...) {
  acc <- predict_acceleration_at_time(time, MSS, MAC)
  vel <- predict_velocity_at_time(time, MSS, MAC)
  air_res <- get_air_resistance(velocity = vel, bodymass = bodymass, ...)

  (bodymass * acc + air_res) * vel
}

#' @rdname predict_kinematics
#' @export
predict_relative_power_at_distance <- function(distance, MSS, MAC, bodymass = 75, ...) {
  predict_power_at_distance(
    distance = distance,
    MSS = MSS,
    MAC = MAC,
    bodymass = bodymass,
    ...
  ) / bodymass
}

#' @rdname predict_kinematics
#' @export
predict_relative_power_at_time <- function(time, MSS, MAC, bodymass = 75, ...) {
  predict_power_at_time(
    time = time,
    MSS = MSS,
    MAC = MAC,
    bodymass = bodymass,
    ...
  ) / bodymass
}

#' Predicts sprint kinematics for 0-6sec (100hz) which include distance,
#'     velocity, acceleration, and relative power.
#'
#' @rdname predict_kinematics
#' @param object \code{shorts_model} object
#' @param max_time Predict from 0 to \code{max_time}. Default is 6seconds
#' @param frequency Number of samples within one second. Default is 100Hz
#' @return Data frame with kinetic and kinematic variables
#' @export
#' @examples
#'
#' # Example for predict_kinematics
#' split_times <- data.frame(
#'   distance = c(5, 10, 20, 30, 35),
#'   time = c(1.20, 1.96, 3.36, 4.71, 5.35)
#' )
#'
#' # Simple model
#' simple_model <- with(
#'   split_times,
#'   model_timing_gates(distance, time)
#' )
#'
#' predict_kinematics(simple_model)
predict_kinematics <- function(object, max_time = 6, frequency = 100, bodymass = 75, ...) {
  df <- NULL

  if (inherits(object, "shorts_model")) {
    df <- data.frame(
      time = seq(0, max_time, length.out = max_time * frequency + 1)
    )

    df$distance <- predict_distance_at_time(
      time = df$time,
      MSS = object$parameters$MSS,
      MAC = object$parameters$MAC
    )

    df$velocity <- predict_velocity_at_time(
      time = df$time,
      MSS = object$parameters$MSS,
      MAC = object$parameters$MAC
    )

    df$acceleration <- predict_acceleration_at_time(
      time = df$time,
      MSS = object$parameters$MSS,
      MAC = object$parameters$MAC
    )

    df$bodymass <- bodymass

    df$net_horizontal_force <- df$bodymass * df$acceleration

    df$air_resistance <- predict_air_resistance_at_time(
      time = df$time,
      MSS = object$parameters$MSS,
      MAC = object$parameters$MAC,
      bodymass = bodymass,
      ...
    )

    df$horizontal_force <- predict_force_at_time(
      time = df$time,
      MSS = object$parameters$MSS,
      MAC = object$parameters$MAC,
      bodymass = bodymass,
      ...
    )

    df$horizontal_force_relative <- df$horizontal_force / bodymass
    df$vertical_force <- (bodymass * 9.81)

    df$resultant_force <- sqrt(df$horizontal_force^2 + df$vertical_force^2)
    df$resultant_force_relative <- df$resultant_force / bodymass
    df$power <- df$horizontal_force * df$velocity
    df$relative_power <- df$horizontal_force_relative * df$velocity
    df$RF <- df$horizontal_force / df$resultant_force
    df$force_angle <- atan(df$vertical_force / df$horizontal_force) * 180 / pi
  } else {
    stop("Please use `shorts_model` object. Unable to proceed.", call. = FALSE)
  }

  df
}

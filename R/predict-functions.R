#' Kinematics prediction functions
#'
#' Predicts kinematic from known \code{MSS} and \code{MAC} parameters
#'
#'
#' @param time,distance,velocity Numeric vectors
#' @param MSS,MAC Numeric vectors. Model parameters
#' @param F0,V0 Numeric vectors. FV profile parameters
#' @param bodymass Body mass in kg. Used to calculate relative power and forwarded to \code{\link{get_air_resistance}}
#' @param inertia External inertia in kg (for example a weight vest, or a sled).
#'         Not included in the air resistance calculation
#' @param resistance External horizontal resistance in Newtons (for example tether device or a sled friction resistance)
#' @inheritDotParams get_air_resistance
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
  TAU * LambertW::W(-exp(1)^(-(distance) / (MSS * TAU) - 1)) + (distance) / MSS + TAU
}

#' @rdname predict_kinematics
#' @export
predict_time_at_distance_FV <- function(distance,
                                        F0,
                                        V0,
                                        bodymass = 75,
                                        inertia = 0,
                                        resistance = 0,
                                        ...) {
  # Convert FVP back to AVP
  AVP <- convert_FVP(
    F0 = F0,
    V0 = V0,
    bodymass = bodymass,
    inertia = inertia,
    resistance = resistance,
    ...
  )

  # Get time at distance
  predict_time_at_distance(distance, MSS = AVP$MSS, MAC = AVP$MAC)
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
predict_force_at_velocity <- function(velocity, MSS, MAC, bodymass = 75, inertia = 0, resistance = 0, ...) {
  slope <- MAC / MSS

  F_air <- get_air_resistance(velocity = velocity, bodymass = bodymass, ...)
  F_acc <- (MAC - velocity * slope) * (bodymass + inertia)

  # Return total
  F_acc + F_air + resistance
}

#' @rdname predict_kinematics
#' @export
predict_force_at_time <- function(time, MSS, MAC, bodymass = 75, inertia = 0, resistance = 0, ...) {
  acc <- predict_acceleration_at_time(time, MSS, MAC)
  vel <- predict_velocity_at_time(time, MSS, MAC)
  air_res <- get_air_resistance(velocity = vel, bodymass = bodymass, ...)

  (bodymass + inertia) * acc + air_res + resistance
}

#' @rdname predict_kinematics
#' @export
predict_force_at_distance <- function(distance, MSS, MAC, bodymass = 75, inertia = 0, resistance = 0, ...) {
  acc <- predict_acceleration_at_distance(distance, MSS, MAC)
  vel <- predict_velocity_at_distance(distance, MSS, MAC)
  air_res <- get_air_resistance(velocity = vel, bodymass = bodymass, ...)

  (bodymass + inertia) * acc + air_res + resistance
}

#' @rdname predict_kinematics
#' @export
predict_power_at_distance <- function(distance, MSS, MAC, bodymass = 75, inertia = 0, resistance = 0, ...) {
  acc <- predict_acceleration_at_distance(distance, MSS, MAC)
  vel <- predict_velocity_at_distance(distance, MSS, MAC)
  air_res <- get_air_resistance(velocity = vel, bodymass = bodymass, ...)

  ((bodymass + inertia) * acc + air_res + resistance) * vel
}


#' @rdname predict_kinematics
#' @export
predict_power_at_time <- function(time, MSS, MAC, bodymass = 75, inertia = 0, resistance = 0, ...) {
  acc <- predict_acceleration_at_time(time, MSS, MAC)
  vel <- predict_velocity_at_time(time, MSS, MAC)
  air_res <- get_air_resistance(velocity = vel, bodymass = bodymass, ...)

  ((bodymass + inertia) * acc + air_res + resistance) * vel
}

#' @rdname predict_kinematics
#' @export
predict_relative_power_at_distance <- function(distance, MSS, MAC, bodymass = 75, inertia = 0, resistance = 0, ...) {
  predict_power_at_distance(
    distance = distance,
    MSS = MSS,
    MAC = MAC,
    bodymass = bodymass,
    inertia = inertia,
    resistance = resistance,
    ...
  ) / bodymass
}

#' @rdname predict_kinematics
#' @export
predict_relative_power_at_time <- function(time, MSS, MAC, bodymass = 75, inertia = 0, resistance = 0, ...) {
  predict_power_at_time(
    time = time,
    MSS = MSS,
    MAC = MAC,
    bodymass = bodymass,
    inertia = inertia,
    resistance = resistance,
    ...
  ) / bodymass
}

#' @rdname predict_kinematics
#' @param ... Forwarded to \code{\link{predict_power_at_time}}
#' @export
predict_work_till_time <- function(time, ...) {
  df <- data.frame(
    time = time,
    ...
  )

  df_list <- split(df, seq(1, nrow(df)))

  res <- purrr::map_dbl(df_list, function(.x) {
    integrand <- function(x) {
      do.call(predict_power_at_time, as.list(tidyr::tibble(time = x, .x[-1])))
    }

    stats::integrate(integrand, lower = 0, upper = .x$time)$value
  })

  unname(res)
}

#' @rdname predict_kinematics
#' @param ... Forwarded to \code{\link{predict_power_at_distance}}
#' @export
predict_work_till_distance <- function(distance, ...) {
  df <- data.frame(
    distance = distance,
    ...
  )

  df_list <- split(df, seq(1, nrow(df)))

  res <- purrr::map_dbl(df_list, function(.x) {
    integrand <- function(x) {
      do.call(predict_power_at_distance, as.list(tidyr::tibble(distance = x, .x[-1])))
    }

    stats::integrate(integrand, lower = 0, upper = .x$time)$value
  })

  unname(res)
}


#' Predicts sprint kinematics for 0-6sec (100hz) which include distance,
#'     velocity, acceleration, and relative power.
#'
#' @rdname predict_kinematics
#' @param object If \code{shorts_model} object is provided, estimated parameters
#'     will be used. Otherwise provide \code{MSS} and \code{MAC} parameters
#' @param max_time Predict from 0 to \code{max_time}. Default is 6seconds
#' @param frequency Number of samples within one second. Default is 100Hz
#' @param add_inertia_to_vertical Should inertia be added to \code{bodymass} when
#'     calculating vertical force? Use \code{TRUE} (Default) when using weight vest, and
#'     \code{FALSE} when dragging sled
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
#'
predict_kinematics <- function(object = NULL,
                               MSS,
                               MAC,
                               max_time = 6,
                               frequency = 100,
                               bodymass = 75,
                               inertia = 0,
                               resistance = 0,
                               add_inertia_to_vertical = TRUE,
                               ...) {
  if (!is.null(object)) {
    if (inherits(object, "shorts_model")) {
      MSS <- object$parameters$MSS
      MAC <- object$parameters$MAC
    } else {
      stop("Please use `shorts_model` object. Unable to proceed.", call. = FALSE)
    }
  }

  df <- data.frame(
    time = seq(0, max_time, length.out = max_time * frequency + 1)
  )

  df$distance <- predict_distance_at_time(
    time = df$time,
    MSS = MSS,
    MAC = MAC
  )

  df$velocity <- predict_velocity_at_time(
    time = df$time,
    MSS = MSS,
    MAC = MAC
  )

  df$acceleration <- predict_acceleration_at_time(
    time = df$time,
    MSS = MSS,
    MAC = MAC
  )

  df$bodymass <- bodymass
  df$inertia <- inertia
  df$resistance <- resistance

  df$air_resistance <- predict_air_resistance_at_time(
    time = df$time,
    MSS = MSS,
    MAC = MAC,
    bodymass = bodymass,
    ...
  )

  df$horizontal_force <- predict_force_at_time(
    time = df$time,
    MSS = MSS,
    MAC = MAC,
    bodymass = bodymass,
    inertia = inertia,
    resistance = resistance,
    ...
  )

  df$horizontal_force_relative <- df$horizontal_force / bodymass

  if (add_inertia_to_vertical == TRUE) {
    df$vertical_force <- ((bodymass + inertia) * 9.81)
  } else {
    df$vertical_force <- (bodymass * 9.81)
  }

  df$resultant_force <- sqrt(df$horizontal_force^2 + df$vertical_force^2)
  df$resultant_force_relative <- df$resultant_force / bodymass

  df$power <- df$horizontal_force * df$velocity
  df$power_relative <- df$horizontal_force_relative * df$velocity
  df$work <- predict_work_till_time(
    time = df$time,
    MSS = MSS,
    MAC = MAC,
    bodymass = bodymass,
    inertia = inertia,
    resistance = resistance,
    ...
  )
  df$average_power <- df$work / df$time
  df$average_power_relative <- df$average_power / bodymass
  df$RF <- df$horizontal_force / df$resultant_force
  df$force_angle <- atan(df$vertical_force / df$horizontal_force) * 180 / pi

  df
}

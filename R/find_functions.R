#' Find functions
#'
#' Family of functions that serve a purpose of finding maximal value and critical distances and times
#'     at which power, acceleration or velocity drops below certain threshold.
#'
#' @param time_correction Numeric vector. Used for correction. Default is 0. See references for more info
#' @param distance_correction Numeric vector. Used for correction. Default is 0. See vignettes for more info
#' @param MSS,TAU Numeric vectors. Model parameters
#' @param percent Numeric vector. Used to calculate critical distance. Default is 0.9
#' @param ... Forwarded to \code{\link{predict_power_at_distance}} for the purpose of calculation of air resistance
#' @references
#' Haugen TA, Tønnessen E, Seiler SK. 2012. The Difference Is in the Start: Impact of Timing and Start
#'         Procedure on Sprint Running Performance: Journal of Strength and Conditioning Research 26:473–479.
#'         DOI: 10.1519/JSC.0b013e318226030b.
#'
#'     Samozino P. 2018. A Simple Method for Measuring Force, Velocity and Power Capabilities and Mechanical
#'         Effectiveness During Sprint Running. In: Morin J-B, Samozino P eds. Biomechanics of Training and Testing.
#'        Cham: Springer International Publishing, 237–267. DOI: 10.1007/978-3-319-05633-3_11.
#' @examples
#' dist <- seq(0, 40, length.out = 1000)
#'
#' velocity <- predict_velocity_at_distance(
#'   distance = dist,
#'   MSS = 10,
#'   TAU = 0.9
#' )
#'
#' acceleration <- predict_acceleration_at_distance(
#'   distance = dist,
#'   MSS = 10,
#'   TAU = 0.9
#' )
#'
#' # Use ... to forward parameters to the shorts::get_air_resistance
#' pwr <- predict_relative_power_at_distance(
#'   distance = dist,
#'   MSS = 10,
#'   TAU = 0.9
#'   # bodyweight = 100,
#'   # bodyheight = 1.9,
#'   # barometric_pressure = 760,
#'   # air_temperature = 25,
#'   # wind_velocity = 0
#' )
#'
#' # Find critical distance when 90% of MSS is reached
#' plot(x = dist, y = velocity, type = "l")
#' abline(h = 10 * 0.9, col = "gray")
#' abline(v = find_velocity_critical_distance(MSS = 10, TAU = 0.9), col = "red")
#'
#' # Find critical distance when 20% of MAC is reached
#' plot(x = dist, y = acceleration, type = "l")
#' abline(h = (10 / 0.9) * 0.2, col = "gray")
#' abline(v = find_acceleration_critical_distance(MSS = 10, TAU = 0.9, percent = 0.2), col = "red")
#'
#' # Find max power and location of max power
#' plot(x = dist, y = pwr, type = "l")
#'
#' max_pwr <- find_max_power_distance(
#'   MSS = 10,
#'   TAU = 0.9
#'   # Use ... to forward parameters to the shorts::get_air_resistance
#' )
#' abline(h = max_pwr$max_power, col = "gray")
#' abline(v = max_pwr$distance, col = "red")
#'
#' # Find distance in which relative power stays over 75% of PMAX'
#' plot(x = dist, y = pwr, type = "l")
#' abline(h = max_pwr$max_power * 0.75, col = "gray")
#' pwr_zone <- find_power_critical_distance(MSS = 10, TAU = 0.9, percent = 0.75)
#' abline(v = pwr_zone$lower, col = "blue")
#' abline(v = pwr_zone$upper, col = "blue")
#' @name find_functions
NULL

#' @rdname find_functions
#' @description \code{find_max_power_distance} finds maximum power and \code{distance} at
#'    which max power occurs
#' @return \code{find_max_power_distance} returns list with two elements: \code{max_power}
#'    and \code{distance} at which max power occurs
#' @export
find_max_power_distance <- function(MSS, TAU, time_correction = 0, distance_correction = 0, ...) {
  max_power <- stats::optimize(
    function(x) {
      predict_power_at_distance(
        distance = x,
        MSS = MSS,
        TAU = TAU,
        time_correction = time_correction,
        distance_correction = distance_correction,
        ...
      )
    },
    interval = c(0, 100),
    maximum = TRUE
  )

  return(list(
    max_power = max_power$objective,
    distance = max_power$maximum
  ))
}

#' @rdname find_functions
#' @description \code{find_max_power_time} finds maximum power and \code{time} at which
#'     max power occurs
#' @return \code{find_max_power_time} returns list with two elements: \code{max_power} and
#'     \code{time} at which max power occurs
#' @export
find_max_power_time <- function(MSS, TAU, time_correction = 0, ...) {
  max_power <- stats::optimize(
    function(x) {
      predict_power_at_time(
        time = x,
        MSS = MSS,
        TAU = TAU,
        time_correction = time_correction,
        ...
      )
    },
    interval = c(0, 10),
    maximum = TRUE
  )

  return(list(
    max_power = max_power$objective,
    time = max_power$maximum
  ))
}


#' @rdname find_functions
#' @description \code{find_velocity_critical_distance} finds critical distance at which \code{percent}
#'     of \code{MSS} is achieved
#' @export
find_velocity_critical_distance <- function(MSS, TAU, time_correction = 0, distance_correction = 0, percent = 0.9) {
  critical_distance <- stats::optimize(
    function(x) {
      velocity <- predict_velocity_at_distance(
        distance = x,
        MSS = MSS,
        TAU = TAU,
        time_correction = time_correction,
        distance_correction = distance_correction
      )

      abs((velocity / MSS) - percent) * x
    },
    interval = c(0, 100)
  )

  return(critical_distance$minimum)
}

#' @rdname find_functions
#' @description \code{find_velocity_critical_time} finds critical time at which \code{percent} of \code{MSS}
#'     is achieved
#' @export
find_velocity_critical_time <- function(MSS, TAU, time_correction = 0, percent = 0.9) {
  critical_distance <- stats::optimize(
    function(x) {
      velocity <- predict_velocity_at_time(
        time = x,
        MSS = MSS,
        TAU = TAU,
        time_correction = time_correction
      )

      abs((velocity / MSS) - percent) * x
    },
    interval = c(0, 10)
  )

  return(critical_distance$minimum)
}


#' @rdname find_functions
#' @description \code{find_acceleration_critical_distance} finds critical distance at which \code{percent}
#'     of \code{MAC} is reached
#' @export
find_acceleration_critical_distance <- function(MSS, TAU, time_correction = 0, distance_correction = 0, percent = 0.9) {
  critical_distance <- stats::optimize(
    function(x) {
      acceleration <- predict_acceleration_at_distance(
        distance = x,
        MSS = MSS,
        TAU = TAU,
        time_correction = time_correction,
        distance_correction = distance_correction
      )

      abs((acceleration / (MSS / TAU)) - percent) * x
    },
    interval = c(0, 100)
  )

  return(critical_distance$minimum)
}

#' @rdname find_functions
#' @description \code{find_acceleration_critical_time} finds critical time at which \code{percent} of
#'     \code{MAC} is reached
#' @export
find_acceleration_critical_time <- function(MSS, TAU, time_correction = 0, percent = 0.9) {
  critical_distance <- stats::optimize(
    function(x) {
      acceleration <- predict_acceleration_at_time(
        time = x,
        MSS = MSS,
        TAU = TAU,
        time_correction = time_correction
      )

      abs((acceleration / (MSS / TAU)) - percent) * x
    },
    interval = c(0, 10)
  )

  return(critical_distance$minimum)
}


#' @rdname find_functions
#' @description \code{find_power_critical_distance} finds critical distances at which maximal power over
#'     \code{percent} is achieved
#' @export
find_power_critical_distance <- function(MSS, TAU, time_correction = 0, distance_correction = 0, percent = 0.9, ...) {
  max_power <- find_max_power_distance(MSS, TAU, time_correction, distance_correction, ...)

  critical_distance_lower <- stats::optimize(
    function(x) {
      pwr <- predict_power_at_distance(
        distance = x,
        MSS = MSS,
        TAU = TAU,
        time_correction = time_correction,
        distance_correction = distance_correction,
        ...
      )

      abs((pwr / max_power$max_power) - percent)
    },
    interval = c(0, max_power$distance)
  )

  critical_distance_upper <- stats::optimize(
    function(x) {
      pwr <- predict_power_at_distance(
        distance = x,
        MSS = MSS,
        TAU = TAU,
        time_correction = time_correction,
        distance_correction = distance_correction,
        ...
      )

      abs((pwr / max_power$max_power) - percent)
    },
    interval = c(max_power$distance, 100)
  )

  return(list(
    lower = critical_distance_lower$minimum,
    upper = critical_distance_upper$minimum
  ))
}


#' @rdname find_functions
#' @description \code{find_power_critical_time} finds critical times at which maximal power over
#'     \code{percent} is achieved
#' @export
find_power_critical_time <- function(MSS, TAU, time_correction = 0, percent = 0.9, ...) {
  max_power <- find_max_power_time(MSS, TAU, time_correction, ...)

  critical_time_lower <- stats::optimize(
    function(x) {
      pwr <- predict_power_at_time(
        time = x,
        MSS = MSS,
        TAU = TAU,
        time_correction = time_correction,
        ...
      )

      abs((pwr / max_power$max_power) - percent)
    },
    interval = c(0, max_power$time)
  )

  critical_time_upper <- stats::optimize(
    function(x) {
      pwr <- predict_power_at_time(
        time = x,
        MSS = MSS,
        TAU = TAU,
        time_correction = time_correction,
        ...
      )

      abs((pwr / max_power$max_power) - percent)
    },
    interval = c(max_power$time, 10)
  )

  return(list(
    lower = critical_time_lower$minimum,
    upper = critical_time_upper$minimum
  ))
}

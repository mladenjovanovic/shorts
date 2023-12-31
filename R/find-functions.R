#' Find functions
#'
#' Family of functions that serve a purpose of finding maximal value and critical distances and times
#'     at which power, acceleration or velocity drops below certain threshold.
#'
#' @param MSS,MAC Numeric vectors. Model parameters
#' @param percent Numeric vector. Used to calculate critical distance. Default is 0.9
#' @param inertia External inertia in kg (for example a weight vest, or a sled).
#'         Not included in the air resistance calculation
#' @param resistance External horizontal resistance in Newtons (for example tether device or a sled friction resistance)
#' @inheritDotParams get_air_resistance
#' @references
#' Haugen TA, Tønnessen E, Seiler SK. 2012. The Difference Is in the Start: Impact of Timing and Start
#'         Procedure on Sprint Running Performance: Journal of Strength and Conditioning Research 26:473–479.
#'         DOI: 10.1519/JSC.0b013e318226030b.
#'
#' Samozino P. 2018. A Simple Method for Measuring Force, Velocity and Power Capabilities and Mechanical
#'         Effectiveness During Sprint Running. In: Morin J-B, Samozino P eds. Biomechanics of Training and Testing.
#'        Cham: Springer International Publishing, 237–267. DOI: 10.1007/978-3-319-05633-3_11.
#' @examples
#' dist <- seq(0, 40, length.out = 1000)
#'
#' velocity <- predict_velocity_at_distance(
#'   distance = dist,
#'   MSS = 10,
#'   MAC = 9
#' )
#'
#' acceleration <- predict_acceleration_at_distance(
#'   distance = dist,
#'   MSS = 10,
#'   MAC = 9
#' )
#'
#' # Use ... to forward parameters to the shorts::get_air_resistance
#' pwr <- predict_power_at_distance(
#'   distance = dist,
#'   MSS = 10,
#'   MAC = 9
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
#' abline(v = find_velocity_critical_distance(MSS = 10, MAC = 9), col = "red")
#'
#' # Find critical distance when 20% of MAC is reached
#' plot(x = dist, y = acceleration, type = "l")
#' abline(h = 9 * 0.2, col = "gray")
#' abline(v = find_acceleration_critical_distance(MSS = 10, MAC = 9, percent = 0.2), col = "red")
#'
#' # Find peak power and location of peak power
#' plot(x = dist, y = pwr, type = "l")
#'
#' peak_pwr <- find_peak_power_distance(
#'   MSS = 10,
#'   MAC = 9
#'   # Use ... to forward parameters to the shorts::get_air_resistance
#' )
#' abline(h = peak_pwr$peak_power, col = "gray")
#' abline(v = peak_pwr$distance, col = "red")
#'
#' # Find distance in which relative power stays over 75% of PMAX'
#' plot(x = dist, y = pwr, type = "l")
#' abline(h = peak_pwr$peak_power * 0.75, col = "gray")
#' pwr_zone <- find_power_critical_distance(MSS = 10, MAC = 9, percent = 0.75)
#' abline(v = pwr_zone$lower, col = "blue")
#' abline(v = pwr_zone$upper, col = "blue")
#'
#' @name find_functions
NULL

#' @rdname find_functions
#' @description \code{find_peak_power_distance} finds peak power and \code{distance} at
#'    which peak power occurs
#' @return \code{find_peak_power_distance} returns list with two elements: \code{peak_power}
#'    and \code{distance} at which peak power occurs
#' @export
find_peak_power_distance <- function(MSS, MAC, inertia = 0, resistance = 0, ...) {
  peak_power <- stats::optimize(
    function(x) {
      predict_power_at_distance(
        distance = x,
        MSS = MSS,
        MAC = MAC,
        inertia = inertia,
        resistance = resistance,
        ...
      )
    },
    interval = c(0, 30),
    maximum = TRUE
  )

  return(list(
    peak_power = peak_power$objective,
    distance = peak_power$maximum
  ))
}

#' @rdname find_functions
#' @description \code{find_peak_power_time} finds peak power and \code{time} at which
#'     peak power occurs
#' @return \code{find_peak_power_time} returns list with two elements: \code{peak_power} and
#'     \code{time} at which peak power occurs
#' @export
find_peak_power_time <- function(MSS, MAC, inertia = 0, resistance = 0, ...) {
  peak_power <- stats::optimize(
    function(x) {
      predict_power_at_time(
        time = x,
        MSS = MSS,
        MAC = MAC,
        inertia = inertia,
        resistance = resistance,
        ...
      )
    },
    interval = c(0, 10),
    maximum = TRUE
  )

  return(list(
    peak_power = peak_power$objective,
    time = peak_power$maximum
  ))
}


#' @rdname find_functions
#' @description \code{find_velocity_critical_distance} finds critical distance at which \code{percent}
#'     of \code{MSS} is achieved
#' @export
find_velocity_critical_distance <- function(MSS, MAC, percent = 0.9) {
  critical_distance <- stats::optimize(
    function(x) {
      velocity <- predict_velocity_at_distance(
        distance = x,
        MSS = MSS,
        MAC = MAC
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
find_velocity_critical_time <- function(MSS, MAC, percent = 0.9) {
  critical_distance <- stats::optimize(
    function(x) {
      velocity <- predict_velocity_at_time(
        time = x,
        MSS = MSS,
        MAC = MAC
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
find_acceleration_critical_distance <- function(MSS, MAC, percent = 0.9) {
  critical_distance <- stats::optimize(
    function(x) {
      acceleration <- predict_acceleration_at_distance(
        distance = x,
        MSS = MSS,
        MAC = MAC
      )

      abs((acceleration / MAC) - percent) * x
    },
    interval = c(0, 100)
  )

  return(critical_distance$minimum)
}

#' @rdname find_functions
#' @description \code{find_acceleration_critical_time} finds critical time at which \code{percent} of
#'     \code{MAC} is reached
#' @export
find_acceleration_critical_time <- function(MSS, MAC, percent = 0.9) {
  critical_distance <- stats::optimize(
    function(x) {
      acceleration <- predict_acceleration_at_time(
        time = x,
        MSS = MSS,
        MAC = MAC
      )

      abs((acceleration / MAC) - percent) * x
    },
    interval = c(0, 10)
  )

  return(critical_distance$minimum)
}


#' @rdname find_functions
#' @description \code{find_power_critical_distance} finds critical distances at which peak power over
#'     \code{percent} is achieved
#' @export
find_power_critical_distance <- function(MSS, MAC, inertia = 0, resistance = 0, percent = 0.9, ...) {
  peak_power <- find_peak_power_distance(MSS, MAC, ...)

  critical_distance_lower <- stats::optimize(
    function(x) {
      pwr <- predict_power_at_distance(
        distance = x,
        MSS = MSS,
        MAC = MAC,
        inertia = inertia,
        resistance = resistance,
        ...
      )

      abs((pwr / peak_power$peak_power) - percent)
    },
    interval = c(0, peak_power$distance)
  )

  critical_distance_upper <- stats::optimize(
    function(x) {
      pwr <- predict_power_at_distance(
        distance = x,
        MSS = MSS,
        MAC = MAC,
        inertia = inertia,
        resistance = resistance,
        ...
      )

      abs((pwr / peak_power$peak_power) - percent)
    },
    interval = c(peak_power$distance, 100)
  )

  return(list(
    lower = critical_distance_lower$minimum,
    upper = critical_distance_upper$minimum
  ))
}


#' @rdname find_functions
#' @description \code{find_power_critical_time} finds critical times at which peak power over
#'     \code{percent} is achieved
#' @export
find_power_critical_time <- function(MSS, MAC, inertia = 0, resistance = 0, percent = 0.9, ...) {
  peak_power <- find_peak_power_time(MSS, MAC, ...)

  critical_time_lower <- stats::optimize(
    function(x) {
      pwr <- predict_power_at_time(
        time = x,
        MSS = MSS,
        MAC = MAC,
        inertia = inertia,
        resistance = resistance,
        ...
      )

      abs((pwr / peak_power$peak_power) - percent)
    },
    interval = c(0, peak_power$time)
  )

  critical_time_upper <- stats::optimize(
    function(x) {
      pwr <- predict_power_at_time(
        time = x,
        MSS = MSS,
        MAC = MAC,
        ...
      )

      abs((pwr / peak_power$peak_power) - percent)
    },
    interval = c(peak_power$time, 10)
  )

  return(list(
    lower = critical_time_lower$minimum,
    upper = critical_time_upper$minimum
  ))
}

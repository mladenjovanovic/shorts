#' Find functions
#'
#' Family of functions that serve a purpose of finding maximal value and critical distances and times
#'     at which power, acceleration or velocity drops below certain threshold.
#'
#' @param MSS,MAC Numeric vectors. Model parameters
#' @param F0,V0 Numeric vectors. FV profile parameters
#' @param bodymass Body mass in kg
#' @param distance Numeric vector
#' @param percent Numeric vector. Used to calculate critical distance. Default is 0.9
#' @param ... Forwarded to \code{\link{predict_power_at_distance}} for the purpose of calculation of air resistance
#' @references
#' Haugen TA, Tønnessen E, Seiler SK. 2012. The Difference Is in the Start: Impact of Timing and Start
#'         Procedure on Sprint Running Performance: Journal of Strength and Conditioning Research 26:473–479.
#'         DOI: 10.1519/JSC.0b013e318226030b.
#'
#' Samozino P. 2018. A Simple Method for Measuring Force, Velocity and Power Capabilities and Mechanical
#'         Effectiveness During Sprint Running. In: Morin J-B, Samozino P eds. Biomechanics of Training and Testing.
#'        Cham: Springer International Publishing, 237–267. DOI: 10.1007/978-3-319-05633-3_11.
#'
#' Samozino P, Peyrot N, Edouard P, Nagahara R, Jimenez‐Reyes P, Vanwanseele B, Morin J. 2022.
#'      Optimal mechanical force‐velocity profile for sprint acceleration performance.
#'       Scandinavian Journal of Medicine & Science in Sports 32:559–575. DOI: 10.1111/sms.14097.
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
#' pwr <- predict_relative_power_at_distance(
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
#' abline(h = (10 / 0.9) * 0.2, col = "gray")
#' abline(v = find_acceleration_critical_distance(MSS = 10, MAC = 9, percent = 0.2), col = "red")
#'
#' # Find max power and location of max power
#' plot(x = dist, y = pwr, type = "l")
#'
#' max_pwr <- find_max_power_distance(
#'   MSS = 10,
#'   MAC = 9
#'   # Use ... to forward parameters to the shorts::get_air_resistance
#' )
#' abline(h = max_pwr$max_power, col = "gray")
#' abline(v = max_pwr$distance, col = "red")
#'
#' # Find distance in which relative power stays over 75% of PMAX'
#' plot(x = dist, y = pwr, type = "l")
#' abline(h = max_pwr$max_power * 0.75, col = "gray")
#' pwr_zone <- find_power_critical_distance(MSS = 10, MAC = 9, percent = 0.75)
#' abline(v = pwr_zone$lower, col = "blue")
#' abline(v = pwr_zone$upper, col = "blue")
#'
#' # Optimal profiles
#' MSS <- 10
#' MAC <- 8
#' bodymass <- 75
#'
#' fv <- make_FV_profile(MSS, MAC, bodymass)
#'
#' dist <- 1:50
#'
#' opt_slope_perc <- find_optimal_MSS_MAC(
#'   distance = dist,
#'   MSS,
#'   MAC)[["slope_perc"]]
#'
#' opt_dist <- find_optimal_MSS_MAC_distance(MSS, MAC)
#'
#' opt_FV_slope_perc <- find_optimal_FV(
#'   distance = dist,
#'   fv$F0_poly,
#'   fv$V0_poly,
#'   fv$bodymass)[["FV_slope_perc"]]
#'
#' opt_FV_dist <- find_optimal_FV_distance(fv$F0_poly, fv$V0_poly, fv$bodymass)
#'
#' plot(x = dist, y = opt_slope_perc, type = "l")
#' lines(x = dist, y = opt_FV_slope_perc, type = "l", col = "blue")
#' points(x = opt_dist, y = 100)
#' points(x = opt_FV_dist, y = 100, col = "blue")
#' abline(h = 100, col = "gray", lty = 2)
#' @name find_functions
NULL

#' @rdname find_functions
#' @description \code{find_max_power_distance} finds maximum power and \code{distance} at
#'    which max power occurs
#' @return \code{find_max_power_distance} returns list with two elements: \code{max_power}
#'    and \code{distance} at which max power occurs
#' @export
find_max_power_distance <- function(MSS, MAC, ...) {
  max_power <- stats::optimize(
    function(x) {
      predict_power_at_distance(
        distance = x,
        MSS = MSS,
        MAC = MAC,
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
find_max_power_time <- function(MSS, MAC, ...) {
  max_power <- stats::optimize(
    function(x) {
      predict_power_at_time(
        time = x,
        MSS = MSS,
        MAC = MAC,
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
#' @description \code{find_power_critical_distance} finds critical distances at which maximal power over
#'     \code{percent} is achieved
#' @export
find_power_critical_distance <- function(MSS, MAC, percent = 0.9, ...) {
  max_power <- find_max_power_distance(MSS, MAC, ...)

  critical_distance_lower <- stats::optimize(
    function(x) {
      pwr <- predict_power_at_distance(
        distance = x,
        MSS = MSS,
        MAC = MAC,
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
        MAC = MAC,
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
find_power_critical_time <- function(MSS, MAC, percent = 0.9, ...) {
  max_power <- find_max_power_time(MSS, MAC, ...)

  critical_time_lower <- stats::optimize(
    function(x) {
      pwr <- predict_power_at_time(
        time = x,
        MSS = MSS,
        MAC = MAC,
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
        MAC = MAC,
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


# Scalar function
find_optimal_FV_scalar <- function(distance, F0, V0, bodymass = 75, ...) {
  opt_func <- function(par) {
    new_F0 <- F0 / par[1]
    new_V0 <- V0 * par[1]

    predict_time_at_distance_FV(
      distance = distance,
      F0 = new_F0,
      V0 = new_V0,
      bodymass = bodymass,
      ...
    )
  }

  get_optim_model <- function() {
    tryCatch(
      {
        stats::optim(
          par = 1,
          fn = opt_func,
          method = "Brent",
          lower = 0,
          upper = 10)
      },
      error = function(cond) {
        return(list(par = NA, value = NA))
      },
      warning = function(cond) {
        return(list(par = NA, value = NA))
      }
    )
  }

  results <- get_optim_model()

  F0_optim <- F0 / results$par
  V0_optim <- V0 * results$par

  FV_slope <- -(F0 / bodymass) / V0
  FV_slope_optim <- -(F0_optim / bodymass) / V0_optim

  FV_slope_perc <- (FV_slope / FV_slope_optim) * 100

  # Probe
  t_nor <- predict_time_at_distance_FV(
    distance = distance,
    F0 = F0,
    V0 = V0,
    bodymass = bodymass,
    ...
  )


  t_F0 <- predict_time_at_distance_FV(
    distance = distance,
    F0 = F0 * 1.05,
    V0 = V0,
    bodymass = bodymass,
    ...
  )

  t_V0 <- predict_time_at_distance_FV(
    distance = distance,
    F0 = F0,
    V0 = V0 * 1.05,
    bodymass = bodymass,
    ...
  )

  # Return the results
  list(
    F0_optim = F0_optim,
    V0_optim = V0_optim,
    FV_slope_perc = FV_slope_perc,
    probe_IMB = (t_nor - t_V0) / (t_nor - t_F0) * 100
  )
}

find_optimal_MSS_MAC_scalar <- function(distance, MSS, MAC) {
  opt_func <- function(par) {
    new_MSS <- MSS / par[1]
    new_MAC <- MAC * par[1]

    predict_time_at_distance(
      distance = distance,
      MSS = new_MSS,
      MAC = new_MAC
    )
  }

  get_optim_model <- function() {
    tryCatch(
      {
        stats::optim(
          par = 1,
          fn = opt_func,
          method = "Brent",
          lower = 0,
          upper = 10)
      },
      error = function(cond) {
        return(list(par = NA, value = NA))
      },
      warning = function(cond) {
        return(list(par = NA, value = NA))
      }
    )
  }

  results <- get_optim_model()

  MSS_optim <- MSS / results$par
  MAC_optim <- MAC * results$par

  slope <- -MAC / MSS
  slope_optim <- -MAC_optim / MSS_optim

  slope_perc <- (slope / slope_optim) * 100

  # Probe
  t_nor <- predict_time_at_distance(
    distance = distance,
    MSS = MSS,
    MAC = MAC
  )


  t_MSS <- predict_time_at_distance(
    distance = distance,
    MSS = MSS * 1.05,
    MAC = MAC
  )

  t_MAC <- predict_time_at_distance(
    distance = distance,
    MSS = MSS,
    MAC = MAC * 1.05
  )

  # Return the results
  list(
    MSS_optim = MSS_optim,
    MAC_optim = MAC_optim,
    slope_perc = slope_perc,
    probe_IMB = (t_nor - t_MSS) / (t_nor - t_MAC) * 100
  )
}

#' @rdname find_functions
#' @description \code{find_optimal_FV} finds "optimal" \code{F0} and \code{V0} where time at distance is
#'     minimized, while keeping the \code{Pmax} the same
#' @return A data frame with the following elements
#'     \describe{
#'         \item{F0_optim}{Optimal F0}
#'         \item{V0_optim}{Optimal V0}
#'         \item{FV_slope_perc}{Percent ratio between slope and optimal slope}
#'         \item{probe_IMB}{Percent ratio between improvement in time when V0 vs F0 increase for 5 percent}
#'     }
#' @export
find_optimal_FV <- function(distance, F0, V0, bodymass = 75, ...) {

  df <- data.frame(
    distance = distance,
    F0 = F0,
    V0 = V0,
    bodymass = bodymass
  )

  df$id <- sprintf(paste0("%0", floor(log10(nrow(df))) + 1, "d"), seq(1, nrow(df)))

  df_list <- split(df, df$id)

  purrr::map_df(df_list, function(.x) {
    data.frame(find_optimal_FV_scalar(
      distance = .x$distance,
      F0 = .x$F0,
      V0 = .x$V0,
      bodymass = .x$bodymass,
      ...))
  })
}


#' @rdname find_functions
#' @description \code{find_optimal_MSS_MAC} finds "optimal" \code{MSS} and \code{MAS} where time at distance is
#'     minimized, while keeping the \code{Pmax} the same
#' @return A data frame with the following elements
#'     \describe{
#'         \item{MSS_optim}{Optimal MSS}
#'         \item{MAC_optim}{Optimal MAC}
#'         \item{slope_perc}{Percent ratio between slope and optimal slope}
#'         \item{probe_IMB}{Percent ratio between improvement in time when MSS vs MAC increase for 5 percent}
#'     }
#' @export
find_optimal_MSS_MAC <- function(distance, MSS, MAC) {

  df <- data.frame(
    distance = distance,
    MSS = MSS,
    MAC = MAC
  )

  df$id <- sprintf(paste0("%0", floor(log10(nrow(df))) + 1, "d"), seq(1, nrow(df)))

  df_list <- split(df, df$id)

  purrr::map_df(df_list, function(.x) {
    data.frame(find_optimal_MSS_MAC_scalar(
      distance = .x$distance,
      MSS = .x$MSS,
      MAC = .x$MAC))
  })
}

#' @rdname find_functions
#' @description \code{find_optimal_FV_distance} finds the distance for which the
#'     FV profile is optimal
#' @param min,max Range over which to find the distance
#' @param metric Metric from \code{\link{find_optimal_FV}}. Default is "FV_slope_perc"
#' @return Distance
#' @export
find_optimal_FV_distance <- function(F0, V0, bodymass = 75, min = 1, max = 60, metric = "FV_slope_perc", ...) {

  opt_func <- function(par) {
    abs(100 - find_optimal_FV(distance = par, F0 = F0, V0 = V0, bodymass = bodymass, ...)[[metric]])
  }

  get_optim_model <- function() {
    tryCatch(
      {
        stats::optim(
          par = min,
          fn = opt_func,
          method = "Brent",
          lower = min,
          upper = max)
      },
      error = function(cond) {
        return(list(par = NA, value = NA))
      },
      warning = function(cond) {
        return(list(par = NA, value = NA))
      }
    )
  }

  results <- get_optim_model()

  results$par
}

#' @rdname find_functions
#' @description \code{find_optimal_MSS_MAC_distance} finds the distance for which the
#'     sprint profile is optimal
#' @param min,max Range over which to find the distance
#' @param metric Metric from \code{\link{find_optimal_MSS_MAC}}. Default is "slope_perc"
#' @return Distance
#' @export
find_optimal_MSS_MAC_distance <- function(MSS, MAC, min = 1, max = 60, metric = "slope_perc") {

  opt_func <- function(par) {
    abs(100 - find_optimal_MSS_MAC(distance = par, MSS = MSS, MAC = MAC)[[metric]])
  }

  get_optim_model <- function() {
    tryCatch(
      {
        stats::optim(
          par = min,
          fn = opt_func,
          method = "Brent",
          lower = min,
          upper = max)
      },
      error = function(cond) {
        return(list(par = NA, value = NA))
      },
      warning = function(cond) {
        return(list(par = NA, value = NA))
      }
    )
  }

  results <- get_optim_model()

  results$par
}

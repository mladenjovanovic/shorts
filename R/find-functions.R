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
#' dist <- seq(5, 50, by = 5)
#'
#' opt_slope_perc <- find_optimal_MSS_MAC(
#'   distance = dist,
#'   MSS,
#'   MAC
#' )[["slope_perc"]]
#'
#' opt_dist <- find_optimal_MSS_MAC_distance(MSS, MAC)
#'
#' opt_FV_slope_perc <- find_optimal_FV(
#'   distance = dist,
#'   fv$F0_poly,
#'   fv$V0_poly,
#'   fv$bodymass
#' )[["slope_perc"]]
#'
#' opt_FV_dist <- find_optimal_FV_distance(fv$F0_poly, fv$V0_poly, fv$bodymass)
#'
#' opt_FV_peak_slope_perc <- find_optimal_FV_peak(
#'   distance = dist,
#'   fv$F0_poly,
#'   fv$V0_poly,
#'   fv$bodymass
#' )[["slope_perc"]]
#'
#' opt_FV_peak_dist <- find_optimal_FV_peak_distance(fv$F0_poly, fv$V0_poly, fv$bodymass)
#'
#' plot(x = dist, y = opt_slope_perc, type = "l")
#' lines(x = dist, y = opt_FV_slope_perc, type = "l", col = "blue")
#' lines(x = dist, y = opt_FV_peak_slope_perc, type = "l", col = "red")
#' points(x = opt_dist, y = 100)
#' points(x = opt_FV_dist, y = 100, col = "blue")
#' points(x = opt_FV_peak_dist, y = 100, col = "red")
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

# ---------------------------------
# Functions to find optimal profile

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
          upper = 10
        )
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
  t_orig <- predict_time_at_distance_FV(
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

  ##############################################
  # Peak Power
  # Original profile
  converted <- convert_FV(F0, V0, bodymass, ...)
  MSS_conv <- converted$MSS
  MAC_conv <- converted$MAC

  Ppeak_dist <- find_max_power_distance(
    MSS = MSS_conv,
    MAC = MAC_conv,
    bodymass = bodymass,
    ...
  )

  Ppeak_time <- find_max_power_time(
    MSS = MSS_conv,
    MAC = MAC_conv,
    bodymass = bodymass,
    ...
  )

  # Optimal profile
  converted_optim <- convert_FV(F0_optim, V0_optim, bodymass, ...)
  MSS_conv_optim <- converted_optim$MSS
  MAC_conv_optim <- converted_optim$MAC

  Ppeak_dist_optim <- find_max_power_distance(
    MSS = MSS_conv_optim,
    MAC = MAC_conv_optim,
    bodymass = bodymass,
    ...
  )

  Ppeak_time_optim <- find_max_power_time(
    MSS = MSS_conv_optim,
    MAC = MAC_conv_optim,
    bodymass = bodymass,
    ...
  )

  # Return the results
  list(
    F0 = F0,
    V0 = V0,
    bodymass = bodymass,
    Pmax = F0 * V0 / 4,
    Pmax_rel = (F0 * V0 / 4) / bodymass,
    slope = FV_slope,
    distance = distance,
    time = t_orig,
    Ppeak = Ppeak_dist$max_power,
    Ppeak_rel = Ppeak_dist$max_power / bodymass,
    Ppeak_dist = Ppeak_dist$distance,
    Ppeak_time = Ppeak_time$time,
    F0_optim = F0_optim,
    F0_coef = 1 / results$par,
    V0_optim = V0_optim,
    V0_coef = results$par,
    Pmax_optim = F0_optim * V0_optim / 4,
    Pmax_rel_optim = (F0_optim * V0_optim / 4) / bodymass,
    slope_optim = FV_slope_optim,
    slope_perc = FV_slope_perc,
    time_optim = results$value,
    time_gain = results$value - t_orig,
    Ppeak_optim = Ppeak_dist_optim$max_power,
    Ppeak_rel_optim = Ppeak_dist_optim$max_power / bodymass,
    Ppeak_dist_optim = Ppeak_dist_optim$distance,
    Ppeak_time_optim = Ppeak_time_optim$time,
    probe_F0 = F0 * 1.05,
    probe_F0_time = t_F0,
    probe_F0_time_gain = t_F0 - t_orig,
    probe_V0 = V0 * 1.05,
    probe_V0_time = t_V0,
    probe_V0_time_gain = t_V0 - t_orig,
    probe_time_gain_perc = (t_V0 - t_orig) / (t_F0 - t_orig) * 100
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
          upper = 10
        )
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
  t_orig <- predict_time_at_distance(
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
    MSS = MSS,
    MAC = MAC,
    Pmax_rel = MSS * MAC / 4,
    slope = slope,
    distance = distance,
    time = t_orig,
    MSS_optim = MSS_optim,
    MSS_coef = 1 / results$par,
    MAC_optim = MAC_optim,
    MAC_coef = results$par,
    Pmax_rel_optim = MSS * MAC / 4,
    slope_optim = slope_optim,
    slope_perc = slope_perc,
    time_optim = results$value,
    time_gain = results$value - t_orig,
    probe_MSS = MSS * 1.05,
    probe_MSS_time = t_MSS,
    probe_MSS_time_gain = t_MSS - t_orig,
    probe_MAC = MAC * 1.05,
    probe_MAC_time = t_MAC,
    probe_MAC_time_gain = t_MAC - t_orig,
    probe_time_gain_perc = (t_MSS - t_orig) / (t_MAC - t_orig) * 100
  )
}

#' @rdname find_functions
#' @description \code{find_optimal_FV} finds "optimal" \code{F0} and \code{V0} where time at distance is
#'     minimized, while keeping the \code{Pmax} the same
#' @return A data frame with the following columns
#'     \describe{
#'         \item{F0}{Original F0}
#'         \item{V0}{Original F0}
#'         \item{bodymass}{Bodymass}
#'         \item{Pmax}{Maximal power estimated using F0 * V0 / 4}
#'         \item{Pmax_rel}{Relative maximal power}
#'         \item{slope}{FV profile slope}
#'         \item{distance}{Distance}
#'         \item{time}{Time to cover distance}
#'         \item{Ppeak}{Peak power estimated quantitatively}
#'         \item{Ppeak_rel}{Relative peak power}
#'         \item{Ppeak_dist}{Distance at which peak power is manifested}
#'         \item{Ppeak_time}{Time at which peak power is manifested}
#'         \item{F0_optim}{Optimal F0}
#'         \item{F0_coef}{Ratio between F0_optim an F0}
#'         \item{V0_optim}{Optimal V0}
#'         \item{V0_coef}{Ratio between V0_optim an V0}
#'         \item{Pmax_optim}{Optimal maximal power estimated F0_optim * V0_optim / 4}
#'         \item{Pmax_rel_optim}{Optimal relative maximal power}
#'         \item{slope_optim}{Optimal FV profile slope}
#'         \item{slope_perc}{Percent ratio between slope and optimal slope}
#'         \item{time_optim}{Time to cover distance when profile is optimal}
#'         \item{time_gain}{Difference in time to cover distance between time_optimal and time}
#'         \item{Ppeak_optim}{Optimal peak power estimated quantitatively}
#'         \item{Ppeak_rel_optim}{Optimal relative peak power}
#'         \item{Ppeak_dist_optim}{Distance at which optimal peak power is manifested}
#'         \item{Ppeak_time_optim}{Time at which optimal peak power is manifested}
#'         \item{probe_F0}{Probing F0}
#'         \item{probe_F0_time}{Time to cover distance when using probe_F0}
#'         \item{probe_F0_time_gain}{Time difference}
#'         \item{probe_V0}{Probing V0}
#'         \item{probe_V0_time}{Time to cover distance when using probe_V0}
#'         \item{probe_V0_time_gain}{Time difference}
#'         \item{probe_time_gain_perc}{Percent ratio between probe_V0_time_gain and probe_F0_time_gain}
#'    }
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
      ...
    ))
  })
}


#' @rdname find_functions
#' @description \code{find_optimal_MSS_MAC} finds "optimal" \code{MSS} and \code{MAS} where time at distance is
#'     minimized, while keeping the \code{Pmax} the same
#' @return A data frame with the following columns
#'     \describe{
#'         \item{MSS}{Original MSS}
#'         \item{MAC}{Original MAC}
#'         \item{Pmax_rel}{Relative maximal power estimated using MSS * MAC / 4}
#'         \item{slope}{Sprint profile slope}
#'         \item{distance}{Distance}
#'         \item{time}{Time to cover distance}
#'         \item{MSS_optim}{Optimal MSS}
#'         \item{MSS_coef}{Ratio between MSS_optim an MSS}
#'         \item{MAC_optim}{Optimal MAC}
#'         \item{MAC_coef}{Ratio between MAC_optim an MAC}
#'         \item{Pmax_rel_optim}{Optimal relative maximal power estimated using MSS_optim * MAC_optim / 4}
#'         \item{slope_optim}{Optimal sprint profile slope}
#'         \item{slope_perc}{Percent ratio between slope and optimal slope}
#'         \item{time_optim}{Time to cover distance when profile is optimal}
#'         \item{time_gain}{Difference in time to cover distance between time_optimal and time}
#'         \item{probe_MSS}{Probing MSS}
#'         \item{probe_MSS_time}{Time to cover distance when using probe_MSS}
#'         \item{probe_MSS_time_gain}{Time difference}
#'         \item{probe_MAC}{Probing MAC}
#'         \item{probe_MAC_time}{Time to cover distance when using probe_MAC}
#'         \item{probe_MAC_time_gain}{Time difference}
#'         \item{probe_time_gain_perc}{Percent ratio between probe_MSS_time_gain and probe_MAC_time_gain}
#'    }
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
      MAC = .x$MAC
    ))
  })
}

#' @rdname find_functions
#' @description \code{find_optimal_FV_distance} finds the distance for which the
#'     FV profile is optimal
#' @param min,max Range over which to find the distance
#' @param metric Metric from \code{\link{find_optimal_FV}}. Default is "slope_perc"
#' @param min_func Function to be minimized. \code{metric} is forwarded as argument.
#'     Used if other metric is used for optimization (e.g., "time_gain")
#' @return Distance
#' @export
find_optimal_FV_distance <- function(F0,
                                     V0,
                                     bodymass = 75,
                                     min = 1,
                                     max = 60,
                                     metric = "slope_perc",
                                     min_func = function(metric) (100 - metric)^2,
                                     ...) {
  opt_func <- function(par) {
    min_func(find_optimal_FV(distance = par, F0 = F0, V0 = V0, bodymass = bodymass, ...)[[metric]])
  }

  get_optim_model <- function() {
    tryCatch(
      {
        stats::optim(
          par = min,
          fn = opt_func,
          method = "Brent",
          lower = min,
          upper = max
        )
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
#' @param min_func Function to be minimized. \code{metric} is forwarded as argument.
#'     Used if other metric is used for optimization (e.g., "time_gain")
#' @return Distance
#' @export
find_optimal_MSS_MAC_distance <- function(MSS,
                                          MAC,
                                          min = 1,
                                          max = 60,
                                          metric = "slope_perc",
                                          min_func = function(metric) (100 - metric)^2) {
  opt_func <- function(par) {
    min_func(find_optimal_MSS_MAC(distance = par, MSS = MSS, MAC = MAC)[[metric]])
  }

  get_optim_model <- function() {
    tryCatch(
      {
        stats::optim(
          par = min,
          fn = opt_func,
          method = "Brent",
          lower = min,
          upper = max
        )
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

# New method of REALLY keeping Peak Power same while changing slope
find_FV_peak_power <- function(F0, V0, bodymass, ...) {
  converted <- convert_FV(F0, V0, bodymass, ...)
  MSS_conv <- converted$MSS
  MAC_conv <- converted$MAC

  find_max_power_distance(
    MSS = MSS_conv,
    MAC = MAC_conv,
    bodymass = bodymass,
    ...
  )$max_power
}

find_V0 <- function(F0, Ppeak, bodymass, ...) {
  opt_func <- function(V0) {
    (Ppeak - find_FV_peak_power(F0, V0, bodymass))
  }

  V0 <- (4 * Ppeak) / F0

  get_optim_model <- function() {
    tryCatch(
      {
        stats::uniroot(f = opt_func, interval = c(1 / 2, 2) * V0)$root
      },
      error = function(cond) {
        return(NA)
      },
      warning = function(cond) {
        return(NA)
      }
    )
  }
  get_optim_model()
}

find_optimal_FV_peak_scalar <- function(distance, F0, V0, bodymass = 75, ...) {
  Ppeak_orig <- find_FV_peak_power(F0, V0, bodymass, ...)

  opt_func <- function(par) {
    new_F0 <- par
    new_V0 <- find_V0(new_F0, Ppeak_orig, bodymass, ...)

    predict_time_at_distance_FV(
      distance = distance,
      F0 = new_F0,
      V0 = new_V0,
      bodymass = bodymass,
      ...
    )
  }

  F0_optim <- find_optimal_FV_scalar(distance, F0, V0, bodymass, ...)$F0_optim

  get_optim_model <- function() {
    tryCatch(
      {
        stats::optim(
          par = F0_optim,
          fn = opt_func,
          method = "Brent",
          lower = F0_optim * 0.8,
          upper = F0_optim * 1.2
        )
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

  F0_optim <- results$par
  V0_optim <- find_V0(F0_optim, Ppeak_orig, bodymass, ...)

  FV_slope <- -(F0 / bodymass) / V0
  FV_slope_optim <- -(F0_optim / bodymass) / V0_optim

  FV_slope_perc <- (FV_slope / FV_slope_optim) * 100

  # Probe
  t_orig <- predict_time_at_distance_FV(
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

  ##############################################
  # Peak Power
  # Original profile
  converted <- convert_FV(F0, V0, bodymass, ...)
  MSS_conv <- converted$MSS
  MAC_conv <- converted$MAC

  Ppeak_dist <- find_max_power_distance(
    MSS = MSS_conv,
    MAC = MAC_conv,
    bodymass = bodymass,
    ...
  )

  Ppeak_time <- find_max_power_time(
    MSS = MSS_conv,
    MAC = MAC_conv,
    bodymass = bodymass,
    ...
  )

  # Optimal profile
  converted_optim <- convert_FV(F0_optim, V0_optim, bodymass, ...)
  MSS_conv_optim <- converted_optim$MSS
  MAC_conv_optim <- converted_optim$MAC

  Ppeak_dist_optim <- find_max_power_distance(
    MSS = MSS_conv_optim,
    MAC = MAC_conv_optim,
    bodymass = bodymass,
    ...
  )

  Ppeak_time_optim <- find_max_power_time(
    MSS = MSS_conv_optim,
    MAC = MAC_conv_optim,
    bodymass = bodymass,
    ...
  )

  # Return the results
  list(
    F0 = F0,
    V0 = V0,
    bodymass = bodymass,
    Pmax = F0 * V0 / 4,
    Pmax_rel = (F0 * V0 / 4) / bodymass,
    slope = FV_slope,
    distance = distance,
    time = t_orig,
    Ppeak = Ppeak_dist$max_power,
    Ppeak_rel = Ppeak_dist$max_power / bodymass,
    Ppeak_dist = Ppeak_dist$distance,
    Ppeak_time = Ppeak_time$time,
    F0_optim = F0_optim,
    F0_coef = F0_optim / F0,
    V0_optim = V0_optim,
    V0_coef = V0_optim / V0,
    Pmax_optim = F0_optim * V0_optim / 4,
    Pmax_rel_optim = (F0_optim * V0_optim / 4) / bodymass,
    slope_optim = FV_slope_optim,
    slope_perc = FV_slope_perc,
    time_optim = results$value,
    time_gain = results$value - t_orig,
    Ppeak_optim = Ppeak_dist_optim$max_power,
    Ppeak_rel_optim = Ppeak_dist_optim$max_power / bodymass,
    Ppeak_dist_optim = Ppeak_dist_optim$distance,
    Ppeak_time_optim = Ppeak_time_optim$time,
    probe_F0 = F0 * 1.05,
    probe_F0_time = t_F0,
    probe_F0_time_gain = t_F0 - t_orig,
    probe_V0 = V0 * 1.05,
    probe_V0_time = t_V0,
    probe_V0_time_gain = t_V0 - t_orig,
    probe_time_gain_perc = (t_V0 - t_orig) / (t_F0 - t_orig) * 100
  )
}

#' @rdname find_functions
#' @description \code{find_optimal_FV_peak} finds "optimal" \code{F0} and \code{V0} where time at distance is
#'     minimized, while keeping the \code{Ppeak} the same
#' @return A data frame with the following columns
#'     \describe{
#'         \item{F0}{Original F0}
#'         \item{V0}{Original F0}
#'         \item{bodymass}{Bodymass}
#'         \item{Pmax}{Maximal power estimated using F0 * V0 / 4}
#'         \item{Pmax_rel}{Relative maximal power}
#'         \item{slope}{FV profile slope}
#'         \item{distance}{Distance}
#'         \item{time}{Time to cover distance}
#'         \item{Ppeak}{Peak power estimated quantitatively}
#'         \item{Ppeak_rel}{Relative peak power}
#'         \item{Ppeak_dist}{Distance at which peak power is manifested}
#'         \item{Ppeak_time}{Time at which peak power is manifested}
#'         \item{F0_optim}{Optimal F0}
#'         \item{F0_coef}{Ratio between F0_optim an F0}
#'         \item{V0_optim}{Optimal V0}
#'         \item{V0_coef}{Ratio between V0_optim an V0}
#'         \item{Pmax_optim}{Optimal maximal power estimated F0_optim * V0_optim / 4}
#'         \item{Pmax_rel_optim}{Optimal relative maximal power}
#'         \item{slope_optim}{Optimal FV profile slope}
#'         \item{slope_perc}{Percent ratio between slope and optimal slope}
#'         \item{time_optim}{Time to cover distance when profile is optimal}
#'         \item{time_gain}{Difference in time to cover distance between time_optimal and time}
#'         \item{Ppeak_optim}{Optimal peak power estimated quantitatively}
#'         \item{Ppeak_rel_optim}{Optimal relative peak power}
#'         \item{Ppeak_dist_optim}{Distance at which optimal peak power is manifested}
#'         \item{Ppeak_time_optim}{Time at which optimal peak power is manifested}
#'         \item{probe_F0}{Probing F0}
#'         \item{probe_F0_time}{Time to cover distance when using probe_F0}
#'         \item{probe_F0_time_gain}{Time difference}
#'         \item{probe_V0}{Probing V0}
#'         \item{probe_V0_time}{Time to cover distance when using probe_V0}
#'         \item{probe_V0_time_gain}{Time difference}
#'         \item{probe_time_gain_perc}{Percent ratio between probe_V0_time_gain and probe_F0_time_gain}
#'    }
#' @export
find_optimal_FV_peak <- function(distance, F0, V0, bodymass = 75, ...) {
  df <- data.frame(
    distance = distance,
    F0 = F0,
    V0 = V0,
    bodymass = bodymass
  )

  df$id <- sprintf(paste0("%0", floor(log10(nrow(df))) + 1, "d"), seq(1, nrow(df)))

  df_list <- split(df, df$id)

  purrr::map_df(df_list, function(.x) {
    data.frame(find_optimal_FV_peak_scalar(
      distance = .x$distance,
      F0 = .x$F0,
      V0 = .x$V0,
      bodymass = .x$bodymass,
      ...
    ))
  })
}

#' @rdname find_functions
#' @description \code{find_optimal_FV_peak_distance} finds the distance for which the
#'     FV profile is optimal
#' @param min,max Range over which to find the distance
#' @param metric Metric from \code{\link{find_optimal_FV_peak}}. Default is "slope_perc"
#' @param min_func Function to be minimized. \code{metric} is forwarded as argument.
#'     Used if other metric is used for optimization (e.g., "time_gain")
#' @return Distance
#' @export
find_optimal_FV_peak_distance <- function(F0,
                                          V0,
                                          bodymass = 75,
                                          min = 1,
                                          max = 60,
                                          metric = "slope_perc",
                                          min_func = function(metric) (100 - metric)^2,
                                          ...) {
  opt_func <- function(par) {
    min_func(find_optimal_FV_peak(
      distance = par,
      F0 = F0, V0 = V0, bodymass = bodymass, ...)[[metric]])
  }

  get_optim_model <- function() {
    tryCatch(
      {
        stats::optim(
          par = min,
          fn = opt_func,
          method = "Brent",
          lower = min,
          upper = max
        )
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

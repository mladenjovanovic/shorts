#' Probe profile functions
#'
#' Family of functions that serve a purpose of probing sprint or force-velocity profile. This is done
#'     by increasing individual sprint parameter for a percentage and calculating which parameter
#'     improvement yield biggest deduction in sprint tim
#'
#' @param MSS,MAC Numeric vectors. Model parameters
#' @param F0,V0 Numeric vectors. FV profile parameters
#' @param bodymass Body mass in kg
#' @param inertia External inertia in kg (for example a weight vest, or a sled).
#'         Not included in the air resistance calculation
#' @param resistance External horizontal resistance in Newtons (for example tether device or a sled friction resistance)
#' @param distance Numeric vector
#' @param perc Numeric vector. Probing percentage. Default is 2.5 percent
#' @inheritDotParams get_air_resistance
#' @examples
#' MSS <- 10
#' MAC <- 8
#' bodymass <- 75
#'
#' fv <- create_FVP(MSS, MAC, bodymass)
#'
#' dist <- seq(5, 40, by = 5)
#'
#' probe_MSS_MAC_profile <- probe_MSS_MAC(
#'   distance = dist,
#'   MSS,
#'   MAC
#' )[["profile_imb"]]
#'
#' probe_FV_profile <- probe_FV(
#'   distance = dist,
#'   fv$F0,
#'   fv$V0,
#'   fv$bodymass
#' )[["profile_imb"]]
#'
#' plot(x = dist, y = probe_MSS_MAC_profile, type = "l", ylab = "Profile imbalance")
#' lines(x = dist, y = probe_FV_profile, type = "l", col = "blue")
#' abline(h = 100, col = "gray", lty = 2)
#' @name probe_functions
NULL

probe_FV_scalar <- function(distance, F0, V0, bodymass = 75, inertia = 0, resistance = 0, perc = 2.5, ...) {
  t_orig <- predict_time_at_distance_FV(
    distance = distance,
    F0 = F0,
    V0 = V0,
    bodymass = bodymass,
    inertia = inertia,
    resistance = resistance,
    ...
  )

  t_F0 <- predict_time_at_distance_FV(
    distance = distance,
    F0 = F0 * (1 + perc / 100),
    V0 = V0,
    bodymass = bodymass,
    inertia = inertia,
    resistance = resistance,
    ...
  )

  t_V0 <- predict_time_at_distance_FV(
    distance = distance,
    F0 = F0,
    V0 = V0 * (1 + perc / 100),
    bodymass = bodymass,
    inertia = inertia,
    resistance = resistance,
    ...
  )

  FV_slope <- -(F0 / bodymass) / V0
  profile_imb <- (t_V0 - t_orig) / (t_F0 - t_orig) * 100

  # Return the results
  list(
    F0 = F0,
    V0 = V0,
    bodymass = bodymass,
    inertia = inertia,
    resistance = resistance,
    Pmax = F0 * V0 / 4,
    Pmax_rel = (F0 * V0 / 4) / bodymass,
    slope = FV_slope,
    distance = distance,
    time = t_orig,
    probe_perc = perc,
    F0_probe = F0 * (1 + perc / 100),
    F0_probe_time = t_F0,
    F0_probe_time_gain = t_F0 - t_orig,
    V0_probe = V0 * (1 + perc / 100),
    V0_probe_time = t_V0,
    V0_probe_time_gain = t_V0 - t_orig,
    profile_imb = profile_imb
  )
}

# -----------------------------------------------------
# Vector function

#' @rdname probe_functions
#' @description \code{probe_FV} "probes" \code{F0} and \code{V0} and calculates which one
#'     improves sprint time for a defined \code{distance}
#' @return \code{probe_FV} returns a data frame with the following columns
#'     \describe{
#'         \item{F0}{Original F0}
#'         \item{V0}{Original F0}
#'         \item{bodymass}{Bodymass}
#'         \item{inertia}{Inertia}
#'         \item{resistance}{Resistance}
#'         \item{Pmax}{Maximal power estimated using F0 * V0 / 4}
#'         \item{Pmax_rel}{Relative maximal power}
#'         \item{slope}{FV profile slope}
#'         \item{distance}{Distance}
#'         \item{time}{Time to cover distance}
#'         \item{probe_perc}{Probe percentage}
#'         \item{F0_probe}{Probing F0}
#'         \item{F0_probe_time}{Predicted time for distance when F0 is probed}
#'         \item{F0_probe_time_gain}{Difference in time to cover distance between time_optimal and time}
#'         \item{V0_probe}{Probing V0}
#'         \item{V0_probe_time}{Predicted time for distance when V0 is probed}
#'         \item{V0_probe_time_gain}{Difference in time to cover distance between time_optimal and time}
#'         \item{profile_imb}{Percent ratio between V0_probe_time_gain and F0_probe_time_gain}
#'    }
#' @export
probe_FV <- function(distance, F0, V0, bodymass = 75, inertia = 0, resistance = 0, perc = 2.5, ...) {
  df <- data.frame(
    distance = distance,
    F0 = F0,
    V0 = V0,
    bodymass = bodymass,
    inertia = inertia,
    resistance = resistance,
    perc = perc,
    ...
  )

  df$id <- sprintf(paste0("%0", floor(log10(nrow(df))) + 1, "d"), seq(1, nrow(df)))

  df_list <- split(df, df$id)

  purrr::map_df(df_list, function(.x) {
    .x$id <- NULL

    res <- do.call(probe_FV_scalar, as.list(.x))

    data.frame(res)
  })
}

# Probe MSS/MAC

probe_MSS_MAC_scalar <- function(distance, MSS, MAC, perc = 2.5) {
  t_orig <- predict_time_at_distance(
    distance = distance,
    MSS = MSS,
    MAC = MAC
  )

  t_MSS <- predict_time_at_distance(
    distance = distance,
    MSS = MSS * (1 + perc / 100),
    MAC = MAC
  )

  t_MAC <- predict_time_at_distance(
    distance = distance,
    MSS = MSS,
    MAC = MAC * (1 + perc / 100)
  )

  slope <- -MAC / MSS
  profile_imb <- (t_MSS - t_orig) / (t_MAC - t_orig) * 100

  # Return the results
  list(
    MSS = MSS,
    MAC = MAC,
    Pmax_rel = MSS * MAC / 4,
    slope = slope,
    distance = distance,
    time = t_orig,
    probe_perc = perc,
    MSS_probe = MSS * (1 + perc / 100),
    MSS_probe_time = t_MSS,
    MSS_probe_time_gain = t_MSS - t_orig,
    MAC_probe = MAC * (1 + perc / 100),
    MAC_probe_time = t_MAC,
    MAC_probe_time_gain = t_MAC - t_orig,
    profile_imb = profile_imb
  )
}

# -----------------------------------------------------
# Vector function

#' @rdname probe_functions
#' @description \code{probe_MSS_MAC} "probes" \code{MSS} and \code{MAC} and calculates which one
#'     improves sprint time for a defined \code{distance}
#' @return \code{probe_MSS_MAC} returns a data frame with the following columns
#'     \describe{
#'         \item{MSS}{Original MSS}
#'         \item{MAC}{Original MAC}
#'         \item{Pmax_rel}{Relative maximal power estimated using MSS * MAC / 4}
#'         \item{slope}{Sprint profile slope}
#'         \item{distance}{Distance}
#'         \item{time}{Time to cover distance}
#'         \item{probe_perc}{Probe percentage}
#'         \item{MSS_probe}{Probing MSS}
#'         \item{MSS_probe_time}{Predicted time for distance when MSS is probed}
#'         \item{MSS_probe_time_gain}{Difference in time to cover distance between probe time and time}
#'         \item{MAC_probe}{Probing MAC}
#'         \item{MAC_probe_time}{Predicted time for distance when MAC is probed}
#'         \item{MAC_probe_time_gain}{Difference in time to cover distance between probing time and time}
#'         \item{profile_imb}{Percent ratio between MSS_probe_time_gain and MAC_probe_time_gain}
#'    }
#' @export
probe_MSS_MAC <- function(distance, MSS, MAC, perc = 2.5) {
  df <- data.frame(
    distance = distance,
    MSS = MSS,
    MAC = MAC,
    perc = perc
  )

  df$id <- sprintf(paste0("%0", floor(log10(nrow(df))) + 1, "d"), seq(1, nrow(df)))

  df_list <- split(df, df$id)

  purrr::map_df(df_list, function(.x) {
    data.frame(probe_MSS_MAC_scalar(
      distance = .x$distance,
      MSS = .x$MSS,
      MAC = .x$MAC,
      perc = .x$perc
    ))
  })
}

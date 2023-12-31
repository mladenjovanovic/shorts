#' Create Force-Velocity Profile
#'
#' Creates Force-Velocity Profile (FVP) modified using ideas by Pierre Samozino and JB-Morin, et al. (2016) and
#'    Pierre Samozino and Nicolas Peyror, et al (2021).
#' @inheritParams predict_kinematics
#' @inheritParams get_air_resistance
#' @return List containing the following elements:
#'     \describe{
#'         \item{bodymass}{Returned \code{bodymass} used in FV profiling}
#'         \item{F0}{Horizontal force when velocity=0}
#'         \item{F0_rel}{\code{F0} divided by \code{bodymass}}
#'         \item{V0}{Velocity when horizontal force=0}
#'         \item{Pmax}{Maximal horizontal power}
#'         \item{Pmax_rel}{\code{Pmax} divided by \code{bodymass}}
#'         \item{FV_slope}{Slope of the FV profile. See References for more info}
#'     }
#'
#' @export
#' @references
#'     Samozino P, Rabita G, Dorel S, Slawinski J, Peyrot N, Saez de Villarreal E, Morin J-B. 2016.
#'     A simple method for measuring power, force, velocity properties, and mechanical effectiveness in
#'     sprint running: Simple method to compute sprint mechanics. Scandinavian Journal of Medicine & Science
#'     in Sports 26:648–658. DOI: 10.1111/sms.12490.
#'
#'     Samozino P, Peyrot N, Edouard P, Nagahara R, Jimenez‐Reyes P, Vanwanseele B, Morin J. 2022.
#'      Optimal mechanical force‐velocity profile for sprint acceleration performance.
#'       Scandinavian Journal of Medicine & Science in Sports 32:559–575. DOI: 10.1111/sms.14097.

#' @examples
#' data("jb_morin")
#'
#' m1 <- model_radar_gun(time = jb_morin$time, velocity = jb_morin$velocity)
#'
#' fv_profile <- create_FVP(
#'   MSS = m1$parameters$MSS,
#'   MAC = m1$parameters$MAC,
#'   bodyheight = 1.72,
#'   bodymass = 120,
#' )
#'
#' fv_profile
create_FVP <- function(MSS,
                       MAC,
                       bodymass = 75,
                       inertia = 0,
                       resistance = 0,
                       wind_velocity = 0,
                       ...) {

  # Find F0, or force when velocity equals 0
  F0 <- predict_force_at_velocity(
    velocity = 0,
    MSS = MSS,
    MAC = MAC,
    bodymass = bodymass,
    inertia = inertia,
    resistance = resistance,
    wind_velocity = wind_velocity,
    ...
  )

  # Find V0, or velocity where force equals zero
  k_rel <- get_air_resistance(
    velocity = 1,
    bodymass = bodymass,
    wind_velocity = 0,
    ...
  ) / bodymass

  V0 <- -(sqrt(4 * k_rel * bodymass * MSS * (-MAC * inertia * MSS - MAC * bodymass * MSS - k_rel * bodymass * MSS * wind_velocity^2 - MSS * resistance) + (MAC * inertia + MAC * bodymass + 2 * k_rel * bodymass * MSS * wind_velocity)^2) - MAC * inertia - MAC * bodymass - 2 * k_rel * bodymass * MSS * wind_velocity) / (2 * k_rel * bodymass * MSS)

  F0_rel <- F0 / bodymass
  Pmax <- (F0 * V0) / 4
  Pmax_rel <- Pmax / bodymass
  FV_slope <- -(F0 / bodymass) / V0

  # Return list
  list(
    bodymass = bodymass,
    F0 = F0,
    F0_rel = F0_rel,
    V0 = V0,
    Pmax = Pmax,
    Pmax_rel = Pmax_rel,
    FV_slope = FV_slope
  )
}

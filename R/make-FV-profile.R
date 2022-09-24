#' Get Force-Velocity Profile
#'
#' Provides Force-Velocity (FV) profile suggested by Pierre Samozino and JB-Morin, et al. (2016) and
#'    Pierre Samozino and Nicolas Peyror, et al (2021).
#' @inheritParams predict_kinematics
#' @param RFmax_cutoff Time cut-off used to estimate \code{RFmax} and \code{Drf}. Default is 0.3s
#' @return List containing the following elements:
#'     \describe{
#'         \item{bodymass}{Returned \code{bodymass} used in FV profiling}
#'         \item{F0}{Horizontal force when velocity=0}
#'         \item{F0_rel}{\code{F0} divided by \code{bodymass}}
#'         \item{V0}{Velocity when horizontal force=0}
#'         \item{Pmax}{Maximal horizontal power}
#'         \item{Pmax_rel}{\code{Pmax} divided by \code{bodymass}}
#'         \item{FV_slope}{Slope of the FV profile. See References for more info}
#'         \item{RFmax}{Maximal force ratio after 0.3sec. See References for more info}
#'         \item{RFmax_cutoff}{Time cut-off used to estimate RFmax}
#'         \item{Drf}{Slope of Force Ratio (RF) and velocity. See References for more info}
#'         \item{RSE_FV}{Residual standard error of the FV profile.}
#'         \item{RSE_Drf}{Residual standard error of the RF-velocity profile}
#'         \item{F0_poly}{Horizontal force when velocity=0, estimated using the analytics/polynomial method}
#'         \item{F0_poly_rel}{\code{F0_poly} divided by \code{bodymass}}
#'         \item{V0_poly}{Velocity when horizontal force=0, estimated using the analytics/polynomial method}
#'         \item{Pmax_poly}{Maximal horizontal power, estimated using the analytics/polynomial method}
#'         \item{Pmax_poly_rel}{\code{Pmax_poly} divided by \code{bodymass}}
#'         \item{FV_slope_poly}{Slope of the FV profile, estimated using the analytics/polynomial method. See References for more info}
#'         \item{data}{Data frame containing simulated data used to estimate parameters}
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
#' fv_profile <- make_FV_profile(
#'   MSS = m1$parameters$MSS,
#'   MAC = m1$parameters$MAC,
#'   bodyheight = 1.72,
#'   bodymass = 120
#' )
#'
#' print(fv_profile)
#' plot(fv_profile)
#' plot(fv_profile, "time")
make_FV_profile <- function(MSS,
                            MAC,
                            bodymass = 75,
                            max_time = 6,
                            frequency = 100,
                            RFmax_cutoff = 0.3,
                            ...) {

  # Create 0-max_time sample
  df <- data.frame(time = seq(0, 6, length.out = frequency * max_time))

  # There is no need for time_correction here since we want to
  # analyze the profile when sprint is initiated
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

  df$net_horizontal_force <- df$bodymass * df$acceleration

  df$air_resistance <- predict_air_resistance_at_time(
    time = df$time,
    MSS = MSS,
    MAC = MAC,
    bodymass = bodymass, ...
  )

  df$horizontal_force <- predict_force_at_time(
    time = df$time,
    MSS = MSS,
    MAC = MAC,
    bodymass = bodymass, ...
  )
  df$horizontal_force_relative <- df$horizontal_force / bodymass
  df$vertical_force <- (bodymass * 9.81)

  df$resultant_force <- sqrt(df$horizontal_force^2 + df$vertical_force^2)
  df$resultant_force_relative <- df$resultant_force / bodymass
  df$power <- df$horizontal_force * df$velocity
  df$relative_power <- df$horizontal_force_relative * df$velocity
  df$RF <- df$horizontal_force / df$resultant_force
  df$force_angle <- atan(df$vertical_force / df$horizontal_force) * 180 / pi

  # Get RF max when t > 0.3sec
  RFmax <- max(df[df$time > RFmax_cutoff, ]$RF)

  # Get DRF
  vel_over_cutoff <- df[df$time > RFmax_cutoff, ]$velocity
  RF_over_cutoff <- df[df$time > RFmax_cutoff, ]$RF

  Drf_model <- stats::lm(RF_over_cutoff ~ vel_over_cutoff)

  Drf <- stats::coef(Drf_model)[[2]]

  # FV profile
  fv_profile <- stats::lm(
    df$horizontal_force ~ df$velocity
  )

  F0 <- stats::coef(fv_profile)[[1]]
  F0_rel <- F0 / bodymass

  Slope <- stats::coef(fv_profile)[[2]] / bodymass
  V0 <- -F0_rel / Slope

  Pmax <- F0 * V0 / 4
  Pmax_rel <- Pmax / bodymass

  ###########################
  # Get polynomial solutions
  # Get k value for the air resistance
  k_rel <- get_air_resistance(velocity = 1, bodymass = bodymass, ...) / bodymass

  tau <- MSS / MAC

  F0_poly <- MAC * bodymass
  V0_poly <- (1 / (2 * k_rel * tau)) * (1 - sqrt(1 - 4 * k_rel * tau * MSS))
  Pmax_poly <- (F0_poly * V0_poly) / 4
  Pmax_poly_rel <- Pmax_poly / bodymass
  Slope_poly <- - (F0_poly / bodymass) / V0_poly

  # Return list
  new_fv_profile(
    bodymass = bodymass,
    F0 = F0,
    F0_rel = F0_rel,
    V0 = V0,
    Pmax = Pmax,
    Pmax_rel = Pmax_rel,
    FV_slope = Slope,
    RFmax_cutoff = RFmax_cutoff,
    RFmax = RFmax,
    Drf = Drf,
    RSE_FV = summary(fv_profile)$sigma,
    RSE_Drf = summary(Drf_model)$sigma,
    F0_poly = F0_poly,
    F0_poly_rel = MAC,
    V0_poly = V0_poly,
    Pmax_poly = Pmax_poly,
    Pmax_poly_rel = Pmax_poly_rel,
    FV_slope_poly = Slope_poly,
    data = df
  )
}

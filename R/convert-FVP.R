#' Convert Force-Velocity profile back to Acceleration-Velocity profile
#'
#' This function converts back the Force-Velocity profile (FVP; F0 and V0 parameters) to
#'     Acceleration-Velocity profile (AVP; MSS and MAC parameters)
#' @inheritParams predict_kinematics
#' @inheritParams get_air_resistance
#' @returns A list with calculated \code{MSS} and \code{MAC} parameters
#' @export
#' @examples
#' FVP <- create_FVP(7, 8.3, inertia = 10, resistance = 50)
#'
#' convert_FVP(FVP$F0, FVP$V0, inertia = 10, resistance = 50)
convert_FVP <- function(F0, V0, bodymass = 75, inertia = 0, resistance = 0, wind_velocity = 0, ...) {
  k_rel <- get_air_resistance(velocity = 1, bodymass = bodymass, wind_velocity = 0, ...) / bodymass

  MAC <- ((F0 - resistance - get_air_resistance(velocity = 0, bodymass = bodymass, wind_velocity = wind_velocity, ...)) /
    (bodymass + inertia))

  MSS <- (MAC * V0 * (inertia + bodymass)) / (MAC * inertia + MAC * bodymass + k_rel * bodymass * V0^2 - 2 * k_rel * bodymass * V0 * wind_velocity + k_rel * bodymass * wind_velocity^2 + resistance)

  list(
    MSS = MSS,
    MAC = MAC
  )
}

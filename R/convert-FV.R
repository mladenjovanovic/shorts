#' Convert Force-Velocity profile back to Acceleration-Velocity profile
#'
#' This function converts back the Force-Velocity (FV) profile to Acceleration-Velocity (AP)
#'     profile
#' @inheritParams predict_kinematics
#' @returns A list with calculated \code{MSS} and \code{MAC} parameters
#' @export
#' @examples
#' FVP <- make_FV_profile(7, 8.3, inertia = 10, resistance = 50)
#'
#' convert_FV(FVP$F0, FVP$V0, inertia = 10, resistance = 50)
convert_FV <- function(F0, V0, bodymass = 75, inertia = 0, resistance = 0, ...) {
  k_rel <- get_air_resistance(velocity = 1, bodymass = bodymass, ...) / bodymass

  MAC <- ((F0 - resistance) / (bodymass + inertia))

  MSS <- (V0 * (F0 - resistance))/(F0 + k_rel * bodymass * V0^2)

  list(
    MSS = MSS,
    MAC = MAC
  )
}

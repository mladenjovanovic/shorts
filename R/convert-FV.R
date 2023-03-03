# This is internal function for converting back the FV profile to MSS/MAC parameters
# It is used in FV profile optimization functions
convert_FV <- function(F0, V0, bodymass, ...) {
  k_rel <- get_air_resistance(velocity = 1, bodymass = bodymass, ...) / bodymass

  F0_rel <- F0 / bodymass

  MAC <- F0_rel

  MSS <- (F0_rel * V0) / (k_rel * V0^2 + F0_rel)

  list(
    MSS = MSS,
    MAC = MAC
  )
}

MSS <- 10
MAC <- 10 / 1.2

fv <- make_FV_profile(10, 8, 80)
find_optimal_FV(distance = 20, fv$F0_poly, fv$V0_poly, fv$bodymass)

find_optimal_FV_distance(fv$F0_poly, fv$V0_poly, fv$bodymass)

predict_time_at_distance(40, MSS, MAC)

predict_time_at_distance_FV(distance = 40, fv$F0_poly, fv$V0_poly, fv$bodymass)
predict_time_at_distance_FV(distance = 40, fv$F0, fv$V0, fv$bodymass)

find_optimal_FV(distance = 20, fv$F0, fv$V0, fv$bodymass)
find_optimal(distance = 20, 10, 10/1.2)


# Analytic solution

make_FV_profile_analytic <- function(MSS, MAC, bodymass = 75, ...) {
  # Get k value for the air resistance
  k_rel <- get_air_resistance(velocity = 1, bodymass = bodymass, ...) / bodymass

  tau <- MSS / MAC

  F0 <- MAC * bodymass
  V0 <- (1 / (2 * k_rel * tau)) * (1 - sqrt(1 - 4 * k_rel * tau * MSS))
  Pmax <- (F0 * V0) / 4
  Pmax_relative <- Pmax / bodymass
}

make_FV_profile_analytic(10, 10/1.2, 80)

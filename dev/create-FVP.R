library(tidyverse)

expand_grid(
  velocity = seq(0, 10, length.out = 100),
  wind = seq(-5, 5, by = 1)
) %>%
  mutate(
    air_resistance = get_air_resistance(velocity, wind_velocity = wind)
  ) %>%
  ggplot(aes(x = velocity, y = air_resistance, group = wind)) +
  geom_line()

expand_grid(
  time = seq(0, 1, length.out = 1000),
  wind = seq(-20, 20, by = 5)
) %>%
  mutate(
    force = predict_force_at_time(time, 8, 7, wind_velocity = wind),
    power = predict_power_at_time(time, 8, 7, wind_velocity = wind)
  ) %>%
  ggplot(aes(x = time, y = force, group = wind)) +
  geom_line()

# ==========================================
# F = a * (BW + In) + Res + Air
# F = (MAC - v * MAC/MSS) * (BW + In) + Res + k * BW * (v - wind)^2 * sign(v-wind)

# A = MAC
# P = MSS
# M = BW
# J = inertia
# R = resistance
# W = wind_velocity
# k = k

# F = (A - v * A/P) * (M + J) + R + k * M * (v - W)^2 * sign(v-W)

# v = -(sqrt(4 * k *M *P *(-A *J *P - A *M *P - k *M *P *W^2 - P * R) + (A * J + A * M + 2* k *M *P *W)^2) - A* J - A* M - 2* k* M *P *W)/(2 *k *M *P)
# v0 = -(sqrt(4 * k_rel * bodymass * MSS * (-MAC * inertia * MSS - MAC * bodymass * MSS - k_rel * bodymass * MSS * wind_velocity^2 - MSS * resistance) + (MAC * inertia + MAC * bodymass + 2 * k_rel * bodymass * MSS * wind_velocity)^2) - MAC * inertia - MAC * bodymass - 2 * k_rel * bodymass * MSS * wind_velocity)/(2 * k_rel * bodymass * MSS)
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
  if ( wind_velocity == 0) {
    # Analytic solution for scenario without wind
    k_rel <- get_air_resistance(
      velocity = 1,
      bodymass = bodymass,
      wind_velocity = wind_velocity,
      ...) / bodymass

    tau <- MSS / MAC
    V0 <- (MAC * (bodymass + inertia) - sqrt(MAC^2 *(bodymass + inertia)^2 - 4 * MAC * k_rel * bodymass * MSS^2 * (bodymass + inertia) - 4 * k_rel * bodymass * resistance * MSS^2)) / (2 * k_rel * bodymass * MSS)
  } else {
    # Quantitative solution for conditions with wind
    V0 <- stats::uniroot(function(x) {
      predict_force_at_velocity(
        velocity = x,
        MSS = MSS,
        MAC = MAC,
        bodymass = bodymass,
        inertia = inertia,
        resistance = resistance,
        wind_velocity = wind_velocity,
        ...
      )
    }, interval = c(0, 100))$root
  }

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

make_FV_profile(9, 7, wind_velocity = -2)[c("F0", "V0", "V0_test")]
create_FVP(9, 7, wind_velocity = -2)[c("F0", "V0", "V0_test")]


# ================
# Convert back to MSS
# A = MAC
# P = MSS
# M = bodymass
# J = inertia
# R = resistance
# W = wind_velocity
# k = k_rel
# v = V0

#MSS = (A * v * (J + M))/(A * J + A * M + k * M * v^2 - 2 * k * M * v * W + k * M * W^2 + R)
#MSS = (MAC * V0 * (inertia + bodymass))/(MAC * inertia + MAC * bodymass + k_rel * bodymass * V0^2 - 2 * k_rel * bodymass * V0 * wind_velocity + k_rel * bodymass * wind_velocity^2 + resistance)

FVP <- create_FVP(9, 5, wind_velocity = 10, resistance = 0, inertia = 0)

convert_FVP(F0 = FVP$F0, V0 = FVP$V0, wind_velocity = 10, resistance = 0, inertia = 0)

predict_time_at_distance(20, 9, 5)
predict_time_at_distance_FV(20, FVP$F0, FVP$V0, wind_velocity = 10)

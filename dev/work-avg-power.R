library(tidyverse)

# ==========================================
# F(t) = a(t) * (BW + In) + Res + Air
# F(t) = (MSS / TAU * exp(1)^(-t / TAU)) * (BW + In) + Res + k * BW * (v(t) - wind)^2 * sign(v(t)-wind)
# where v(t) = (MSS * (1 - exp(1)^(-(t / TAU))))
# F(t) = (MSS / TAU * exp(1)^(-t / TAU)) * (BW + In) + Res + k * BW * ((MSS * (1 - exp(1)^(-(t / TAU)))) - wind)^2 * sign((MSS * (1 - exp(1)^(-(t / TAU))))-wind)

# P(t) = (MSS * (1 - exp(1)^(-(t / TAU)))) * ((MSS / TAU * exp(1)^(-t / TAU)) * (BW + In) + Res + k * BW * ((MSS * (1 - exp(1)^(-(t / TAU)))) - wind)^2 * sign((MSS * (1 - exp(1)^(-(t / TAU))))-wind))

# S = MSS
# A = TAU
# M = BW
# J = inertia
# R = resistance
# W = wind_velocity
# k = k_rel

# P(t) = (S * (1 - exp(1)^(-(t / A)))) * ((S / A * exp(1)^(-t / A)) * (M + J) + R + k * M * ((S * (1 - exp(1)^(-(t / A)))) - W)^2 * sign((S * (1 - exp(1)^(-(t / A))))-W))

# Define the integrand function
integrand <- function(t, MSS, MAC, bodymass, inertia, resistance, wind_velocity) {
  k_rel <- get_air_resistance(
    velocity = 1,
    bodymass = bodymass,
    wind_velocity = wind_velocity,
    ...) / bodymass



}

# Define the values for the variables
S_val <- 2  # Replace with your value
A_val <- 3  # Replace with your value
M_val <- 1  # Replace with your value
J_val <- 2  # Replace with your value
R_val <- 3  # Replace with your value
k_val <- 0.5  # Replace with your value
W_val <- 1  # Replace with your value

# Define the function for integration from 0 to t
integrate_result <- function(t) {
  integrate(integrand, lower = 0, upper = t, S = S_val, A = A_val, M = M_val, J = J_val, R = R_val, k = k_val, W = W_val)$value
}

# Calculate the result of the integral for a specific 't'
t <- 5  # Replace with your desired value of t
result <- integrate_result(t)
cat("The result of the integral from 0 to t is:", result)

predict_work_till_time <- function(time, ...) {
  df <- data.frame(
    time = time,
    ...
  )

  df_list <- split(df, seq(1, nrow(df)))

  res <- purrr::map_dbl(df_list, function(.x) {
    integrand <- function(x) {
      do.call(predict_power_at_time, as.list(tidyr::tibble(time = x, .x[-1])))
    }

    stats::integrate(integrand, lower = 0, upper = .x$time)$value
  })

  unname(res)
}




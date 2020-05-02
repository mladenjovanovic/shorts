## code to prepare `radar_gun_data` dataset
require(tidyverse)

set.seed(1667)

# Sampling rate 100Hz
time <- seq(0, 6, length.out = 6 * 100)

radar_gun_error <- 0.005

radar_gun_data <- tribble(
  ~athlete, ~bodyweight, ~MSS, ~MAC,
  "John", 75, 8, 7.5,
  "Kimberley", 55, 9, 7,
  "Jim", 105, 8, 9,
  "James", 65, 10, 9,
  "Samantha", 45, 6.5, 9.5,
)

radar_gun_data$TAU <- with(
  radar_gun_data,
  MSS / MAC
)

radar_gun_data <- expand_grid(radar_gun_data, time)

radar_gun_data <- radar_gun_data %>%
  mutate(
    velocity = shorts::predict_velocity_at_time(time, MSS, TAU)
  ) %>%

  # Select columns
  select(athlete, bodyweight, time, velocity) %>%
  # Add error
  mutate(velocity = velocity * rnorm(n(), 1, radar_gun_error) + velocity * rnorm(n(), 0, radar_gun_error),
         velocity = round(velocity, 3),
         velocity = ifelse(velocity < 0, 0, velocity),
         time = round(time, 3))


usethis::use_data(radar_gun_data, overwrite = TRUE)

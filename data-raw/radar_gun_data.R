## code to prepare `radar_gun_data` dataset
require(tidyverse)

set.seed(1667)

# Sampling rate 100Hz
time <- seq(0, 6, length.out = 6 * 100)

radar_gun_error <- 0.005

john_data <- tibble(
  bodyweight = 75,
  time = time,
  velocity = predict_velocity_at_time(time, MSS = 8, TAU = 0.6)
)

kimberley_data <- tibble(
  bodyweight = 55,
  time = time,
  velocity = predict_velocity_at_time(time, MSS = 7.2, TAU = 0.3)
)

jim_data <- tibble(
  bodyweight = 105,
  time = time,
  velocity = predict_velocity_at_time(time, MSS = 9, TAU = 1.1)
)

james_data <- tibble(
  bodyweight = 65,
  time = time,
  velocity = predict_velocity_at_time(time, MSS = 10, TAU = 0.2)
)


samantha_data <- tibble(
  bodyweight = 45,
  time = time,
  velocity = predict_velocity_at_time(time, MSS = 7, TAU = 0.2)
)

# Combine together
radar_gun_data <- rbind(
  data.frame(athlete = "John", john_data),
  data.frame(athlete = "Kimberley", kimberley_data),
  data.frame(athlete = "Jim", jim_data),
  data.frame(athlete = "James", james_data),
  data.frame(athlete = "Samantha", samantha_data)
)

# Add error
radar_gun_data <- radar_gun_data %>%
  mutate(velocity = velocity * rnorm(n(), 1, radar_gun_error) + velocity * rnorm(n(), 0, radar_gun_error),
         velocity = round(velocity, 3),
         velocity = ifelse(velocity < 0, 0, velocity),
         time = round(time, 3))


usethis::use_data(radar_gun_data, overwrite = TRUE)

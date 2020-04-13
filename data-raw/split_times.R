## code to prepare `split_times` dataset
require(tidyverse)

set.seed(1667)

distance <- c(5, 10, 15, 20, 30, 40)

timing_device_error <- 0.01

john_data <- tibble(
  bodyweight = 75,
  distance = distance,
  time = predict_time_at_distance(distance, MSS = 8, TAU = 0.6)
)

kimberley_data <- tibble(
  bodyweight = 55,
  distance = distance,
  time = predict_time_at_distance(distance, MSS = 7.2, TAU = 0.3)
)

jim_data <- tibble(
  bodyweight = 105,
  distance = distance,
  time = predict_time_at_distance(distance, MSS = 9, TAU = 1.1)
)

james_data <- tibble(
  bodyweight = 65,
  distance = distance,
  time = predict_time_at_distance(distance, MSS = 10, TAU = 0.2)
)


samantha_data <- tibble(
  bodyweight = 45,
  distance = distance,
  time = predict_time_at_distance(distance, MSS = 7, TAU = 0.2)
)

# Combine together
split_times <- rbind(
  data.frame(athlete = "John", john_data),
  data.frame(athlete = "Kimberley", kimberley_data),
  data.frame(athlete = "Jim", jim_data),
  data.frame(athlete = "James", james_data),
  data.frame(athlete = "Samantha", samantha_data)
)

# Add error
split_times <- split_times %>%
  mutate(time = time * rnorm(n(), 1, timing_device_error),
         time = round(time, 3))

usethis::use_data(split_times, overwrite = TRUE)

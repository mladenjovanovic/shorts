## code to prepare `split_times` dataset
require(tidyverse)

set.seed(1667)

distance <- c(5, 10, 15, 20, 30, 40)

timing_device_error <- 0.00

split_times <- tribble(
  ~athlete, ~bodyweight, ~distance_shift, ~MSS, ~MAC,
  "John", 75, 0.2, 8, 7.5,
  "Kimberley", 55, 0.3, 9, 7,
  "Jim", 105, 0.5, 8, 9,
  "James", 65, 0.1, 10, 9,
  "Samantha", 45, 0.4, 6.5, 9.5,
)

split_times$TAU <- with(
  split_times,
  MSS / MAC
)

split_times <- expand_grid(split_times, distance)

split_times <- split_times %>%
  mutate(
    true_distance = distance + distance_shift,
    true_time = shorts::predict_time_at_distance(true_distance, MSS, MAC),
    time_diff = shorts::predict_time_at_distance(distance_shift, MSS, MAC),
    time = true_time - time_diff
  ) %>%
  # Select columns
  select(athlete, bodyweight, distance, time) %>%
  # Add error
  mutate(
    time = time + rnorm(n(), 0, timing_device_error),
    time = round(time, 3)
  )

usethis::use_data(split_times, overwrite = TRUE)

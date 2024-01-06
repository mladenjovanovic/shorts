## code to prepare `laser_gun_data` dataset goes here
library(tidyverse)
library(signal)

# Load data
laser_gun_data <- read.csv("data-raw/laser-gun-data.csv")

laser_gun_data <- laser_gun_data %>%
  mutate(
    raw_acceleration = c(NA, diff(velocity) / diff(time))
  ) %>%
  select(time, distance, velocity, raw_velocity, raw_acceleration) %>%
  dplyr::filter(time > 0.375) %>%
  # mutate velocity
  mutate(velocity = velocity - min(velocity))

# Butter filter
# Define sampling frequency
sampling_frequency <- 1000  # 1000Hz as you mentioned

# Define cutoff frequency for human movement
cutoff_frequency <- 1  # 1Hz

# Calculate normalized cutoff frequency (Wn)
Wn <- cutoff_frequency / (sampling_frequency / 2)

# Design a low-pass Butterworth filter (4th order)
butter_filter <- butter(4, Wn)

# Apply the filter to your acceleration data
laser_gun_data <- laser_gun_data %>%
  mutate(
    butter_acceleration = filtfilt(butter_filter, raw_acceleration)
  ) %>%
  dplyr::filter(time > 0.414)

laser_gun_data %>%
  ggplot(aes(y = velocity, x = time)) +
  geom_point()

laser_gun_data %>%
  ggplot(aes(y = raw_acceleration, x = time)) +
  geom_point() +
  geom_line(aes(y = butter_acceleration), color = "red")

laser_gun_data %>%
  ggplot(aes(y = butter_acceleration, x = velocity)) +
  geom_point()

usethis::use_data(laser_gun_data, overwrite = TRUE)

# Tether
distance <- c(5, 10, 20, 30, 40)

velocity <- predict_velocity_at_distance(distance + 5, MSS = 10, MAC = 8)

m1 <- model_distance_velocity_DC(distance = distance, velocity = velocity)

split_distances <- seq(0, 50)
split_times <- create_timing_gates_splits(
  gates = split_distances,
  MSS = 10,
  MAC = 9,
  FD = 1,
  TC = 0.5,
  noise = 0.0000001
)

df <- tibble(
  distance = split_distances,
  time = split_times
)

# Simple model

simple_model <- model_distance_time_FD_TC(df$distance, df$time)

print(simple_model)
coef(simple_model)
plot(simple_model)

df$`.pred` = fitted(simple_model)

df %>%
ggplot(aes(x = time, y = distance)) +
  geom_point() +
  geom_line(aes(x = .pred), color = "red")

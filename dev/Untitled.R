gates <- c(5, 10, 20, 30, 40)
splits <- create_timing_gates_splits(8, 7, gates = gates, FD = 0, noise = 0.001)

m1 <- model_timing_gates_FD(gates, splits)

df <- data.frame(
  distance = gates,
  time = splits
)

model <- minpack.lm::nlsLM(
  time ~ predict_time_at_distance(distance + FD, MSS, MAC) - predict_time_at_distance(FD, MSS, MAC),
  data = df,
  start = list(MSS = 7, MAC = 7, FD = 0)
)
model
confint(model)

# --------------------------
df <- create_sprint_trace(8, 7, distance = c(5, 10, 20, 30, 40), FD = 0.5)
df
m1 <- model_tether_DC(df$distance, df$velocity)

plot(m1, "residuals")

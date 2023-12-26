# Model Timing Gates (with Flying Distance Correction and Time Correction)
df <- create_sprint_trace(MSS = 8, MAC = 6, distance = c(5, 10, 15, 20, 30, 40), TC = 0.2, FD = 0.5)
m1 <- model_timing_gates_FD_TC(
  distance = df$distance,
  time = df$time,
  # Control forwarded to minpack.lm::nlsLM()
  control = minpack.lm::nls.lm.control(maxiter = 1000))
m1
plot(m1)

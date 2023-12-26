df <- create_sprint_trace(MSS = 8, MAC = 6, distance = c(5, 10, 20, 30))
m1 <- model_timing_gates(distance = df$distance, time = df$time)
m1
plot(m1)




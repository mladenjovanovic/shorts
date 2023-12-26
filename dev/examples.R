split_distances <- c(10, 20, 30, 40, 50)
split_times <- create_timing_gates_splits(
  gates = split_distances,
  MSS = 10,
  MAC = 9,
  FD = 0,
  TC = 0,
  noise = 0.01
)
#'
# Simple model
simple_model <- model_timing_gates(split_distances, split_times)
confint(simple_model)

#' Create Timing Gates Splits
#'
#' This function is used to generate timing gates splits with predetermined parameters
#'
#' @param MSS,MAC  Numeric vectors. Model parameters
#' @param gates Numeric vectors. Distances of the timing gates
#' @param FD Numeric vector. Flying start distance. Default is 0
#' @param TC Numeric vector. Time-correction added to split times (e.g., reaction time). Default is 0
#' @param noise Numeric vector. SD of Gaussian noise added to the split times. Default is 0
#' @export
#' @seealso \code{\link{create_sprint_trace}}
#' @examples
#' create_timing_gates_splits(
#'   gates = c(10, 20, 30, 40, 50),
#'   MSS = 10,
#'   MAC = 9,
#'   FD = 0.5,
#'   TC = 0
#' )
create_timing_gates_splits <- function(MSS, MAC, gates = c(5, 10, 20, 30, 40), FD = 0, TC = 0, noise = 0) {
  df <- data.frame(
    gates = gates,
    MSS = MSS,
    MAC = MAC,
    FD = FD,
    TC = TC,
    noise = noise
  )

  true_distance <- df$gates + df$FD
  true_time <- predict_time_at_distance(true_distance, df$MSS, df$MAC)
  time_flying <- predict_time_at_distance(df$FD, df$MSS, df$MAC)
  time <- true_time - time_flying + df$TC
  time <- time + stats::rnorm(n = length(time), mean = 0, sd = df$noise)
  time
}

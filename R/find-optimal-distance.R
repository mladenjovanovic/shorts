#' Function that finds the distance at which the sprint, probe, or
#'     FV profile is optimal (i.e., equal to 100 perc)
#' @param ... Forwarded to selected \code{optimal_func}
#' @param optimal_func Selected profile optimization function. Default is \code{\link{optimal_FV}}
#' @param min,max Distance over which to find optimal profile distance
#' @return Distance
#' @export
#' @examples
#' MSS <- 10
#' MAC <- 8
#' bodymass <- 75
#'
#' fv <- make_FV_profile(MSS, MAC, bodymass)
#'
#' find_optimal_distance(
#'   F0 = fv$F0_poly,
#'   V0 = fv$V0_poly,
#'   bodymass = fv$bodymass,
#'   optimal_func = optimal_FV,
#'   method = "max"
#' )
#'
#' find_optimal_distance(
#'   MSS = MSS,
#'   MAC = MAC,
#'   optimal_func = optimal_MSS_MAC
#' )
#'
#' find_optimal_distance(
#'   MSS = MSS,
#'   MAC = MAC,
#'   optimal_func = probe_MSS_MAC
#' )
find_optimal_distance <- function(...,
                                  optimal_func = optimal_FV,
                                  min = 1,
                                  max = 60) {
  opt_func <- function(par) {
    (100 - optimal_func(distance = par, ...)[["profile_imb"]])^2
  }

  results <- stats::optim(
    par = min,
    fn = opt_func,
    method = "Brent",
    lower = min,
    upper = max
  )

  results$par
}

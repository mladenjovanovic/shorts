#' DynaSpeed Single Sprint Data
#'
#' DynaSpeed(TM) data collected for a single athlete (female, 177cm, 64kg) and a single sprint over 40m.
#'     Sampling frequency is 1,000Hz. Additional time and distance shift is added to the dataset
#'     to provide a sandbox for potential issues during the analysis
#' @format Data frame with 4 variables and 7,251 observations:
#' \describe{
#'    \item{time}{time in seconds}
#'    \item{distance}{Distance in meters}
#'    \item{velocity}{Smoothed velocity in meters per second}
#'    \item{raw_velocity}{Velocity in meters per second}
#' }
#' @author Håkan Andersson\cr
#'     The High-Performance Center\cr
#'     Växjö, Sweden\cr
#'     \email{hakan.andersson@@hpcsweden.com}
#' @usage data(dynaspeed)
"dynaspeed"

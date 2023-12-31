#' JB Morin Sample Dataset
#'
#' Sample radar gun data provided by Jean-Benoît Morin on his website. See
#'   \url{https://jbmorin.net/2017/12/13/a-spreadsheet-for-sprint-acceleration-force-velocity-power-profiling/}
#'    for more details.
#'
#' @details
#' This dataset represents a sample data provided by Jean-Benoît Morin on a single individual running
#'     approximately 35m from a stand still position that is measured with the radar gun. Individual's body
#'     mass is 75kg, height is 1.72m. Conditions of the run are the following: air temperature 25C,
#'     barometric pressure 760mmHg, wind velocity 0m/s.
#'
#'     The purpose of including this dataset in the package is to check the agreement of the model estimates
#'     with Jean-Benoît Morin Microsoft Excel spreadsheet.
#'
#' @format Data frame with 2 variables and 232 observations:
#' \describe{
#'    \item{time}{Time in seconds}
#'    \item{velocity}{Velocity in m/s}
#' }
#' @usage data(jb_morin)
#' @author Jean-Benoît Morin\cr
#'      Inter-university Laboratory of Human Movement Biology\cr
#'      Saint-Étienne, France
#'      \url{https://jbmorin.net/}
#'
#' @references
#'     Morin JB. 2017.A spreadsheet for Sprint acceleration Force-Velocity-Power profiling.
#'     Available at https://jbmorin.net/2017/12/13/a-spreadsheet-for-sprint-acceleration-force-velocity-power-profiling/
#'     (accessed October 27, 2020).
"jb_morin"

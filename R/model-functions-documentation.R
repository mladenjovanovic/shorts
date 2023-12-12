#' Model functions
#'
#' Family of functions that serve a purpose of estimating short sprint parameters
#'
#' @param time Numeric vector
#' @param velocity Numeric vector
#' @param distance Numeric vector
#' @param acceleration Numeric v
#' @param weights Numeric vector. Default is 1
#' @param CV Should cross-validation be used to estimate model fit? Default is \code{NULL}. Otherwise use integer
#'     indicating number of folds.
#' @param na.rm Logical. Default is FALSE
#' @param ... Forwarded to \code{\link[minpack.lm]{nlsLM}} function
#' @return List object with the following elements:
#'     \describe{
#'         \item{data}{Data frame used to estimate the sprint parameters}
#'         \item{model_info}{Extra information regarding model used}
#'         \item{model}{Model returned by the \code{\link[minpack.lm]{nlsLM}} function}
#'         \item{parameters}{List with the following estimated parameters:
#'             \code{MSS}, \code{TAU}, \code{MAC}, and \code{PMAX}}
#'         \item{correction}{List with additional model correcitons}
#'         \item{predictions}{Data frame with \code{.predictor}, \code{.observed},
#'             \code{.predicted}, and \code{.residual} columns}
#'         \item{model_fit}{List with multiple model fit estimators}
#'         \item{CV}{If cross-validation is performed, this will included the data as above, but
#'              for each fold}
#'         }
#' @references
#'     Samozino P. 2018. A Simple Method for Measuring Force, Velocity and Power Capabilities and Mechanical
#'         Effectiveness During Sprint Running. In: Morin J-B, Samozino P eds. Biomechanics of Training and Testing.
#'        Cham: Springer International Publishing, 237–267. DOI: 10.1007/978-3-319-05633-3_11.
#'
#' @name model_functions
NULL

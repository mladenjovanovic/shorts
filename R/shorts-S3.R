#' S3 method for extracting model parameters from \code{shorts_model} object
#' @param object \code{shorts_model} object
#' @param ... Extra arguments. Not used
#' @examples
#' split_distances <- c(10, 20, 30, 40, 50)
#' split_times <- create_timing_gates_splits(
#'   gates = split_distances,
#'   MSS = 10,
#'   MAC = 9,
#'   FD = 0.25,
#'   TC = 0
#' )
#'
#' # Simple model
#' simple_model <- model_distance_time(split_distances, split_times)
#' coef(simple_model)
#' @export
coef.shorts_model <- function(object, ...) {
  return(unlist(object$parameters))
}


#' S3 method for returning residuals of \code{shorts_model}
#'
#' @param object \code{shorts_model} object
#' @param ... Extra arguments. Not used
#' @examples
#' split_distances <- c(10, 20, 30, 40, 50)
#' split_times <- create_timing_gates_splits(
#'   gates = split_distances,
#'   MSS = 10,
#'   MAC = 9,
#'   FD = 0.25,
#'   TC = 0
#' )
#'
#' # Simple model
#' simple_model <- model_distance_time(split_distances, split_times)
#' residuals(simple_model)
#' @export
residuals.shorts_model <- function(object, ...) {
  object$predictions$.residual
}

#' S3 method for returning predictions of \code{shorts_model}
#'
#' @param object \code{shorts_model} object
#' @param ... Extra arguments. Not used
#' @examples
#' split_distances <- c(10, 20, 30, 40, 50)
#' split_times <- create_timing_gates_splits(
#'   gates = split_distances,
#'   MSS = 10,
#'   MAC = 9,
#'   FD = 0.25,
#'   TC = 0
#' )
#'
#' # Simple model
#' simple_model <- model_distance_time(split_distances, split_times)
#' fitted(simple_model)
#' @export
fitted.shorts_model <- function(object, ...) {
  object$predictions$.predicted
}

#' S3 method for making predictions using \code{shorts_model}
#'
#' @param object \code{shorts_model} object
#' @param ... Forwarded to generic \code{predict()} function
#' @examples
#' split_distances <- c(10, 20, 30, 40, 50)
#' split_times <- create_timing_gates_splits(
#'   gates = split_distances,
#'   MSS = 10,
#'   MAC = 9,
#'   FD = 0.25,
#'   TC = 0
#' )
#'
#' # Simple model
#' simple_model <- model_distance_time(split_distances, split_times)
#' predict(simple_model)
#' @export
predict.shorts_model <- function(object, ...) {
  stats::predict(object$model, ...)
}

#' S3 method for printing \code{shorts_model} object
#' @param x \code{shorts_model} object
#' @param ... Not used
#' @examples
#' split_distances <- c(10, 20, 30, 40, 50)
#' split_times <- create_timing_gates_splits(
#'   gates = split_distances,
#'   MSS = 10,
#'   MAC = 9,
#'   FD = 0.25,
#'   TC = 0
#' )
#'
#' # Simple model
#' simple_model <- model_distance_time(split_distances, split_times)
#' simple_model
#' @export
print.shorts_model <- function(x, ...) {
  cat("Estimated model parameters\n")
  cat("--------------------------\n")
  print(unlist(x$parameters))

  if(!is.null(x$corrections)) {
    cat("\nEstimated model corrections\n")
    cat("--------------------------\n")
    print(unlist(x$corrections))
  }

  cat("\nModel fit estimators\n")
  cat("--------------------\n")
  print(unlist(x$model_fit))

  if (!is.null(x$CV)) {
    cat("\n\nCross-Validation\n")
    cat("------------------------------\n")

    cat("Parameters:\n")
    print(x$CV$parameters)
    cat("\nTesting model fit estimators (overall):\n")
    print(unlist(x$CV$model_fit_overall))
  }
}


#' S3 method for providing summary for the \code{shorts_model} object
#' @param object \code{shorts_model} object
#' @param ... Not used
#' @examples
#' split_distances <- c(10, 20, 30, 40, 50)
#' split_times <- create_timing_gates_splits(
#'   gates = split_distances,
#'   MSS = 10,
#'   MAC = 9,
#'   FD = 0.25,
#'   TC = 0
#' )
#'
#' # Simple model
#' simple_model <- model_distance_time(split_distances, split_times)
#' summary(simple_model)
#' @export
summary.shorts_model <- function(object, ...) {
  summary(object$model)
}

#' S3 method for plotting \code{shorts_model} object
#' @param x \code{shorts_model} object
#' @param type Not used
#' @param ... Not used
#' @return \link[ggplot2]{ggplot} object
#' @examples
#' split_times <- data.frame(
#'   distance = c(5, 10, 20, 30, 35),
#'   time = c(1.20, 1.96, 3.36, 4.71, 5.35)
#' )
#'
#' # Simple model with time splits
#' simple_model <- with(
#'   split_times,
#'   model_distance_time(distance, time)
#' )
#'
#' coef(simple_model)
#' plot(simple_model)
#'
#' # Simple model with radar gun data
#' instant_velocity <- data.frame(
#'   time = c(0, 1, 2, 3, 4, 5, 6),
#'   velocity = c(0.00, 4.99, 6.43, 6.84, 6.95, 6.99, 7.00)
#' )
#'
#' radar_model <- with(
#'   instant_velocity,
#'   model_time_velocity(time, velocity)
#' )
#'
#' # sprint_model$parameters
#' coef(radar_model)
#' plot(radar_model)
#' @export
plot.shorts_model <- function(x, type = NULL, ...) {
  # ----------------
  Fitted <- NULL
  Residual <- NULL
  # ----------------

  df <- data.frame(
    Fitted = x$predictions$.predicted,
    Residual = x$predictions$.residual
  )

  ggplot2::ggplot(df, ggplot2::aes(x = Fitted, y = Residual)) +
    ggplot2::geom_point(alpha = 0.8, shape = 21)
}

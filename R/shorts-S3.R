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
#' simple_model <- model_timing_gates(split_distances, split_times)
#' coef(simple_model)
#' @export
coef.shorts_model <- function(object, ...) {
  # This return model parameters
  stats::coef(object$model, ...)
  # This only return sprint parameters
  # return(unlist(object$parameters))
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
#' simple_model <- model_timing_gates(split_distances, split_times)
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
#' simple_model <- model_timing_gates(split_distances, split_times)
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
#' simple_model <- model_timing_gates(split_distances, split_times)
#' predict(simple_model)
#' @export
predict.shorts_model <- function(object, ...) {
  stats::predict(object$model, ...)
}

#' S3 method for providing confidence intervals for the \code{shorts_model}
#'
#' @param object \code{shorts_model} object
#' @param ... Forwarded to generic \code{confint()} function
#' @examples
#' \dontrun{
#' split_distances <- c(10, 20, 30, 40, 50)
#' split_times <- create_timing_gates_splits(
#'   gates = split_distances,
#'   MSS = 10,
#'   MAC = 9,
#'   FD = 0,
#'   TC = 0,
#'   noise = 0.01
#' )
#'
#' # Simple model
#' simple_model <- model_timing_gates(split_distances, split_times)
#' confint(simple_model)
#' }
#' @export
confint.shorts_model <- function(object, ...) {
  stats::confint(object$model, ...)
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
#' simple_model <- model_timing_gates(split_distances, split_times)
#' simple_model
#' @export
print.shorts_model <- function(x, ...) {
  cat("Estimated model parameters\n")
  cat("--------------------------\n")
  print(unlist(x$parameters))

  if (!is.null(x$corrections)) {
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
#' simple_model <- model_timing_gates(split_distances, split_times)
#' summary(simple_model)
#' @export
summary.shorts_model <- function(object, ...) {
  summary(object$model)
}

#' S3 method for plotting \code{shorts_model} object
#' @param x \code{shorts_model} object
#' @param type Type of plot. Can be "model" (default), "kinematics-time",
#'     "kinematics-distance", or "residuals"
#' @param ... Not used
#' @return \code{\link[ggplot2]{ggplot}} object
#' @examples
#' # Simple model with radar gun data
#' instant_velocity <- data.frame(
#'   time = c(0, 1, 2, 3, 4, 5, 6),
#'   velocity = c(0.00, 4.99, 6.43, 6.84, 6.95, 6.99, 7.00)
#' )
#'
#' radar_model <- with(
#'   instant_velocity,
#'   model_radar_gun(time, velocity)
#' )
#'
#' plot(radar_model)
#' plot(radar_model, "kinematics-time")
#' plot(radar_model, "kinematics-distance")
#' plot(radar_model, "residuals")
#' @export
plot.shorts_model <- function(x, type = "model", ...) {

  # ----------------
  Fitted <- NULL
  Residual <- NULL
  .observed <- NULL
  .predicted <- NULL
  .predictor <- NULL
  distance <- NULL
  kinematic <- NULL
  time <- NULL
  value <- NULL
  # ----------------

  if (!(type %in% c("model", "kinematics-time", "kinematics-distance", "residuals"))) {
    stop("Wrong plot type. Please use either 'model', 'kinematics-time', 'kinematics-distance', or 'residuals'", call. = FALSE)
  }

  if (type == "model") {
    df <- data.frame(x$predictions)

    ggplot2::ggplot(df, ggplot2::aes(x = .predictor)) +
      ggplot2::geom_point(ggplot2::aes(y = .observed), alpha = 0.8, shape = 21) +
      ggplot2::geom_line(ggplot2::aes(y = .predicted), alpha = 0.8) +
      ggplot2::xlab(x$model_info$predictor) +
      ggplot2::ylab(x$model_info$target)
  } else if (type == "kinematics-time") {
    MSS <- x$parameters$MSS
    MAC <- x$parameters$MAC

    df <- data.frame(time = seq(0, 6, length.out = 1000))

    df$velocity <- predict_velocity_at_time(df$time, MSS, MAC)
    df$acceleration <- predict_acceleration_at_time(df$time, MSS, MAC)
    df$power <- df$velocity * df$acceleration

    df <- tidyr::pivot_longer(
      data = df,
      cols = c("velocity", "acceleration", "power"),
      names_to = "kinematic",
      values_to = "value"
    )

    df$kinematic <- factor(df$kinematic, levels = c("velocity", "acceleration", "power"))

    ggplot2::ggplot(df, ggplot2::aes(x = time)) +
      ggplot2::geom_line(ggplot2::aes(y = value, color = kinematic), alpha = 0.8) +
      ggplot2::ylab(NULL)
  } else if (type == "kinematics-distance") {
    MSS <- x$parameters$MSS
    MAC <- x$parameters$MAC

    df <- data.frame(distance = seq(0, 60, length.out = 1000))

    df$velocity <- predict_velocity_at_distance(df$distance, MSS, MAC)
    df$acceleration <- predict_acceleration_at_distance(df$distance, MSS, MAC)
    df$power <- df$velocity * df$acceleration

    df <- tidyr::pivot_longer(
      data = df,
      cols = c("velocity", "acceleration", "power"),
      names_to = "kinematic",
      values_to = "value"
    )

    df$kinematic <- factor(df$kinematic, levels = c("velocity", "acceleration", "power"))

    ggplot2::ggplot(df, ggplot2::aes(x = distance)) +
      ggplot2::geom_line(ggplot2::aes(y = value, color = kinematic), alpha = 0.8) +
      ggplot2::ylab(NULL)
  } else if (type == "residuals") {
    df <- data.frame(
      Fitted = x$predictions$.predicted,
      Residual = x$predictions$.residual
    )

    ggplot2::ggplot(df, ggplot2::aes(x = Fitted, y = Residual)) +
      ggplot2::geom_point(alpha = 0.7, shape = 21)
  }
}

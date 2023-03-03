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
  return(unlist(object$parameters))
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
  object$data[[4]]
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

  cat("\nModel fit estimators\n")
  cat("--------------------\n")
  print(unlist(x$model_fit))

  if (!is.null(x$CV)) {
    cat("\n\nCross-Validation\n")
    cat("------------------------------\n")

    cat("Parameters:\n")
    print(x$CV$parameters)
    cat("\nTesting model fit:\n")
    print(unlist(x$CV$model_fit))
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

#' S3 method for providing residuals for the \code{shorts_model} object
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
#' residuals(simple_model)
#' @export
residuals.shorts_model <- function(object, ...) {
  object$data[[2]] - object$data[[4]]
}

#' S3 method for printing \code{shorts_fv_profile} object
#' @param x \code{shorts_fv_profile} object
#' @param ... Not used
#' @examples
#' data("jb_morin")
#'
#' m1 <- model_radar_gun(time = jb_morin$time, velocity = jb_morin$velocity)
#'
#' fv_profile <- make_FV_profile(
#'   MSS = m1$parameters$MSS,
#'   MAC = m1$parameters$MAC,
#'   bodyheight = 1.72,
#'   bodymass = 120
#' )
#'
#' print(fv_profile)
#' @export
print.shorts_fv_profile <- function(x, ...) {
  cat("Estimated Force-Velocity Profile\n")
  cat("--------------------------------\n")
  print(unlist(x[-19]))
}


#' S3 method for plotting \code{shorts_fv_profile} object
#' @param x \code{shorts_fv_profile} object
#' @param type Type of plot. Options are "velocity" (default) and "time"
#' @param ... Not used
#' @return \link[ggplot2]{ggplot} object
#' @examples
#' data("jb_morin")
#'
#' m1 <- model_radar_gun(time = jb_morin$time, velocity = jb_morin$velocity)
#'
#' fv_profile <- make_FV_profile(
#'   MSS = m1$parameters$MSS,
#'   MAC = m1$parameters$MAC,
#'   bodyheight = 1.72,
#'   bodymass = 120
#' )
#'
#' plot(fv_profile)
#' plot(fv_profile, "time")
#' @export
plot.shorts_fv_profile <- function(x, type = "velocity", ...) {

  # +++++++++++++++++++++++++++++++++++++++++++
  # Code chunk for dealing with R CMD check note
  value <- NULL
  # +++++++++++++++++++++++++++++++++++++++++++

  if (!any(type %in% c("velocity", "time"))) {
    stop("Please use either 'time' or 'velocity' type plot.", call. = FALSE)
  }

  plot_data <- x$data

  if (type == "velocity") {
    plot_data <- plot_data[-1]
    plot_data <- tidyr::pivot_longer(
      plot_data,
      cols = -1,
      names_to = "variable",
      values_to = "value"
    )
  } else {
    plot_data <- tidyr::pivot_longer(
      plot_data,
      cols = -1,
      names_to = "variable",
      values_to = "value"
    )
  }
  colnames(plot_data)[1] <- "x"

  plot_data$variable <- factor(
    plot_data$variable,
    levels = c(
      "time", "velocity", "acceleration", "bodymass",
      "net_horizontal_force", "air_resistance", "horizontal_force", "horizontal_force_relative",
      "vertical_force", "resultant_force", "resultant_force_relative", "power",
      "relative_power", "RF", "force_angle"
    )
  )

  gg <- ggplot2::ggplot(plot_data, ggplot2::aes(x = x, y = value)) +
    ggplot2::geom_line(color = "black") +
    ggplot2::facet_wrap(~variable, scales = "free_y") +
    ggplot2::ylab(NULL) +
    ggplot2::xlab(type)

  gg
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
#'   model_timing_gates(distance, time)
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
#'   model_radar_gun(time, velocity)
#' )
#'
#' # sprint_model$parameters
#' coef(radar_model)
#' plot(radar_model)
#' @export
plot.shorts_model <- function(x, type = NULL, ...) {

  # +++++++++++++++++++++++++++++++++++++++++++
  # Code chunk for dealing with R CMD check note
  value <- NULL
  variable <- NULL
  # +++++++++++++++++++++++++++++++++++++++++++

  # Check which models is done?
  model_type <- names(x$data[1])

  if (model_type == "distance") {
    # This is time-split model
    df <- data.frame(
      distance = seq(0, max(x$data$distance), length.out = 1000)
    )

    df$velocity <- predict_velocity_at_distance(
      distance = df$distance,
      MSS = x$parameters$MSS,
      MAC = x$parameters$MAC
    )

    df$acceleration <- predict_acceleration_at_distance(
      distance = df$distance,
      MSS = x$parameters$MSS,
      MAC = x$parameters$MAC
    )
    df$power <- df$velocity * df$acceleration
  } else {
    # This is radar model
    df <- data.frame(
      time = seq(0, max(x$data$time), length.out = 1000)
    )

    df$velocity <- predict_velocity_at_time(
      time = df$time,
      MSS = x$parameters$MSS,
      MAC = x$parameters$MAC
    )

    df$acceleration <- predict_acceleration_at_time(
      time = df$time,
      MSS = x$parameters$MSS,
      MAC = x$parameters$MAC
    )

    df$power <- df$velocity * df$acceleration
  }

  colnames(df)[1] <- "x"

  plot_data <- tidyr::pivot_longer(df, cols = -1, names_to = "variable", values_to = "value")
  plot_data$variable <- factor(
    plot_data$variable,
    levels = c("acceleration", "velocity", "power")
  )

  gg <- ggplot2::ggplot(plot_data, ggplot2::aes(x = x, y = value, color = variable)) +
    ggplot2::geom_line(alpha = 0.8) +
    ggplot2::xlab(model_type) +
    ggplot2::ylab(NULL)
  gg
}

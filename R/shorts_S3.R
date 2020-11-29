#' S3 method for extracting model parameters from \code{shorts_model} object
#' @param object \code{shorts_model} object
#' @param ... Extra arguments. Not used
#' @examples
#' split_times <- data.frame(
#'   distance = c(5, 10, 20, 30, 35),
#'   time = c(1.20, 1.96, 3.36, 4.71, 5.35)
#' )
#'
#' # Simple model
#' simple_model <- with(
#'   split_times,
#'   model_using_splits(distance, time)
#' )
#'
#' # unlist(simple_model$parameters)
#' coef(simple_model)
#' @export
coef.shorts_model <- function(object, ...) {
  return(unlist(object$parameters))
}

#' S3 method for extracting model parameters from \code{shorts_mixed_model} object
#' @param object \code{shorts_mixed_model} object
#' @param ... Extra arguments. Not used
#' @examples
#' data("split_times")
#'
#' mixed_model <- mixed_model_using_splits(
#'   data = split_times,
#'   distance = "distance",
#'   time = "time",
#'   athlete = "athlete"
#' )
#'
#' # mixed_model$parameters
#' coef(mixed_model)
#' @export
coef.shorts_mixed_model <- function(object, ...) {
  object$parameters$fixed <- unlist(object$parameters$fixed)
  return(object$parameters)
}

#' S3 method for returning predictions of \code{shorts_model}
#'
#' @param object \code{shorts_model} object
#' @param ... Extra arguments. Not used
#' @examples
#' split_times <- data.frame(
#'   distance = c(5, 10, 20, 30, 35),
#'   time = c(1.20, 1.96, 3.36, 4.71, 5.35)
#' )
#'
#' # Simple model
#' simple_model <- with(
#'   split_times,
#'   model_using_splits(distance, time)
#' )
#'
#' predict(simple_model)
#' @export
predict.shorts_model <- function(object, ...) {
  object$data[[4]]
}

#' S3 method for returning predictions of \code{shorts_mixed_model}
#'
#' @param object \code{shorts_mixed_model} object
#' @param ... Extra arguments. Not used
#' @examples
#' data("split_times")
#'
#' mixed_model <- mixed_model_using_splits(
#'   data = split_times,
#'   distance = "distance",
#'   time = "time",
#'   athlete = "athlete"
#' )
#'
#' predict(mixed_model)
#' @export
predict.shorts_mixed_model <- function(object, ...) {
  as.numeric(object$data[[4]])
}

#' S3 method for printing \code{shorts_model} object
#' @param x \code{shorts_model} object
#' @param ... Not used
#' @examples
#' split_times <- data.frame(
#'   distance = c(5, 10, 20, 30, 35),
#'   time = c(1.20, 1.96, 3.36, 4.71, 5.35)
#' )
#'
#' # Simple model
#' simple_model <- with(
#'   split_times,
#'   model_using_splits(distance, time)
#' )
#'
#' print(simple_model)
#' @export
print.shorts_model <- function(x, ...) {
  cat("Estimated model parameters\n")
  cat("--------------------------\n")
  print(unlist(x$parameters))

  cat("\nModel fit estimators\n")
  cat("--------------------\n")
  print(unlist(x$model_fit))

  if(!is.null(x$LOOCV)){
    cat("\n\nLeave-One-Out Cross-Validation\n")
    cat("------------------------------\n")

    cat("Parameters:\n")
    print(x$LOOCV$parameters)
    cat("\nModel fit:\n")
    print(unlist(x$LOOCV$model_fit))
  }
}


#' S3 method for providing summary for the \code{shorts_model} object
#' @param object \code{shorts_model} object
#' @param ... Not used
#' @examples
#' split_times <- data.frame(
#'   distance = c(5, 10, 20, 30, 35),
#'   time = c(1.20, 1.96, 3.36, 4.71, 5.35)
#' )
#'
#' # Simple model
#' simple_model <- with(
#'   split_times,
#'   model_using_splits(distance, time)
#' )
#'
#' summary(simple_model)
#' @export
summary.shorts_model <- function(object, ...) {
  summary(object$model)
}


#' S3 method for printing \code{shorts_mixed_model} object
#' @param x \code{shorts_mixed_model} object
#' @param ... Not used
#' @examples
#' data("split_times")
#'
#' mixed_model <- mixed_model_using_splits(
#'   data = split_times,
#'   distance = "distance",
#'   time = "time",
#'   athlete = "athlete"
#' )
#'
#' print(mixed_model)
#' @export
print.shorts_mixed_model <- function(x, ...) {
  cat("Estimated fixed model parameters\n")
  cat("--------------------------------\n")
  print(unlist(x$parameters$fixed))

  cat("\nEstimated random model parameters\n")
  cat("----------------------------------\n")
  print(x$parameters$random)

  cat("\nModel fit estimators\n")
  cat("--------------------\n")
  print(unlist(x$model_fit))

  if(!is.null(x$LOOCV)){
    cat("\n\nLeave-One-Out Cross-Validation\n")
    cat("------------------------------\n")

    cat("Fixed parameters:\n")
    print(x$LOOCV$parameters$fixed)
    cat("\nModel fit:\n")
    print(unlist(x$LOOCV$model_fit))
  }
}

#' S3 method for providing summary for the \code{shorts_mixed_model} object
#' @param object \code{shorts_mixed_model} object
#' @param ... Not used
#' @examples
#' data("split_times")
#'
#' mixed_model <- mixed_model_using_splits(
#'   data = split_times,
#'   distance = "distance",
#'   time = "time",
#'   athlete = "athlete"
#' )
#'
#' summary(mixed_model)
#' @export
summary.shorts_mixed_model <- function(object, ...) {
  summary(object$model)
}


#' S3 method for providing residuals for the \code{shorts_model} object
#' @param object \code{shorts_model} object
#' @param ... Not used
#' @examples
#' split_times <- data.frame(
#'   distance = c(5, 10, 20, 30, 35),
#'   time = c(1.20, 1.96, 3.36, 4.71, 5.35)
#' )
#'
#' # Simple model
#' simple_model <- with(
#'   split_times,
#'   model_using_splits(distance, time)
#' )
#'
#' residuals(simple_model)
#' @export
residuals.shorts_model <- function(object, ...) {
  object$data[[4]] - object$data[[2]]
}


#' S3 method for providing residuals for the \code{shorts_mixed_model} object
#' @param object \code{shorts_mixed_model} object
#' @param ... Not used
#' @examples
#' data("split_times")
#'
#' mixed_model <- mixed_model_using_splits(
#'   data = split_times,
#'   distance = "distance",
#'   time = "time",
#'   athlete = "athlete"
#' )
#'
#' residuals(mixed_model)
#' @export
residuals.shorts_mixed_model <- function(object, ...) {
  object$data[[4]] - object$data[[3]]
}

#' S3 method for printing \code{shorts_fv_profile} object
#' @param x \code{shorts_fv_profile} object
#' @param ... Not used
#' @examples
#' data("jb_morin")
#'
#' m1 <- model_using_radar_with_time_correction(time = jb_morin$time, velocity = jb_morin$velocity)
#'
#' fv_profile <- get_FV_profile(
#'   MSS = m1$parameters$MSS,
#'   TAU = m1$parameters$TAU,
#'   bodyheight = 1.72,
#'   bodymass = 120)
#'
#' print(fv_profile)
#' @export
print.shorts_fv_profile <- function(x, ...) {
  cat("Estimated Force-Velocity Profile\n")
  cat("--------------------------\n")
  print(unlist(x[-13]))
}


#' S3 method for plotting \code{shorts_fv_profile} object
#' @param x \code{shorts_fv_profile} object
#' @param type Type of plot. Options are "velocity" (default) and "time"
#' @param ... Not used
#' @return \link[ggplot2]{ggplot} object
#' @examples
#' data("jb_morin")
#'
#' m1 <- model_using_radar_with_time_correction(time = jb_morin$time, velocity = jb_morin$velocity)
#'
#' fv_profile <- get_FV_profile(
#'   MSS = m1$parameters$MSS,
#'   TAU = m1$parameters$TAU,
#'   bodyheight = 1.72,
#'   bodymass = 120)
#'
#' plot(fv_profile)
#' plot(fv_profile, "time")
#' @export
plot.shorts_fv_profile <- function(x, type = "velocity", ...) {

  # +++++++++++++++++++++++++++++++++++++++++++
  # Code chunk for dealing with R CMD check note
  value <- NULL
  # +++++++++++++++++++++++++++++++++++++++++++

  if(!any(type %in% c("velocity", "time"))) {
    stop("Please use either 'force' and 'velocity' type plot.", call. = FALSE)
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
    ))

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
#'   model_using_splits(distance, time)
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
#'   model_using_radar(time, velocity)
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
      distance = seq(0, max(x$data$distance), length.out = 100)
    )

    df$velocity <- predict_velocity_at_distance(
      distance = df$distance,
      MSS = x$parameters$MSS,
      TAU = x$parameters$TAU,
      time_correction = x$parameters$time_correction,
      distance_correction = x$parameters$distance_correction
    )

    df$acceleration <- predict_acceleration_at_distance(
      distance = df$distance,
      MSS = x$parameters$MSS,
      TAU = x$parameters$TAU,
      time_correction = x$parameters$time_correction,
      distance_correction = x$parameters$distance_correction
    )
    df$power <- df$velocity * df$acceleration

  } else {
    # This is radar model
    df <- data.frame(
      time = seq(0, max(x$data$time), length.out = 100)
    )

    df$velocity <- predict_velocity_at_time(
      time = df$time,
      MSS = x$parameters$MSS,
      TAU = x$parameters$TAU,
      time_correction = x$parameters$time_correction
    )

    df$acceleration <- predict_acceleration_at_time(
      time = df$time,
      MSS = x$parameters$MSS,
      TAU = x$parameters$TAU,
      time_correction = x$parameters$time_correction
    )

    df$power <- df$velocity * df$acceleration
  }

  colnames(df)[1] <- "x"

  plot_data <- tidyr::pivot_longer(df, cols = -1, names_to = "variable", values_to = "value")
  plot_data$variable <- factor(
    plot_data$variable,
    levels = c("acceleration", "velocity", "power"))

  gg <- ggplot2::ggplot(plot_data, ggplot2::aes(x = x, y = value, color = variable)) +
    ggplot2::geom_line(alpha = 0.8) +
    ggplot2::xlab(model_type) +
    ggplot2::ylab(NULL)
  gg
}


#' S3 method for plotting \code{shorts_mixed_model} object
#' @param x \code{shorts_mixed_model} object
#' @param type Not used
#' @param ... Not used
#' @return \link[ggplot2]{ggplot} object
#' @examples
#' # Split times
#' data("split_times")
#' mixed_model_splits <- mixed_model_using_splits(
#'   data = split_times,
#'   distance = "distance",
#'   time = "time",
#'   athlete = "athlete"
#' )
#'
#' plot(mixed_model_splits)
#'
#' # Radar gun data
#' data("radar_gun_data")
#' mixed_model_radar <- mixed_model_using_radar(radar_gun_data, "time", "velocity", "athlete")
#'
#' plot(mixed_model_radar)
#' @export
plot.shorts_mixed_model <- function(x, type = NULL, ...) {

  # +++++++++++++++++++++++++++++++++++++++++++
  # Code chunk for dealing with R CMD check note
  value <- NULL
  variable <- NULL
  # +++++++++++++++++++++++++++++++++++++++++++

  # Check which models is done?
  model_type <- names(x$data[2])

  if (model_type == "distance") {
    # This is time-split model
    df <- tidyr::expand_grid(
      x$parameters$random,
      distance = seq(0, max(x$data$distance), length.out = 100)
    )

    df$velocity <- predict_velocity_at_distance(
      distance = df$distance,
      MSS = df$MSS,
      TAU = df$TAU,
      time_correction = df$time_correction,
      distance_correction = df$distance_correction
    )

    df$acceleration <- predict_acceleration_at_distance(
      distance = df$distance,
      MSS = df$MSS,
      TAU = df$TAU,
      time_correction = df$time_correction,
      distance_correction = df$distance_correction
    )

    df$power <- df$velocity * df$acceleration

  } else {
    # This is radar model
    df <- tidyr::expand_grid(
      x$parameters$random,
      time = seq(0, max(x$data$time), length.out = 100)
    )

    df$velocity <- predict_velocity_at_time(
      time = df$time,
      MSS = df$MSS,
      TAU = df$TAU,
      time_correction = df$time_correction
    )

    df$acceleration <- predict_acceleration_at_time(
      time = df$time,
      MSS = df$MSS,
      TAU = df$TAU,
      time_correction = df$time_correction
    )

    df$power <- df$velocity * df$acceleration
  }

  colnames(df)[8] <- "x"

  plot_data <- tidyr::pivot_longer(df, cols = -(1:8), names_to = "variable", values_to = "value")
  plot_data$variable <- factor(
    plot_data$variable,
    levels = c("acceleration", "velocity", "power"))

  gg <- ggplot2::ggplot(plot_data, ggplot2::aes(x = x, y = value, color = variable)) +
    ggplot2::geom_line(alpha = 0.8) +
    ggplot2::facet_wrap(~athlete) +
    ggplot2::xlab(model_type) +
    ggplot2::ylab(NULL)
  gg
}

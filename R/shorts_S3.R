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

#' S3 method for predicting kinematics from \code{shorts_model} object
#'
#' Predicts sprint kinematics for 0-6sec (100hz) which include distance,
#'     velocity, acceleration, and relative power
#'
#' @param object \code{shorts_model} object
#' @param ... Extra arguments. Not used
#' @return Data frame
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
#' head(predict(simple_model))
#' @export
predict.shorts_model <- function(object, ...) {
  df <- data.frame(
    time = seq(0, 6, length.out = 6 * 100 + 1)
  )

  df$distance <- predict_distance_at_time(
    time = df$time,
    MSS = object$parameters$MSS,
    TAU = object$parameters$TAU,
    time_correction = object$parameters$time_correction,
    distance_correction = object$parameters$distance_correction
  )

  df$velocity <- predict_velocity_at_time(
    time = df$time,
    MSS = object$parameters$MSS,
    TAU = object$parameters$TAU,
    time_correction = object$parameters$time_correction
  )

  df$acceleration <- predict_acceleration_at_time(
    time = df$time,
    MSS = object$parameters$MSS,
    TAU = object$parameters$TAU,
    time_correction = object$parameters$time_correction
  )

  df$power <- predict_relative_power_at_time(
    time = df$time,
    MSS = object$parameters$MSS,
    TAU = object$parameters$TAU,
    time_correction = object$parameters$time_correction
  )

  return(df)
}

#' S3 method for predicting kinematics from \code{shorts_mixed_model} object
#'
#' Predicts sprint kinematics for 0-6sec (100hz) which include distance,
#'     velocity, acceleration, and relative power. This is done for each
#'     athlete (i.e. level) inside the model
#'
#' @param object \code{shorts_model} object
#' @param ... Extra arguments. Not used
#' @return Data frame
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
#' head(predict(mixed_model))
#' @export
predict.shorts_mixed_model <- function(object, ...) {
  df <- expand.grid(
    time = seq(0, 6, length.out = 6 * 100 + 1),
    athlete = object$parameters$random$athlete
  )

  df <- merge(df, object$parameters$random, by = "athlete", all = TRUE)

  df$distance <- predict_distance_at_time(
    time = df$time,
    MSS = df$MSS,
    TAU = df$TAU,
    time_correction = df$time_correction,
    distance_correction = df$distance_correction
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

  df$power <- predict_relative_power_at_time(
    time = df$time,
    MSS = df$MSS,
    TAU = df$TAU,
    time_correction = df$time_correction
  )

  df <- df[c("athlete", "time", "distance", "velocity", "acceleration", "power")]
  return(df)
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

  cat("\nEstimated frandom model parameters\n")
  cat("----------------------------------\n")
  print(x$parameters$random)

  cat("\nModel fit estimators\n")
  cat("--------------------\n")
  print(unlist(x$model_fit))
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

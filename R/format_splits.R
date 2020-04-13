#' Format Split Data
#'
#' Function formats split data and calculates split distances, split times and average split velocity
#'
#' @param distance Numeric vector
#' @param time Numeric vector
#' @return Data frame with the following columns:
#'     \describe{
#'          \item{split}{Split number}
#'          \item{split_distance_start}{Distance at which split starts}
#'          \item{split_distance_stop}{Distance at which split ends}
#'          \item{split_distance}{Split distance}
#'          \item{split_time_start}{Time at which distance starts}
#'          \item{split_time_stop}{Time at which distance ends}
#'          \item{split_time}{Split time}
#'          \item{split_mean_velocity}{Mean velocity over split distance}
#'     }
#' @export
#' @examples
#' data('split_times')
#'
#' john_data <- split_times[split_times$athlete == "John", ]
#'
#' format_splits(john_data$distance, john_data$time)
#'
format_splits <- function(distance, time) {

  df <- data.frame(distance = distance, time = time)

  # Function for creating laf
  create_lag <- function(x) {
    x_lag <- x
    for( i in seq(1, length(x) - 1)){
      x_lag[i+1] <- x[i]
    }

    x_lag[1] <- 0
    return(x_lag)
  }

  # =============================================================================
  # Order df based on distance
  df <- df[order(df$distance),]

  # --------------------
  # Create lag variables

  # Distance
  df$split_distance_start <- create_lag(df$distance)
  df$split_distance_stop <-df$distance
  df$split_distance <- df$split_distance_stop - df$split_distance_start
  # Time
  df$split_time_start <- create_lag(df$time)
  df$split_time_stop <-df$time
  df$split_time <- df$split_time_stop - df$split_time_start
  # mean velocity
  df$split_mean_velocity <- df$split_distance / df$split_time

  # Split number
  df$split <- seq(1, nrow(df))

  # Order columns
  df <- df[c(
    "split",
    "split_distance_start",
    "split_distance_stop",
    "split_distance",
    "split_time_start",
    "split_time_stop",
    "split_time",
    "split_mean_velocity"
  )]

return(df)
}


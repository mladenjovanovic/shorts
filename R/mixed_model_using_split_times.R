#' Mixed Model Using Split Times
#'
#' This function models the sprint split times using mono-exponential equation and non-linear
#'     mixed model using \code{\link[nlme]{nlme}} to estimate fixed and random maximum sprinting speed (\code{MSS})
#'     and relative acceleration (\code{TAU}) parameters. In mixed model, fixed and random effects are estimated for
#'     \code{MSS} and \code{TAU} parameters using \code{athlete} as levels. \code{time} is used as target or outcome
#'     variable, and \code{distance} as predictor.
#' @param data Data frame
#' @param distance Character string. Name of the column in \code{data}
#' @param time Character string. Name of the column in \code{data}
#' @param time_correction Numeric vector. Used to correct for different starting techniques.  This correction is
#'     done by adding \code{time_correction} to \code{time}. Default is 0. See more in Haugen et al. (2018)
#' @param athlete Character string. Name of the column in \code{data}. Used as levels in the \code{\link[nlme]{nlme}}
#' @param na.rm Logical. Default is FALSE
#' @param ... Forwarded to \code{\link[nlme]{nlme}} function
#' @return List object with the following elements:
#'     \describe{
#'         \item{parameters}{List with two data frames: \code{fixed} and \code{random} containing the following
#'             estimated parameters: \code{MSS}, \code{TAU}, \code{MAC}, and \code{PMAX}}
#'         \item{model_fit}{List with the following components:
#'             \code{RSE}, \code{R_squared}, \code{minErr}, \code{maxErr}, and \code{RMSE}}
#'         \item{model}{Model returned by the \code{\link[nlme]{nlme}} function}
#'         \item{data}{Data frame used to estimate the sprint parameters, consisting of \code{athlete}, \code{distance},
#'             \code{time}, and \code{time_correction}, \code{corrected_time}, and \code{pred_time} columns}
#'         }
#' @export
#' @references
#'     Haugen TA, Tønnessen E, Seiler SK. 2012. The Difference Is in the Start: Impact of Timing and Start
#'         Procedure on Sprint Running Performance: Journal of Strength and Conditioning Research 26:473–479.
#'         DOI: 10.1519/JSC.0b013e318226030b.
#' @examples
#' data("split_times")
#'
#' mixed_model <- mixed_model_using_split_times(split_times, "distance", "time", "athlete")
#' mixed_model$parameters
mixed_model_using_split_times <- function(data,
                                          distance,
                                          time,
                                          athlete,
                                          time_correction = 0,
                                          # weights = rep(1, nrow(data)),
                                          na.rm = FALSE,
                                          ...) {

  # Combine to DF
  df <- data.frame(
    athlete = data[[athlete]],
    distance = data[[distance]],
    time = data[[time]],
    time_correction = time_correction,
    corrected_time = data[[time]] + time_correction#,
    #weights = weights
  )

  # Remove NAs
  if (na.rm) {
    df <- stats::na.omit(df)
  }

  # Create mixed model
  mixed_model <- nlme::nlme(
    corrected_time~TAU*I(LambertW::W(-exp(1)^(-distance/(MSS*TAU)-1))) + distance / MSS + TAU,
    data = df,
    fixed = MSS + TAU~1,
    random = MSS + TAU~1,
    groups = ~athlete,
    # weights = ~weights,
    start = c(MSS=7,TAU=0.8),
    ...
  )

  # Pull estimates
  fixed_effects <- nlme::fixed.effects(mixed_model)
  random_effects <- nlme::random.effects(mixed_model)

  # Fixed effects
  fixed_effects <- data.frame(t(fixed_effects))
  fixed_effects$MAC <- fixed_effects$MSS / fixed_effects$TAU
  fixed_effects$PMAX <- (fixed_effects$MSS * fixed_effects$MAC) / 4


  random_effects$athlete <- rownames(random_effects)
  random_effects$MSS <- random_effects$MSS + fixed_effects$MSS
  random_effects$TAU <- random_effects$TAU + fixed_effects$TAU
  rownames(random_effects) <- NULL
  random_effects <- random_effects[c("athlete", "MSS", "TAU")]
  random_effects$MAC <- random_effects$MSS / random_effects$TAU
  random_effects$PMAX <- (random_effects$MSS * random_effects$MAC) / 4

  # Model fit

  pred_time <- stats::predict(mixed_model, newdata = df)
  pred_time <- pred_time - time_correction

  RSE <- summary(mixed_model)$sigma
  R_squared <- stats::cor(df$time, pred_time)^2
  minErr <- min(pred_time - df$time)
  maxErr <- max(pred_time - df$time)
  RMSE <- sqrt(mean((pred_time -df$time)^2))

  # Add predicted time to df
  df$pred_time <- pred_time

  return(list(
    parameters = list(
      fixed = fixed_effects,
      random = random_effects),
    model_fit = list(
      RSE = RSE,
      R_squared = R_squared,
      minErr = minErr,
      maxErr = maxErr,
      RMSE = RMSE
    ),
    model = mixed_model,
    data = df
  ))

}

#' Model Using Split Times
#'
#' This function models the sprint split times using mono-exponential equation that estimates
#'     maximum sprinting speed (\code{MSS}) and relative acceleration (\code{TAU}). \code{time} is used as target or outcome
#'     variable, and \code{distance} as predictor.
#' @param distance Numeric vector
#' @param time Numeric vector
#' @param weights Numeric vector. Default is vector of 1
#'     This is used to give more weight to particular observations. For example, use \code{1\\distance} to give
#'     more weight to observations from shorter distances.
#' @param na.rm Logical. Default is FALSE
#' @return List object with the following elements:
#'     \describe{
#'         \item{parameters}{List with the following estimated parameters:
#'             \code{MSS}, \code{TAU}, \code{MAC}, and \code{PMAX}}
#'         \item{model_fit}{List with the following components:
#'             \code{RSE}, \code{R_squared}, \code{minErr}, \code{maxErr},
#'             \code{maxAbsErr}, and \code{RMSE}}
#'         \item{model}{Model returned by the \code{\link[stats]{nls}} function}
#'         \item{data}{Data frame used to estimate the sprint parameters, consisting of \code{distance},
#'             \code{time}, \code{weights} and \code{pred_time} columns}
#'         }
#' @export
#' @examples
#' split_times <- data.frame(
#' distance = c(5, 10, 20, 30, 35),
#' time = c(1.21, 1.99, 3.38, 4.71, 5.30)
#' )
#'
#' sprint_model <- with(
#'   split_times,
#'   model_using_split_times(distance, time)
#' )
#'
#' sprint_model$parameters
model_using_split_times <- function(distance,
                                    time,
                                    weights = 1,
                                    na.rm = FALSE) {


  # Put data into data frame
  df <- data.frame(
    distance = distance,
    time = time,
    weights = weights
    )

  # Remove NAs
  if (na.rm) {
    df <- stats::na.omit(df)
  }

  # Non-linear model
  speed_mod <- stats::nls(
    time ~ TAU * I(LambertW::W(-exp(1)^(-distance / (MSS * TAU) - 1))) + distance / MSS + TAU,
    data = df,
    start = list(MSS = 7, TAU = 0.8),
    weights = df$weights
  )

  # Maximal Sprinting Speed
  MSS <- stats::coef(speed_mod)[[1]]
  TAU <- stats::coef(speed_mod)[[2]]

  # Maximal acceleration
  MAC <- MSS / TAU

  # Maximal Power (relative)
  PMAX <- (MSS * MAC) / 4

  # Model fit
  pred_time <- TAU * I(LambertW::W(-exp(1)^(-df$distance / (MSS * TAU) - 1))) + df$distance / MSS + TAU

  RSE <- summary(speed_mod)$sigma
  R_squared <- stats::cor(df$time, pred_time)^2
  minErr <- min(pred_time - df$time)
  maxErr <- max(pred_time - df$time)
  maxAbsErr <- max(abs(pred_time - df$time))
  RMSE <- sqrt(mean((pred_time - df$time)^2))

  # Add predicted time to df
  df$pred_time <- pred_time

  # Return object
  return(list(
    parameters = list(
      MSS = MSS,
      TAU = TAU,
      MAC = MAC,
      PMAX = PMAX),
    model_fit = list(
      RSE = RSE,
      R_squared = R_squared,
      minErr = minErr,
      maxErr = maxErr,
      maxAbsErr = maxAbsErr,
      RMSE = RMSE
    ),
    model = speed_mod,
    data = df
  ))
}


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
#' @param athlete Character string. Name of the column in \code{data}. Used as levels in the \code{\link[nlme]{nlme}}
#' @param na.rm Logical. Default is FALSE
#' @return List object with the following elements:
#'     \describe{
#'         \item{parameters}{List with two data frames: \code{fixed} and \code{random} containing the following
#'             estimated parameters: \code{MSS}, \code{TAU}, \code{MAC}, and \code{PMAX}}
#'         \item{model_fit}{List with the following components:
#'             \code{RSE}, \code{R_squared}, \code{minErr}, \code{maxErr},
#'             \code{maxAbsErr}, and \code{RMSE}}
#'         \item{model}{Model returned by the \code{\link[nlme]{nlme}} function}
#'         \item{data}{Data frame used to estimate the sprint parameters, consisting of \code{athlete}, \code{distance},
#'             \code{time}, and  \code{pred_time} columns}
#'         }
#' @export
#' @examples
#' data("split_times")
#'
#' mixed_model <- mixed_model_using_split_times(split_times, "distance", "time", "athlete")
#' mixed_model$parameters
mixed_model_using_split_times <- function(data,
                                          distance,
                                          time,
                                          athlete,
                                    # weights = rep(1, nrow(data)),
                                    na.rm = FALSE) {
  # Combine to DF
  df <- data.frame(
    athlete = data[[athlete]],
    distance = data[[distance]],
    time = data[[time]]#,
    #weights = weights
  )

  # Remove NAs
  if (na.rm) {
    df <- stats::na.omit(df)
  }

  # Create mixed model
  mixed_model <- nlme::nlme(
    time~TAU*I(LambertW::W(-exp(1)^(-distance/(MSS*TAU)-1))) + distance / MSS + TAU,
    data = df,
    fixed = MSS + TAU~1,
    random = MSS + TAU~1,
    groups = ~athlete,
    # weights = ~weights,
    start = c(MSS=7,TAU=0.8)
  )

  # Pull estimates
  fixed_effects <- nlme::fixed.effects(mixed_model)
  random_effects <- nlme::random.effects(mixed_model)

  # Fixed effects
  fixed_effects <- data.frame(t(fixed_effects))
  fixed_effects$MAC <- fixed_effects$MSS / fixed_effects$TAU
  fixed_effects$PMAX <- (fixed_effects$MSS * fixed_effects$TAU) / 4


  random_effects$athlete <- rownames(random_effects)
  random_effects$MSS <- random_effects$MSS + fixed_effects$MSS
  random_effects$TAU <- random_effects$TAU + fixed_effects$TAU
  rownames(random_effects) <- NULL
  random_effects <- random_effects[c("athlete", "MSS", "TAU")]
  random_effects$MAC <- random_effects$MSS / random_effects$TAU
  random_effects$PMAX <- (random_effects$MSS * random_effects$TAU) / 4

   # Model fit

  pred_time <- stats::predict(mixed_model, newdata = df)
  RSE <- summary(mixed_model)$sigma
  R_squared <- stats::cor(df$time, pred_time)^2
  minErr <- min(pred_time - df$time)
  maxErr <- max(pred_time - df$time)
  maxAbsErr <- max(abs(pred_time - df$time))
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
     maxAbsErr = maxAbsErr,
     RMSE = RMSE
   ),
   model = mixed_model,
   data = df
 ))

}

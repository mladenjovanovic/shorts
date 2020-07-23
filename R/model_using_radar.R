#' Model Using Instantaneous Velocity or Radar Gun
#'
#' This function models the sprint instantaneous velocity using mono-exponential equation that estimates
#'     maximum sprinting speed (\code{MSS}) and relative acceleration (\code{TAU}). \code{velocity} is used as target or outcome
#'     variable, and \code{time} as predictor.
#'
#' @param time Numeric vector
#' @param velocity Numeric vector
#' @param time_correction Numeric vector. Used to filter out noisy data from the radar gun. This correction is
#'     done by adding \code{time_correction} to \code{time}. Default is 0. See more in Samozino (2018)
#' @param weights Numeric vector. Default is 1
#' @param na.rm Logical. Default is FALSE
#' @return List object with the following elements:
#'     \describe{
#'         \item{parameters}{List with the following estimated parameters:
#'             \code{MSS}, \code{TAU}, \code{MAC}, and \code{PMAX}}
#'         \item{model_fit}{List with the following components:
#'             \code{RSE}, \code{R_squared}, \code{minErr}, \code{maxErr}, and \code{RMSE}}
#'         \item{model}{Model returned by the \code{\link[stats]{nls}} function}
#'         \item{data}{Data frame used to estimate the sprint parameters, consisting of \code{time},
#'            \code{velocity}, \code{weights}, and \code{pred_velocity} columns}
#'         }
#' @export
#' @references
#'     Samozino P. 2018. A Simple Method for Measuring Force, Velocity and Power Capabilities and Mechanical
#'         Effectiveness During Sprint Running. In: Morin J-B, Samozino P eds. Biomechanics of Training and Testing.
#'        Cham: Springer International Publishing, 237–267. DOI: 10.1007/978-3-319-05633-3_11.
#'
#' @examples
#' instant_velocity <- data.frame(
#'   time = c(0, 1, 2, 3, 4, 5, 6),
#'   velocity = c(0.00, 4.99, 6.43, 6.84, 6.95, 6.99, 7.00)
#' )
#'
#' sprint_model <- with(
#'   instant_velocity,
#'   model_using_radar(time, velocity)
#' )
#'
#' sprint_model$parameters
model_using_radar <- function(time,
                              velocity,
                              time_correction = 0,
                              weights = 1,
                              na.rm = FALSE) {

  # Put data into data frame
  df <- data.frame(
    time = time,
    time_correction = time_correction,
    corrected_time = time + time_correction,
    velocity = velocity,
    weights = weights
  )

  # Remove NAs
  if (na.rm) {
    df <- stats::na.omit(df)
  }

  # Non-linear model
  speed_mod <- stats::nls(
    velocity ~ MSS * (1 - exp(1)^(-(corrected_time) / TAU)),
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
  pred_velocity <- MSS * (1 - exp(1)^(-(df$corrected_time) / TAU))

  model_fit <- shorts_model_fit(
    model = speed_mod,
    observed = df$velocity,
    predicted = pred_velocity,
    na.rm = na.rm
  )

  # Add predicted velocity to df
  df <- data.frame(
    time = time,
    velocity = velocity,
    weights = weights,
    pred_velocity = pred_velocity
  )

  # Return object
  return(new_shorts_model(
    parameters = list(
      MSS = MSS,
      TAU = TAU,
      MAC = MAC,
      PMAX = PMAX
    ),
    model_fit = model_fit,
    model = speed_mod,
    data = df
  ))
}


#' Mixed Model Using Instantaneous Velocity
#'
#' This function models the sprint instantaneous velocity using mono-exponential equation and non-linear
#'     mixed model using \code{\link[nlme]{nlme}} to estimate fixed and random maximum sprinting speed (\code{MSS})
#'     and relative acceleration (\code{TAU}) parameters. In mixed model, fixed and random effects are estimated for
#'     \code{MSS} and \code{TAU} parameters using \code{athlete} as levels. \code{velocity} is used as target or outcome
#'     variable, and \code{time} as predictor.
#' @param data Data frame
#' @param time Character string. Name of the column in \code{data}
#' @param velocity Character string. Name of the column in \code{data}
#' @param athlete Character string. Name of the column in \code{data}. Used as levels in the \code{\link[nlme]{nlme}}
#' @param time_correction Numeric vector. Used to filter out noisy data from the radar gun.
#'     This correction is done by adding \code{time_correction} to \code{time}. Default is 0. See more in Samozino (2018)
#' @param na.rm Logical. Default is FALSE
#' @return List object with the following elements:
#'     \describe{
#'         \item{parameters}{List with two data frames: \code{fixed} and \code{random} containing the following
#'             estimated parameters: \code{MSS}, \code{TAU}, \code{MAC}, and \code{PMAX}}
#'         \item{model_fit}{List with the following components:
#'             \code{RSE}, \code{R_squared}, \code{minErr}, \code{maxErr}, and \code{RMSE}}
#'         \item{model}{Model returned by the \code{\link[nlme]{nlme}} function}
#'         \item{data}{Data frame used to estimate the sprint parameters, consisting of \code{athlete}, \code{time},
#'           \code{velocity}, and \code{pred_velocity} columns}
#'         }
#' @export
#' @references
#'     Samozino P. 2018. A Simple Method for Measuring Force, Velocity and Power Capabilities and Mechanical
#'         Effectiveness During Sprint Running. In: Morin J-B, Samozino P eds. Biomechanics of Training and Testing.
#'        Cham: Springer International Publishing, 237–267. DOI: 10.1007/978-3-319-05633-3_11.
#'
#' @examples
#' data("radar_gun_data")
#' mixed_model <- mixed_model_using_radar(radar_gun_data, "time", "velocity", "athlete")
#' mixed_model$parameters
mixed_model_using_radar <- function(data,
                                    time,
                                    velocity,
                                    athlete,
                                    time_correction = 0,
                                    # weights = rep(1, nrow(data)),
                                    na.rm = FALSE) {


  # Combine to DF
  df <- data.frame(
    athlete = data[[athlete]],
    time = data[[time]],
    time_correction = time_correction,
    corrected_time = data[[time]] + time_correction,
    velocity = data[[velocity]] # ,
    # weights = weights
  )

  # Remove NAs
  if (na.rm) {
    df <- stats::na.omit(df)
  }

  # Create mixed model
  mixed_model <- nlme::nlme(
    velocity ~ MSS * (1 - exp(1)^(-(corrected_time) / TAU)),
    data = df,
    fixed = MSS + TAU ~ 1,
    random = MSS + TAU ~ 1,
    groups = ~athlete,
    # weights = ~weights,
    start = c(MSS = 7, TAU = 0.8)
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
  pred_velocity <- stats::predict(mixed_model, newdata = df)

  model_fit <- shorts_model_fit(
    model = mixed_model,
    observed = df$velocity,
    predicted = pred_velocity,
    na.rm = na.rm
  )

  # Add predicted velocity to df
  df <- data.frame(
    athlete = data[[athlete]],
    time = data[[time]],
    velocity = data[[velocity]],
    pred_velocity = pred_velocity # ,
    # weights = weights
  )

  return(new_shorts_mixed_model(
    parameters = list(
      fixed = fixed_effects,
      random = random_effects
    ),
    model_fit = model_fit,
    model = mixed_model,
    data = df
  ))
}
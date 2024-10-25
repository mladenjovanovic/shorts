# Internal prepare data function
prepare_in_situ_data <- function(df,
                                 acceleration_threshold,
                                 velocity_threshold,
                                 velocity_step,
                                 n_observations,
                                 filter_func,
                                 na.rm) {

  # Remove acceleration over threshold
  df <- df[df$acceleration >= acceleration_threshold, ]

  # Remove velocity over threshold
  df <- df[df$velocity >= velocity_threshold, ]

  # Create groups
  df$group <- cut(
    df$velocity,
    breaks = seq(velocity_threshold, max(df$velocity, na.rm = TRUE) + 1, velocity_step),
    labels = FALSE,
    include.lowest = TRUE
  )

  # Extract observations per group
  df_list <- split(df, df$group)

  purrr::map_dfr(df_list, function(x) {
    # Filter functions
    x <- filter_func(x)

    best_acc <- x[order(x$acceleration, decreasing = TRUE), ]

    if (nrow(x) >  n_observations) {
      best_acc <- best_acc[seq(1, n_observations), ]
    }

    best_acc
  })
}

#' @rdname model_functions
#' @description \code{model_in_situ} estimates short sprint parameters using velocity-acceleration trace,
#'     provided by the monitoring systems like GPS or LPS. See references for the information
#'
#' @param acceleration_threshold Acceleration cuttof. Default is 0
#' @param velocity_threshold Velocity cutoff. If \code{NULL} (default), velocity of the observation with
#'     the fastest acceleration is taken as the cutoff value
#' @param velocity_step Velocity increment size for finding max acceleration. Default is 0.2 m/s
#' @param n_observations Number of top acceleration observations to keep in velocity bracket.
#'      Default is 2
#' @param filter_func Function to filter outliers within each velocity bracket. This is used to remove
#'     noise in the data. Default is \code{function(x){x}} which doesn't involve any filtering.
#'
#' @references
#'     Clavel, P., Leduc, C., Morin, J.-B., Buchheit, M., & Lacome, M. (2023).
#'     Reliability of individual acceleration-speed profile in-situ in elite youth
#'     soccer players. Journal of Biomechanics, 153, 111602.
#'     https://doi.org/10.1016/j.jbiomech.2023.111602
#'
#'     Morin, J.-B. (2021). The “in-situ” acceleration-speed profile for team
#'     sports: testing players without testing them. JB Morin, PhD – Sport Science website.
#'     Accessed 31. Dec. 2023.
#'     https://jbmorin.net/2021/07/29/the-in-situ-sprint-profile-for-team-sports-testing-players-without-testing-them/
#'
#' @export
#' @examples
#'
#' # Model In-Situ (Embedded profiling)
#' data("LPS_session")
#' m1 <- model_in_situ(
#'   velocity = LPS_session$velocity,
#'   acceleration = LPS_session$acceleration,
#'   # Use specific cutoff value
#'   velocity_threshold = 4)
#' m1
#' plot(m1)
#'
model_in_situ <- function(velocity,
                          acceleration,
                          weights = 1,
                          acceleration_threshold = 0,
                          velocity_threshold = NULL,
                          velocity_step = 0.2,
                          n_observations = 2,
                          filter_func = function(x) {x},
                          CV = NULL,
                          na.rm = FALSE,
                          ...) {

  # Estimation function
  model_func <- function(train, test, ...) {

    # If velocity threshold is null
    if (is.null(velocity_threshold)) {
      # find the velocity of the highest acceleration observation
      velocity_threshold <- train$velocity[which.max(train$acceleration)]
    }

    # Filter data
    train <- prepare_in_situ_data(
      train,
      acceleration_threshold = acceleration_threshold,
      velocity_threshold = velocity_threshold,
      velocity_step = velocity_step,
      n_observations = n_observations,
      filter_func = filter_func,
      na.rm = na.rm
    )

    test <- prepare_in_situ_data(
      test,
      acceleration_threshold = acceleration_threshold,
      velocity_threshold = velocity_threshold,
      velocity_step = velocity_step,
      n_observations = n_observations,
      filter_func = filter_func,
      na.rm = na.rm
    )

    param_start <- list(MSS = 7, MAC = 7)
    param_lower <- c(MSS = 0, MAC = 0)
    param_upper <- c(MSS = Inf, MAC = Inf)

    # Linear model
    model <- minpack.lm::nlsLM(
      acceleration ~ predict_acceleration_at_velocity(velocity, MSS, MAC),
      data = train,
      start = param_start,
      lower = param_lower,
      upper = param_upper,
      weights = train$weight,
      ...
    )

    # Parameters
    MSS <- stats::coef(model)[["MSS"]]
    MAC <- stats::coef(model)[["MAC"]]
    TAU <- MSS / MAC
    PMAX <- (MSS * MAC) / 4

    # Model fit
    pred_acceleration <- stats::predict(model, newdata = data.frame(velocity = test$velocity))
    resid_acceleration <- test$acceleration - pred_acceleration

    return(list(
      data = train,
      model_info = list(
        predictor = "velocity",
        target = "acceleration"
      ),
      model = model,
      parameters = list(
        MSS = MSS,
        MAC = MAC,
        TAU = TAU,
        PMAX = PMAX
      ),
      corrections = list(
        velocity_threshold = velocity_threshold,
        velocity_step = velocity_step,
        n_observations = n_observations
      ),
      predictions = list(
        .predictor = test$velocity,
        .observed = test$acceleration,
        .predicted = pred_acceleration,
        .residual = resid_acceleration
      )
    ))
  }

  model_sprint(
    df = data.frame(
      velocity = velocity,
      acceleration = acceleration,
      weight = weights
    ),
    CV = CV,
    na.rm = na.rm,
    model_func = model_func,
    ...
  )
}

# Internal wrapper function that performs the model building, model fit estimates,
# predictons and cross-validations
model_sprint <- function(df,
                         CV = NULL,
                         na.rm = FALSE,
                         model_func = function(train, test, ...) {
                           list(
                             data = NULL,
                             model_info = NULL,
                             model = NULL,
                             parameters = NULL,
                             corrections = NULL,
                             predictions = NULL
                           )
                         },
                         ...) {

  # Remove NAs
  if (na.rm) {
    df <- stats::na.omit(df)
  }

  # Run model
  training_model <- model_func(
    train = df,
    test = df,
    ...
  )

  # Get the predictions
  predictions <- training_model$predictions

  training_model_fit <- shorts_model_fit(
    observed = predictions$.observed,
    predicted = predictions$.predicted,
    na.rm = na.rm
  )

  # Cross validation
  CV_data <- NULL

  if (!is.null(CV)) {
    # Shuffle data
    cv_df <- df[sample(nrow(df)), ]

    cv_folds <- data.frame(
      fold = as.numeric(cut(seq(1, nrow(cv_df)), breaks = CV, labels = FALSE)),
      index = seq(1, nrow(cv_df))
    )

    cv_folds_list <- split(cv_folds, cv_folds$fold)

    testing <- purrr::map(cv_folds_list, function(fold) {
      train_data <- cv_df[-fold$index, ]
      test_data <- cv_df[fold$index, ]

      model_func(
        train = train_data,
        test = test_data,
        ...
      )
    })

    # Extract predictions
    testing_predictions <- purrr::imap_dfr(testing, function(x, id) {
      data.frame(.fold = id, x$predictions)
    })

    # Model fit per fold
    testing_fold_model_fit <- purrr::imap_dfr(testing, function(x, id) {
      data.frame(
        .fold = id,
        data.frame(
          shorts_model_fit(
            observed = x$predictions$.observed,
            predicted = x$predictions$.predicted,
            na.rm = na.rm
          )
        )
      )
    })

    # Create overall model fit, across all folds
    testing_overall_model_fit <- shorts_model_fit(
      observed = testing_predictions$.observed,
      predicted = testing_predictions$.predicted,
      na.rm = na.rm
    )

    # Extract model parameters
    testing_parameters <- purrr::imap_dfr(testing, function(x, id) {
      data.frame(.fold = id, x$parameters)
    })

    testing_corrections <- purrr::imap_dfr(testing, function(x, id) {
      data.frame(.fold = id, x$corrections)
    })

    # Testing df
    testing_df <- cv_df

    # Save everything in the object
    CV_data <- list(
      data = testing_df,
      n_folds = CV,
      folds = cv_folds,
      parameters = testing_parameters,
      corrections = testing_corrections,
      model_fit_folds = testing_fold_model_fit,
      model_fit_overall = testing_overall_model_fit,
      predictions = testing_predictions
    )
  }

  # Return object
  return(new_shorts_model(
    data = training_model$data,
    model_info = training_model$model_info,
    model = training_model$model,
    parameters = training_model$parameters,
    corrections = training_model$corrections,
    predictions = training_model$predictions,
    model_fit = training_model_fit,
    CV = CV_data
  ))
}

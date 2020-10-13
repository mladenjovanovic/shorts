# shorts 1.1.2

* Added `get_air_resistance` function to estimate air resistance in newtons
* Added `predict_air_resistance_at_time` and `predict_air_resistance_at_time` functions
* Added `predict_force_at_time` and `predict_force_at_distance` functions
* Added `predict_power_at_time` and `predict_power_at_distance` functions
* Now power calculations in `predict_relative_power_at_distance` and `predict_relative_power_at_time` use air resistance to do the calculations. The default bodymass is 75kg. To replicate earlier function behavior, use `predict_velocity_at_` multiplied by `predict_acceleration_at_` to get relative power without air resistance
* Function `predict_kinematics` now uses aforementioned changes in `predict_relative_power_` functions, and added force and air resistance in the output
* Functions `find_max_power_distance` and `find_power_critical_distance` now uses aforementioned changes and returns absolute power, rather than relative, and air resistance is used in the power calculations
* Added `find_max_power_time`, `find_velocity_critical_time`, `find_acceleration_critical_time`, and `find_power_critical_time` functions
* Added `model_using_radar_with_time_correction` where time_correction parameter is estimated
* Added `mixed_model_using_radar_with_time_correction` where time_correction parameter is estimated

# shorts 1.1.1

* Added James Vescovi contributed dataset (`data("vescovi")`). For more info see `?vescovi`


# shorts 1.1.0

* Used constructor functions for all modeling functions
* Added modular short_model_fit function with three extra estimators: MAE, MAPE, and MaxAbsErr. This function is hidden from the user, but allows easier extension and modularity as well avoidance of code repetition
* Added `time_correction` and `distance_correction` to all model parameters output to align them
* Added S3 `predict_kinematics` function that predicts 0-6s distance (100Hz), velocity, acceleration, and relative power
* Added S3 `print`, `coef`, `summary`, `predict`, and `residuals` methods
* Added LOOCV option to modeling functions which provided cross-validated predictions and estimated model parameters
* Added `random` parameter to `mixed_` family of functions to allow higher flexibility in model definitions, but removed `corrections_as_random_effects` parameter. The default behavious is to use all parameters as random effects. 
* Added `...` to `model_using_radar` and `mixed_model_using_radar` to be forwarded to `nlme::nlme`
* Expanded the Readme.md file
* Expanded the `sprint-corrections` vignette by adding a short simulation study


# shorts 1.0.1

* Corrected the error in vignette that showed on Solaris system. Increased the random noise to allow nls to fit


# shorts 1.0.0

* Renamed `time_delay` to `time_correction` in `shorts::model_using_instant_velocity` and `shorts::mixed_model_using_instant_velocity` functions to be more consistent across functions. Also, this correction is **added** to time, so use negative numbers instead
* `time_correction` in `shorts::mixed_model_using_instant_velocity` and `shorts::mixed_model_using_split_times` is now numeric vector, not column name
* Implemented `time_correction` in `shorts::model_using_split_times` and `shorts::mixed_model_using_split_times`
* Implemented `time_correction` in `shorts:predict_` family of functions
* Implemented `distance_correction` in `shorts:predict_` family of functions
* Implemented estimation of `time_correction` in `shorts::model_using_split_times_with_time_correction` and `shorts::mixed_model_using_split_times_with_time_correction`
* Implemented estimation of `distance_correction` in `shorts::model_using_split_times_with_corrections` and `shorts::mixed_model_using_split_times_with_corrections`
* Fixed error in calculating PMAX in `shorts::mixed_model_` functions
* Removed `maxAbsErr` from model_fit element 
* Wrote "sprint-corrections" vignette explaining the idea behind sprint time and distance corrections
* Implemented `find_` family of functions for finding max power and critical distance when velocity or acceleration reaches certain threshold
* Implemented `...` to all modeling functions so that extra parameters can be forwarded to the optimization engine
* Data `split_times` recreated using distance shift behind the first timing gate. Needed to demo mixed models with corrections
* Renamed `_model_using_instant_velocity` to `_model_using_radar`
* Renamed `_model_using_split_times_` to `_model_using_splits_`


# shorts 0.0.1

* Initial version with all core functionalities
* This package was submitted to CRAN on 2020-04-17. (commit b59402c9c5)  

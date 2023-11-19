# shorts 2.5.0

* Added `inertia` (mass; in kg) and `resistance` (force; in Newtons) to `predict_force_`, `predict_power_`, `predict_relative_power_`, as well as `predict_kinematics()` functions. This increases the ability of the {shorts} package to deal with external resistance, like weight vest, sled, and tether devices.
* Modified `predict_kinematics()` function to include `inertia` (mass; in kg) and `resistance` (force; in Newtons) parameters, as well as to allow the user to either provide the `shorts_model` object, or provide `MSS` and `MAC` parameters. Additional parameter `add_inertia_to_vertical` to allow user to decided whether inertia should be added to `bodymass` when calculating vertical force. 
* Added `predict_force_at_velocity()` function
* Modified `find_max_power_` and `find_power_critial_` function to add support for the `inertia` (mass; in kg) and `resistance` (force; in Newtons) parameters
* Simplified `make_FV_profile()` function and added support for the `inertia` (mass; in kg) and `resistance` (force; in Newtons) parameters. Removed the S3 print and plot methods, as well as the class type involving this function
* Made internal `convert_FV()` function available for the users and added support for the `inertia` (mass; in kg) and `resistance` (force; in Newtons) parameters
* Updated `predict_time_at_distance_FV()` function to use simpler `convert_FV()` function
* Renamed `find_max_power` functions to `find_peak_power` to avoid confusion between `Pmax` and `Ppeak`
* Implemented `inertia` (mass; in kg) and `resistance` (force; in Newtons) to `optimal_FV()` function
* Implemented `inertia` (mass; in kg) and `resistance` (force; in Newtons) to `probe_FV()` function
* I added the `model_tether_DC()` function with an additional `distance correction` (DC) parameter estimate. DC parameter serves as an intercept when distance doesn't start at zero

# shorts 2.4.0

* Removed "Suggest" from the package DESCRIPTION file
* Added `use_observed_MSS` parameter to `model_radar_gun()` and `model_tether()` functions. This allows user to estimate `MSS` parameter from the observed data
* Add `FD` parameter to `model_timing_gates_FD()` and `model_timing_gates_FD_TC()` functions, which allow user to provide fixed `FD` parameter value. 
* Updated README.Rmd file with the example on how to use these feature. Also added a reference to a published article that was previously available only as pre-print.
* Changed generic `predict()` function not to return fitted values, but to use generic predict method on the object. This allows user to use `newdata` parameter. Added `fitted()` method to return fitted values instead

# shorts 2.3.0

* Moved optimal profile functions to "optional-functions.R" file, with accompanying documentation name
* Renamed the `FV_slope_perc` to `profile_imb` in the output of the `optimal_` functions
* The functions `find_optimal_FV()` and `find_optimal_FV_peak()` are now wrapped inside the `optimal_FV`, but the method is selected using `method` parameter ("max" for `find_optimal_FV()` and "peak" for `find_optimal_FV_peak()`). Please refer to examples
* Probing analysis is now removed from `optimal_` functions and moved to `probe_FV()` and `probe_MSS_MAC()` functions. Please refer to examples
* Finding for the distance where the profile is optimal is now done using the `find_optimal_distance()` function that can take other optimal and probe functions as parameter. Please refer to examples
* Removed a bug in `find_optimal_FV_peak()` (now implemented in `find_optimal_FV()` function using `method="peak"` parameter) where information needed to calculate air resistance was not forwarded

# shorts 2.2.0

* Expanded the results output of the `find_optimal_MSS_MAC()` and `find_optimal_FV()` functions. Some of the columns are renamed to better clarity
* Added internal function `convert_FV()` to convert from FV profile to sprint profile. This is used to calculate Peak Power (`Ppeak`) metric and its location
* Added `min_func` parameter to `find_optimal_MSS_MAC_distance()` and `find_optimal_FV_distance()`. This allows to optimize by using any other metric
* Added `find_optimal_FV_peak()` function, which optimize the profile while keeping the Peak Power the same. Accompanying `find_optimal_FV_peak_distance()` is also added
* Added extra reference to DESCRIPTION

# shorts 2.1.0

* Added mean acceleration over distance to `format_splits()`
* Added `model_tether()` function for modeling data output from tether devices, which involve velocity at distance
* Change the `Pmax_relative` to `Pmax_rel` in the output of the `make_FV_profile()` function
* Added analytically/polynomially estimated `F0_poly`, `F0_poly_rel`, `V0_poly`, `Pmax_poly`, `Pmax_poly_rel`, and `FV_slope_poly` to the output of the `make_FV_profile()` function using the method outlined in Pierre Samozino and Nicolas Peyror, et al (2021) <doi: 10.1111/sms.14097>
* Added `predict_time_at_distance_FV()` which uses FV profile's `F0` and `V0` to predict time at distance. For more info see Pierre Samozino and Nicolas Peyror, et al (2021) <doi: 10.1111/sms.14097>
* Added optimization functions: `find_optimal_MSS_MAC()`, `find_optimal_MSS_MAC_distance()`, `find_optimal_FV()`, `find_optimal_FV_distance()`. For more info on the FV optimization see Pierre Samozino and Nicolas Peyror, et al (2021) <doi: 10.1111/sms.14097>
* Removed DOI from citation info

# shorts 2.0.0

This is NEW version of the {shorts} package **INCOMPATIBLE** with the previous due to drastic changes in functions. Here are the changes utilized:

* Removed the mixed-effects function due to their small usage in practice.
* In `predict_` functions, `time_correction` and `distance_correction` are no longer used, since due to novel models of estimation, it is hard to neatly implement them into functions. Now the `predict_` functions predict on a scale where sprint starts at `t=0` and `d=0`, rather than on the original (data) scale. This will also remove the confusion for the user.
 * In `predict_` functions, the user now uses `MSS` and `MAC` parameters
* Changed the non-linear regression estimation function from `stats::nls()` to `minpack.lm::nlsLM()` in `model_` functions. This is done to avoid "singular gradient" error and inability of the `stats::nls()` to estimate when there are zero residuals. Please make note that now when you use `...` in `model_` function, it will be forwarded to `minpack.lm::nlsLM()`. If you have been using `control = stats::nls.control(warnOnly = TRUE)` to avoid `stats::nls()` to throw error when fitting when there are zero residuals, now you can remove it. If needed use `control = minpack.lm::nls.lm.control()` instead. 
* Added `create_timing_gates_splits()` function to generate timing gates splits
* For modeling timing gates, the following functions are now available: `model_timing_gates()`, `model_timing_gates_TC()`, `model_timing_gates_FD()`, and `model_timing_gates_FD_TC()`. All other functions have been removed
* For modeling radar gun data, there is now only one function `model_radar_gun()` which also estimates time correction (`TC`) parameter.
* Function `model_radar_gun()` feature n-folds *cross-validation*, as opposed to `model_timing_gates()` family of functions, which features leave-one-out cross-validation (LOOCV) due to small number of observations. Using the `CV` parameter, set n-fold cross-validations for the `model_radar_gun()` function. 
* Renamed the element `LOOCV` in the `shorts_model` object to `CV` to reflect above changes in `model_radar_gun()` function
* Removed vignettes. I am working on a better pre-print as well as one peer-reviewed simulation paper and will reference those instead

# shorts 1.1.6

* Updated documentation regarding the below mentioned potential issues
* IMPORTANT: For the `model_using_splits_with_distance_correction()` function the `predict_XXX_at_distance()`  family of functions doesn't work correctly if `distance_correction` is used as parameter (i.e., different than zero). This is because the model definition is completely different, and predicting on the same distance scale is not possible. Please refer to Jovanović, M., Vescovi, J.D. (2020) for more information.
* Jovanović, M., Vescovi, J.D. (2020). **shorts: An R Package for Modeling Short Sprints**. Preprint available at *SportRxiv*. https://doi.org/10.31236/osf.io/4jw62

# shorts 1.1.5

* Fixed wrong error message from the `plot.shorts_fv_profile()` function
* Minor model print aesthetic changes
* Fixed calculation of the residuals, which is now correctly calculated using observed - predicted. This calculation is implemented in the `residuals()` S3 method, as well as with internal function `shorts_model_fit()` that provides model fit estimates (i.e. RMSE, MAE, MAPE)
* Fixed calculation of the residuals in the vignette
* Added `model_using_splits_with_distance_correction()` function that implements novel model definition to estimate flying start distance
* Added `mixed_model_using_splits_with_distance_correction()` function that implements novel model definition to estimate flying start distance

# shorts 1.1.4

* Added `ggplot2` and `tidyr` package dependency and implemented it in S3 plotting functions
* `get_FV_profile` now return a object class `shorts_fv_profile`
*  Added S3 print and plot methods for `shorts_fv_profile` object
* Renamed column `force` and `relative_force` to `horizontal_force` and `relative_horizontal_force` in the `shorts_fv_profile` object and data frame returned by the `predict_kinematics` 
* Added `acceleration`, `bodymass`, `net_horizontal_force`, `air_resistance`, and `vertical_force`, `resultant_force_relative`, `power`, and `force_angle` in the `shorts_fv_profile` object and data frame returned by the `predict_kinematics`
* Improved examples and README.Rmd
* Added S3 plot method for `shorts_model` and `shorts_mixed_model` objects


# shorts 1.1.3

* Added sample radar gun data from Jean-Benoît Morin Microsoft Excel spreadsheet, freely available at his [website](https://jbmorin.net/2017/12/13/a-spreadsheet-for-sprint-acceleration-force-velocity-power-profiling/) (accessed October 27, 2020)). For more information `?jb_morin`

* Added `get_FV_profile` for generating Force-Velocity profile summary using Pierre Samozino and Jean-Benoît Morin method (for more DOI:10.1111/sms.12490)

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

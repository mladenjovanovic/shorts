# shorts 0.0.1.9000

* Renamed `time_delay` to `time_correction` in `shorts::model_using_instant_velocity` and `shorts::mixed_model_using_instant_velocity` functions to be more consistend across functions. Also, this correction is **added** to time, so use negative numbers instead
* `time_correction` in `shorts::mixed_model_using_instant_velocity` and `shorts::mixed_model_using_split_times` is now numeric vector, not column name
* Implemented `time_correction` in `shorts::model_using_split_times` and `shorts::mixed_model_using_split_times`
* Implemented `time_correction` in `shorts:predict_` family of functions
* Implemented `distance_correction` in `shorts:predict_` family of functions
* Implemented estimation of `time_correction` in `shorts::model_using_split_times_with_time_correction` and `shorts::mixed_model_using_split_times_with_time_correction`
* Implemented estimation of `distance_correction` in `shorts::model_using_split_times_with_corrections` and `shorts::mixed_model_using_split_times_with_corrections`
* Fixed error in calculating PMAX in `shorts::mixed_model_` functions
* Removed `maxAbsErr` from model_fit elemet 
* Wrote "sprint-corrections" vignette explaining the idea behind sprint time and distance corrections
* Implemented `...` to all modeling functions so that extra parameters can be forwarded to the optimization engine
* Data `split_times` recreated using distance shift behind the first timing gate. Needed to demo mixed models with corrections


# shorts 0.0.1

* Initial version with all core functionalities
* This package was submitted to CRAN on 2020-04-17. (commit b59402c9c5)  

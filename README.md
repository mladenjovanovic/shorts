
<!-- README.md is generated from README.Rmd. Please edit that file -->

# shorts <img src="man/figures/logo.png" align="right" width="200" />

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/shorts)](https://cran.r-project.org/package=shorts)
[![DOI](https://zenodo.org/badge/254907272.svg)](https://zenodo.org/badge/latestdoi/254907272)
<!-- badges: end -->

This package creates short sprint (\<6sec) profiles using the split
times or the radar gun data. Mono-exponential equation is used to
estimate maximal sprinting speed (MSS), relative acceleration (TAU), and
other parameters. These parameters can be used to predict kinematic and
kinetics variables and to compare individuals.

## Installation

``` r
# Install from CRAN
install.packages("shorts")

# Or the development version from GitHub
# install.packages("devtools")
devtools::install_github("mladenjovanovic/shorts")
```

## Examples

`shorts` comes with two sample data sets: `split_times` and
`radar_gun_data` with N=5 athletes. Let’s load them both:

``` r
require(shorts)
require(tidyverse)

data("split_times", "radar_gun_data")
```

### Profiling using split times

To model sprint performance using split times, distance will be used as
predictor and time as target. Since `split_times` contains data for
multiple athletes, let’s extract only one athlete and model it using
`shorts::model_using_splits` function:

``` r
kimberley_data <- filter(split_times, athlete == "Kimberley")

kimberley_profile <- shorts::model_using_splits(
  distance = kimberley_data$distance,
  time = kimberley_data$time)  

# Parameters
unlist(kimberley_profile$parameters)
#>        MSS        TAU        MAC       PMAX 
#>  8.5911421  0.8113282 10.5889855 22.7428698

# Model performance/fit
unlist(kimberley_profile$model_fit)
#>         RSE   R_squared      minErr      maxErr        RMSE 
#>  0.03403413  0.99965531 -0.02699169  0.05293444  0.02778875
```

`shorts::model_using_splits` returns an object with `parameters`,
`model_fit`, `model` returned from `stats::nls` function and `data` used
to estimate parameters. Parameters estimated using mono-exponential
equation are *maximal sprinting speed* (MSS), and *relative
acceleration* (TAU). Additional parameters computed from MSS and TAU are
*maximal acceleration* (MAC) and *maximal relative power* (PMAX).

If you are interested in calculating average split velocity, use
`shorts::format_splits`

``` r
shorts::format_splits(
  distance = kimberley_data$distance,
  time = kimberley_data$time)
#>   split split_distance_start split_distance_stop split_distance
#> 1     1                    0                   5              5
#> 2     2                    5                  10              5
#> 3     3                   10                  15              5
#> 4     4                   15                  20              5
#> 5     5                   20                  30             10
#> 6     6                   30                  40             10
#>   split_time_start split_time_stop split_time split_mean_velocity
#> 1                0           1.158      1.158        4.317789....
#> 2            1.158           1.893      0.735        6.802721....
#> 3            1.893           2.541      0.648        7.716049....
#> 4            2.541           3.149      0.608        8.223684....
#> 5            3.149           4.313      1.164        8.591065....
#> 6            4.313           5.444      1.131        8.841732....
```

Let’s plot observed vs fitted split times. For this we can use `data`
returned from `shorts::model_using_splits` since it contains `pred_time`
column, but we can also use `shorts:predict_` family of functions.

``` r
ggplot(kimberley_profile$data, aes(x = distance)) +
  theme_bw() +
  geom_point(aes(y = time)) +
  geom_line(aes(y = pred_time)) +
  xlab("Distance (m)") +
  ylab("Time (s)")
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="90%" style="display: block; margin: auto;" />

To plot predicted velocity, acceleration, and relative power over
distance, use `shorts:predict_`

``` r
kimberley_pred <- tibble(
  distance = seq(0, 40, length.out = 1000),
  
  # Velocity
  pred_velocity = shorts::predict_velocity_at_distance(
    distance,
    kimberley_profile$parameters$MSS,
    kimberley_profile$parameters$TAU),
  
  # Acceleration
  pred_acceleration = shorts::predict_acceleration_at_distance(
    distance,
    kimberley_profile$parameters$MSS,
    kimberley_profile$parameters$TAU),
  
  # Power
  pred_power = shorts::predict_relative_power_at_distance(
    distance,
    kimberley_profile$parameters$MSS,
    kimberley_profile$parameters$TAU),
)

# Convert to long
kimberley_pred <- gather(kimberley_pred, "metric", "value", -distance)

ggplot(kimberley_pred, aes(x = distance, y = value)) +
  theme_bw() +
  geom_line() +
  facet_wrap(~metric, scales = "free_y") + 
  xlab("Distance (m)") +
  ylab(NULL)
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="90%" style="display: block; margin: auto;" />

Package `shorts` comes with `find_` family of functions that allow
finding peak power and it’s location, as well as *critical distance*
over which velocity, acceleration, or power drops below certain
threshold:

``` r
# Peak power and location
shorts::find_max_power_distance(
  kimberley_profile$parameters$MSS,
  kimberley_profile$parameters$TAU
)
#> $max_power
#> [1] 22.74287
#> 
#> $distance
#> [1] 1.346271

# Distance over which power is over 50%
shorts::find_power_critical_distance(
  MSS = kimberley_profile$parameters$MSS,
  TAU = kimberley_profile$parameters$TAU,
  percent = 0.5
)
#> $lower
#> [1] 0.08295615
#> 
#> $upper
#> [1] 7.441024

# Distance over which acceleration is under 50%
shorts::find_acceleration_critical_distance(
  MSS = kimberley_profile$parameters$MSS,
  TAU = kimberley_profile$parameters$TAU,
  percent = 0.5
)
#> [1] 1.346279

# Distance over which velocity is over 95%
shorts::find_velocity_critical_distance(
  MSS = kimberley_profile$parameters$MSS,
  TAU = kimberley_profile$parameters$TAU,
  percent = 0.95
)
#> [1] 14.25922
```

#### Mixed-effect models

Each individual can be modeled separately, or we can perform *non-linear
mixed model* using `nlme` function from *nlme* package (Pinheiro *et
al.*, 2019). This is done using `shorts::mixed_model_using_splits`:

``` r
mixed_model <- shorts::mixed_model_using_splits(
  data = split_times,
  distance = "distance",
  time = "time",
  athlete = "athlete"
)

# Parameters
mixed_model$parameters
#> $fixed
#>        MSS       TAU      MAC     PMAX
#> 1 8.064911 0.6551988 12.30911 24.81796
#> 
#> $random
#>     athlete      MSS       TAU      MAC     PMAX
#> 1     James 9.691736 0.8469741 11.44278 27.72510
#> 2       Jim 7.833622 0.5048535 15.51663 30.38785
#> 3      John 7.780395 0.7274302 10.69573 20.80424
#> 4 Kimberley 8.569518 0.8022235 10.68221 22.88535
#> 5  Samantha 6.449284 0.3945129 16.34746 26.35735

# Model performance/fit
unlist(mixed_model$model_fit)
#>         RSE   R_squared      minErr      maxErr        RMSE 
#>  0.02600213  0.99982036 -0.02934519  0.04964582  0.02139178
```

`shorts::mixed_model_using_splits` return the similar object, but
`parameters` contain two elements: `fixed` and `random`.

Let’s plot predicted velocity over distance for athletes in the
`split_times` data set:

``` r
velocity_over_distance <- merge(
    mixed_model$parameters$random,
    data.frame(distance = seq(0, 40, length.out = 1000))
)

velocity_over_distance$pred_velocity <- with(velocity_over_distance,
  shorts::predict_velocity_at_distance(
    distance = distance,
    MSS = MSS,
    TAU = TAU)
)

ggplot(velocity_over_distance, aes(x = distance, y = pred_velocity, color = athlete)) +
  theme_bw() +
  geom_line() +
  xlab("Distance (m)") +
  ylab("Predicted velocity (m/s)")
```

<img src="man/figures/README-unnamed-chunk-8-1.png" width="90%" style="display: block; margin: auto;" />

### Profiling using radar gun data

The radar gun data is modeled using measured velocity as target variable
and time as predictor. Individual analysis is performed using
`shorts::model_using_radar` function. Let’s do analysis for Jim:

``` r
jim_data <- filter(radar_gun_data, athlete == "Jim")

jim_profile <- shorts::model_using_radar(
  time = jim_data$time,
  velocity = jim_data$velocity
)

# Parameters
unlist(jim_profile$parameters)
#>        MSS        TAU        MAC       PMAX 
#>  7.9979331  0.8886595  8.9999977 17.9953449

# Model performance/fit
unlist(jim_profile$model_fit)
#>         RSE   R_squared      minErr      maxErr        RMSE 
#>  0.05058726  0.99924408 -0.15099212  0.16415830  0.05050288
```

The object returned from `shorts::model_using_radar` is same as object
returned from `shorts::model_using_splits`. Let’s plot Jim’s measured
velocity and predicted velocity:

``` r
ggplot(jim_profile$data, aes(x = time)) +
  theme_bw() +
  geom_line(aes(y = velocity), alpha = 0.5) +
  geom_line(aes(y = pred_velocity), color = "red", alpha = 0.5) +
  xlab("Time (s)") +
  ylab("Velocity (m/s)")
```

<img src="man/figures/README-unnamed-chunk-10-1.png" width="90%" style="display: block; margin: auto;" />

Radar gun data can be modeled individually or using *non-linear mixed
model* implemented in `shorts::mixed_model_using_radar`:

``` r
mixed_model <- shorts::mixed_model_using_radar(
  data = radar_gun_data,
  time = "time",
  velocity = "velocity",
  athlete = "athlete"
)

mixed_model$parameters
#> $fixed
#>        MSS      TAU     MAC     PMAX
#> 1 8.301178 1.007782 8.23708 17.09437
#> 
#> $random
#>     athlete      MSS       TAU      MAC     PMAX
#> 1     James 9.998556 1.1108457 9.000851 22.49888
#> 2       Jim 7.997945 0.8886712 8.999892 17.99516
#> 3      John 8.000051 1.0690357 7.483427 14.96695
#> 4 Kimberley 9.005500 1.2855706 7.005061 15.77102
#> 5  Samantha 6.503839 0.6847851 9.497635 15.44277

mixed_model$model_fit
#> $RSE
#> [1] 0.05164818
#> 
#> $R_squared
#> [1] 0.9994217
#> 
#> $minErr
#> [1] -0.2191295
#> 
#> $maxErr
#> [1] 0.198329
#> 
#> $RMSE
#> [1] 0.05156203
```

Let’s plot predicted acceleration over time (0-6sec) for athletes in the
`radar_gun_data` data set:

``` r
acceleration_over_time <- merge(
    mixed_model$parameters$random,
    data.frame(time = seq(0, 6, length.out = 100))
)

acceleration_over_time$pred_acceleration <- with(acceleration_over_time,
  shorts::predict_acceleration_at_time(
    time = time,
    MSS = MSS,
    TAU = TAU)
)

ggplot(acceleration_over_time, aes(x = time, y = pred_acceleration, color = athlete)) +
  theme_bw() +
  geom_line() +
  xlab("Time (s)") +
  ylab("Predicted acceleration (m/s^2)")
```

<img src="man/figures/README-unnamed-chunk-12-1.png" width="90%" style="display: block; margin: auto;" />

### Using corrections

You have probably noticed that estimated MSS and TAU were a bit too high
for splits data. Biased estimates are due to differences in starting
positions and *timing triggering methods* for certain measurement
approaches (e.g. starting behind first timing gate, or allowing for body
rocking). This topic is further explained in `sprint-corrections`
[vignette](https://mladenjovanovic.github.io/shorts/articles/sprint-corrections.html)
that can be accessed by typing:

``` r
vignette("sprint-corrections")
```

Here I will provide quick summary. Often, this bias in estimates is
dealt with by using heuristic rule of thumb of adding `time_correction`
to split times (e.g. from 0.3-0.5sec; see more in Haugen *et al.*,
2012). This functionality is available in all covered `shorts`
functions:

``` r
mixed_model_corrected <- shorts::mixed_model_using_splits(
  data = split_times,
  distance = "distance",
  time = "time",
  athlete = "athlete", 
  time_correction = 0.3
)

# Parameters
mixed_model$parameters
#> $fixed
#>        MSS      TAU     MAC     PMAX
#> 1 8.301178 1.007782 8.23708 17.09437
#> 
#> $random
#>     athlete      MSS       TAU      MAC     PMAX
#> 1     James 9.998556 1.1108457 9.000851 22.49888
#> 2       Jim 7.997945 0.8886712 8.999892 17.99516
#> 3      John 8.000051 1.0690357 7.483427 14.96695
#> 4 Kimberley 9.005500 1.2855706 7.005061 15.77102
#> 5  Samantha 6.503839 0.6847851 9.497635 15.44277

# Model performance/fit
unlist(mixed_model$model_fit)
#>         RSE   R_squared      minErr      maxErr        RMSE 
#>  0.05164818  0.99942171 -0.21912952  0.19832897  0.05156203
```

And `time_correction` can also be used in `predict_` and `find_` family
of functions:

``` r
velocity_over_distance_corrected <- merge(
    mixed_model_corrected$parameters$random,
    data.frame(distance = seq(0, 40, length.out = 1000))
)

velocity_over_distance_corrected$pred_velocity <- with(velocity_over_distance,
  shorts::predict_velocity_at_distance(
    distance = distance,
    MSS = MSS,
    TAU = TAU,
    time_correction = 0.3)
)

ggplot(velocity_over_distance_corrected, aes(x = distance, y = pred_velocity, color = athlete)) +
  theme_bw() +
  geom_line() +
  xlab("Distance (m)") +
  ylab("Predicted velocity (m/s)")
```

<img src="man/figures/README-unnamed-chunk-14-1.png" width="90%" style="display: block; margin: auto;" />

Instead of providing for `time_correction`, this parameter can be
estimated using `shorts::model_using_splits_with_time_correction` and
`shorts::mixed_model_using_splits_with_time_correction`:

``` r
kimberley_profile_with_time_correction <- shorts::model_using_splits_with_time_correction(
  distance = kimberley_data$distance,
  time = kimberley_data$time)  

# Parameters
unlist(kimberley_profile_with_time_correction$parameters)
#>             MSS             TAU             MAC            PMAX time_correction 
#>       8.9748353       1.2348565       7.2679175      16.3070907       0.2346537

# Model performance/fit
unlist(kimberley_profile_with_time_correction$model_fit)
#>           RSE     R_squared        minErr        maxErr          RMSE 
#>  0.0011290466  0.9999996942 -0.0012094658  0.0011807342  0.0007983565


# Mixed-effect model using `time_correction` as fixed effect only
# To use `time_correction` as random effects, use corrections_as_random_effects = TRUE
mixed_model_with_time_correction <- shorts::mixed_model_using_splits_with_time_correction(
  data = split_times,
  distance = "distance",
  time = "time",
  athlete = "athlete",
  corrections_as_random_effects = FALSE
)

# Parameters
mixed_model_with_time_correction$parameters
#> $fixed
#>        MSS       TAU time_correction     MAC     PMAX
#> 1 8.304014 0.9687348       0.1989677 8.57202 17.79554
#> 
#> $random
#>     athlete       MSS       TAU time_correction       MAC     PMAX
#> 1     James 10.186327 1.2429367       0.1989677  8.195370 20.87018
#> 2       Jim  7.946099 0.7643674       0.1989677 10.395655 20.65123
#> 3      John  7.996262 1.0488272       0.1989677  7.624003 15.24088
#> 4 Kimberley  8.899472 1.1615147       0.1989677  7.661953 17.04683
#> 5  Samantha  6.491911 0.6260282       0.1989677 10.369998 16.83028

# Model performance/fit
unlist(mixed_model_with_time_correction$model_fit)
#>          RSE    R_squared       minErr       maxErr         RMSE 
#>  0.005976815  0.999990286 -0.016508275  0.009370607  0.004882226
```

For more details, please refer to `sprint-corrections`
[vignette](https://mladenjovanovic.github.io/shorts/articles/sprint-corrections.html).

## Citation

To cite `shorts`, please use the following command to get the BibTex
entry:

``` r
citation("shorts")
```

## References

Please refer to these publications for more information on short sprints
modeling using mono-exponential equation, as well as on performing mixed
non-linear models with `nlme` package:

Chelly SM, Denis C. 2001. Leg power and hopping stiffness: relationship
with sprint running performance: Medicine and Science in Sports and
Exercise:326–333. DOI: 10.1097/00005768-200102000-00024.

Clark KP, Rieger RH, Bruno RF, Stearne DJ. 2017. The NFL Combine 40-Yard
Dash: How Important is Maximum Velocity? Journal of Strength and
Conditioning Research:1. DOI: 10.1519/JSC.0000000000002081.

Furusawa K, Hill AV, and Parkinson JL. The dynamics of" sprint" running.
Proceedings of the Royal Society of London. Series B, Containing Papers
of a Biological Character 102 (713): 29-42, 1927

Greene PR. 1986. Predicting sprint dynamics from maximum-velocity
measurements. Mathematical Biosciences 80:1–18. DOI:
10.1016/0025-5564(86)90063-5.

Haugen TA, Tønnessen E, Seiler SK. 2012. The Difference Is in the Start:
Impact of Timing and Start Procedure on Sprint Running Performance:
Journal of Strength and Conditioning Research 26:473–479. DOI:
10.1519/JSC.0b013e318226030b.

Pinheiro J, Bates D, DebRoy S, Sarkar D, R Core Team. 2019. nlme: Linear
and nonlinear mixed effects models.

Samozino P. 2018. A Simple Method for Measuring Force, Velocity and
Power Capabilities and Mechanical Effectiveness During Sprint Running.
In: Morin J-B, Samozino P eds. Biomechanics of Training and Testing.
Cham: Springer International Publishing, 237–267. DOI:
10.1007/978-3-319-05633-3\_11.


<!-- README.md is generated from README.Rmd. Please edit that file -->

# shorts <img src="man/figures/shorts-logo.png" align="right" width="200" />

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
`shorts::model_using_split_times` function:

``` r
kimberley_data <- filter(split_times, athlete == "Kimberley")

kimberley_profile <- shorts::model_using_split_times(
  distance = kimberley_data$distance,
  time = kimberley_data$time)  

kimberley_profile$parameters
#> $MSS
#> [1] 8.591142
#> 
#> $TAU
#> [1] 0.8113282
#> 
#> $MAC
#> [1] 10.58899
#> 
#> $PMAX
#> [1] 22.74287

kimberley_profile$model_fit
#> $RSE
#> [1] 0.03403413
#> 
#> $R_squared
#> [1] 0.9996553
#> 
#> $minErr
#> [1] -0.02699169
#> 
#> $maxErr
#> [1] 0.05293444
#> 
#> $RMSE
#> [1] 0.02778875
```

`shorts::model_using_split_times` returns an object with `parameters`,
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
returned from `shorts::model_using_split_times` since it contains
`pred_time` column, but we can also use `shorts:predict_` family of
functions.

``` r
ggplot(kimberley_profile$data, aes(x = distance)) +
  theme_bw() +
  geom_point(aes(y = time)) +
  geom_line(aes(y = pred_time)) +
  xlab("Distance (m)") +
  ylab("Time (s)")
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="90%" style="display: block; margin: auto;" />

To plot predicted velocity over distance, use `shorts:predict_`

``` r
velocity_over_distance <- tibble(
  distance = seq(0, 40),
  pred_velocity = shorts::predict_velocity_at_distance(
    distance,
    kimberley_profile$parameters$MSS,
    kimberley_profile$parameters$TAU)
)

ggplot(velocity_over_distance, aes(x = distance, y = pred_velocity)) +
  theme_bw() +
  geom_line() +
  xlab("Distance (m)") +
  ylab("Predicted velocity (m/s)")
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="90%" style="display: block; margin: auto;" />

Each individual can be modeled separately, or we can perform *non-linear
mixed model* using `nlme` function from *nlme* package (Pinheiro *et
al.*, 2019). This is done using `shorts::mixed_model_using_split_times`:

``` r
mixed_model <- shorts::mixed_model_using_split_times(
  data = split_times,
  distance = "distance",
  time = "time",
  athlete = "athlete"
)

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

mixed_model$model_fit
#> $RSE
#> [1] 0.02600213
#> 
#> $R_squared
#> [1] 0.9998204
#> 
#> $minErr
#> [1] -0.02934519
#> 
#> $maxErr
#> [1] 0.04964582
#> 
#> $RMSE
#> [1] 0.02139178
```

`shorts::mixed_model_using_split_times` return the similar object, but
`parameters` contain two elements: `fixed` and `random`.

Let’s plot predicted velocity over distance for athletes in the
`split_times` data set:

``` r
velocity_over_distance <- merge(
    mixed_model$parameters$random,
    data.frame(distance = seq(0, 40))
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

<img src="man/figures/README-unnamed-chunk-7-1.png" width="90%" style="display: block; margin: auto;" />

Both `shorts::model_using_split_times` and
`shorts::model_using_split_times`allow for using `time_correction`
variable, which is used to adjust for different starting positions (for
details see Haugen *et al.*, 2012). This is implemented in all
`shorts:predict_` functions as well.

### Profiling using radar gun data

The radar gun data is modeled using measured velocity as target variable
and time as predictor. Individual analysis is performed using
`shorts::model_using_instant_velocity` function. Let’s do analysis for
Jim:

``` r
jim_data <- filter(radar_gun_data, athlete == "Jim")

jim_profile <- shorts::model_using_instant_velocity(
  time = jim_data$time,
  velocity = jim_data$velocity
)

jim_profile$parameters
#> $MSS
#> [1] 7.997933
#> 
#> $TAU
#> [1] 0.8886595
#> 
#> $MAC
#> [1] 8.999998
#> 
#> $PMAX
#> [1] 17.99534

jim_profile$model_fit
#> $RSE
#> [1] 0.05058726
#> 
#> $R_squared
#> [1] 0.9992441
#> 
#> $minErr
#> [1] -0.1509921
#> 
#> $maxErr
#> [1] 0.1641583
#> 
#> $RMSE
#> [1] 0.05050288
```

The object returned from `shorts::model_using_instant_velocity` is same
as object returned from `shorts::model_using_split_times`. Let’s plot
Jim’s measured velocity and predicted velocity:

``` r
ggplot(jim_profile$data, aes(x = time)) +
  theme_bw() +
  geom_line(aes(y = velocity), alpha = 0.5) +
  geom_line(aes(y = pred_velocity), color = "red", alpha = 0.5) +
  xlab("Time (s)") +
  ylab("Velocity (m/s)")
```

<img src="man/figures/README-unnamed-chunk-9-1.png" width="90%" style="display: block; margin: auto;" />

Radar gun data can be modeled individually or using *non-linear mixed
model* implemented in `shorts::mixed_model_using_instant_velocity`:

``` r
mixed_model <- shorts::mixed_model_using_instant_velocity(
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

<img src="man/figures/README-unnamed-chunk-11-1.png" width="90%" style="display: block; margin: auto;" />

Both `shorts::model_using_instant_velocity` and
`shorts::mixed_model_using_instant_velocity`allow for using
`time_correction` variable, which is sometimes used with radar gun data
(see Samozino, 2018 for more information).

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

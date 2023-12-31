
<!-- README.md is generated from README.Rmd. Please edit that file  -->

# shorts <img src="man/figures/logo.png" align="right" width="200"/>

<!-- badges: start -->

[![DOI](https://zenodo.org/badge/254907272.svg)](https://zenodo.org/badge/latestdoi/254907272)
[![R-CMD-check](https://github.com/mladenjovanovic/shorts/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mladenjovanovic/shorts/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/shorts)](https://CRAN.R-project.org/package=shorts)
<!-- badges: end -->

**{shorts}** is an R package aimed for the analysis of the un-resisted
and resisted short sprints (\<6sec; without deceleration), creation of
*acceleration-velocity profiles* (AVP), *force-velocity profiles* (FVP),
and *optimization profiles* using variety of sprint traces (e.g.,
time-velocity from laser/radar gun, distance-time from timing
gates/photocells). It represents a simple to use tool for researcher and
practitioners interested in modeling short sprints performance.

## Installation

``` r
# Install from CRAN
install.packages("shorts")

# Or the development version from GitHub
# install.packages("remotes")
remotes::install_github("mladenjovanovic/shorts")
```

## Examples

**{shorts}** comes with multiple sample data sets. Let’s load
`split_times` and `radar_gun_data` with N=5 athletes:

``` r
library(shorts)
library(tidyverse)
library(knitr)

data("split_times", "radar_gun_data")
```

### Profiling using split times

**{shorts}** package utilizes modified *mono-exponential functions* to
model short sprint performance. To model sprint performance using split
times, distance will be used as predictor and time as target. Since
`split_times` dataset contains data for multiple athletes, let’s extract
only one athlete and model it using `shorts::model_timing_gates()`
function.

``` r
kimberley_data <- filter(split_times, athlete == "Kimberley")

kable(kimberley_data)
```

| athlete   | bodyweight | distance |  time |
|:----------|-----------:|---------:|------:|
| Kimberley |         55 |        5 | 1.158 |
| Kimberley |         55 |       10 | 1.893 |
| Kimberley |         55 |       15 | 2.541 |
| Kimberley |         55 |       20 | 3.149 |
| Kimberley |         55 |       30 | 4.313 |
| Kimberley |         55 |       40 | 5.444 |

Parameters estimated using mono-exponential equation are *maximal
sprinting speed* ($MSS$), and *maximal acceleration* (MAC). Additional
parameters computed from $MSS$ and $MAC$ are *relative acceleration*
($TAU$) and *maximal relative power* ($PMAX$) (which is calculated as
$MAC \cdot MSS\div4$).

``` r
kimberley_profile <- shorts::model_timing_gates(
  distance = kimberley_data$distance,
  time = kimberley_data$time
)

kimberley_profile
#> Estimated model parameters
#> --------------------------
#>        MSS        MAC        TAU       PMAX 
#>  8.5911431 10.5889817  0.8113285 22.7428642 
#> 
#> Model fit estimators
#> --------------------
#>             R2        meanErr   meanErr_perc         minErr    minErr_perc 
#>    0.999655312   -0.003093343   -0.538602775   -0.052934568   -4.571206233 
#>         maxErr    maxErr_perc      maxAbsErr maxAbsErr_perc           RMSE 
#>    0.026991611    0.857148665    0.052934568    4.571206233    0.027788752 
#>      RMSE_perc            MAE       MAE_perc 
#>    1.939218713    0.023333409    1.192632747

summary(kimberley_profile)
#> 
#> Formula: time ~ predict_time_at_distance(distance, MSS, MAC)
#> 
#> Parameters:
#>     Estimate Std. Error t value Pr(>|t|)    
#> MSS   8.5911     0.1225   70.13 2.48e-07 ***
#> MAC  10.5890     0.4599   23.03 2.11e-05 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 0.03403 on 4 degrees of freedom
#> 
#> Number of iterations to convergence: 5 
#> Achieved convergence tolerance: 1.49e-08

coef(kimberley_profile)
#>       MSS       MAC 
#>  8.591143 10.588982

confint(kimberley_profile, level = 0.95)
#>         2.5%     97.5%
#> MSS 8.273337  8.963098
#> MAC 9.421593 12.022126
```

To return the predicted/fitted values (in this case time variable), use
`predict()` function:

``` r
predict(kimberley_profile)
#> [1] 1.210935 1.897021 2.521028 3.122008 4.299243 5.466324
```

To create a simple plot use S3 `plot()` method. There are four type
options: `"model"` (default), `"kinematics-time"`,
`"kinematics-distance"`, or `"residuals"`:

``` r
plot(kimberley_profile) +
  theme_bw()
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="90%" style="display: block; margin: auto;" />

``` r
plot(kimberley_profile, "kinematics-time") +
  theme_bw()
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="90%" style="display: block; margin: auto;" />

``` r
plot(kimberley_profile, "kinematics-distance") +
  theme_bw()
```

<img src="man/figures/README-unnamed-chunk-7-1.png" width="90%" style="display: block; margin: auto;" />

``` r
plot(kimberley_profile, "residuals") +
  theme_bw()
```

<img src="man/figures/README-unnamed-chunk-8-1.png" width="90%" style="display: block; margin: auto;" />

If you are interested in calculating average split velocity, use
`shorts::format_splits()`

``` r
kable(shorts::format_splits(
  distance = kimberley_data$distance,
  time = kimberley_data$time
))
```

| split | split_distance_start | split_distance_stop | split_distance | split_time_start | split_time_stop | split_time | split_mean_velocity | split_mean_acceleration |
|------:|---------------------:|--------------------:|---------------:|-----------------:|----------------:|-----------:|--------------------:|------------------------:|
|     1 |                    0 |                   5 |              5 |                0 |           1.158 |      1.158 |          4.317789…. |              3.728660…. |
|     2 |                    5 |                  10 |              5 |            1.158 |           1.893 |      0.735 |          6.802721…. |              3.380859…. |
|     3 |                   10 |                  15 |              5 |            1.893 |           2.541 |      0.648 |          7.716049…. |              1.409457…. |
|     4 |                   15 |                  20 |              5 |            2.541 |           3.149 |      0.608 |          8.223684…. |              0.834925…. |
|     5 |                   20 |                  30 |             10 |            3.149 |           4.313 |      1.164 |          8.591065…. |              0.315619…. |
|     6 |                   30 |                  40 |             10 |            4.313 |           5.444 |      1.131 |          8.841732…. |              0.221633…. |

To plot predicted velocity, acceleration, air resistance, force, and
power over distance, use `shorts:predict_XXX()`. Please note that to
calculate force, air resistance, and power, we need Kimberley’s bodymass
and height (as well as other characteristics such as air pressure,
temperature and wind - see `get_air_resistance()` function).

``` r
kimberley_bodymass <- 60 # in kilograms
kimberley_bodyheight <- 1.7 # in meters

kimberley_pred <- tibble(
  distance = seq(0, 40, length.out = 1000),

  # Velocity
  pred_velocity = shorts::predict_velocity_at_distance(
    distance,
    kimberley_profile$parameters$MSS,
    kimberley_profile$parameters$TAU
  ),

  # Acceleration
  pred_acceleration = shorts::predict_acceleration_at_distance(
    distance,
    kimberley_profile$parameters$MSS,
    kimberley_profile$parameters$TAU
  ),

  # Air resistance
  pred_air_resistance = shorts::predict_air_resistance_at_distance(
    distance,
    kimberley_profile$parameters$MSS,
    kimberley_profile$parameters$TAU,
    bodymass = kimberley_bodymass,
    bodyheight = kimberley_bodyheight
  ),

  # Force
  pred_force = shorts::predict_force_at_distance(
    distance,
    kimberley_profile$parameters$MSS,
    kimberley_profile$parameters$TAU,
    bodymass = kimberley_bodymass,
    bodyheight = kimberley_bodyheight
  ),

  # Power
  pred_power = shorts::predict_power_at_distance(
    distance,
    kimberley_profile$parameters$MSS,
    kimberley_profile$parameters$TAU,
    bodymass = kimberley_bodymass,
    bodyheight = kimberley_bodyheight
  ),
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

<img src="man/figures/README-unnamed-chunk-10-1.png" width="90%" style="display: block; margin: auto;" />

To do prediction simpler, use `shorts::predict_kinematics()` function.
This will provide kinetics and kinematics for 0-6 $s$ sprint using 100
$Hz$.

``` r
predicted_kinematics <- predict_kinematics(
  kimberley_profile,
  bodymass = kimberley_bodymass,
  bodyheight = kimberley_bodyheight
)

kable(head(predicted_kinematics))
```

| time |  distance |  velocity | acceleration | bodymass | inertia | resistance | air_resistance | horizontal_force | horizontal_force_relative | vertical_force | resultant_force | resultant_force_relative |     power | power_relative |      work | average_power | average_power_relative |        RF | force_angle |
|-----:|----------:|----------:|-------------:|---------:|--------:|-----------:|---------------:|-----------------:|--------------------------:|---------------:|----------------:|-------------------------:|----------:|---------------:|----------:|--------------:|-----------------------:|----------:|------------:|
| 0.00 | 0.0000000 | 0.0000000 |    10.588982 |       60 |       0 |          0 |      0.0000000 |         635.3389 |                 10.588982 |          588.6 |        866.0863 |                 14.43477 |   0.00000 |       0.000000 | 0.0000000 |           NaN |                    NaN | 0.7335746 |    42.81309 |
| 0.01 | 0.0005273 | 0.1052399 |    10.459269 |       60 |       0 |          0 |      0.0026620 |         627.5588 |                 10.459313 |          588.6 |        860.3952 |                 14.33992 |  66.04424 |       1.100737 | 0.3322639 |      33.22639 |              0.5537732 | 0.7293843 |    43.16520 |
| 0.02 | 0.0021005 | 0.2091907 |    10.331145 |       60 |       0 |          0 |      0.0105181 |         619.8792 |                 10.331320 |          588.6 |        854.8100 |                 14.24683 | 129.67294 |       2.161216 | 1.3128332 |      65.64166 |              1.0940276 | 0.7251660 |    43.51734 |
| 0.03 | 0.0047068 | 0.3118680 |    10.204590 |       60 |       0 |          0 |      0.0233774 |         612.2988 |                 10.204980 |          588.6 |        849.3290 |                 14.15548 | 190.95642 |       3.182607 | 2.9179055 |      97.26352 |              1.6210586 | 0.7209206 |    43.86946 |
| 0.04 | 0.0083337 | 0.4132876 |    10.079586 |       60 |       0 |          0 |      0.0410543 |         604.8162 |                 10.080270 |          588.6 |        843.9506 |                 14.06584 | 249.96305 |       4.166051 | 5.1243723 |     128.10931 |              2.1351551 | 0.7166488 |    44.22151 |
| 0.05 | 0.0129685 | 0.5134649 |     9.956112 |       60 |       0 |          0 |      0.0633688 |         597.4301 |                  9.957169 |          588.6 |        838.6732 |                 13.97789 | 306.75937 |       5.112656 | 7.9097991 |     158.19598 |              2.6365997 | 0.7123515 |    44.57343 |

To get model residuals, use `residuals()` function:

``` r
residuals(kimberley_profile)
#> [1] -0.052934568 -0.004021215  0.019971704  0.026991611  0.013756880
#> [6] -0.022324473
```

Package **{shorts}** comes with `find_XXX()` family of functions that
allow finding peak power and it’s location, as well as *critical
distance* over which velocity, acceleration, or power drops below
certain threshold:

``` r
# Peak power and location
shorts::find_peak_power_distance(
  MSS = kimberley_profile$parameters$MSS,
  MAC = kimberley_profile$parameters$MAC, 
  bodymass = kimberley_bodymass,
  bodyheight = kimberley_bodyheight
)
#> $peak_power
#> [1] 1384.248
#> 
#> $distance
#> [1] 1.424183

# Distance over which power is over 80%
shorts::find_power_critical_distance(
  MSS = kimberley_profile$parameters$MSS,
  MAC = kimberley_profile$parameters$MAC, 
  bodymass = kimberley_bodymass,
  bodyheight = kimberley_bodyheight,
  percent = 0.8
)
#> $lower
#> [1] 0.3421958
#> 
#> $upper
#> [1] 4.26947

# Distance over which acceleration is under 50%
shorts::find_acceleration_critical_distance(
  MSS = kimberley_profile$parameters$MSS,
  MAC = kimberley_profile$parameters$MAC,
  percent = 0.5
)
#> [1] 1.34628

# Distance over which velocity is over 95%
shorts::find_velocity_critical_distance(
  MSS = kimberley_profile$parameters$MSS,
  MAC = kimberley_profile$parameters$MAC,
  percent = 0.95
)
#> [1] 14.25923
```

### Profiling using radar gun data

The radar gun data is modeled using measured velocity as target variable
and time as predictor. Individual analysis is performed using
`shorts::model_radar_gun()` function or `shorts::model_laser_gun()`
(they are aliases). Let’s do analysis for Jim:

``` r
jim_data <- filter(radar_gun_data, athlete == "Jim")

jim_profile <- shorts::model_radar_gun(
  time = jim_data$time,
  velocity = jim_data$velocity
)

jim_profile
#> Estimated model parameters
#> --------------------------
#>        MSS        MAC        TAU       PMAX 
#>  7.9980115  8.9987070  0.8887956 17.9929403 
#> 
#> Estimated model corrections
#> --------------------------
#>            TC 
#> -0.0001104572 
#> 
#> Model fit estimators
#> --------------------
#>             R2        meanErr   meanErr_perc         minErr    minErr_perc 
#>   9.992441e-01  -2.481182e-08           -Inf  -1.640451e-01           -Inf 
#>         maxErr    maxErr_perc      maxAbsErr maxAbsErr_perc           RMSE 
#>   1.511234e-01   2.332511e+00   1.640451e-01            Inf   5.050254e-02 
#>      RMSE_perc            MAE       MAE_perc 
#>            Inf   3.927236e-02            Inf

summary(jim_profile)
#> 
#> Formula: velocity ~ predict_velocity_at_time(time - TC, MSS, MAC)
#> 
#> Parameters:
#>       Estimate Std. Error t value Pr(>|t|)    
#> MSS  7.9980115  0.0031934 2504.55   <2e-16 ***
#> MAC  8.9987070  0.0199701  450.61   <2e-16 ***
#> TC  -0.0001105  0.0012283   -0.09    0.928    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 0.05063 on 597 degrees of freedom
#> 
#> Number of iterations to convergence: 4 
#> Achieved convergence tolerance: 1.49e-08

confint(jim_profile)
#>             2.5%       97.5%
#> MSS  7.991747860 8.004286924
#> MAC  8.959592160 9.037967809
#> TC  -0.002530815 0.002291737

plot(jim_profile) +
  theme_bw()
```

<img src="man/figures/README-unnamed-chunk-14-1.png" width="90%" style="display: block; margin: auto;" />

In addition to $MSS$ and $MAC$ parameters, `shorts::model_radar_gun()`
function also estimated *time-correction* ($TC$) parameter.

Rather than estimating $MSS$, `shorts::model_radar_gun()` function
allows you to utilize peak velocity observed in the data as $MSS$. This
is done by setting the `use_observed_MSS` parameter to `TRUE`:

``` r
jim_profile <- shorts::model_radar_gun(
  time = jim_data$time,
  velocity = jim_data$velocity,
  use_observed_MSS = TRUE
)

jim_profile
#> Estimated model parameters
#> --------------------------
#>        MSS        MAC        TAU       PMAX 
#>  8.0950000  8.6782211  0.9327949 17.5625500 
#> 
#> Estimated model corrections
#> --------------------------
#>          TC 
#> -0.01117683 
#> 
#> Model fit estimators
#> --------------------
#>             R2        meanErr   meanErr_perc         minErr    minErr_perc 
#>     0.99875693    -0.03881082           -Inf    -0.22869200           -Inf 
#>         maxErr    maxErr_perc      maxAbsErr maxAbsErr_perc           RMSE 
#>     0.18253953     2.81740287     0.22869200            Inf     0.07984600 
#>      RMSE_perc            MAE       MAE_perc 
#>            Inf     0.06431872            Inf

summary(jim_profile)
#> 
#> Formula: velocity ~ predict_velocity_at_time(time - TC, MSS, MAC)
#> 
#> Parameters:
#>      Estimate Std. Error  t value Pr(>|t|)    
#> MSS  8.095000   0.005209 1554.099  < 2e-16 ***
#> MAC  8.678221   0.030174  287.601  < 2e-16 ***
#> TC  -0.011177   0.002025   -5.519 5.09e-08 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 0.08005 on 597 degrees of freedom
#> 
#> Number of iterations to convergence: 5 
#> Achieved convergence tolerance: 1.49e-08
```

### Profiling using tether devices

Some tether devices provide data out in a velocity-at-distance format.
In this case, velocity is the outcome variable and distance is the
predictor. To estimate sprint profiles from *tether data*, use
`shorts::model_tether()` function.

``` r
# This creates sprint trace
tether_df <- shorts::create_sprint_trace(
  MSS = 7, MAC = 6,
  time = seq(0.01, 6, by = 0.01))

m1 <- model_tether(
  distance = tether_df$distance,
  velocity = tether_df$velocity)

m1
#> Estimated model parameters
#> --------------------------
#>       MSS       MAC       TAU      PMAX 
#>  7.000000  6.000000  1.166667 10.500000 
#> 
#> Model fit estimators
#> --------------------
#>             R2        meanErr   meanErr_perc         minErr    minErr_perc 
#>   1.000000e+00   4.638882e-16   8.491813e-13  -3.808065e-14  -3.200666e-11 
#>         maxErr    maxErr_perc      maxAbsErr maxAbsErr_perc           RMSE 
#>   2.984279e-13   4.995146e-10   2.984279e-13   4.995146e-10   1.259855e-14 
#>      RMSE_perc            MAE       MAE_perc 
#>   2.044889e-11   1.438294e-15   9.813336e-13

plot(m1)
```

<img src="man/figures/README-unnamed-chunk-16-1.png" width="90%" style="display: block; margin: auto;" />

Setting `use_observed_MSS` parameter to `TRUE` in the
`shorts::model_tether()` function also allows you to use observed *peak
velocity* as $MSS$.

In the case when distance is not centered at zero, use
`shorts::model_tether_DC()` which also estimated the *distance
correction* ($DC$) parameter, serving as model intercept (for more info
see [Using corrections](#using-corrections) section):

``` r
# This creates sprint trace
tether_df <- shorts::create_sprint_trace(
  MSS = 7, MAC = 6,
  time = seq(0.001, 6, by = 0.01), 
  # Add distance shift
  DC = 5)

m1 <- model_tether_DC(
  distance = tether_df$distance,
  velocity = tether_df$velocity)

m1
#> Estimated model parameters
#> --------------------------
#>       MSS       MAC       TAU      PMAX 
#>  7.000000  6.000000  1.166667 10.500000 
#> 
#> Estimated model corrections
#> --------------------------
#> DC 
#>  5 
#> 
#> Model fit estimators
#> --------------------
#>             R2        meanErr   meanErr_perc         minErr    minErr_perc 
#>   1.000000e+00   6.483757e-11   8.098034e-07  -1.517186e-11  -2.897114e-10 
#>         maxErr    maxErr_perc      maxAbsErr maxAbsErr_perc           RMSE 
#>   2.873803e-08   4.791725e-04   2.873803e-08   4.791725e-04   1.181241e-09 
#>      RMSE_perc            MAE       MAE_perc 
#>   1.956287e-05   7.412052e-11   8.099718e-07

plot(m1)
```

<img src="man/figures/README-unnamed-chunk-17-1.png" width="90%" style="display: block; margin: auto;" />

### Embedded (i.e., *in-situ*) Profiling

With the modern technologies like GPS and LPS, session acceleration and
velocity can be tracked continuously. This provides an opportunity to
estimate short sprint profiles from *in-situ*, without the need for
explicit testing (assuming the maximal effort was performed). The
analysis is based on the theoretical model where acceleration and
velocity have linear relationship (i.e., mono-exponential model applied
thus far). The time frame of the analysis can vary from single drills
(e.g., sprint drills), session, week, to multiple weeks.

Here is an example of the data collected during one basketball session
for a single person. Duration was approx. 90 min with 20 $Hz$ sampling
rate. This is the positional data:

``` r
data("LPS_session")

LPS_session %>%
  ggplot(aes(x = x, y = y)) +
  theme_bw() +
  geom_point(alpha = 0.1)
```

<img src="man/figures/README-unnamed-chunk-18-1.png" width="90%" style="display: block; margin: auto;" />

The next figure plots instant acceleration and velocity:

``` r
LPS_session %>%
  ggplot(aes(x = velocity, y = acceleration)) +
  theme_bw() +
  geom_point(alpha = 0.1)
```

<img src="man/figures/README-unnamed-chunk-19-1.png" width="90%" style="display: block; margin: auto;" />

To estimate embedded short sprint profile, we need to filter out
positive acceleration and velocities over 3 $ms{-1}$ (default), then
filter few top acceleration observations per velocity bracket (for more
information please see Clavel *et al.* (2023)). Here is the graphical
representation:

``` r
embedded_model <- model_in_situ(
  LPS_session$velocity,
  LPS_session$acceleration,
  velocity_threshold = 4)
                                
LPS_session %>%
  filter(acceleration > 0) %>%
  ggplot(aes(x = velocity, y = acceleration)) +
  theme_bw() +
  geom_point(alpha = 0.1) +
  geom_point(
    data = embedded_model$data, 
    color = "red"
  ) +
  geom_abline(
    intercept = coef(embedded_model$model)[[1]],
    slope = coef(embedded_model$model)[[2]],
    linetype = "dotted", color = "red") +
  scale_x_continuous(expand = c(0, 0), limits = c(0,  embedded_model$parameters$MSS)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, embedded_model$parameters$MAC)) 
```

<img src="man/figures/README-unnamed-chunk-20-1.png" width="90%" style="display: block; margin: auto;" />

### Force-Velocity Profiling

To estimate *Force-Velocity Profile* (FVP) using approach by Samozino
*et al.* (2016, 2022) use `shorts::create_FVP()`:

``` r
kimberley_fv <- shorts::create_FVP(
  MSS = kimberley_profile$parameters$MSS,
  MAC = kimberley_profile$parameters$MAC,
  # These are needed to estimate air resistance
  bodymass = kimberley_bodymass,
  bodyheight = kimberley_bodyheight
)

kimberley_fv
#> $bodymass
#> [1] 60
#> 
#> $F0
#> [1] 635.3389
#> 
#> $F0_rel
#> [1] 10.58898
#> 
#> $V0
#> [1] 8.845438
#> 
#> $Pmax
#> [1] 1404.963
#> 
#> $Pmax_rel
#> [1] 23.41605
#> 
#> $FV_slope
#> [1] -1.197112
```

To convert back to *Acceleration-Velocity Profile* (AVP), use:

``` r
kimberley_avp <- shorts::convert_FVP(
  F0 = kimberley_fv$F0,
  V0 = kimberley_fv$V0,
  bodymass = kimberley_bodymass,
  bodyheight = kimberley_bodyheight
)

kimberley_avp
#> $MSS
#> [1] 8.591143
#> 
#> $MAC
#> [1] 10.58898
```

#### Using external load

**{shorts}** package also allows utilizing external load in estimating
FVP, as well as using FVP parameters to predict kinematic and kinetic
variables. External load is represented either with additional
**inertia** (i.e., weight vest), horizontal **resistance** (i.e., tether
device that create additional resistance or help, or a hill sprinting),
or both (i.e., a sled, which have both inertia and resistance due to
friction forces). One might also consider head and tail wind as a form
of resistance (or assistance).

Let’s see how theoretical model, assuming FVP is *determinant of
performance* (which I do not agree with, BTW), predicts changes in
sprint characteristics (i.e., $MSS$ and $MAC$) under different external
load conditions and magnitudes using Kimberley’s estimated FVP:

``` r
loads_df <- rbind(
  tibble(type = "Weight vest", magnitude = seq(0, 20, length.out = 100), inertia = magnitude, resistance = 0),
  tibble(type = "Tether", magnitude = seq(-50, 200, length.out = 100), inertia = 0, resistance = magnitude),
  tibble(type = "Sled", magnitude = seq(0, 40, length.out = 100), inertia = magnitude, resistance = magnitude * 9.81 * 0.4)
) %>%
  mutate(
    data.frame(shorts::convert_FVP(
      F0 = kimberley_fv$F0,
      V0 = kimberley_fv$V0,
      bodymass = kimberley_bodymass,
      bodyheight = kimberley_bodyheight,
      inertia = inertia,
      resistance = resistance
    ))
  ) 

loads_df %>%
  pivot_longer(cols = c(MSS, MAC), names_to = "parameter") %>%
  ggplot(aes(x = magnitude, y = value, color = parameter)) +
  theme_bw() + 
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_line() +
  facet_wrap(~type, scales = "free_x") +
  ylab(NULL)
```

<img src="man/figures/README-unnamed-chunk-23-1.png" width="90%" style="display: block; margin: auto;" />

Following figure depicts the effect on split times under different load
types and magnitudes, assuming FVP to be determinant of performance
(i.e., causal mechanism):

``` r
dist_df <- expand_grid(
  loads_df,
  distance = c(5, 10, 20, 30, 40)
) %>%
  mutate(
    time = predict_time_at_distance(distance, MSS, MAC),
    distance = factor(
      paste0(distance, "m"), levels = c("5m", "10m", "20m", "30m", "40m"))
  ) 

dist_df %>%
  ggplot(aes(x = magnitude, y = time, color = distance)) +
  theme_bw() + 
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_line() +
  facet_wrap(~type, scales = "free_x") +
  ylab("Time (s)")
```

<img src="man/figures/README-unnamed-chunk-24-1.png" width="90%" style="display: block; margin: auto;" />

One can use external resistance when predicting force or power:

``` r
shorts::predict_force_at_time(
  time = 0.5,
  MSS = 9,
  MAC = 7,
  bodymass = 75,
  inertia = 20,
  resistance = 50)
#> [1] 503.0126

shorts::predict_power_at_time(
  time = 0.5,
  MSS = 9,
  MAC = 7,
  bodymass = 75,
  inertia = 20,
  resistance = 50)
#> [1] 1458.593

shorts::predict_time_at_distance_FV(
  distance = 10,
  F0 = 750,
  V0 = 8,
  bodymass = 75,
  inertia = 20,
  resistance = 50)
#> [1] 2.259444
```

External resistances can also be utilized in the
[Optimization](#optimization) functions, covered later.

### Using corrections

You have probably noticed that estimated $MSS$ and $TAU$ were a bit too
high for splits data. Biased estimates are due to differences in
starting positions and *timing triggering methods* for certain
measurement approaches (e.g. starting behind first timing gate, or
allowing for body rocking).

Here I will provide quick summary (see more in Jovanović M., 2023).
Often, this bias in estimates is dealt with by using heuristic rule of
thumb of adding time correction (`time_correction`) to split times
(e.g. from 0.3-0.5 $sec$; see more in Haugen *et al.*, 2012). To do
this, just add time correction to time split:

``` r
kimberley_profile_fixed_TC <- shorts::model_timing_gates(
  distance = kimberley_data$distance,
  time = kimberley_data$time + 0.3
)

kimberley_profile_fixed_TC
#> Estimated model parameters
#> --------------------------
#>       MSS       MAC       TAU      PMAX 
#>  9.127769  6.625731  1.377624 15.119536 
#> 
#> Model fit estimators
#> --------------------
#>             R2        meanErr   meanErr_perc         minErr    minErr_perc 
#>    0.999971375    0.001009227    0.125589837   -0.007689947   -0.222961644 
#>         maxErr    maxErr_perc      maxAbsErr maxAbsErr_perc           RMSE 
#>    0.016398637    1.124735020    0.016398637    1.124735020    0.008139867 
#>      RMSE_perc            MAE       MAE_perc 
#>    0.477039255    0.006393853    0.285701750

summary(kimberley_profile_fixed_TC)
#> 
#> Formula: time ~ predict_time_at_distance(distance, MSS, MAC)
#> 
#> Parameters:
#>     Estimate Std. Error t value Pr(>|t|)    
#> MSS  9.12777    0.05355   170.4 7.11e-09 ***
#> MAC  6.62573    0.06569   100.9 5.79e-08 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 0.009969 on 4 degrees of freedom
#> 
#> Number of iterations to convergence: 5 
#> Achieved convergence tolerance: 1.49e-08

coef(kimberley_profile_fixed_TC)
#>      MSS      MAC 
#> 9.127769 6.625731
```

Instead of providing for `TC`, this parameter can be estimated using
`shorts::model_timing_gates_TC()`.

``` r
kimberley_profile_TC <- shorts::model_timing_gates_TC(
  distance = kimberley_data$distance,
  time = kimberley_data$time
)

kimberley_profile_TC
#> Estimated model parameters
#> --------------------------
#>       MSS       MAC       TAU      PMAX 
#>  8.974835  7.267917  1.234857 16.307090 
#> 
#> Estimated model corrections
#> --------------------------
#>         TC 
#> -0.2346537 
#> 
#> Model fit estimators
#> --------------------
#>             R2        meanErr   meanErr_perc         minErr    minErr_perc 
#>   9.999997e-01   1.854313e-11   1.816275e-03  -1.180734e-03  -6.237372e-02 
#>         maxErr    maxErr_perc      maxAbsErr maxAbsErr_perc           RMSE 
#>   1.209466e-03   5.974772e-02   1.209466e-03   6.237372e-02   7.983565e-04 
#>      RMSE_perc            MAE       MAE_perc 
#>   3.748224e-02   6.586034e-04   2.823532e-02
```

Instead of estimating `TC`, **{shorts}** package features a method of
estimating flying start distance (`FD`):

``` r
kimberley_profile_FD <- shorts::model_timing_gates_FD(
  distance = kimberley_data$distance,
  time = kimberley_data$time
)

kimberley_profile_FD
#> Estimated model parameters
#> --------------------------
#>       MSS       MAC       TAU      PMAX 
#>  9.002681  6.991283  1.287701 15.735073 
#> 
#> Estimated model corrections
#> --------------------------
#>        FD 
#> 0.3015635 
#> 
#> Model fit estimators
#> --------------------
#>             R2        meanErr   meanErr_perc         minErr    minErr_perc 
#>   1.000000e+00   6.446653e-07   3.182633e-04  -4.036158e-04  -1.281727e-02 
#>         maxErr    maxErr_perc      maxAbsErr maxAbsErr_perc           RMSE 
#>   4.557031e-04   1.056580e-02   4.557031e-04   1.281727e-02   2.758661e-04 
#>      RMSE_perc            MAE       MAE_perc 
#>   8.402638e-03   2.367538e-04   7.829105e-03
```

If you want to use fixed `FD` parameter (e.g., when you know what is the
flying distance), in a similar vein of using fixed `TC` correction, use:

``` r
kimberley_profile_fixed_FD <- shorts::model_timing_gates_FD(
  distance = kimberley_data$distance,
  time = kimberley_data$time,
  FD = 0.5
)

kimberley_profile_fixed_FD
#> Estimated model parameters
#> --------------------------
#>       MSS       MAC       TAU      PMAX 
#>  9.178464  6.231413  1.472935 14.298700 
#> 
#> Estimated model corrections
#> --------------------------
#>  FD 
#> 0.5 
#> 
#> Model fit estimators
#> --------------------
#>             R2        meanErr   meanErr_perc         minErr    minErr_perc 
#>    0.999973897    0.001247560    0.177401418   -0.007903690   -0.250990469 
#>         maxErr    maxErr_perc      maxAbsErr maxAbsErr_perc           RMSE 
#>    0.015461986    1.335231979    0.015461986    1.335231979    0.007939415 
#>      RMSE_perc            MAE       MAE_perc 
#>    0.564926522    0.006718969    0.349905002
```

There are other corrections involving time correction (`TC`), distance
correction (`DC`), flying distance correction (`FD`), and time and
distance corrections (`TC+DC`). They are implemented in the
`model_timing_gates_` and `model_time_distance_` functions. The
difference between the `model_timing_gates_` and `model_time_distance_`
is in reversing predictor and outcome variables.

### Cross-Validation (CV)

`model_` family of functions come with CV feature that is performed by
setting the function parameter CV to desired number of folds. This
feature is very useful for checking model parameters robustness and
model predictions on unseen data. Let’s use Kimberley again, but this
time perform special kind of CV, leave-one-out-cross-validation (LOOCV):

``` r
kimberley_profile_CV <- shorts::model_timing_gates(
  distance = kimberley_data$distance,
  time = kimberley_data$time,
  # To perform LOOCV number of folds is equal to 
  # number of observations
  CV = nrow(kimberley_data)
)

kimberley_profile_CV
#> Estimated model parameters
#> --------------------------
#>        MSS        MAC        TAU       PMAX 
#>  8.5911431 10.5889817  0.8113285 22.7428642 
#> 
#> Model fit estimators
#> --------------------
#>             R2        meanErr   meanErr_perc         minErr    minErr_perc 
#>    0.999655312   -0.003093343   -0.538602775   -0.052934568   -4.571206233 
#>         maxErr    maxErr_perc      maxAbsErr maxAbsErr_perc           RMSE 
#>    0.026991611    0.857148665    0.052934568    4.571206233    0.027788752 
#>      RMSE_perc            MAE       MAE_perc 
#>    1.939218713    0.023333409    1.192632747 
#> 
#> 
#> Cross-Validation
#> ------------------------------
#> Parameters:
#>   .fold      MSS      MAC       TAU     PMAX
#> 1     1 8.693800 10.15512 0.8561005 22.07163
#> 2     2 8.560667 10.76319 0.7953649 23.03503
#> 3     3 8.394674 11.05010 0.7596925 23.19049
#> 4     4 8.571600 10.75079 0.7972998 23.03786
#> 5     5 8.608052 10.58783 0.8130141 22.78514
#> 6     6 8.599600 10.54821 0.8152661 22.67760
#> 
#> Testing model fit estimators (overall):
#>             R2        meanErr   meanErr_perc         minErr    minErr_perc 
#>     0.99901083    -0.01236576    -0.85484642    -0.08009034    -5.96012088 
#>         maxErr    maxErr_perc      maxAbsErr maxAbsErr_perc           RMSE 
#>     0.03444978     1.09399112     0.08009034     5.96012088     0.04742764 
#>      RMSE_perc            MAE       MAE_perc 
#>     2.59202750     0.03923868     1.72270380
```

Radar gun data often comes with much more observations, thus we can set
smaller CV parameter:

``` r
jim_profile_CV <- shorts::model_radar_gun(
  time = jim_data$time,
  velocity = jim_data$velocity,
  CV = 10
)

jim_profile_CV
#> Estimated model parameters
#> --------------------------
#>        MSS        MAC        TAU       PMAX 
#>  7.9980115  8.9987070  0.8887956 17.9929403 
#> 
#> Estimated model corrections
#> --------------------------
#>            TC 
#> -0.0001104572 
#> 
#> Model fit estimators
#> --------------------
#>             R2        meanErr   meanErr_perc         minErr    minErr_perc 
#>   9.992441e-01  -2.481182e-08           -Inf  -1.640451e-01           -Inf 
#>         maxErr    maxErr_perc      maxAbsErr maxAbsErr_perc           RMSE 
#>   1.511234e-01   2.332511e+00   1.640451e-01            Inf   5.050254e-02 
#>      RMSE_perc            MAE       MAE_perc 
#>            Inf   3.927236e-02            Inf 
#> 
#> 
#> Cross-Validation
#> ------------------------------
#> Parameters:
#>    .fold      MSS      MAC       TAU     PMAX
#> 1      1 7.997012 8.994930 0.8890577 17.98314
#> 2      2 7.997697 9.003190 0.8883182 18.00120
#> 3      3 7.997266 8.997429 0.8888391 17.98871
#> 4      4 7.998063 8.997914 0.8888796 17.99147
#> 5      5 7.999258 8.998932 0.8889119 17.99619
#> 6      6 7.998143 9.002309 0.8884545 18.00044
#> 7      7 7.998979 8.995074 0.8892622 17.98785
#> 8      8 7.998321 9.001374 0.8885668 17.99897
#> 9      9 7.998101 9.006872 0.8879998 18.00947
#> 10    10 7.997287 8.988672 0.8897072 17.97125
#> 
#> Testing model fit estimators (overall):
#>             R2        meanErr   meanErr_perc         minErr    minErr_perc 
#>   9.992387e-01  -1.379258e-05           -Inf  -1.616499e-01           -Inf 
#>         maxErr    maxErr_perc      maxAbsErr maxAbsErr_perc           RMSE 
#>   1.507892e-01   2.327353e+00   1.616499e-01            Inf   5.068118e-02 
#>      RMSE_perc            MAE       MAE_perc 
#>            Inf   3.944147e-02            Inf
```

### Optimization

Using the method outlined in Samozino *et al* (2022), one can find the
optimal profiles, as well as the profile imbalance (compared to the
optimal), for both sprint profiles (i.e., $MSS$ and $MAC$) and
Force-Velocity (FV). In addition to this, one can *probe* the profiles
(i.e., increase $V0$ / $F0$ or $MSS$ / $MAC$ for say 2.5% to check which
improvement yield more improvement in sprint time). The following graph
depicts estimate profile imbalances. Note that \>100% is velocity
deficit (i.e., increasing *velocity*; $MSS$ or $V0$; will yield more
improvement in sprint times), while \<100% is *force* deficit.

``` r
MSS <- 10
MAC <- 8
bodymass <- 75

fv <- create_FVP(MSS, MAC, bodymass)

opt_df <- tibble(
  dist = seq(5, 50, by = 5)
) %>%
  mutate(
    `Sprint Profile` = optimal_MSS_MAC(
      distance = dist,
      MSS,
      MAC
    )[["profile_imb"]],
    `FV Profile` = optimal_FV(
      distance = dist,
      fv$F0,
      fv$V0,
      bodymass
    )[["profile_imb"]],
    `FV Profile (PeakPower)` = optimal_FV(
      distance = dist,
      fv$F0,
      fv$V0,
      bodymass,
      method = "peak"
    )[["profile_imb"]],
    `Probe FV` = probe_FV(
      distance = dist,
      fv$F0,
      fv$V0,
      bodymass
    )[["profile_imb"]],
    `Probe MSS/MAC` = probe_MSS_MAC(
      distance = dist,
      MSS,
      MAC
    )[["profile_imb"]]
  ) %>%
  pivot_longer(-dist, names_to = "profile")

opt_dist <- tibble(
  `Sprint Profile` = find_optimal_distance(
    MSS,
    MAC,
    optimal_func = optimal_MSS_MAC
  ),
  `FV Profile` = find_optimal_distance(
    fv$F0,
    fv$V0,
    bodymass,
    optimal_func = optimal_FV
  ),
  `FV Profile (PeakPower)` = find_optimal_distance(
    fv$F0,
    fv$V0,
    bodymass,
    optimal_func = optimal_FV,
    method = "peak"
  ),
  `Probe FV` = find_optimal_distance(
    fv$F0,
    fv$V0,
    bodymass,
    optimal_func = probe_FV
  ),
  `Probe MSS/MAC` = find_optimal_distance(
    MSS,
    MAC,
    optimal_func = probe_MSS_MAC
  )
) %>%
  pivot_longer(cols = 1:5, names_to = "profile")

ggplot(opt_df, aes(x = dist, y = value, color = profile)) +
  theme_bw() +
  geom_hline(yintercept = 100, linetype = "dashed", alpha = 0.6) +
  geom_line() +
  geom_point(data = opt_dist, aes(x = value, y = 100), size = 2) +
  xlab("Distance (m)") +
  ylab("Profile imbalance")
```

<img src="man/figures/README-unnamed-chunk-32-1.png" width="90%" style="display: block; margin: auto;" />

### Creating your own data

One can use the **{shorts}}** package for simulating data by using two
functions: `create_sprint_trace()` and `create_timing_gates_splits()`:

``` r
create_sprint_trace(
  MSS = 7, MAC = 6,
  distance = c(5, 10, 20, 30, 40),
  # Add flying distance
  FD = 0.5)
#>       time distance velocity acceleration sprint_time sprint_distance
#> 1 1.241196        5 5.333967   1.42802852    1.674709             5.5
#> 2 2.100178       10 6.202142   0.68387846    2.533690            10.5
#> 3 3.625761       20 6.784214   0.18495957    4.059274            20.5
#> 4 5.079956       30 6.937957   0.05318010    5.513469            30.5
#> 5 6.515848       40 6.981879   0.01553227    6.949361            40.5

create_timing_gates_splits(
  MSS = 7, MAC = 6,
  gates = c(5, 10, 20, 30, 40),
  # Add time-shift (i.e., rection time of 200ms)
  TC = 0.2)
#> [1] 1.779729 2.652703 4.185497 5.641381 7.077741
```

Using `predict_` family of functions, one can predict kinematics and
kinetics using *known* $MSS$ and $MAC$ parameters.

## Publications

1.  Jovanović, M., Vescovi, J.D. (2022). **{shorts}: An R Package for
    Modeling Short Sprints**. *International Journal of Strength and
    Conditioning, 2(1).* <https://doi.org/10.47206/ijsc.v2i1.74>

2.  Jovanović M. (2023). **Bias in estimated short sprint profiles using
    timing gates due to the flying start: simulation study and proposed
    solutions.** *Computer Methods in Biomechanics and Biomedical
    Engineering:1–11*. <https://doi.org/10.1080/10255842.2023.2170713>

3.  Vescovi, JD and Jovanović, M. (2021). **Sprint Mechanical
    Characteristics of Female Soccer Players: A Retrospective Pilot
    Study to Examine a Novel Approach for Correction of Timing Gate
    Starts.** *Front Sports Act Living 3: 629694, 2021.*
    <https://doi.org/10.3389/fspor.2021.629694>

## Citation

To cite **{shorts}**, please use the following command to get the BibTex
entry:

``` r
citation("shorts")
```

## References

Please refer to these publications for more information on short sprints
modeling using mono-exponential equation:

Chelly SM, Denis C. 2001. Leg power and hopping stiffness: relationship
with sprint running performance: Medicine and Science in Sports and
Exercise:326–333. DOI: 10.1097/00005768-200102000-00024.

Clark KP, Rieger RH, Bruno RF, Stearne DJ. 2017. The NFL Combine 40-Yard
Dash: How Important is Maximum Velocity? Journal of Strength and
Conditioning Research:1. DOI: 10.1519/JSC.0000000000002081.

Clavel, P., Leduc, C., Morin, J.-B., Buchheit, M., & Lacome, M. (2023).
Reliability of individual acceleration-speed profile in-situ in elite
youth soccer players. Journal of Biomechanics, 153, 111602.
<https://doi.org/10.1016/j.jbiomech.2023.111602>

Furusawa K, Hill AV, and Parkinson JL. The dynamics of” sprint” running.
Proceedings of the Royal Society of London. Series B, Containing Papers
of a Biological Character 102 (713): 29-42, 1927

Greene PR. 1986. Predicting sprint dynamics from maximum-velocity
measurements. Mathematical Biosciences 80:1–18. DOI:
10.1016/0025-5564(86)90063-5.

Haugen TA, Tønnessen E, Seiler SK. 2012. The Difference Is in the Start:
Impact of Timing and Start Procedure on Sprint Running Performance:
Journal of Strength and Conditioning Research 26:473–479. DOI:
10.1519/JSC.0b013e318226030b.

Samozino P, Rabita G, Dorel S, Slawinski J, Peyrot N, Saez de Villarreal
E, Morin J-B. 2016. A simple method for measuring power, force, velocity
properties, and mechanical effectiveness in sprint running: Simple
method to compute sprint mechanics. Scandinavian Journal of Medicine &
Science in Sports 26:648–658. DOI: 10.1111/sms.12490.

Samozino P. 2018. A Simple Method for Measuring Force, Velocity and
Power Capabilities and Mechanical Effectiveness During Sprint Running.
In: Morin J-B, Samozino P eds. Biomechanics of Training and Testing.
Cham: Springer International Publishing, 237–267. DOI:
10.1007/978-3-319-05633-3_11.

Samozino P, Peyrot N, Edouard P, Nagahara R, Jimenez‐Reyes P,
Vanwanseele B, Morin J. 2022. Optimal mechanical force‐velocity profile
for sprint acceleration performance.Scandinavian Journal of Medicine &
Science in Sports 32:559–575. DOI: 10.1111/sms.14097.

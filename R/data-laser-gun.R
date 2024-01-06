#' Laser Gun Data
#'
#' Performance of 35m sprint by a youth basketball player done using standing start. Sample was
#'      collected by laser gun (CMP3 Distance Sensor, Noptel Oy, Oulu, Finland)
#'      and was sampled at a rate of 2.56 KHz. A polynomial function modeling the relationship
#'      between distance and time was employed and subsequently resampled at a frequency
#'      of 1,000 Hz using Musclelab™ v10.232.107.5298, a software developed by Ergotest
#'      Technology AS located in Langesund, Norway. Data was further modified by calculating
#'      raw acceleration using dv/dt (using smoothed velocity provided by the system), and then
#'      smoothed out using 4th-order Butterworth filter with a cutoff frequency of 1 Hz.
#' @format Data frame with 6 variables and 4805 observations:
#' \describe{
#'    \item{time}{Time vector in seconds}
#'    \item{distance}{Distance vector in meters}
#'    \item{velocity}{Smoothed velocity vector in m/s; this represent step-averaged velocity}
#'    \item{raw_velocity}{Raw velocity vector in m/s}
#'    \item{raw_acceleration}{Raw acceleration vector in m/s/s; calculated using difference in
#'     smoothed velocity divided by time difference (i.e., dv/dt method of derivation)}
#'    \item{butter_acceleration}{Smoothed acceleration vector in m/s/s; smoothed out using
#'    4th-order Butterworth filter with a cutoff frequency of 1 Hz}
#' }
#' @usage data(laser_gun_data)
"laser_gun_data"

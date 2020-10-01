#' Get Air Resistance
#'
#' \code{get_air_resistance} estimates air resitance in Newtons
#'
#' @param velocity Instantaneous running velocity in meters per second (m/s)
#' @param bodymass In kilograms (kg)
#' @param bodyheight In meters (m)
#' @param barometric_pressure In Torrs
#' @param air_temperature In Celzius (C)
#' @param wind_velocity In meters per second (m/s). Use negative number as head
#'     wind, and positive number as back wind
#' @return Air resistance in Newtons (N)
#' @export
#' @examples
#' get_air_resistance(
#'   velocity = 5,
#'   bodymass = 80,
#'   bodyheight = 1.90,
#'   barometric_pressure = 760,
#'   air_temperature = 16,
#'   wind_velocity = -0.5
#'  )
#' @references
#'     Arsac LM, Locatelli E. 2002. Modeling the energetics of 100-m running by using speed curves of
#'         world champions. Journal of Applied Physiology 92:1781–1788.
#'         DOI: 10.1152/japplphysiol.00754.2001.
#'
#'     Samozino P, Rabita G, Dorel S, Slawinski J, Peyrot N, Saez de Villarreal E, Morin J-B. 2016.
#'         A simple method for measuring power, force, velocity properties, and mechanical
#'         effectiveness in sprint running: Simple method to compute sprint mechanics.
#'         Scandinavian Journal of Medicine & Science in Sports 26:648–658. DOI: 10.1111/sms.12490.
#'
#'     van Ingen Schenau GJ, Jacobs R, de Koning JJ. 1991. Can cycle power predict sprint running
#'        performance? European Journal of Applied Physiology and Occupational Physiology 63:255–260.
#'        DOI: 10.1007/BF00233857.
get_air_resistance <- function(velocity,
                               bodymass = 75,
                               bodyheight = 1.75,
                               barometric_pressure = 760,
                               air_temperature = 25,
                               wind_velocity = 0) {

  air_density <- 1.293 * (barometric_pressure/760) * (273/(273 + air_temperature))
  frontal_area <- (0.2025 * (bodyheight^0.725) * (bodymass^0.425)) * 0.266
  drag_coefficient <- 0.9

  k <- 0.5 * air_density * frontal_area * drag_coefficient

  # Return air resistance
  k * (velocity - wind_velocity)^2
}


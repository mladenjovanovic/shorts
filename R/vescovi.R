#' Vescovi Timing Gates Sprint Times
#'
#' Timing gates sprint times involving 52 female athletes. Timing gates were located at
#'     5m, 10m, 20m, 30m, and 35m. See \strong{Details} for more information.
#'
#' @details
#' This data-set represents sub-set of data from a total of 220 high-level female athletes
#'    (151 soccer players and 69 field hockey players).  Using a random number generator,
#'    a total of 52 players (35 soccer and 17 field hockey) were selected for this data-set.
#'    Soccer players were older (24.6±3.6 vs. 18.9±2.7 yr, p < 0.001), however there were no
#'    differences for height (167.3±5.9 vs. 167.0±5.7 cm, p = 0.886),
#'    body mass (62.5±5.9 vs. 64.0±9.4 kg, p = 0.500) or any sprint interval time (p > 0.650).
#'
#'    The protocol for assessing linear sprint speed has been described previously (Vescovi 2014,
#'    2016, 2012) and was identical for each cohort.  Briefly, all athletes performed a standardized
#'    warm-up that included general exercises such as jogging, shuffling, multi-directional movements,
#'    and dynamic stretching exercises. Infrared timing gates (Brower Timing, Utah) were positioned at
#'    the start line and at 5, 10, 20, and 35 meters at a height of approximately 1.0 meter.
#'    Participants stood with their lead foot positioned approximately 5 cm behind the initial infrared beam
#'    (i.e., start line). Only forward movement was permitted (no leaning or rocking backwards) and timing
#'    started when the laser of the starting gate was triggered. The best 35 m time, and all associated split
#'    times were kept for analysis.  The assessment of linear sprints using infrared timing gates does not
#'    require familiarization (Moir, Button, Glaister, and Stone 2004).
#'
#' @format Data frame with 17 variables and 52 observations:
#' \describe{
#'    \item{Team}{Team or sport. Contains the following levels: 'W Soccer' (Women Soccer),
#'    'FH Sr' (Field Hockey Seniors), 'FH U21' (Field Hockey Under 21), and 'FH U17' (Field Hockey Under 17)}
#'    \item{Surface}{Type of testing surface. Contains the following levels: 'Hard Cours' and 'Natural Grass'}
#'    \item{Athlete}{Athlete ID}
#'    \item{Age}{Athlete age in years}
#'    \item{Height}{Body height in cm}
#'    \item{Bodyweight}{Body weight in kg}
#'    \item{BMI}{Body Mass Index}
#'    \item{BSA}{Body Surface Area. Calculated using Mosteller equation \code{sqrt((height/weight)/3600)}}
#'    \item{5m}{Time in seconds at 5m gate}
#'    \item{10m}{Time in seconds at 10m gate}
#'    \item{20m}{Time in seconds at 20m gate}
#'    \item{30m}{Time in seconds at 30m gate}
#'    \item{35m}{Time in seconds at 35m gate}
#'    \item{10m-5m split}{Split time in seconds between 10m and 5m gate}
#'    \item{20m-10m split}{Split time in seconds between 20m and 10m gate}
#'    \item{30m-20m split}{Split time in seconds between 30m and 20m gate}
#'    \item{35m-30m split}{Split time in seconds between 35m and 30m gate}
#' }
#' @usage data(vescovi)
#' @author Jason D. Vescovi\cr
#'     University of Toronto\cr
#'     Faculty of Kinesiology and Physical Education\cr
#'     Graduate School of Exercise Science\cr
#'     Toronto, ON Canada\cr
#'     \email{vescovij@@gmail.com}
#'
#' @references
#'     Moir G, Button C, Glaister M, Stone MH (2004). "Inlfuence of Familiarization on the Reliability
#'     of Vertical Jump and Acceleration Sprinting Performance in Physically Active Men."
#'     The Journal of Strength and Conditioning Research, 18(2), 276. ISSN 1064-8011, 1533-4287.
#'     doi:10.1519/R-13093.1.
#'
#'     Vescovi JD (2012). "Sprint Speed Characteristics of High-Level American Female Soccer
#'     Players: Female Athletes in Motion (FAiM) Study." Journal of Science and Medicine in
#'     Sport, 15(5), 474-478. ISSN 14402440. doi:10.1016/j.jsams.2012.03.006.
#'
#'     Vescovi JD (2014). "Impact of Maximum Speed on Sprint Performance During High-Level
#'     Youth Female Field Hockey Matches: Female Athletes in Motion (FAiM) Study." International
#'     Journal of Sports Physiology and Performance, 9(4), 621-626. ISSN 1555-0265,
#'    1555-0273. doi:10.1123/ijspp.2013-0263.
#'
#'    Vescovi JD (2016). "Locomotor, Heart-Rate, and Metabolic Power Characteristics of Youth
#'    Women's Field Hockey: Female Athletes in Motion (FAiM) Study." Research Quarterly for
#'    Exercise and Sport, 87(1), 68-77. ISSN 0270-1367, 2168-3824.
#'    doi:10.1080/02701367.2015.1124972.
"vescovi"

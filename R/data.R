#' World Records for Male Athletes
#'
#' A dataset containing world record times for male athletes across 11 distances from 1912 to 2023 in long format.
#'
#' @format A data frame with 1,232 rows and 3 columns:
#' \describe{
#'   \item{Year}{The year of the world record, ranging from 1912 to 2023}
#'   \item{Distance}{The running distance in meters. Possible values are 100, 200, 400, 800, 1500, 1609, 3000, 5000, 10000, 21100 (half marathon), and 42195 (marathon)}
#'   \item{Time}{World record time in seconds for the given year and distance}
#' }
#' @source \url{https://www.worldathletics.org/records/by-category/world-records}
"WR_males"

#' World Records for Female Athletes
#'
#' A dataset containing world record times for female athletes across 11 distances from 1912 to 2023 in long format.
#'
#' @format A data frame with 1,232 rows and 3 columns:
#' \describe{
#'   \item{Year}{The year of the world record, ranging from 1912 to 2023}
#'   \item{Distance}{The running distance in meters. Possible values are 100, 200, 400, 800, 1500, 1609, 3000, 5000, 10000, 21100 (half marathon), and 42195 (marathon)}
#'   \item{Time}{World record time in seconds for the given year and distance}
#' }
#' @source \url{https://www.worldathletics.org/records/by-category/world-records}
"WR_females"


#' Elite Runners Performance Data
#'
#' A dataset containing performance data for elite runners across various distances.
#'
#' @format A data frame with 115 rows and 9 variables:
#' \describe{
#'   \item{Athlete}{Name of the athlete}
#'   \item{Distance}{Running distance in meters}
#'   \item{h}{Hours component of the running time}
#'   \item{m}{Minutes component of the running time}
#'   \item{s}{Seconds component of the running time}
#'   \item{Time}{Total running time in seconds}
#'   \item{VO2}{Measured VO2 max of the athlete, when available}
#'   \item{BM}{Body mass of the athlete in kg}
#'   \item{height}{Height of the athlete in cm}
#' }
#' @source \url{https://www.worldathletics.org/}
"elite_runners"


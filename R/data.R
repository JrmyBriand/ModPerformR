#' World Records for Male Athletes
#'
#' A dataset containing world records for various events for male athletes.
#'
#' @format A data frame with [number of rows] rows and [number of columns] columns:
#' \describe{
#'   \item{event}{Name of the event}
#'   \item{time}{Record time}
#'   \item{athlete}{Name of the athlete}
#'   \item{country}{Athlete's country}
#'   \item{date}{Date when the record was set}
#'   ...
#' }
#' @source \url{https://www.worldathletics.org/records/by-category/world-records}
"wr_males"

#' World Records for Female Athletes
#'
#' A dataset containing world records for various events for female athletes.
#'
#' @format A data frame with [number of rows] rows and [number of columns] columns:
#' \describe{
#'   \item{event}{Name of the event}
#'   \item{time}{Record time}
#'   \item{athlete}{Name of the athlete}
#'   \item{country}{Athlete's country}
#'   \item{date}{Date when the record was set}
#'   ...
#' }
#' @source \url{https://www.worldathletics.org/records/by-category/world-records}
"wr_females"


#' Elite Runners Performance Data
#'
#' A dataset containing performance data for elite runners across various distances.
#'
#' @format A data frame with [number of rows] rows and 9 variables:
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


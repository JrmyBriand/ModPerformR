#' Convert Running Velocity to Power
#'
#' This function calculates power output based on running velocity and distance,
#' using the di Prampero equation.
#'
#' @param velocity Numeric. Running velocity in m/s.
#' @param distance Numeric. Running distance in meters.
#' @param C1 Numeric. Parameter of the equation. Default is 3.86.
#' @param C2 Numeric. Parameter of the equation. Default is 0.4.
#' @param C3 Numeric. Parameter of the equation. Default is 2.
#' @param BMR Numeric. Basal metabolic rate in W/kg. Default is 1.2.
#' @param BSA Numeric. Body surface area in m^2. Default is 1.8.
#' @param BM Numeric. Body mass in kg. Default is 70.
#' @param eff Numeric. Assumed mechanical efficiency. Default is 0.2.
#'
#' @return Numeric. Power output in watts.
#'
#' @examples
#' di_prampero_running(velocity = 5, distance = 1000)
#' di_prampero_running(velocity = 6, distance = 5000, BM = 60, BSA = 1.7)
#'
#' @references
#'Di Prampero, P. E., and S. Fusi. “Il Costo Energetico Della Corsa. / Energy Cost of Running.”
#'Medicina Dello Sport; Rivista Di Fisiopatologia Dello Sport 54, no. 1 (2001): 43–50.
#'http://articles.sirc.ca/search.cfm?id=S-925527 https://search.ebscohost.com/login.aspx?direct=true&db=s3h&AN=SPHS-925527&amp;lang=fr&site=ehost-live.
#'
#' @export
di_prampero_running <- function(velocity, distance, C1 = 3.86, C2 = 0.4, C3 = 2,
                                BMR = 1.2, BSA = 1.8, BM = 70, eff = 0.2) {

  # Check for positive velocity
  if (velocity <= 0) {
    stop("Velocity must be positive")
  }

  # Check for positive distance
  if (distance <= 0) {
    stop("Distance must be positive")
  }

  p <- BMR + C1 * velocity + C2 * BSA/BM * velocity^3 + C3*velocity^3/distance
  power <- eff * p
  power <- power * BM
  return(power)
}


#' Inverse Di Prampero Running Function
#'
#' This function calculates the distance covered given a power output and duration,
#' using an inverse of the Di Prampero running equation.
#'
#' @param Power Numeric. Power output in watts.
#' @param t Numeric. Running duration in seconds.
#' @param C1 Numeric. Energy cost of running per unit distance. Default is 3.86 J/(kg*m).
#' @param C2 Numeric. Coefficient related to air resistance. Default is 0.4.
#' @param C3 Numeric. Coefficient related to acceleration. Default is 2.
#' @param BMR Numeric. Basal metabolic rate in W/kg. Default is 1.2.
#' @param BSA Numeric. Body surface area in m^2. Default is 1.8.
#' @param BM Numeric. Body mass in kg. Default is 70.
#' @param eff Numeric. Assumed mechanical efficiency. Default is 0.2.
#'
#' @return Numeric. Estimated distance covered in meters.
#'
#' @examples
#' inv_di_prampero_running(Power = 300, t = 600)
#'
#' @references
#'Di Prampero, P. E., and S. Fusi. “Il Costo Energetico Della Corsa. / Energy Cost of Running.”
#'Medicina Dello Sport; Rivista Di Fisiopatologia Dello Sport 54, no. 1 (2001): 43–50.
#'http://articles.sirc.ca/search.cfm?id=S-925527 https://search.ebscohost.com/login.aspx?direct=true&db=s3h&AN=SPHS-925527&amp;lang=fr&site=ehost-live.
#' @export
inv_di_prampero_running <- function(Power, t, C1 = 3.86, C2 = 0.4, C3 = 2,
                                    BMR = 1.2, BSA = 1.8, BM = 70, eff = 0.2) {
  # Input validation
  if (Power <= 0) stop("Power must be positive")
  if (t <= 0) stop("Running duration (t) must be positive")
  if (BM <= 0) stop("Body mass (BM) must be positive")
  if (eff <= 0 || eff > 1) stop("Efficiency (eff) must be between 0 and 1")

  # Define the error function to minimize
  error_power <- function(velocity) {
    distance <- velocity * t
    di_prampero <- di_prampero_running(velocity, distance, C1, C2, C3, BMR, BSA, BM, eff)
    return(abs(Power - di_prampero))
  }

  # Initial guess for velocity (m/s)
  v_init <- 5

  # Use optimisation to find the velocity that minimizes the error
  result <- optim(v_init, error_power, method = "Brent", lower = 0, upper = 100)

  # Extract the optimal velocity
  optimal_velocity <- result$par

  # Calculate and return the distance
  distance <- optimal_velocity * t
  return(distance)
}

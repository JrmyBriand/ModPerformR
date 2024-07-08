#' Péronnet-Thibault Running Performance Model
#'
#' This function calculates the power output for a given running duration based on
#' the model proposed by Péronnet and Thibault (1989).
#'
#' @param A Anaerobic capacity in J/kg
#' @param MAP Maximal aerobic power in w/kg
#' @param E Endurance in w/kg/log(time in s)
#' @param t Running duration in seconds
#' @param BMR Basal metabolic rate in w/kg
#' @param Tmap Running duration eliciting MAP in seconds
#'
#' @return Power output in W/kg
#'
#' @references
#' Péronnet, F., & Thibault, G. (1989). Mathematical analysis of running performance
#' and world running records. Journal of applied physiology, 67(1), 453-465.
#'
#' @examples
#' # Example usage (replace with realistic values):
#' peronnet_thibault_model(t = 600, A = 1500, MAP = 25, E = -1.5, Tmap = 420, BMR = 1.2)
#'
#' @export
peronnet_thibault_model <- function(t, A, MAP, E, Tmap,  BMR = 1.2) {

  if (A <= 0) stop("Anaerobic capacity (A) must be positive")
  if (MAP <= 0) stop("Maximal aerobic power (MAP) must be positive")
  if (E >= 0) stop("Endurance (E) must be negative")
  if (t <= 0) stop("Running duration (t) must be positive")
  if (BMR <= 0) stop("Basal metabolic rate (BMR) must be positive")
  if (Tmap <= 0) stop("Time to MAP (Tmap) must be positive")


  if(t < Tmap) {
    S <- A
    p_anaero <- S/t * (1 - exp(-t/20))
    B <- MAP - BMR
    p_aero <- 1/t * (BMR*t + B*t + 30*B * (exp(-t/30) - 1))
    power_output <-  p_anaero + p_aero
  }
  if(t >= Tmap){
    S <- A * (1 - 0.233*log(t/Tmap))
    p_anaero <- S/t * (1 - exp(-t/20))
    B <- MAP - BMR + (E * log(t/Tmap))
    p_aero <- 1/t * (BMR*t + B*t + 30*B * (exp(-t/30) - 1))
    power_output <-  p_anaero + p_aero
  }


  return(power_output)
}

#' Critical Power Model
#'
#' This function calculates the power output based on the Critical Power model.
#'
#' @param t Numeric. Running duration in seconds.
#' @param cp Numeric. Critical power in w.
#' @param Wprime Numeric. Anaerobic energy reserve in J.
#'
#' @return Numeric. Power output in watts.
#'
#' @references
#' Hill, D. W. "The Critical Power Concept. A Review." Sports Medicine 16, no. 4 (October 1993): 237–54. https://doi.org/10.2165/00007256-199316040-00003.
#'
#' Monod, H., and J. Scherrer. "The Work Capacity of a Synergic Muscle Group." Ergonomics 8, no. 3 (July 1, 1965): 329–38. https://doi.org/10.1080/00140136508930810.
#'
#' Moritani, T., A. Nagata, H. A. deVries, and M. Muro. "Critical Power as a Measure of Physical Work Capacity and Anaerobic Threshold." Ergonomics 24, no. 5 (May 1981): 339–50. https://doi.org/10.1080/00140138108924856.
#'
#' @examples
#' critical_power_model(t = 300, cp = 300, Wprime = 20000)
#'
#' @export
critical_power_model <- function(t, cp, Wprime) {
  if (t <= 0) {
    stop("Running duration (t) must be positive")
  }
  if (cp <= 0) {
    stop("Critical power (cp) must be positive")
  }
  if (Wprime <= 0) {
    stop("Anaerobic energy reserve (Wprime) must be positive")
  }
  power_output <- cp + Wprime / t
  return(power_output)
}

#' 3-Parameter Critical Power Model
#'
#' This function calculates the power output based on the 3-Parameter Critical Power model.
#'
#' @param t Numeric. Running duration in seconds. Must be positive.
#' @param cp Numeric. Critical power in w. Must be positive.
#' @param Wprime Numeric. Anaerobic energy reserve in J. Must be positive.
#' @param Pmax Numeric. Maximal instantaneous power in w. Must be positive and greater than cp.
#'
#' @return Numeric. Power output in watts.
#'
#' @references
#' Morton, R. H. "A 3-Parameter Critical Power Model." Ergonomics 39, no. 4 (April 1996): 611–19. https://doi.org/10.1080/00140139608964484.
#'
#' @examples
#' critical_power_model_3p(t = 300, cp = 300, Wprime = 20000, Pmax = 1000)
#'
#' @export
critical_power_model_3p <- function(t, cp, Wprime, Pmax) {
  if (t <= 0) {
    stop("Running duration (t) must be positive")
  }
  if (cp <= 0) {
    stop("Critical power (cp) must be positive")
  }
  if (Wprime <= 0) {
    stop("Anaerobic energy reserve (Wprime) must be positive")
  }
  if (Pmax <= 0) {
    stop("Maximal instantaneous power (Pmax) must be positive")
  }
  if (Pmax <= cp) {
    stop("Maximal instantaneous power (Pmax) must be greater than critical power (cp)")
  }

  power_output <- cp + Wprime / t - (Wprime / (Pmax - cp))
  return(power_output)
}



#' Péronnet-Thibault Running Performance Model
#'
#' This function calculates the power output for a given running duration based on
#' the model proposed by Péronnet and Thibault (1989).
#'
#' @param A Anaerobic capacity in J/kg
#' @param MAP Maximal aerobic power in W/kg
#' @param E Endurance in W/kg/log(time in s)
#' @param T_est Running duration in seconds
#' @param BMR Basal metabolic rate in W/kg
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
#' peronnet_thibault_model(T_est = 600, A = 1500, MAP = 25, E = -1.5, Tmap = 420, BMR = 1.2)
#'
#' @export
peronnet_thibault_model <- function(T_est, A, MAP, E, Tmap,  BMR = 1.2) {

  if (A <= 0) stop("Anaerobic capacity (A) must be positive")
  if (MAP <= 0) stop("Maximal aerobic power (MAP) must be positive")
  if (E >= 0) stop("Endurance (E) must be negative")
  if (T_est <= 0) stop("Running duration (T_est) must be positive")
  if (BMR <= 0) stop("Basal metabolic rate (BMR) must be positive")
  if (Tmap <= 0) stop("Time to MAP (Tmap) must be positive")


  if(T_est < Tmap) {
    S <- A
    p_anaero <- S/T_est * (1 - exp(-T_est/20))
    B <- MAP - BMR
    p_aero <- 1/T_est * (BMR*T_est + B*T_est + 30*B * (exp(-T_est/30) - 1))
    power_output <-  p_anaero + p_aero
  }
  if(T_est >= Tmap){
    S <- A * (1 - 0.233*log(T_est/Tmap))
    p_anaero <- S/T_est * (1 - exp(-T_est/20))
    B <- MAP - BMR + (E * log(T_est/Tmap))
    p_aero <- 1/T_est * (BMR*T_est + B*T_est + 30*B * (exp(-T_est/30) - 1))
    power_output <-  p_anaero + p_aero
  }


  return(power_output)
}

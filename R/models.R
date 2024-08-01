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

#' Kennelly Velocity Model
#'
#' Calculates running velocity based on distance using Kennelly's model.
#'
#' @param Distance Numeric. Running distance in meters (must be positive).
#' @param Ck Numeric. Empirical coefficient determined from a set of performances (must be positive).
#'
#' @return Numeric. Running velocity in m/s.
#'
#' @references
#' Kennelly, A. E. "An Approximate Law of Fatigue in the Speeds of Racing Animals."
#' Proceedings of the American Academy of Arts and Sciences 42, no. 15 (1906): 275–331.
#' https://doi.org/10.2307/20022230.
#'
#' @export
#'
#' @examples
#' Kennelly_1906_V(Distance = 1000, Ck = 10)
Kennelly_1906_V <- function(Distance, Ck) {
  if (Distance <= 0) stop("Distance must be positive")
  if (Ck <= 0) stop("Ck must be positive")

  Velocity <- Ck * Distance^(-1/8)
  return(Velocity)
}

#' Kennelly Power Model
#'
#' Calculates running power based on distance using Kennelly's model and di Prampero's equation.
#'
#' @inheritParams Kennelly_1906_V
#' @inheritParams di_prampero_running
#'
#' @return Numeric. Power output in watts.
#'
#' @references
#' Kennelly, A. E. "An Approximate Law of Fatigue in the Speeds of Racing Animals."
#' Proceedings of the American Academy of Arts and Sciences 42, no. 15 (1906): 275–331.
#' https://doi.org/10.2307/20022230.
#'
#' @export
#'
#' @examples
#' Kennelly_1906_P(Distance = 1000, Ck = 10)
Kennelly_1906_P <- function(Distance, Ck, C1 = 3.86, C2 = 0.4, C3 = 2,
                            BMR = 1.2, BSA = 1.8, BM = 70, eff = 0.2) {
  if (Distance <= 0) stop("Distance must be positive")
  if (Ck <= 0) stop("Ck must be positive")

  Velocity <- Ck * Distance^(-1/8)
  Power <- di_prampero_running(Velocity, Distance, C1, C2, C3, BMR, BSA, BM, eff)
  return(Power)
}

#' Riegel Time Model
#'
#' Calculates running time based on distance using Riegel's model.
#'
#' @param Distance Numeric. Running distance in meters (must be positive).
#' @param a Numeric. Empirical constant dependent on measurement units (must be positive).
#' @param b Numeric. "Fatigue factor" constant as described by Riegel (must be positive).
#'
#' @return Numeric. Running time in seconds.
#'
#' @references
#' Riegel, Peter S. "Athletic Records and Human Endurance: A Time-vs.-Distance Equation
#' Describing World-Record Performances May Be Used to Compare the Relative Endurance
#' Capabilities of Various Groups of People." American Scientist 69, no. 3 (1981): 285–90.
#' http://www.jstor.org/stable/27850427.
#'
#' @export
#'
#' @examples
#' Riegel_time(Distance = 1000, a = 10, b = 1.06)
Riegel_time <- function(Distance, a, b) {
  if (Distance <= 0) stop("Distance must be positive")
  if (a <= 0) stop("a must be positive")
  if (b <= 0) stop("b must be positive")

  Time <- a * Distance^b
  return(Time)
}

#' Riegel Velocity Model
#'
#' Calculates running velocity based on distance using Riegel's model.
#'
#' @inheritParams Riegel_time
#'
#' @return Numeric. Running velocity in m/s.
#'
#' @references
#' Riegel, Peter S. "Athletic Records and Human Endurance: A Time-vs.-Distance Equation
#' Describing World-Record Performances May Be Used to Compare the Relative Endurance
#' Capabilities of Various Groups of People." American Scientist 69, no. 3 (1981): 285–90.
#' http://www.jstor.org/stable/27850427.
#'
#' @export
#'
#' @examples
#' Riegel_velocity(Distance = 1000, a = 2.5, b = 1.06)
Riegel_velocity <- function(Distance, a, b) {
  if (Distance <= 0) stop("Distance must be positive")
  if (a <= 0) stop("a must be positive")
  if (b <= 0) stop("b must be positive")

  Time <- a * Distance^b
  Velocity <- Distance / Time
  return(Velocity)
}

#' Riegel Power Model
#'
#' Calculates running power based on distance using Riegel's model and di Prampero's equation.
#'
#' @inheritParams Riegel_time
#' @inheritParams di_prampero_running
#'
#' @return Numeric. Power output in watts.
#'
#' @references
#' Riegel, Peter S. "Athletic Records and Human Endurance: A Time-vs.-Distance Equation
#' Describing World-Record Performances May Be Used to Compare the Relative Endurance
#' Capabilities of Various Groups of People." American Scientist 69, no. 3 (1981): 285–90.
#' http://www.jstor.org/stable/27850427.
#'
#' @export
#'
#' @examples
#' Riegel_power(Distance = 1000, a = 2.5, b = 1.06)
Riegel_power <- function(Distance, a, b, C1 = 3.86, C2 = 0.4, C3 = 2,
                         BMR = 1.2, BSA = 1.8, BM = 70, eff = 0.2) {
  if (Distance <= 0) stop("Distance must be positive")
  if (a <= 0) stop("a must be positive")
  if (b <= 0) stop("b must be positive")

  Time <- a * Distance^b
  Velocity <- Distance / Time
  Power <- di_prampero_running(Velocity, Distance, C1, C2, C3, BMR, BSA, BM, eff)
  return(Power)
}

#' Rumball Velocity Model
#'
#' Calculates running velocity based on distance using Rumball's model.
#'
#' @param Distance Numeric. Running distance in meters (must be positive).
#' @param f Numeric. Empirical coefficient (slope of the relationship) referred to as the "fatigue coefficient" by Rumball (must be positive).
#' @param c Numeric. Empirical coefficient (intercept of the relationship).
#'
#' @return Numeric. Running velocity in m/s.
#'
#' @references
#' Rumball, W. M., and C. E. Coleman. "Analysis of Running and the Prediction of Ultimate Performance."
#' Nature 228, no. 5267 (1970): 184–85. https://www.nature.com/articles/228184a0.
#'
#' @export
#'
#' @examples
#' Rumball_velocity(Distance = 1000, f = 0.5, c = 2)
Rumball_velocity <- function(Distance, f, c) {
  if (Distance <= 0) stop("Distance must be positive")
  if (f <= 0) stop("Fatigue coefficient (f) must be positive")

  Distance_miles <- Distance / 1609
  Rate <- f * log(Distance) + c
  Velocity <- 1 / Rate * 1609 / 60
  return(Velocity)
}

#' Rumball Power Model
#'
#' Calculates running power based on distance using Rumball's model and di Prampero's equation.
#'
#' @inheritParams Rumball_velocity
#' @inheritParams di_prampero_running
#'
#' @return Numeric. Power output in watts.
#'
#' @references
#' Rumball, W. M., and C. E. Coleman. "Analysis of Running and the Prediction of Ultimate Performance."
#' Nature 228, no. 5267 (1970): 184–85. https://www.nature.com/articles/228184a0.
#'
#' @export
#'
#' @examples
#' Rumball_power(Distance = 1000, f = 0.5, c = 2)
Rumball_power <- function(Distance, f, c, C1 = 3.86, C2 = 0.4, C3 = 2,
                          BMR = 1.2, BSA = 1.8, BM = 70, eff = 0.2) {
  if (Distance <= 0) stop("Distance must be positive")
  if (f <= 0) stop("Fatigue coefficient (f) must be positive")

  Distance_miles <- Distance / 1609
  Rate <- f * log(Distance) + c
  Velocity <- 1 / Rate * 1609 / 60
  Power <- di_prampero_running(Velocity, Distance, C1, C2, C3, BMR, BSA, BM, eff)
  return(Power)
}

#' Lloyd Model
#'
#' Calculates power output based on time using Lloyd's model.
#'
#' @param Pmax Numeric. Maximum instantaneous power in W/kg (must be positive).
#' @param g Numeric. Time constant of the decrease of anaerobic metabolism contribution (must be positive).
#' @param MAP Numeric. Maximal aerobic power in W/kg (must be positive).
#' @param k Numeric. Time delay in the rise of aerobic metabolism power contribution from resting level to maximum (must be positive).
#' @param Time Numeric. Running duration in seconds (must be positive).
#' @param BMR Numeric. Basal metabolic rate in W/kg. Default is 1.2.
#'
#' @return Numeric. Power output in W/kg.
#'
#' @references
#' Lloyd. "WORLD RUNNING RECORDS AS MAXIMAL PERFORMANCES-OXYGEN DEBT AND OTHER LIMITING FACTORS."
#' In Circulation Research, 20:I218. LIPPINCOTT WILLIAMS & WILKINS 227 EAST WASHINGTON SQ,
#' PHILADELPHIA, PA 19106, 1967.
#'
#' @export
#'
#' @examples
#' Lloyd(Pmax = 25, g = 0.05, MAP = 25, k = 10, Time = 100)
Lloyd <- function(Pmax, g, MAP, k, Time, BMR = 1.2) {
  if (Pmax <= 0) stop("Pmax must be positive")
  if (g <= 0) stop("g must be positive")
  if (MAP <= 0) stop("MAP must be positive")
  if (k <= 0) stop("k must be positive")
  if (Time <= 0) stop("Time must be positive")
  if (BMR <= 0) stop("BMR must be positive")

  B <- MAP - BMR
  P <- Pmax * (exp(-g * Time)) + B * (1 - k / Time)
  return(P)
}

#' Ward-Smith Model
#'
#' Calculates power output based on time using Ward-Smith's model.
#'
#' @param Pmax Numeric. Maximal instantaneous power in W/kg (must be positive).
#' @param MAP Numeric. Maximal aerobic power in W/kg (must be positive).
#' @param lambda Numeric. Time constant of both the rise of aerobic metabolism contribution and the decay of anaerobic metabolism (must be positive).
#' @param Time Numeric. Running duration in seconds (must be positive).
#'
#' @return Numeric. Power output in W/kg.
#'
#' @references
#' Ward-Smith, A. J. "A Mathematical Theory of Running, Based on the First Law of Thermodynamics,
#' and Its Application to the Performance of World-Class Athletes." Journal of Biomechanics 18,
#' no. 5 (1985): 337–49. https://doi.org/10.1016/0021-9290(85)90289-1.
#'
#' @export
#'
#' @examples
#' WardSmith(Pmax = 70, MAP = 25, lambda = 0.05, Time = 100)
WardSmith <- function(Pmax, MAP, lambda, Time) {
  if (Pmax <= 0) stop("Pmax must be positive")
  if (MAP <= 0) stop("MAP must be positive")
  if (lambda <= 0) stop("lambda must be positive")
  if (Time <= 0) stop("Time must be positive")

  P <- Pmax * exp(-lambda * Time) + MAP * (1 - exp(-lambda * Time))
  return(P)
}







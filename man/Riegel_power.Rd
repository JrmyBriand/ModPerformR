% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/models.R
\name{Riegel_power}
\alias{Riegel_power}
\title{Riegel Power Model}
\usage{
Riegel_power(
  Distance,
  a,
  b,
  C1 = 3.86,
  C2 = 0.4,
  C3 = 2,
  BMR = 1.2,
  BSA = 1.8,
  BM = 70,
  eff = 0.2
)
}
\arguments{
\item{Distance}{Numeric. Running distance in meters (must be positive).}

\item{a}{Numeric. Empirical constant dependent on measurement units (must be positive).}

\item{b}{Numeric. "Fatigue factor" constant as described by Riegel (must be positive).}

\item{C1}{Numeric. Parameter of the equation. Default is 3.86.}

\item{C2}{Numeric. Parameter of the equation. Default is 0.4.}

\item{C3}{Numeric. Parameter of the equation. Default is 2.}

\item{BMR}{Numeric. Basal metabolic rate in W/kg. Default is 1.2.}

\item{BSA}{Numeric. Body surface area in m^2. Default is 1.8.}

\item{BM}{Numeric. Body mass in kg. Default is 70.}

\item{eff}{Numeric. Assumed mechanical efficiency. Default is 0.2.}
}
\value{
Numeric. Power output in watts.
}
\description{
Calculates running power based on distance using Riegel's model and di Prampero's equation.
}
\examples{
Riegel_power(Distance = 1000, a = 2.5, b = 1.06)
}
\references{
Riegel, Peter S. "Athletic Records and Human Endurance: A Time-vs.-Distance Equation
Describing World-Record Performances May Be Used to Compare the Relative Endurance
Capabilities of Various Groups of People." American Scientist 69, no. 3 (1981): 285–90.
http://www.jstor.org/stable/27850427.
}

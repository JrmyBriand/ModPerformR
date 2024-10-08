% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/models.R
\name{WardSmith}
\alias{WardSmith}
\title{Ward-Smith Model}
\usage{
WardSmith(Pmax, MAP, lambda, Time)
}
\arguments{
\item{Pmax}{Numeric. Maximal instantaneous power in W/kg (must be positive).}

\item{MAP}{Numeric. Maximal aerobic power in W/kg (must be positive).}

\item{lambda}{Numeric. Time constant of both the rise of aerobic metabolism contribution and the decay of anaerobic metabolism (must be positive).}

\item{Time}{Numeric. Running duration in seconds (must be positive).}
}
\value{
Numeric. Power output in W/kg.
}
\description{
Calculates power output based on time using Ward-Smith's model.
}
\examples{
WardSmith(Pmax = 70, MAP = 25, lambda = 0.05, Time = 100)
}
\references{
Ward-Smith, A. J. "A Mathematical Theory of Running, Based on the First Law of Thermodynamics,
and Its Application to the Performance of World-Class Athletes." Journal of Biomechanics 18,
no. 5 (1985): 337–49. https://doi.org/10.1016/0021-9290(85)90289-1.
}

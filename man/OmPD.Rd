% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/models.R
\name{OmPD}
\alias{OmPD}
\title{Omni-Domain Power-Duration Model}
\usage{
OmPD(Pmax, Wprime, CP, A, Time)
}
\arguments{
\item{Pmax}{Numeric. Maximal instantaneous power in W (must be positive).}

\item{Wprime}{Numeric. Anaerobic energy reserve in J (must be positive).}

\item{CP}{Numeric. Critical power in W (must be positive).}

\item{A}{Numeric. Index representing the power for exercise durations greater than 30 minutes.}

\item{Time}{Numeric. Running time in seconds (must be positive).}
}
\value{
Numeric. Power output in W.
}
\description{
This function calculates power output based on the Omni-Domain Power-Duration model.
}
\note{
The Wprime and CP parameters are not equivalent to those presented in the original critical power model.
}
\examples{
OmPD(Pmax = 25, Wprime = 20000, CP = 300, A = 0.05, Time = 1800)
}
\references{
Puchowicz, Michael J., Jonathan Baker, and David C. Clarke. "Development and Field Validation of an Omni-Domain Power-Duration Model."
Journal of Sports Sciences 38, no. 7 (April 2020): 801–13. https://doi.org/10.1080/02640414.2020.1735609.
}

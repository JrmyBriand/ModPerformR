
# ModPerformR

<!-- badges: start -->
[![R-CMD-check](https://github.com/JrmyBriand/ModPerformR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/JrmyBriand/ModPerformR/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of ModPerformR is to provide tools for modeling and analyzing running performance, with a focus on elite athletes and world records.

## Features

- Implementation of the Péronnet-Thibault model for estimating power output in running
- Datasets of world records for male and female athletes
- Dataset of elite runners' performance across various distances

## Installation

You can install the development version of ModPerformR from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("JrmyBriand/ModPerformR")

```

## Usage

### Péronnet-Thibault Model

The `peronnet_thibault_model` function calculates the power output for a given running duration:

``` r
library(ModPerformR)

# Example usage
power_output <- peronnet_thibault_model(
  A = 1500,    # Anaerobic capacity in J/kg
  MAP = 25,    # Maximal aerobic power in W/kg
  E = -1.5,    # Endurance in W/kg/log(time in s)
  t = 600, # Running duration in seconds
  BMR = 1.2,   # Basal metabolic rate in W/kg
  Tmap = 420   # Running duration eliciting MAP in seconds
)
```

print(power_output)

### Critical Power Model

The `critical_power_model` function calculates the power output based on the Critical Power model:

``` r
power_output <- critical_power_model(
  t = 300,     # Running duration in seconds (must be positive)
  cp = 300,    # Critical power in w (must be positive)
  Wprime = 20000 # Anaerobic energy reserve in J (must be positive)
)

print(power_output)
```

### 3-Parameter Critical Power Model

The `critical_power_model_3p` function calculates the power output based on the 3-Parameter Critical Power model:

``` r
power_output <- critical_power_model_3p(
  t = 300,     # Running duration in seconds (must be positive)
  cp = 300,    # Critical power in watts (must be positive)
  Wprime = 20000, # Anaerobic energy reserve in joules (must be positive)
  Pmax = 1000  # Maximal instantaneous power in watts (must be positive and greater than cp)
)

print(power_output)
```


## Datasets

ModPerformR includes three datasets:

World Records for Male Athletes (`wr_males`)
World Records for Female Athletes (`wr_females`)
Elite Runners Performance Data (`elite_runners`)
You can load these datasets using the `data()` function:

```r
data(wr_males)
data(wr_females)
data(elite_runners)

# Example: View the first few rows of the elite runners dataset
head(elite_runners)

```

## Contributing

Contributions to ModPerformR are welcome! Please refer to the [contribution guidelines](CONTRIBUTING.md) for more information.

## License

This project is licensed under the [MIT License](LICENSE.md).


library(readr)
library(dplyr)

# Read the CSV file
elite_runners <- read_csv("data-raw/Elite_Runners.csv")

# Perform any necessary data cleaning or preprocessing
elite_runners <- elite_runners %>%
  mutate(across(c(Distance, Time, VO2, BM, height), as.numeric))

# Save the data in the package
usethis::use_data(elite_runners, overwrite = TRUE)


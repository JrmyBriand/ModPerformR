library(tidyverse)

# Read the CSV files
wr_males <- read_csv("data-raw/WRMales.csv")
wr_females <- read_csv("data-raw/WRFemalescsv.csv")

# Perform any necessary data cleaning or preprocessing here

# Function to arrange data in long format

arranging_data <- function(data) {
  # Define the distances vector
  Distance <- c(100, 200, 400, 800, 1500, 1609, 3000, 5000, 10000, 21100, 42195)
  # Suppress warnings and use lapply to process columns
  suppressWarnings({
    profiles_list <- lapply(1:112, function(j) {

      time <- data[1:11, j]

      # Extract year from the column names
      year <- as.numeric(names(time))
      # Extract times for the current column
      time <- unlist(time)

      # Convert time to numeric, handling possible non-numeric values
      Time <- as.numeric(as.character(time))

      # Create a tibble for the current profile
      profile <- tibble(Year = year, Distance = Distance, Time = Time)

      return(profile)

    })
  })
  # Combine all profiles into one tibble
  DAT <- bind_rows(profiles_list)

  # Return the final combined tibble
  return(DAT)
}

# performing preprocessing

WR_males <- arranging_data(wr_males)
WR_females <- arranging_data(wr_females)


# Save the data as internal datasets
usethis::use_data(WR_males, overwrite = TRUE)
usethis::use_data(WR_females, overwrite = TRUE)

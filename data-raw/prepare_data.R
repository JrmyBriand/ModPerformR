# Read the CSV files
wr_males <- read.csv("data-raw/WRMales.csv", stringsAsFactors = FALSE)
wr_females <- read.csv("data-raw/WRFemalescsv.csv", stringsAsFactors = FALSE)

# Perform any necessary data cleaning or preprocessing here
# For example:
# wr_males$date <- as.Date(wr_males$date)
# wr_females$date <- as.Date(wr_females$date)

# Save the data as internal datasets
usethis::use_data(wr_males, overwrite = TRUE)
usethis::use_data(wr_females, overwrite = TRUE)

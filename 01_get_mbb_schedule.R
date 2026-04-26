## Load necessary libraries
library(hoopR)
library(dplyr)
library(lubridate)
library(readr)

## Define the seasons to load
seasons = 2022:2025

## Load the schedule for the defined seasons
schedule = hoopR::load_mbb_schedule(seasons = seasons)

names(schedule)
glimpse(schedule)

# Save the schedule data to a CSV file
write_csv(schedule, "mbb_schedule_2022_2025.csv")
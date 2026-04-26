# Load necessary libraries
library(dplyr)
library(lubridate)
library(readr)

# Load MBB schedule data
schedule = read_csv("mbb_schedule_2022_2025.csv", show_col_types = FALSE)

# Define ACC conference ID
ACC_ID = 2

acc_games = schedule %>%
  filter(
    home_conference_id == ACC_ID,
    away_conference_id == ACC_ID,
    status_type_completed == TRUE,
    !is.na(home_score),
    !is.na(away_score)
  )

# Process ACC game data
acc_model_data = acc_games %>%
  transmute(
    season = season,
    date = as.Date(ymd_hms(date, tz = "UTC")),
    home_team = home_display_name,
    away_team = away_display_name,
    home_points = home_score,
    away_points = away_score,
    pt_spread = home_score - away_score
  ) %>%
  arrange(date)

# Inspect the resulting data frame
nrow(acc_model_data)
head(acc_model_data, 10)
summary(acc_model_data$pt_spread)
sum(is.na(acc_model_data$pt_spread))

# Save the processed ACC MBB historical games data to a CSV file
write_csv(acc_model_data, "acc_mbb_historical_games.csv")

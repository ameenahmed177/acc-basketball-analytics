library(dplyr)
library(readr)

schedule = read_csv("mbb_schedule_2022_2025.csv", show_col_types = FALSE)

ACC_ID = 2

train_data = schedule %>%
  filter(
    (home_conference_id == ACC_ID | away_conference_id == ACC_ID),
    status_type_completed == TRUE,
    !is.na(home_score),
    !is.na(away_score)
  ) %>%
  transmute(
    home_team = home_display_name,
    away_team = away_display_name,
    pt_spread = home_score - away_score
  ) %>%
  mutate(
    home_team = factor(home_team),
    away_team = factor(away_team)
  )

baseline_model = lm(pt_spread ~ home_team + away_team, data = train_data)

saveRDS(baseline_model, "baseline_team_strength_model.rds")
summary(baseline_model)

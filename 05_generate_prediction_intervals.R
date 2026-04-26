library(dplyr)
library(readr)

# ===== SETTINGS =====
LEVEL = 0.75  # 0.70 is the minimum target; 0.75 is a safer buffer
TEAM_NAME = "Twilight Wolfpack Analytics"
TEAM_MEMBERS = "Ameen Ahmed"
EMAIL = "aahmed39@ncsu.edu"

# ===== LOAD MODEL =====
baseline_model = readRDS("baseline_team_strength_model.rds")

# ===== LOAD CI TEMPLATE (DO NOT CHANGE Date/Away/Home) =====
template = read_csv(
  "tsa_ci_template_2026 - Sheet1.csv",
  show_col_types = FALSE
)

# Drop completely blank rows (often exist below the real games)
template = template %>%
  filter(!(is.na(Date) & is.na(Away) & is.na(Home)))

# Full team names used inside the model
home_levels = levels(baseline_model$model$home_team)
away_levels = levels(baseline_model$model$away_team)

# Map template team names (short) -> model team names (full)
map_team = function(short_name, full_levels) {
  if (is.na(short_name)) return(NA_character_)

  short_name = trimws(short_name)

  # Special cases / ambiguities
  if (short_name == "Pitt") return("Pittsburgh Panthers")
  if (short_name == "Virginia") return("Virginia Cavaliers")
  if (short_name == "Virginia Tech") return("Virginia Tech Hokies")
  if (short_name == "Miami") return("Miami Hurricanes")

    # Other special cases
  if (short_name == "California") return("California Golden Bears")
  if (short_name == "Michigan") return("Michigan Wolverines")
  if (short_name == "North Carolina") return("North Carolina Tar Heels")

  # Exact match (rare but safe)
  if (short_name %in% full_levels) return(short_name)

  # Flexible prefix match (works with space, "(", "-", etc.)
  hits = full_levels[grepl(paste0("^", gsub("([\\W])", "\\\\\\1", short_name), "\\b"), full_levels)]
  if (length(hits) == 1) return(hits)

  hits2 = full_levels[startsWith(full_levels, paste0(short_name, " "))]
  if (length(hits2) == 1) return(hits2)

  return(NA_character_)
}

# Create NEW columns for prediction (leave Home/Away untouched!)
template_pred = template %>%
  mutate(
    home_team_full = vapply(Home, map_team, character(1), full_levels = home_levels),
    away_team_full = vapply(Away, map_team, character(1), full_levels = away_levels),
    home_team_full = factor(home_team_full, levels = home_levels),
    away_team_full = factor(away_team_full, levels = away_levels)
  )

# Checks
cat("Unmatched Home teams:", sum(is.na(template_pred$home_team_full)), "\n")
cat("Unmatched Away teams:", sum(is.na(template_pred$away_team_full)), "\n")

# Stop if there are unmatched teams
if (sum(is.na(template_pred$home_team_full)) > 0 | sum(is.na(template_pred$away_team_full)) > 0) {
  cat("Unmatched Home team names:\n")
  print(sort(unique(template$Home[is.na(template_pred$home_team_full)])))
  cat("Unmatched Away team names:\n")
  print(sort(unique(template$Away[is.na(template_pred$away_team_full)])))
  stop("Fix the unmatched team name mappings above, then re-run.")
}

# ===== PREDICTION INTERVALS =====
pi_mat = predict(
  baseline_model,
  newdata = data.frame(
    home_team = template_pred$home_team_full,
    away_team = template_pred$away_team_full
  ),
  interval = "prediction",
  level = LEVEL
)

# pi_mat has columns: fit, lwr, upr
template_final = template %>%
  mutate(
    ci_lb = round(pi_mat[, "lwr"], 2),
    ci_ub = round(pi_mat[, "upr"], 2),
    team_name = TEAM_NAME,
    team_members = TEAM_MEMBERS,
    email = EMAIL
  )

# Final checks
cat("Missing ci_lb:", sum(is.na(template_final$ci_lb)), "\n")
cat("Missing ci_ub:", sum(is.na(template_final$ci_ub)), "\n")
head(template_final)

# ===== SAVE FINAL SUBMISSION FILE =====
write_csv(template_final, "tsa_pi_Twilight_Wolfpack_Analytics_2026.csv")

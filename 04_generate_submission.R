library(dplyr)
library(readr)

baseline_model = readRDS("baseline_team_strength_model.rds")

template = read_csv(
  "tsa_pt_spread_template_2026 - Sheet1.csv",
  show_col_types = FALSE
)

# Drop completely blank rows (often exist below the real games)
template = template %>%
  filter(!(is.na(Date) & is.na(Away) & is.na(Home)))

home_levels = levels(baseline_model$model$home_team)
away_levels = levels(baseline_model$model$away_team)

map_team = function(short_name, full_levels) {
  if (is.na(short_name)) return(NA_character_)

  short_name = trimws(short_name)

  # Special cases (template nicknames / ambiguities)
  if (short_name == "Pitt") return("Pittsburgh Panthers")
  if (short_name == "Virginia") return("Virginia Cavaliers")
  if (short_name == "Virginia Tech") return("Virginia Tech Hokies")

  # Critical ambiguity: "Miami" could match "Miami Hurricanes" and "Miami (OH) Redhawks"
  if (short_name == "Miami") return("Miami Hurricanes")

# Other special cases
  if (short_name == "California") return("California Golden Bears")
  if (short_name == "Michigan") return("Michigan Wolverines")
  if (short_name == "North Carolina") return("North Carolina Tar Heels")

  # Exact match (rare but safe)
  if (short_name %in% full_levels) return(short_name)

  # More flexible prefix match:
  # - Works whether the next character is a space, "(" , "-", etc.
  # - Example: "Miami" matches "Miami Hurricanes" and "Miami (OH) Redhawks"
  hits = full_levels[grepl(paste0("^", gsub("([\\W])", "\\\\\\1", short_name), "\\b"), full_levels)]

  if (length(hits) == 1) return(hits)

  # If multiple hits, try the "Team Mascot" version first (space after name)
  hits2 = full_levels[startsWith(full_levels, paste0(short_name, " "))]
  if (length(hits2) == 1) return(hits2)

  return(NA_character_)
}

# Map team names from template to full names used in the model
template_pred = template %>%
  mutate(
    home_team_full = vapply(Home, map_team, character(1), full_levels = home_levels),
    away_team_full = vapply(Away, map_team, character(1), full_levels = away_levels),
    home_team_full = factor(home_team_full, levels = home_levels),
    away_team_full = factor(away_team_full, levels = away_levels)
  )

# Report unmatched teams
cat("Unmatched Home teams:", sum(is.na(template_pred$home_team_full)), "\n")
cat("Unmatched Away teams:", sum(is.na(template_pred$away_team_full)), "\n")

# Detailed lists
# cat("Unmatched Away team names:\n")
# print(sort(unique(template$Away[is.na(template_pred$away_team_full)])))

# Predict point spreads
template_pred$pt_spread = round(
  predict(
    baseline_model,
    newdata = data.frame(
      home_team = template_pred$home_team_full,
      away_team = template_pred$away_team_full
    )
  ),
  2
)

# Prepare final submission data frame
template_final = template %>%
  mutate(
    pt_spread = template_pred$pt_spread,
    team_name = "Twilight Wolfpack Analytics",
    team_members = "Ameen Ahmed",
    email = "aahmed39@ncsu.edu"
  )

cat("Missing pt_spread:", sum(is.na(template_final$pt_spread)), "\n")
head(template_final)

write_csv(template_final, "tsa_pt_spread_Twilight_Wolfpack_Analytics_2026.csv")

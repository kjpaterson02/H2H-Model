#----------------------------------------------------------
# Display summary statistics of dataset
#----------------------------------------------------------


summary_stats <- epl_database %>%
  summarise(
    home_goals_mean = mean(home_goals, na.rm = TRUE),
    home_goals_median = median(home_goals, na.rm = TRUE),
    home_goals_sd = sd(home_goals, na.rm = TRUE),
    home_goals_min = min(home_goals, na.rm = TRUE),
    home_goals_max = max(home_goals, na.rm = TRUE),
    
    away_goals_mean = mean(away_goals, na.rm = TRUE),
    away_goals_median = median(away_goals, na.rm = TRUE),
    away_goals_sd = sd(away_goals, na.rm = TRUE),
    away_goals_min = min(away_goals, na.rm = TRUE),
    away_goals_max = max(away_goals, na.rm = TRUE),
    
    home_xG_mean = mean(home_xG, na.rm = TRUE),
    home_xG_median = median(home_xG, na.rm = TRUE),
    home_xG_sd = sd(home_xG, na.rm = TRUE),
    home_xG_min = min(home_xG, na.rm = TRUE),
    home_xG_max = max(home_xG, na.rm = TRUE),
    
    away_xG_mean = mean(away_xG, na.rm = TRUE),
    away_xG_median = median(away_xG, na.rm = TRUE),
    away_xG_sd = sd(away_xG, na.rm = TRUE),
    away_xG_min = min(away_xG, na.rm = TRUE),
    away_xG_max = max(away_xG, na.rm = TRUE)
  )

summary_stats
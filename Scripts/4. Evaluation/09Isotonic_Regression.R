#==========================================================
# Isotonic Regression Experiments
#==========================================================
library(dplyr)
library(tidyr)
library(here)

source(here("Scripts", "4. Evaluation", "Isotonic_Regression_Functions.R"))

#----------------------------------------------------------
# 1. Prepare Isotonic Regression Data 
#----------------------------------------------------------

# Create calibration datasets for the Wilkens and final weighted H2H models.
wilkens_calibration_data <- build_calibration_data(
  data = epl_database,
  predictions = wilkens_predictions
)

h2h_calibration_data <- build_calibration_data(
  data = epl_database,
  predictions = final_weighted_h2h_predictions
)

#----------------------------------------------------------
# 2. Two-Season Isotonic Regression Experiment
#----------------------------------------------------------

# Apply rolling isotonic calibration using the previous two seasons.
wilkens_calibrated_2 <- rolling_isotonic_calibration(
  wilkens_calibration_data,
  calibration_window = 2
)

h2h_calibrated_2 <- rolling_isotonic_calibration(
  h2h_calibration_data,
  calibration_window = 2
)


######### 2.1 Construct Common Evaluation Sample #########

# Combine the two models so all comparisons use identical matches.
two_season_comparison <- wilkens_calibrated_2 %>%
  select(
    season, date, home_team, away_team,
    result, obs_home, obs_draw, obs_away,
    wilkens_raw_home = raw_home_prob,
    wilkens_raw_draw = raw_draw_prob,
    wilkens_raw_away = raw_away_prob,
    wilkens_cal_home = home_prob,
    wilkens_cal_draw = draw_prob,
    wilkens_cal_away = away_prob
  ) %>%
  
  inner_join(
    
    h2h_calibrated_2 %>%
      select(
        season, date, home_team, away_team,
        h2h_raw_home = raw_home_prob,
        h2h_raw_draw = raw_draw_prob,
        h2h_raw_away = raw_away_prob,
        h2h_cal_home = home_prob,
        h2h_cal_draw = draw_prob,
        h2h_cal_away = away_prob
      ),
    
    by = c(
      "season",
      "date",
      "home_team",
      "away_team"
    )
  ) %>%
  
  drop_na(
    wilkens_raw_home,
    wilkens_raw_draw,
    wilkens_raw_away,
    wilkens_cal_home,
    wilkens_cal_draw,
    wilkens_cal_away,
    h2h_raw_home,
    h2h_raw_draw,
    h2h_raw_away,
    h2h_cal_home,
    h2h_cal_draw,
    h2h_cal_away
  )


######### 2.2 Predictive Performance #########

# Compare raw and calibrated probabilities for both models.
two_season_results <- bind_rows(
  
  evaluate_probabilities(
    two_season_comparison,
    "wilkens_raw_home",
    "wilkens_raw_draw",
    "wilkens_raw_away",
    "Wilkens Raw"
  ),
  
  evaluate_probabilities(
    two_season_comparison,
    "wilkens_cal_home",
    "wilkens_cal_draw",
    "wilkens_cal_away",
    "Wilkens + Isotonic"
  ),
  
  evaluate_probabilities(
    two_season_comparison,
    "h2h_raw_home",
    "h2h_raw_draw",
    "h2h_raw_away",
    "Weighted H2H Raw"
  ),
  
  evaluate_probabilities(
    two_season_comparison,
    "h2h_cal_home",
    "h2h_cal_draw",
    "h2h_cal_away",
    "Weighted H2H + Isotonic"
  )
)

two_season_results %>%
  mutate(
    across(
      where(is.numeric),
      ~ sprintf("%.5f", .x)
    )
  )


######### 2.3 Calibration Performance #########

# Calculate ECE and MCE for each model specification.
two_season_calibration_metrics <- bind_rows(
  
  get_calibration_metrics(
    two_season_comparison,
    "wilkens_raw_home",
    "wilkens_raw_draw",
    "wilkens_raw_away",
    "Wilkens Raw"
  ),
  
  get_calibration_metrics(
    two_season_comparison,
    "wilkens_cal_home",
    "wilkens_cal_draw",
    "wilkens_cal_away",
    "Wilkens + Isotonic"
  ),
  
  get_calibration_metrics(
    two_season_comparison,
    "h2h_raw_home",
    "h2h_raw_draw",
    "h2h_raw_away",
    "Weighted H2H Raw"
  ),
  
  get_calibration_metrics(
    two_season_comparison,
    "h2h_cal_home",
    "h2h_cal_draw",
    "h2h_cal_away",
    "Weighted H2H + Isotonic"
  )
)

two_season_calibration_metrics

#----------------------------------------------------------
# 3. Four-Season Robustness Check 
#----------------------------------------------------------

# Repeat calibration with a longer four-season training window.
wilkens_calibrated_4 <- rolling_isotonic_calibration(
  wilkens_calibration_data,
  calibration_window = 4
)

# Retain observations with complete raw and calibrated probabilities.
wilkens_4_evaluation <- wilkens_calibrated_4 %>%
  drop_na(
    raw_home_prob,
    raw_draw_prob,
    raw_away_prob,
    home_prob,
    draw_prob,
    away_prob
  )

# Compare predictive performance.
wilkens_4_results <- compare_raw_calibrated(
  wilkens_4_evaluation
)

wilkens_4_results


# Compare calibration performance.
wilkens_4_calibration_metrics <- bind_rows(
  
  get_calibration_metrics(
    wilkens_4_evaluation,
    "raw_home_prob",
    "raw_draw_prob",
    "raw_away_prob",
    "Wilkens Raw"
  ),
  
  get_calibration_metrics(
    wilkens_4_evaluation,
    "home_prob",
    "draw_prob",
    "away_prob",
    "Wilkens + Isotonic"
  )
)

wilkens_4_calibration_metrics
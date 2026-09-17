library(dplyr)
library(purrr)
library(here)

source(here("Scripts","3. ModelBuilding", "ModelBuild_Functions.R"))


#----------------------------------------------------------
# Regather saved predictions
#----------------------------------------------------------

all_predictions <- readRDS("Data/all_model_predictions.rds")

wilkens_predictions <- all_predictions$wilkens
h2h_predictions <- all_predictions$h2h
weighted_h2h_predictions <- all_predictions$weighted_h2h
final_weighted_h2h_predictions <- all_predictions$final_weighted_h2h
baseline_predictions <- all_predictions$baseline
market_predictions <- all_predictions$market
B365_predictions <- all_predictions$B365



#----------------------------------------------------------
# Wilken's model
#----------------------------------------------------------


wilkens_predictions <- wilkens_model(
  epl_database,
  n = 3
)


#----------------------------------------------------------
# Unweighted model
#----------------------------------------------------------

h2h_predictions <- h2h_model(
  epl_database,
  n = 3,
  h2h_seasons = 3,
  max_h2h = 3,
  alpha = 0.35
)

#----------------------------------------------------------
# Initial weighted specification
#----------------------------------------------------------

weighted_h2h_predictions <- weighted_h2h_model(
  data = epl_database,
  n = 3,
  h2h_seasons = 3,
  max_h2h = 3,
  alpha = 0.35,
  half_life = 2
)

#----------------------------------------------------------
# Final weighted specification
#----------------------------------------------------------

final_weighted_h2h_predictions <- weighted_h2h_model(
  data = epl_database,
  n = 3,
  h2h_seasons = 6,
  max_h2h = 6,
  alpha = 0.40,
  half_life = 3
)

#----------------------------------------------------------
# Generate baseline model
#----------------------------------------------------------

baseline_probs <- epl_database %>%
  summarise(
    home_prob = mean(result == "H"),
    draw_prob = mean(result == "D"),
    away_prob = mean(result == "A")
  )

baseline_predictions <- tibble(
  home_prob = rep(baseline_probs$home_prob, nrow(epl_database)),
  draw_prob = rep(baseline_probs$draw_prob, nrow(epl_database)),
  away_prob = rep(baseline_probs$away_prob, nrow(epl_database))
)


#----------------------------------------------------------
# Gather average market and Bet365 implied probabilities
#----------------------------------------------------------

market_predictions <- implied_probabilities(
  epl_database$Avg_home,
  epl_database$Avg_draw,
  epl_database$Avg_away
)

B365_predictions <- implied_probabilities(
  epl_database$B365_home,
  epl_database$B365_draw,
  epl_database$B365_away
)

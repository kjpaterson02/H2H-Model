#==========================================================
# Evaluate models using performance metrics
#==========================================================
library(dplyr)
library(here)

source(here("Scripts", "4. Evaluation", "Eval_Functions.R"))

#----------------------------------------------------------
# 1. Define common evaluation sample 
#----------------------------------------------------------

# Keep matches which the Wilkens model produces complete probabilities.
eval_rows <- complete.cases(
  wilkens_predictions$home_prob,
  wilkens_predictions$draw_prob,
  wilkens_predictions$away_prob
)

# Create observed outcome indicators and restrict data to the evaluation sample.
evaluation <- epl_database %>%
  mutate(
    obs_home = as.integer(result == "H"),
    obs_draw = as.integer(result == "D"),
    obs_away = as.integer(result == "A")
  ) %>%
  slice(which(eval_rows))

#----------------------------------------------------------
# 2. Restrict all model predictions to the same sample 
#----------------------------------------------------------
wilkens_eval <- wilkens_predictions[eval_rows, ]

h2h_eval <- h2h_predictions[eval_rows, ]

final_weighted_h2h_eval <-
  final_weighted_h2h_predictions[eval_rows, ]

market_eval <- market_predictions[eval_rows, ]

B365_eval <- B365_predictions[eval_rows, ]

baseline_eval <- baseline_predictions[eval_rows, ]

#----------------------------------------------------------
# 3. Evaluate and compare all models 
#----------------------------------------------------------

# Calculate Brier score, Log Loss and Accuracy for each model
# using an identical set of matches.
results <- bind_rows(
  
  Baseline = evaluate_model(
    evaluation,
    baseline_eval$home_prob,
    baseline_eval$draw_prob,
    baseline_eval$away_prob
  ),
  
  Wilkens = evaluate_model(
    evaluation,
    wilkens_eval$home_prob,
    wilkens_eval$draw_prob,
    wilkens_eval$away_prob
  ),
  
  `Unweighted H2H` = evaluate_model(
    evaluation,
    h2h_eval$home_prob,
    h2h_eval$draw_prob,
    h2h_eval$away_prob
  ),
  
  `Weighted H2H` = evaluate_model(
    evaluation,
    final_weighted_h2h_eval$home_prob,
    final_weighted_h2h_eval$draw_prob,
    final_weighted_h2h_eval$away_prob
  ),
  
  `Market Average` = evaluate_model(
    evaluation,
    market_eval$home_prob,
    market_eval$draw_prob,
    market_eval$away_prob
  ),
  
  Bet365 = evaluate_model(
    evaluation,
    B365_eval$home_prob,
    B365_eval$draw_prob,
    B365_eval$away_prob
  ),
  
  .id = "Model"
)

results %>%
  mutate(
    Brier = sprintf("%.4f", Brier),
    LogLoss = sprintf("%.4f", LogLoss),
    Accuracy = sprintf("%.4f", Accuracy)
  )
#==========================================================
# Test Model Calibrations
#==========================================================

library(dplyr)
library(rlang)
library(ggplot2)
library(patchwork)
library(here)

source(here("Scripts", "4. Evaluation", "Eval_Functions.R"))

#----------------------------------------------------------
# 1. Calculate model calibration 
#----------------------------------------------------------
# Assess calibration for each model using the common evaluation sample.
rolling <- calibrate_model(
  evaluation,
  wilkens_eval,
  "Wilkens"
)

Unweighted_H2H <- calibrate_model(
  evaluation,
  h2h_eval,
  "Unweighted H2H"
)

final_weighted_h2h <- calibrate_model(
  evaluation,
  final_weighted_h2h_eval,
  "Weighted H2H"
)

market <- calibrate_model(
  evaluation,
  market_eval,
  "Market Average"
)

#----------------------------------------------------------
#2. Combine calibration metrics 
#----------------------------------------------------------
# Combine calibration results from all models into a single table.
calibration_results <- bind_rows(
  rolling$metrics,
  Unweighted_H2H$metrics,
  final_weighted_h2h$metrics,
  market$metrics
)

calibration_results

#----------------------------------------------------------
#3. Display calibration plots
#----------------------------------------------------------

(rolling$plots) /
  (Unweighted_H2H$plots) /
  (final_weighted_h2h$plots) /
  (market$plots)
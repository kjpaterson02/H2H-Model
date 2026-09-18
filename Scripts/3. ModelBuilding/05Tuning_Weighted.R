# ============================================================
# Weighted H2H Model: Parameter selection and sensitivity tests
# ============================================================

library(dplyr)      
library(purrr)      
library(tibble) 
library(ggplot2)  
library(patchwork)  


source(here("Scripts","4. Evaluation", "Eval_Functions.R"))
# ----------------------------------------------------------
# 0.Initial half-life test: Three-match H2H model
# ----------------------------------------------------------

# Build H2H history
h2h_history <- build_h2h_history(
  epl_database,
  h2h_seasons = 3,
  max_h2h = 3
)


# Calculate baseline Wilkens expected-goals parameters.
wilkens_lambdas <- tibble(
  home_lambda = rolling_xg(
    epl_database,
    venue = "home",
    n = 3
  ),
  
  away_lambda = rolling_xg(
    epl_database,
    venue = "away",
    n = 3
  )
)


# Candidate half-lives
half_lives <- c(
  0.25,
  0.5,
  0.75,
  1,
  1.5,
  2,
  2.5,
  3,
  4,
  5,
  10
)


#Only evaluate matches with probabilities 
eval_rows <- complete.cases(
  wilkens_predictions$home_prob,
  wilkens_predictions$draw_prob,
  wilkens_predictions$away_prob
)


evaluation <- epl_database %>%
  mutate(
    obs_home = as.integer(result == "H"),
    obs_draw = as.integer(result == "D"),
    obs_away = as.integer(result == "A")
  ) %>%
  slice(which(eval_rows))

# Evaluate weighted H2H model at each half-life.
half_life_results <- purrr::map_dfr(
  half_lives,
  ~ evaluate_half_life(
    half_life = .x,
    h2h_history = h2h_history,
    wilkens_lambdas = wilkens_lambdas,
    evaluation = evaluation,
    eval_rows = eval_rows,
    alpha = 0.35
  )
)

half_life_results %>%
  mutate(
    Brier = round(Brier, 5),
    LogLoss = round(LogLoss, 5),
    Accuracy = round(Accuracy, 5)
  )

half_life_results %>%
  arrange(LogLoss)

half_life_results

# Add unweighted result for comparison
unweighted_result <- evaluate_model(
  evaluation,
  h2h_predictions$home_prob[eval_rows],
  h2h_predictions$draw_prob[eval_rows],
  h2h_predictions$away_prob[eval_rows]
)%>%
  mutate(
    # Infinite half-life corresponds conceptually to no decay.
    half_life = Inf
  )

half_life_comparison <- bind_rows(
  half_life_results,
  unweighted_result
)

half_life_comparison %>%
  arrange(LogLoss) %>%
  mutate(
    Brier = sprintf("%.6f", Brier),
    LogLoss = sprintf("%.6f", LogLoss),
    Accuracy = sprintf("%.6f", Accuracy)
  )


#  ----------------------------------------------------------
# 1. TESTING A LARGER UNWEIGHTED H2H SAMPLE
# ----------------------------------------------------------


# Construct the largest H2H history required by the experiment
h2h_history_6 <- build_h2h_history(
  data = epl_database,
  h2h_seasons = 6,
  max_h2h = 6
)


# Evaluate using a 3,4,5,6 H2H fixtures
h2h_sample_results <- purrr::map_dfr(
  3:6,
  evaluate_h2h_n
)


h2h_sample_results %>%
  mutate(
    Brier = sprintf("%.6f", Brier),
    LogLoss = sprintf("%.6f", LogLoss),
    Accuracy = sprintf("%.6f", Accuracy),
    mean_h2h_used = sprintf("%.3f", mean_h2h_used),
    pct_using_max = sprintf("%.1f", pct_using_max)
  )




# ----------------------------------------------------------
# 2. WEIGHTED SIX-MATCH H2H MODEL AND EFFECTIVE SAMPLE SIZE
# ----------------------------------------------------------

# Candidate half-lives 
half_lives <- c(
  0.5,
  0.75,
  1,
  1.5,
  2,
  2.5,
  3,
  4,
  5,
  7.5,
  10
)


# Evaluate each half-life using the expanded six-match H2H history.
weighted_h2h_6_results <- purrr::map_dfr(
  half_lives,
  evaluate_half_life_6
)


# Extract the six-match unweighted specification to provide benchmark. 
unweighted_6 <- h2h_sample_results %>%
  dplyr::filter(max_h2h == 6) %>%
  dplyr::transmute(
    Brier,
    LogLoss,
    Accuracy,
    half_life = Inf,
    mean_effective_n = mean_h2h_used
  )


# Join the unweighted benchmark to the weighted results.
weighted_h2h_6_results <- dplyr::bind_rows(
  weighted_h2h_6_results,
  unweighted_6
)

weighted_h2h_6_results



#  ----------------------------------------------------------
# 3. PLOT HALF-LIFE SENSITIVITY RESULTS
#  ----------------------------------------------------------

# Store the unweighted log-loss benchmark.
unweighted_brier <- unweighted_6$Brier

brier_plot <- ggplot(
  dplyr::filter(
    weighted_h2h_6_results,
    is.finite(half_life)
  ),
  aes(x = half_life, y = Brier)
) +
  geom_line() +
  geom_point(size = 2.5) +
  geom_hline(
    yintercept = unweighted_brier,
    linetype = "dashed"
  ) +
  labs(
    title = "Effect of H2H Recency Weighting on Brier Score",
    subtitle = "Dashed line represents the unweighted H2H specification",
    x = "Half-life (years)",
    y = "Brier Score"
  ) +
  theme_minimal()

brier_plot


# Store the unweighted log-loss benchmark.
unweighted_logloss <- unweighted_6$LogLoss


# Log-loss sensitivity plot.
logloss_plot <- ggplot(
  dplyr::filter(
    weighted_h2h_6_results,
    is.finite(half_life)
  ),
  aes(x = half_life, y = LogLoss)
) +
  geom_line() +
  geom_point(size = 2.5) +
  geom_hline(
    yintercept = unweighted_logloss,
    linetype = "dashed"
  ) +
  labs(
    title = "Effect of H2H Recency Weighting on Log Loss",
    subtitle = "Dashed line represents the unweighted H2H specification",
    x = "Half-life (years)",
    y = "Log Loss"
  ) +
  theme_minimal()

logloss_plot



# ----------------------------------------------------------
# 4. TEST THE H2H MIXING PARAMETER (ALPHA)
#  ----------------------------------------------------------

# Candidate alpha values
alpha_values <- seq(0, 1, by = 0.05)


# Evaluate predictive performance at each alpha value.
weighted_alpha_results <- purrr::map_dfr(
  alpha_values,
  evaluate_alpha_weighted
)

weighted_alpha_results %>%
  dplyr::arrange(LogLoss)


#  ----------------------------------------------------------
# 5. JOINT OPTIMISATION OF ALPHA AND HALF-LIFE
# ----------------------------------------------------------

# Candidate half-life values concentrated around best performing region
half_life_values <- c(
  1.5,
  2,
  2.5,
  3,
  3.5,
  4
)


# Candidate H2H mixing weights.
alpha_values <- c(
  0.30,
  0.35,
  0.40,
  0.45,
  0.50
)



#Precompute weighted H2H values
weighted_h2h_cache <- purrr::map(
  half_life_values,
  ~ weighted_h2h_from_history(
    h2h_history = h2h_history_6,
    half_life = .x
  )
)

names(weighted_h2h_cache) <- as.character(
  half_life_values
)

#Evaluate full parameter grid

joint_results <- purrr::map_dfr(
  half_life_values,
  function(h) {
    h2h_values <- weighted_h2h_cache[[as.character(h)]]
    purrr::map_dfr(
      alpha_values,
      ~ evaluate_weighted_combo(
        h2h_values = h2h_values,
        half_life = h,
        alpha = .x
      )
    )
  }
)

joint_results %>%
  arrange(LogLoss) %>%
  mutate(
    Brier = sprintf("%.6f", Brier),
    LogLoss = sprintf("%.6f", LogLoss),
    Accuracy = sprintf("%.6f", Accuracy)
  )


#Calculate mean effective sample size by half-life
ess_by_half_life <- tibble(
  half_life = c(
    2,
    2.5,
    3,
    3.5,
    4
  )
) %>%
  mutate(
    mean_ess = purrr::map_dbl(
      half_life,
      ~ weighted_h2h_from_history(
        h2h_history_6,
        half_life = .x
      ) %>%
        summarise(
          mean_effective_n = mean(
            effective_n[effective_n > 0],
            na.rm = TRUE
          )
        ) %>%
        pull(mean_effective_n)
    )
  )


# Attach mean ESS 
weighted_joint_results_with_ess <- joint_results %>%
  left_join(
    ess_by_half_life,
    by = "half_life"
  )

weighted_joint_results_with_ess %>%
  arrange(LogLoss) %>%
  mutate(
    Brier = sprintf("%.6f", Brier),
    LogLoss = sprintf("%.6f", LogLoss),
    Accuracy = sprintf("%.6f", Accuracy),
    mean_ess = sprintf("%.3f", mean_ess)
  )


#  ----------------------------------------------------------
# 6. FINAL HALF-LIFE SENSITIVITY ANALYSIS: ALPHA = 0.40
#  ----------------------------------------------------------

half_lives <- c(
  0.5,
  0.75,
  1,
  1.5,
  2,
  2.5,
  3,
  4,
  5,
  7.5,
  10
)


# Re-evaluate half-life while holding alpha at 0.4
weighted_h2h_final_results <- purrr::map_dfr(
  half_lives,
  ~ evaluate_half_life_6(
    half_life = .x,
    alpha = 0.40
  )
)

weighted_h2h_final_results

# ----------------------------------------------------------
# 7. FINAL HALF-LIFE SENSITIVITY PLOTS: ALPHA = 0.40
#  ----------------------------------------------------------

# Store unweighted benchmark values.
unweighted_brier <- unweighted_6$Brier
unweighted_logloss <- unweighted_6$LogLoss



# Brier score panel
brier_plot <- ggplot(
  weighted_h2h_final_results,
  aes(x = half_life, y = Brier)
) +
  geom_line() +
  geom_point(size = 2.5) +
  
  geom_hline(
    aes(
      yintercept = unweighted_brier,
      colour = "Unweighted H2H",
      linetype = "Unweighted H2H"
    ),
    linewidth = 0.8
  ) +
  
  geom_vline(
    aes(
      xintercept = 3,
      colour = "Selected half-life",
      linetype = "Selected half-life"
    ),
    linewidth = 0.8
  ) +
  
  scale_colour_manual(
    name = NULL,
    values = c(
      "Unweighted H2H" = "red",
      "Selected half-life" = "blue"
    )
  ) +
  
  scale_linetype_manual(
    name = NULL,
    values = c(
      "Unweighted H2H" = "dashed",
      "Selected half-life" = "dotted"
    )
  ) +
  
  labs(
    title = "Brier Score",
    x = "Half-life (years)",
    y = "Brier Score"
  ) +
  
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )


# Log-loss panel
logloss_plot <- ggplot(
  weighted_h2h_final_results,
  aes(x = half_life, y = LogLoss)
) +
  geom_line() +
  geom_point(size = 2.5) +
  
  geom_hline(
    aes(
      yintercept = unweighted_logloss,
      colour = "Unweighted H2H",
      linetype = "Unweighted H2H"
    ),
    linewidth = 0.8
  ) +
  
  geom_vline(
    aes(
      xintercept = 3,
      colour = "Selected half-life",
      linetype = "Selected half-life"
    ),
    linewidth = 0.8
  ) +
  
  scale_colour_manual(
    name = NULL,
    values = c(
      "Unweighted H2H" = "red",
      "Selected half-life" = "blue"
    )
  ) +
  
  scale_linetype_manual(
    name = NULL,
    values = c(
      "Unweighted H2H" = "dashed",
      "Selected half-life" = "dotted"
    )
  ) +
  
  labs(
    title = "Log Loss",
    x = "Half-life (years)",
    y = "Log Loss"
  ) +
  
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )



#Combine panels
combined_plot <- brier_plot + logloss_plot +
  plot_layout(
    guides = "collect"
  ) +
  plot_annotation(
    title = "Effect of H2H Recency Weighting on Predictive Performance",
    subtitle = expression(alpha == 0.40)
  ) &
  theme(
    legend.position = "bottom"
  )

combined_plot
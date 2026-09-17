library(ggplot2)
source(here("Scripts","3. ModelBuilding", "Control_Functions.R"))
######### 1. Build matched random-history control pools #########

# Create pools of eligible historical fixtures to be randomly selected in place of  H2H fixtures.
random_control_pools <- build_random_control_pools(
  data = epl_database,
  h2h_seasons = 6,
  max_h2h = 6
)


######### 2. Full 500-simulation random control #########

set.seed(123)

n_simulations <- 500

# Repeat the matched random-history experiment 500 times.
random_control_results <- purrr::map_dfr(
  seq_len(n_simulations),
  ~ run_random_control_simulation(
    data = epl_database,
    control_pools = random_control_pools,
    half_life = 3,
    alpha = 0.40
  ) %>%
    mutate(simulation = .x)
)

# Calculate mean performance and variability across simulations.
random_control_summary <- random_control_results %>%
  summarise(
    mean_Brier = mean(Brier),
    sd_Brier = sd(Brier),
    
    mean_LogLoss = mean(LogLoss),
    sd_LogLoss = sd(LogLoss),
    
    mean_Accuracy = mean(Accuracy),
    sd_Accuracy = sd(Accuracy),
    
    mean_home_ESS = mean(mean_home_effective_n),
    mean_away_ESS = mean(mean_away_effective_n)
  )

random_control_summary


######### 3. Final matched random-history control plots #########

# Brier Score distribution.
random_brier_plot <- ggplot(
  random_control_results,
  aes(x = Brier)
) +
  geom_histogram(
    bins = 30
  ) +
  geom_vline(
    aes(
      xintercept = h2h_brier,
      colour = "Weighted H2H"
    ),
    linetype = "dashed",
    linewidth = 1
  ) +
  scale_colour_manual(
    name = NULL,
    values = c(
      "Weighted H2H" = "red"
    )
  ) +
  labs(
    title = "Brier Score",
    x = "Brier Score",
    y = "Number of Simulations"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )


# Log Loss distribution.
random_logloss_plot <- ggplot(
  random_control_results,
  aes(x = LogLoss)
) +
  geom_histogram(
    bins = 30
  ) +
  geom_vline(
    aes(
      xintercept = h2h_logloss,
      colour = "Weighted H2H"
    ),
    linetype = "dashed",
    linewidth = 1
  ) +
  scale_colour_manual(
    name = NULL,
    values = c(
      "Weighted H2H" = "red"
    )
  ) +
  labs(
    title = "Log Loss",
    x = "Log Loss",
    y = "Number of Simulations"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )


######### 4. Combine final plots #########
h2h_performance <- evaluate_model(
  evaluation,
  final_weighted_h2h_predictions$home_prob[eval_rows],
  final_weighted_h2h_predictions$draw_prob[eval_rows],
  final_weighted_h2h_predictions$away_prob[eval_rows]
)

h2h_brier <- h2h_performance$Brier
h2h_logloss <- h2h_performance$LogLoss
h2h_accuracy <- h2h_performance$Accuracy

# Display Brier Score and Log Loss distributions side by side
# with a shared legend and overall title.
random_control_plot <- 
  random_brier_plot + random_logloss_plot +
  plot_layout(
    guides = "collect"
  ) +
  plot_annotation(
    title = "Weighted H2H vs Matched Random-History Control",
    subtitle = "Distributions across 500 matched random-history simulations"
  ) &
  theme(
    legend.position = "bottom"
  )

random_control_plot


######### 5. Compare simulations with genuine H2H model #########
comparison_random_h2h <- random_control_results %>%
  summarise(
    n_better_Brier = sum(Brier < h2h_brier),
    n_better_LogLoss = sum(LogLoss < h2h_logloss),
    n_better_Accuracy = sum(Accuracy > h2h_accuracy)
  )

comparison_random_h2h
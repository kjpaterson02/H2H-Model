# ============================================================
# BETTING STRATEGY
# ============================================================
library(dplyr)
library(tidyr)
library(ggplot2)
library(here)

source(here("Scripts", "5. Betting", "Betting_Functions.R"))


# ------------------------------------------------------------
# 1. Prepare betting data
# ------------------------------------------------------------

wilkens_betting_data <- epl_database %>%
  select(
    season,
    date,
    home_team,
    away_team,
    result,
    Avg_home,
    Avg_draw,
    Avg_away
  ) %>%
  bind_cols(final_weighted_h2h_predictions) %>%
  mutate(
    # Raw implied market probabilities
    market_home_raw = 1 / Avg_home,
    market_draw_raw = 1 / Avg_draw,
    market_away_raw = 1 / Avg_away,
    
    # Bookmaker overround
    market_overround =
      market_home_raw +
      market_draw_raw +
      market_away_raw,
    
    # Overround-adjusted market probabilities
    market_home_prob = market_home_raw / market_overround,
    market_draw_prob = market_draw_raw / market_overround,
    market_away_prob = market_away_raw / market_overround,
    
    # Expected value
    home_ev = home_prob * Avg_home - 1,
    draw_ev = draw_prob * Avg_draw - 1,
    away_ev = away_prob * Avg_away - 1,
    
    # Model-market probability edge
    home_delta_p = home_prob - market_home_prob,
    draw_delta_p = draw_prob - market_draw_prob,
    away_delta_p = away_prob - market_away_prob
  )


# ------------------------------------------------------------
# 2. Restrict to complete observations
# ------------------------------------------------------------

wilkens_betting_sample <- wilkens_betting_data %>%
  filter(
    complete.cases(
      home_prob,
      draw_prob,
      away_prob,
      Avg_home,
      Avg_draw,
      Avg_away
    )
  )


# ------------------------------------------------------------
# 3. Convert to long format
# ------------------------------------------------------------

wilkens_betting_long <- bind_rows(
  
  wilkens_betting_sample %>%
    transmute(
      season,
      date,
      home_team,
      away_team,
      result,
      outcome = "Home",
      model_prob = home_prob,
      market_prob = market_home_prob,
      odds = Avg_home,
      ev = home_ev,
      delta_p = home_delta_p,
      won = as.integer(result == "H")
    ),
  
  wilkens_betting_sample %>%
    transmute(
      season,
      date,
      home_team,
      away_team,
      result,
      outcome = "Draw",
      model_prob = draw_prob,
      market_prob = market_draw_prob,
      odds = Avg_draw,
      ev = draw_ev,
      delta_p = draw_delta_p,
      won = as.integer(result == "D")
    ),
  
  wilkens_betting_sample %>%
    transmute(
      season,
      date,
      home_team,
      away_team,
      result,
      outcome = "Away",
      model_prob = away_prob,
      market_prob = market_away_prob,
      odds = Avg_away,
      ev = away_ev,
      delta_p = away_delta_p,
      won = as.integer(result == "A")
    )
  
) %>%
  mutate(
    profit = ifelse(won == 1, odds - 1, -1)
  )


# ------------------------------------------------------------
# 4. Define betting threshold grid
# ------------------------------------------------------------

wilkens_grid <- tidyr::expand_grid(
  ev_min = seq(0.05, 0.50, by = 0.05),
  odds_max = seq(1.50, 10.00, by = 0.50),
  delta_p_min = seq(0.05, 0.25, by = 0.05)
)


# ------------------------------------------------------------
# 5. Rolling two-season optimisation
# ------------------------------------------------------------

rolling_results <- do.call(
  rbind,
  lapply(
    c("pnl", "roi", "sharpe"),
    function(objective_i) {
      
      do.call(
        rbind,
        lapply(
          c("Home", "Draw", "Away"),
          function(outcome_i) {
            
            run_rolling_betting(
              data = wilkens_betting_long,
              outcome_name = outcome_i,
              objective = objective_i
            )
          }
        )
      )
    }
  )
)


# ------------------------------------------------------------
# 6. Reconstruct out-of-sample bets
# ------------------------------------------------------------

rolling_bets_all <- extract_rolling_bets(
  betting_data = wilkens_betting_long,
  rolling_results = rolling_results
)

# Retain season-outcome combinations with at least 5 OOS bets
rolling_bets_reportable <- rolling_bets_all[
  rolling_bets_all$reportable,
  ,
  drop = FALSE
]


# ------------------------------------------------------------
# 7. Calculate final results by outcome
# ------------------------------------------------------------

final_results <- list()
counter <- 1

for (objective_i in c("pnl", "roi", "sharpe")) {
  
  for (outcome_i in c("Home", "Draw", "Away")) {
    
    subset_data <- rolling_bets_reportable[
      rolling_bets_reportable$objective == objective_i &
        rolling_bets_reportable$outcome == outcome_i,
      ,
      drop = FALSE
    ]
    
    final_results[[counter]] <- data.frame(
      objective = objective_i,
      outcome = outcome_i,
      summarise_betting_strategy(subset_data),
      stringsAsFactors = FALSE
    )
    
    counter <- counter + 1
  }
}

final_results <- do.call(rbind, final_results)


# ------------------------------------------------------------
# 8. Calculate overall results
# ------------------------------------------------------------

overall_results <- lapply(
  c("pnl", "roi", "sharpe"),
  function(objective_i) {
    
    subset_data <- rolling_bets_reportable[
      rolling_bets_reportable$objective == objective_i,
      ,
      drop = FALSE
    ]
    
    data.frame(
      objective = objective_i,
      outcome = "All",
      summarise_betting_strategy(subset_data),
      stringsAsFactors = FALSE
    )
  }
)

overall_results <- do.call(rbind, overall_results)


# ------------------------------------------------------------
# 9. Final betting results table
# ------------------------------------------------------------

final_betting_results <- rbind(
  final_results,
  overall_results
)

final_betting_results <- final_betting_results[
  order(
    match(
      final_betting_results$objective,
      c("pnl", "roi", "sharpe")
    ),
    match(
      final_betting_results$outcome,
      c("Home", "Draw", "Away", "All")
    )
  ),
]

rownames(final_betting_results) <- NULL

final_betting_results

# ------------------------------------------------------------
# 10. Best available odds robustness check
# ------------------------------------------------------------

# Extract the maximum available market odds for each match.
best_odds_lookup <- epl_database %>%
  select(
    season,
    date,
    home_team,
    away_team,
    Max_home,
    Max_draw,
    Max_away
  )


# Add the best available odds to the bets selected by the original
# strategy and recalculate profit using these odds.
rolling_bets_best <- rolling_bets_reportable %>%
  left_join(
    best_odds_lookup,
    by = c(
      "season",
      "date",
      "home_team",
      "away_team"
    )
  ) %>%
  mutate(
    best_odds = case_when(
      outcome == "Home" ~ Max_home,
      outcome == "Draw" ~ Max_draw,
      outcome == "Away" ~ Max_away
    ),
    
    profit = ifelse(
      won == 1,
      best_odds - 1,
      -1
    )
  )


# ------------------------------------------------------------
# 11. Calculate results by objective and outcome
# ------------------------------------------------------------

best_final_results <- list()
counter <- 1

# Summarise Home, Draw and Away bets separately for each objective.
for (objective_i in c("pnl", "roi", "sharpe")) {
  
  for (outcome_i in c("Home", "Draw", "Away")) {
    
    subset_data <- rolling_bets_best[
      rolling_bets_best$objective == objective_i &
        rolling_bets_best$outcome == outcome_i,
      ,
      drop = FALSE
    ]
    
    best_final_results[[counter]] <- data.frame(
      objective = objective_i,
      outcome = outcome_i,
      summarise_betting_strategy(subset_data),
      stringsAsFactors = FALSE
    )
    
    counter <- counter + 1
  }
}

best_final_results <- do.call(
  rbind,
  best_final_results
)


# ------------------------------------------------------------
# 12. Calculate overall results for each objective
# ------------------------------------------------------------

# Combine Home, Draw and Away bets to obtain overall strategy performance.
best_overall_results <- lapply(
  c("pnl", "roi", "sharpe"),
  function(objective_i) {
    
    subset_data <- rolling_bets_best[
      rolling_bets_best$objective == objective_i,
      ,
      drop = FALSE
    ]
    
    data.frame(
      objective = objective_i,
      outcome = "All",
      summarise_betting_strategy(subset_data),
      stringsAsFactors = FALSE
    )
  }
)

best_overall_results <- do.call(
  rbind,
  best_overall_results
)


# ------------------------------------------------------------
# 13. Combine final best-odds results
# ------------------------------------------------------------

# Combine outcome-specific and overall results into one table.
best_odds_results <- rbind(
  best_final_results,
  best_overall_results
)

# Order results consistently by objective and outcome.
best_odds_results <- best_odds_results[
  order(
    match(
      best_odds_results$objective,
      c("pnl", "roi", "sharpe")
    ),
    match(
      best_odds_results$outcome,
      c("Home", "Draw", "Away", "All")
    )
  ),
]

rownames(best_odds_results) <- NULL

best_odds_results


# ------------------------------------------------------------
# 14. Compare average and best available odds
# ------------------------------------------------------------

# Extract the overall P&L-optimised strategy using average odds.
average_primary <- final_betting_results[
  final_betting_results$objective == "pnl" &
    final_betting_results$outcome == "All",
  ,
  drop = FALSE
]

# Extract the equivalent strategy evaluated using best available odds.
best_primary <- best_odds_results[
  best_odds_results$objective == "pnl" &
    best_odds_results$outcome == "All",
  ,
  drop = FALSE
]

# Combine both results for a direct robustness comparison.
odds_comparison <- rbind(
  data.frame(
    odds_type = "Average",
    average_primary
  ),
  data.frame(
    odds_type = "Best available",
    best_primary
  )
)

odds_comparison
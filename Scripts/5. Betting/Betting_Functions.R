market_calibration <- function(
    data,
    model_col,
    market_col,
    outcome
) {
  
  # Check required columns exist
  required_cols <- c(model_col, market_col, "result")
  
  if (!all(required_cols %in% names(data))) {
    stop(
      "Missing columns: ",
      paste(
        required_cols[!required_cols %in% names(data)],
        collapse = ", "
      )
    )
  }
  
  model_prob <- data[[model_col]]
  market_prob <- data[[market_col]]
  actual <- as.integer(data$result == outcome)
  
  keep <- complete.cases(
    model_prob,
    market_prob,
    actual
  )
  
  model_prob <- model_prob[keep]
  market_prob <- market_prob[keep]
  actual <- actual[keep]
  
  market_band <- cut(
    market_prob,
    breaks = seq(0, 1, by = 0.1),
    include.lowest = TRUE
  )
  
  temp <- data.frame(
    market_band = market_band,
    actual = actual,
    market_prob = market_prob,
    model_prob = model_prob
  )
  
  groups <- split(
    seq_len(nrow(temp)),
    temp$market_band,
    drop = TRUE
  )
  
  result <- do.call(
    rbind,
    lapply(
      names(groups),
      function(g) {
        
        i <- groups[[g]]
        
        data.frame(
          market_band = g,
          n = length(i),
          actual_rate = mean(temp$actual[i]),
          market_prob = mean(temp$market_prob[i]),
          model_prob = mean(temp$model_prob[i])
        )
      }
    )
  )
  
  tibble::as_tibble(result)
}

evaluate_betting_parameters <- function(
    data,
    ev_min,
    odds_max,
    delta_p_min,
    odds_min = 1.25
) {
  
  bets <- data %>%
    filter(
      ev >= ev_min,
      odds >= odds_min,
      odds <= odds_max,
      delta_p >= delta_p_min
    )
  
  n_bets <- nrow(bets)
  
  if (n_bets < 5) {
    return(
      tibble(
        bets = n_bets,
        wins = ifelse(
          n_bets == 0,
          0,
          sum(bets$won)
        ),
        pnl = NA_real_,
        roi = NA_real_,
        sharpe = NA_real_
      )
    )
  }
  
  pnl <- sum(bets$profit)
  
  roi <- pnl / n_bets
  
  sharpe <- ifelse(
    sd(bets$profit) == 0,
    NA_real_,
    mean(bets$profit) / sd(bets$profit)
  )
  
  tibble(
    bets = n_bets,
    wins = sum(bets$won),
    pnl = pnl,
    roi = roi,
    sharpe = sharpe
  )
}

run_betting_grid <- function(
    data,
    outcome_name
) {
  
  # Convert to ordinary data.frame
  data <- as.data.frame(data)
  
  # Select requested outcome
  outcome_data <- data[
    data$outcome == outcome_name,
    ,
    drop = FALSE
  ]
  
  n_grid <- nrow(wilkens_grid)
  
  # Preallocate result vectors
  outcome_result <- rep(outcome_name, n_grid)
  
  ev_min_result <- wilkens_grid$ev_min
  odds_max_result <- wilkens_grid$odds_max
  delta_p_min_result <- wilkens_grid$delta_p_min
  
  bets_result <- integer(n_grid)
  wins_result <- integer(n_grid)
  
  pnl_result <- rep(NA_real_, n_grid)
  roi_result <- rep(NA_real_, n_grid)
  sharpe_result <- rep(NA_real_, n_grid)
  
  for (i in seq_len(n_grid)) {
    
    keep <-
      outcome_data$ev >= ev_min_result[i] &
      outcome_data$odds >= 1.25 &
      outcome_data$odds <= odds_max_result[i] &
      outcome_data$delta_p >= delta_p_min_result[i]
    
    # Remove any NA logical values
    keep[is.na(keep)] <- FALSE
    
    bets_i <- outcome_data[
      keep,
      ,
      drop = FALSE
    ]
    
    n_bets <- nrow(bets_i)
    
    bets_result[i] <- n_bets
    
    if (n_bets > 0) {
      wins_result[i] <- sum(bets_i$won)
    } else {
      wins_result[i] <- 0
    }
    
    # Wilkens minimum of five qualifying bets
    if (n_bets >= 5) {
      
      pnl_i <- sum(bets_i$profit)
      
      roi_i <- pnl_i / n_bets
      
      profit_sd <- stats::sd(bets_i$profit)
      
      if (
        !is.na(profit_sd) &&
        profit_sd > 0
      ) {
        sharpe_i <- mean(bets_i$profit) / profit_sd
      } else {
        sharpe_i <- NA_real_
      }
      
      pnl_result[i] <- pnl_i
      roi_result[i] <- roi_i
      sharpe_result[i] <- sharpe_i
    }
  }
  
  result <- data.frame(
    outcome = outcome_result,
    ev_min = ev_min_result,
    odds_max = odds_max_result,
    delta_p_min = delta_p_min_result,
    bets = bets_result,
    wins = wins_result,
    pnl = pnl_result,
    roi = roi_result,
    sharpe = sharpe_result,
    stringsAsFactors = FALSE
  )
  
  result
}

select_wilkens_parameters <- function(
    grid_results,
    objective = c("pnl", "roi", "sharpe")
) {
  
  objective <- match.arg(objective)
  
  valid_results <- grid_results[
    !is.na(grid_results[[objective]]),
    ,
    drop = FALSE
  ]
  
  # Rank parameter combinations by objective
  ranked <- valid_results[
    order(
      -valid_results[[objective]]
    ),
    ,
    drop = FALSE
  ]
  
  # Five best-performing combinations
  top_five <- head(
    ranked,
    5
  )
  
  # Median parameter profile
  median_ev <- median(
    top_five$ev_min
  )
  
  median_odds <- median(
    top_five$odds_max
  )
  
  median_delta <- median(
    top_five$delta_p_min
  )
  
  # Median profile must exist in the full factorial grid
  selected <- valid_results[
    valid_results$ev_min == median_ev &
      valid_results$odds_max == median_odds &
      valid_results$delta_p_min == median_delta,
    ,
    drop = FALSE
  ]
  
  list(
    top_five = top_five,
    
    median_profile = data.frame(
      ev_min = median_ev,
      odds_max = median_odds,
      delta_p_min = median_delta
    ),
    
    selected = selected
  )
}


run_rolling_betting <- function(
    data,
    outcome_name,
    objective
) {
  
  seasons <- sort(
    unique(data$season)
  )
  
  results <- list()
  
  # First two seasons are required for training,
  # so OOS testing begins with season 3
  for (i in 3:length(seasons)) {
    
    train_seasons <- seasons[
      c(i - 2, i - 1)
    ]
    
    test_season <- seasons[i]
    
    # -------------------------
    # 1. Training data
    # -------------------------
    
    training_data <- data[
      data$season %in% train_seasons,
      ,
      drop = FALSE
    ]
    
    # -------------------------
    # 2. Run 900-combination grid
    # -------------------------
    
    grid_results <- run_betting_grid(
      data = training_data,
      outcome_name = outcome_name
    )
    
    # -------------------------
    # 3. Select parameters
    # -------------------------
    
    selection <- select_wilkens_parameters(
      grid_results = grid_results,
      objective = objective
    )
    
    selected <- selection$selected
    
    # -------------------------
    # 4. OOS data
    # -------------------------
    
    test_data <- data[
      data$season == test_season &
        data$outcome == outcome_name,
      ,
      drop = FALSE
    ]
    
    # -------------------------
    # 5. Apply frozen thresholds
    # -------------------------
    
    keep <-
      test_data$ev >= selected$ev_min &
      test_data$odds >= 1.25 &
      test_data$odds <= selected$odds_max &
      test_data$delta_p >= selected$delta_p_min
    
    keep[is.na(keep)] <- FALSE
    
    bets <- test_data[
      keep,
      ,
      drop = FALSE
    ]
    
    n_bets <- nrow(bets)
    
    # -------------------------
    # 6. OOS performance
    # -------------------------
    
    if (n_bets > 0) {
      
      wins <- sum(bets$won)
      pnl <- sum(bets$profit)
      roi <- pnl / n_bets
      
      profit_sd <- sd(
        bets$profit
      )
      
      if (
        !is.na(profit_sd) &&
        profit_sd > 0
      ) {
        sharpe <- mean(bets$profit) /
          profit_sd
      } else {
        sharpe <- NA_real_
      }
      
    } else {
      
      wins <- 0
      pnl <- 0
      roi <- NA_real_
      sharpe <- NA_real_
    }
    
    # -------------------------
    # 7. Save season result
    # -------------------------
    
    results[[length(results) + 1]] <- data.frame(
      
      outcome = outcome_name,
      objective = objective,
      
      train_season_1 = train_seasons[1],
      train_season_2 = train_seasons[2],
      test_season = test_season,
      
      ev_min = selected$ev_min,
      odds_max = selected$odds_max,
      delta_p_min = selected$delta_p_min,
      
      bets = n_bets,
      wins = wins,
      pnl = pnl,
      roi = roi,
      sharpe = sharpe,
      
      reportable = n_bets >= 5,
      
      stringsAsFactors = FALSE
    )
  }
  
  do.call(
    rbind,
    results
  )
}


extract_rolling_bets <- function(
    betting_data,
    rolling_results
) {
  
  all_bets <- list()
  
  for (i in seq_len(nrow(rolling_results))) {
    
    row <- rolling_results[i, ]
    
    test_data <- betting_data[
      betting_data$season == row$test_season &
        betting_data$outcome == row$outcome,
      ,
      drop = FALSE
    ]
    
    keep <-
      test_data$ev >= row$ev_min &
      test_data$odds >= 1.25 &
      test_data$odds <= row$odds_max &
      test_data$delta_p >= row$delta_p_min
    
    keep[is.na(keep)] <- FALSE
    
    bets_i <- test_data[
      keep,
      ,
      drop = FALSE
    ]
    
    if (nrow(bets_i) > 0) {
      
      bets_i$objective <- row$objective
      bets_i$test_season <- row$test_season
      bets_i$reportable <- row$reportable
      
      all_bets[[length(all_bets) + 1]] <- bets_i
    }
  }
  
  do.call(
    rbind,
    all_bets
  )
}

max_drawdown <- function(profit) {
  
  cumulative_profit <- cumsum(profit)
  
  running_peak <- cummax(
    c(0, cumulative_profit)
  )[-1]
  
  drawdown <- running_peak -
    cumulative_profit
  
  max(
    drawdown,
    na.rm = TRUE
  )
}

summarise_betting_strategy <- function(data) {
  
  data <- data[
    order(
      data$date
    ),
    ,
    drop = FALSE
  ]
  
  n_bets <- nrow(data)
  wins <- sum(data$won)
  pnl <- sum(data$profit)
  
  data.frame(
    bets = n_bets,
    
    wins = wins,
    
    win_rate =
      wins / n_bets,
    
    pnl =
      pnl,
    
    roi =
      pnl / n_bets,
    
    sharpe =
      mean(data$profit) /
      sd(data$profit),
    
    max_drawdown =
      max_drawdown(
        data$profit
      )
  )
}

plot_cumulative_metric <- function(
    betting_data,
    objective_name,
    end_date
) {
  
  # Calculate cumulative P&L separately for each outcome.
  outcome_data <- betting_data %>%
    filter(objective == objective_name) %>%
    arrange(outcome, date) %>%
    group_by(outcome) %>%
    mutate(
      cumulative_pnl = cumsum(profit)
    ) %>%
    ungroup()
  
  
  # Extend each outcome line to the end of the evaluation period.
  outcome_endpoints <- outcome_data %>%
    group_by(outcome) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    mutate(
      date = end_date
    )
  
  outcome_data <- bind_rows(
    outcome_data,
    outcome_endpoints
  ) %>%
    arrange(outcome, date)
  
  
  # Calculate cumulative P&L across all selected bets.
  total_data <- betting_data %>%
    filter(objective == objective_name) %>%
    arrange(date) %>%
    mutate(
      cumulative_pnl = cumsum(profit),
      outcome = "Total"
    )
  
  
  # Extend the total P&L line to the end of the evaluation period.
  total_data <- bind_rows(
    total_data,
    total_data %>%
      slice_tail(n = 1) %>%
      mutate(date = end_date)
  ) %>%
    arrange(date)
  
  
  # Plot outcome-specific and total cumulative P&L.
  ggplot() +
    
    geom_hline(
      yintercept = 0,
      linetype = "dashed",
      colour = "grey50"
    ) +
    
    geom_line(
      data = outcome_data,
      aes(
        x = date,
        y = cumulative_pnl,
        colour = outcome
      ),
      linewidth = 0.7
    ) +
    
    geom_line(
      data = total_data,
      aes(
        x = date,
        y = cumulative_pnl,
        colour = outcome
      ),
      linewidth = 1.1
    ) +
    
    labs(
      x = "Date",
      y = "Cumulative P&L (units)",
      colour = "Outcome"
    ) +
    
    theme_minimal() +
    
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )
}
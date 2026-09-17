# ============================================================
# 2. FIT OBSERVATION-LEVEL ISOTONIC REGRESSION
# ============================================================

fit_isotonic <- function(prob, outcome) {
  
  fit_data <- tibble(
    prob = prob,
    outcome = outcome
  ) %>%
    filter(
      !is.na(prob),
      !is.na(outcome)
    ) %>%
    arrange(prob)
  
  iso_fit <- isoreg(
    fit_data$prob,
    fit_data$outcome
  )
  
  tibble(
    x = iso_fit$x,
    y = iso_fit$yf
  ) %>%
    group_by(x) %>%
    summarise(
      y = mean(y),
      .groups = "drop"
    ) %>%
    arrange(x)
}

# ============================================================
# 3. APPLY ISOTONIC CALIBRATION
#    - linear interpolation
#    - constant boundaries
#    - clip to [0.05, 0.95]
# ============================================================

apply_isotonic <- function(prob, mapping) {
  
  calibrated <- approx(
    x = mapping$x,
    y = mapping$y,
    xout = prob,
    method = "linear",
    rule = 2
  )$y
  
  calibrated <- ifelse(
    is.na(calibrated),
    NA_real_,
    pmin(
      pmax(calibrated, 0.05),
      0.95
    )
  )
  
  calibrated
}

# ============================================================
# 4. ROLLING FOUR-SEASON CALIBRATION
# ============================================================

rolling_isotonic_calibration <- function(
    data,
    calibration_window = 4
) {
  
  seasons <- sort(unique(data$season))
  
  output <- vector(
    "list",
    length(seasons)
  )
  
  names(output) <- seasons
  
  for (s in seasons) {
    
    training_seasons <-
      (s - calibration_window):(s - 1)
    
    # Only proceed if all four prior seasons exist
    if (!all(training_seasons %in% seasons)) {
      next
    }
    
    train <- data %>%
      filter(
        season %in% training_seasons
      )
    
    test <- data %>%
      filter(
        season == s
      )
    
    # Fit home, draw and away separately
    home_mapping <- fit_isotonic(
      train$raw_home_prob,
      train$obs_home
    )
    
    draw_mapping <- fit_isotonic(
      train$raw_draw_prob,
      train$obs_draw
    )
    
    away_mapping <- fit_isotonic(
      train$raw_away_prob,
      train$obs_away
    )
    
    # Apply to current season
    iso_home_prob <- apply_isotonic(
      test$raw_home_prob,
      home_mapping
    )
    
    iso_draw_prob <- apply_isotonic(
      test$raw_draw_prob,
      draw_mapping
    )
    
    iso_away_prob <- apply_isotonic(
      test$raw_away_prob,
      away_mapping
    )
    
    # Sum before renormalisation
    iso_sum <-
      iso_home_prob +
      iso_draw_prob +
      iso_away_prob
    
    output[[as.character(s)]] <- test %>%
      mutate(
        iso_home_prob = iso_home_prob,
        iso_draw_prob = iso_draw_prob,
        iso_away_prob = iso_away_prob,
        
        iso_sum = iso_sum,
        
        home_prob = iso_home_prob / iso_sum,
        draw_prob = iso_draw_prob / iso_sum,
        away_prob = iso_away_prob / iso_sum
      )
  }
  
  bind_rows(output)
}

calibration_metrics <- function(prob, outcome, n_bins = 10) {
  
  data <- tibble(
    prob = prob,
    outcome = outcome
  ) %>%
    filter(
      !is.na(prob),
      !is.na(outcome)
    ) %>%
    mutate(
      bin = cut(
        prob,
        breaks = seq(0, 1, length.out = n_bins + 1),
        include.lowest = TRUE
      )
    )
  
  calibration_table <- data %>%
    group_by(bin) %>%
    summarise(
      n = n(),
      mean_prob = mean(prob),
      actual_rate = mean(outcome),
      error = abs(mean_prob - actual_rate),
      .groups = "drop"
    )
  
  tibble(
    ECE = weighted.mean(
      calibration_table$error,
      calibration_table$n
    ),
    
    MCE = max(
      calibration_table$error
    )
  )
}

calibration_table_check <- function(prob, outcome, n_bins = 10) {
  
  tibble(
    prob = prob,
    outcome = outcome
  ) %>%
    filter(
      !is.na(prob),
      !is.na(outcome)
    ) %>%
    mutate(
      bin = cut(
        prob,
        breaks = seq(0, 1, length.out = n_bins + 1),
        include.lowest = TRUE
      )
    ) %>%
    group_by(bin) %>%
    summarise(
      n = n(),
      mean_prob = mean(prob),
      actual_rate = mean(outcome),
      error = abs(mean_prob - actual_rate),
      .groups = "drop"
    )
}

build_calibration_data <- function(data, predictions) {
  
  data %>%
    select(
      season,
      date,
      home_team,
      away_team,
      result
    ) %>%
    bind_cols(
      predictions %>%
        rename(
          raw_home_prob = home_prob,
          raw_draw_prob = draw_prob,
          raw_away_prob = away_prob
        )
    ) %>%
    mutate(
      obs_home = as.integer(result == "H"),
      obs_draw = as.integer(result == "D"),
      obs_away = as.integer(result == "A")
    )
}

compare_raw_calibrated <- function(data) {
  
  raw_brier <- mean(
    (
      (data$raw_home_prob - data$obs_home)^2 +
        (data$raw_draw_prob - data$obs_draw)^2 +
        (data$raw_away_prob - data$obs_away)^2
    ) / 3
  )
  
  calibrated_brier <- mean(
    (
      (data$home_prob - data$obs_home)^2 +
        (data$draw_prob - data$obs_draw)^2 +
        (data$away_prob - data$obs_away)^2
    ) / 3
  )
  
  raw_logloss <- -mean(
    data$obs_home * log(data$raw_home_prob) +
      data$obs_draw * log(data$raw_draw_prob) +
      data$obs_away * log(data$raw_away_prob)
  )
  
  calibrated_logloss <- -mean(
    data$obs_home * log(data$home_prob) +
      data$obs_draw * log(data$draw_prob) +
      data$obs_away * log(data$away_prob)
  )
  
  raw_prediction <- c("H", "D", "A")[
    max.col(
      cbind(
        data$raw_home_prob,
        data$raw_draw_prob,
        data$raw_away_prob
      )
    )
  ]
  
  calibrated_prediction <- c("H", "D", "A")[
    max.col(
      cbind(
        data$home_prob,
        data$draw_prob,
        data$away_prob
      )
    )
  ]
  
  raw_accuracy <- mean(
    raw_prediction == data$result
  )
  
  calibrated_accuracy <- mean(
    calibrated_prediction == data$result
  )
  
  tibble(
    Version = c("Raw", "Calibrated"),
    Brier = c(
      raw_brier,
      calibrated_brier
    ),
    LogLoss = c(
      raw_logloss,
      calibrated_logloss
    ),
    Accuracy = c(
      raw_accuracy,
      calibrated_accuracy
    )
  )
}

evaluate_probabilities <- function(
    data,
    home_prob,
    draw_prob,
    away_prob,
    model_name
) {
  
  home <- data[[home_prob]]
  draw <- data[[draw_prob]]
  away <- data[[away_prob]]
  
  brier <- mean(
    (
      (home - data$obs_home)^2 +
        (draw - data$obs_draw)^2 +
        (away - data$obs_away)^2
    ) / 3
  )
  
  logloss <- -mean(
    data$obs_home * log(home) +
      data$obs_draw * log(draw) +
      data$obs_away * log(away)
  )
  
  predicted_result <- c("H", "D", "A")[
    max.col(
      cbind(home, draw, away)
    )
  ]
  
  accuracy <- mean(
    predicted_result == data$result
  )
  
  tibble(
    Model = model_name,
    Brier = brier,
    LogLoss = logloss,
    Accuracy = accuracy
  )
}

get_calibration_metrics <- function(
    data,
    home_col,
    draw_col,
    away_col,
    model_name
) {
  
  bind_rows(
    
    calibration_metrics(
      data[[home_col]],
      data$obs_home
    ) %>%
      mutate(
        Model = model_name,
        Outcome = "Home"
      ),
    
    calibration_metrics(
      data[[draw_col]],
      data$obs_draw
    ) %>%
      mutate(
        Model = model_name,
        Outcome = "Draw"
      ),
    
    calibration_metrics(
      data[[away_col]],
      data$obs_away
    ) %>%
      mutate(
        Model = model_name,
        Outcome = "Away"
      )
    
  ) %>%
    select(Model, Outcome, ECE, MCE)
}

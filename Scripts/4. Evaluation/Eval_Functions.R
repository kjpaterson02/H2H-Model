brier_score <- function(home_prob,
                        draw_prob,
                        away_prob,
                        obs_home,
                        obs_draw,
                        obs_away) {
  
  (
    (home_prob - obs_home)^2 +
      (draw_prob - obs_draw)^2 +
      (away_prob - obs_away)^2
  ) / 3
  
}

log_loss <- function(home_prob,
                     draw_prob,
                     away_prob,
                     result,
                     eps = 1e-15) {
  
  home_prob <- pmax(home_prob, eps)
  draw_prob <- pmax(draw_prob, eps)
  away_prob <- pmax(away_prob, eps)
  
  case_when(
    result == "H" ~ -log(home_prob),
    result == "D" ~ -log(draw_prob),
    result == "A" ~ -log(away_prob)
  )
  
}

prediction_accuracy <- function(home_prob,
                                draw_prob,
                                away_prob,
                                result) {
  
  prediction <- case_when(
    home_prob == pmax(home_prob, draw_prob, away_prob) ~ "H",
    draw_prob == pmax(home_prob, draw_prob, away_prob) ~ "D",
    TRUE ~ "A"
  )
  
  mean(prediction == result)
  
}

evaluate_model <- function(data,
                           home_prob,
                           draw_prob,
                           away_prob) {
  
  keep <- complete.cases(
    home_prob,
    draw_prob,
    away_prob,
    data$result
  )
  
  home_prob <- home_prob[keep]
  draw_prob <- draw_prob[keep]
  away_prob <- away_prob[keep]
  
  data <- data[keep, ]
  
  tibble(
    Brier = mean(
      brier_score(
        home_prob,
        draw_prob,
        away_prob,
        data$obs_home,
        data$obs_draw,
        data$obs_away
      )
    ),
    
    LogLoss = mean(
      log_loss(
        home_prob,
        draw_prob,
        away_prob,
        data$result
      )
    ),
    
    Accuracy = prediction_accuracy(
      home_prob,
      draw_prob,
      away_prob,
      data$result
    )
  )
}

implied_probabilities <- function(home_odds,
                                  draw_odds,
                                  away_odds,
                                  prefix) {
  
  raw_home <- 1 / home_odds
  raw_draw <- 1 / draw_odds
  raw_away <- 1 / away_odds
  
  overround <- raw_home + raw_draw + raw_away
  
  probs <- tibble(
    home = raw_home / overround,
    draw = raw_draw / overround,
    away = raw_away / overround
  )
  
  names(probs) <- paste0(
    prefix,
    c("_home_prob", "_draw_prob", "_away_prob")
  )
  
  probs
}


calibration_table <- function(data,
                              prob_col,
                              outcome_col,
                              bins = 10) {
  
  prob_col <- ensym(prob_col)
  outcome_col <- ensym(outcome_col)
  
  data %>%
    filter(!is.na(!!prob_col), !is.na(!!outcome_col)) %>%
    mutate(
      bin = ntile(!!prob_col, bins)
    ) %>%
    group_by(bin) %>%
    summarise(
      predicted = mean(!!prob_col),
      observed = mean(!!outcome_col),
      n = n(),
      min_prob = min(!!prob_col),
      max_prob = max(!!prob_col),
      .groups = "drop"
    ) %>%
    mutate(
      abs_error = abs(predicted - observed)
    )
}

plot_calibration <- function(calibration,
                             title = NULL) {
  
  ggplot(calibration,
         aes(predicted, observed)) +
    
    geom_abline(
      intercept = 0,
      slope = 1,
      linetype = "dashed",
      colour = "red"
    ) +
    
    geom_point(size = 3) +
    
    coord_equal(
      xlim = c(0,1),
      ylim = c(0,1)
    ) +
    
    labs(
      title = title,
      x = "Predicted probability",
      y = "Observed frequency"
    ) +
    
    theme_minimal()
}

calibration_metrics <- function(calibration) {
  
  tibble(
    
    ECE = sum(calibration$n *
                calibration$abs_error) /
      sum(calibration$n),
    
    MCE = max(calibration$abs_error)
    
  )
  
}

evaluate_alpha <- function(data, alpha) {
  
  # Blend rolling xG and H2H xG
  data <- data %>%
    mutate(
      home_lambda_alpha =
        if_else(
          is.na(h2h_home_xg),
          home_lambda,
          (1 - alpha) * home_lambda + alpha * h2h_home_xg
        ),
      
      away_lambda_alpha =
        if_else(
          is.na(h2h_away_xg),
          away_lambda,
          (1 - alpha) * away_lambda + alpha * h2h_away_xg
        )
    )
  
  # Convert lambdas to probabilities
  probs <- pmap_dfr(
    list(
      data$home_lambda_alpha,
      data$away_lambda_alpha
    ),
    score_matrix
  )
  
  # Evaluate
  metrics <- evaluate_model(
    data,
    probs$home_prob,
    probs$draw_prob,
    probs$away_prob
  )
  
  metrics %>%
    mutate(alpha = alpha)
}


calibrate_model <- function(
    evaluation,
    predictions,
    model_name
) {
  
  # Combine observed outcomes with model predictions.
  calibration_data <- dplyr::bind_cols(
    evaluation,
    predictions
  )
  
  # Use 10 bins for home and away probabilities,
  # and 5 bins for the more concentrated draw probabilities.
  home <- calibration_table(
    calibration_data,
    home_prob,
    obs_home,
    bins = 10
  )
  
  draw <- calibration_table(
    calibration_data,
    draw_prob,
    obs_draw,
    bins = 5
  )
  
  away <- calibration_table(
    calibration_data,
    away_prob,
    obs_away,
    bins = 10
  )
  
  # Produce calibration plots for each outcome.
  plots <-
    (
      plot_calibration(
        home,
        paste(model_name, "Home")
      ) +
        plot_calibration(
          draw,
          paste(model_name, "Draw")
        ) +
        plot_calibration(
          away,
          paste(model_name, "Away")
        )
    )
  
  # Calculate ECE and MCE for each outcome.
  metrics <- tibble::tibble(
    Model = c(
      paste(model_name, "Home"),
      paste(model_name, "Draw"),
      paste(model_name, "Away")
    ),
    
    ECE = c(
      calibration_metrics(home)$ECE,
      calibration_metrics(draw)$ECE,
      calibration_metrics(away)$ECE
    ),
    
    MCE = c(
      calibration_metrics(home)$MCE,
      calibration_metrics(draw)$MCE,
      calibration_metrics(away)$MCE
    )
  )
  
  list(
    plots = plots,
    metrics = metrics
  )
}
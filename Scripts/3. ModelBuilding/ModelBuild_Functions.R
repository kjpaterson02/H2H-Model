#Calculate rolling xG averages for every match
rolling_xg <- function(data, venue = c("home", "away"), n = 3){
  
  venue <- match.arg(venue)
  
  if(venue == "home"){
    
    team_col <- "home_team"
    xg_col   <- "home_xG"
    
  } else{
    
    team_col <- "away_team"
    xg_col   <- "away_xG"
    
  }
  
  lambda <- rep(NA_real_, nrow(data))
  
  for(i in seq_len(nrow(data))){
    
    current_team   <- data[[team_col]][i]
    current_season <- data$season[i]
    current_date   <- data$date[i]
    
    previous_games <-
      data %>%
      filter(
        .data[[team_col]] == current_team,
        season == current_season,
        date < current_date
      ) %>%
      arrange(desc(date)) %>%
      slice(1:n)
    
    if(nrow(previous_games) == n){
      
      lambda[i] <- mean(previous_games[[xg_col]])
      
    }
    
  }
  
  lambda
  
}
  
  #Create Score Matrix for each match
  score_matrix <- function(home_lambda,
                           away_lambda,
                           max_goals = 10){
    
    # Possible goal totals
    goals <- 0:max_goals
    
    # Poisson probabilities
    home_probs <- dpois(goals, lambda = home_lambda)
    away_probs <- dpois(goals, lambda = away_lambda)
    
    # Joint scoreline probabilities
    score_matrix <- outer(home_probs, away_probs)
    
    # Outcome probabilities
    home_win <- sum(score_matrix[row(score_matrix) > col(score_matrix)])
    draw     <- sum(diag(score_matrix))
    away_win <- sum(score_matrix[row(score_matrix) < col(score_matrix)])
    
    tibble(
      home_prob = home_win,
      draw_prob = draw,
      away_prob = away_win
    )
  }
  
  wilkens_model <- function(data, n = 3, max_goals = 10) {
    
    home_lambda <- rolling_xg(
      data,
      venue = "home",
      n = n
    )
    
    away_lambda <- rolling_xg(
      data,
      venue = "away",
      n = n
    )
    
    predictions <- purrr::map2_dfr(
      home_lambda,
      away_lambda,
      ~score_matrix(
        home_lambda = .x,
        away_lambda = .y,
        max_goals = max_goals
      )
    )
    
    predictions
  }
  
  
  h2h_xg <- function(data,
                     venue = c("home", "away"),
                     n = 3,
                     seasons_back = 3) {
    
    venue <- match.arg(venue)
    
    xg <- rep(NA_real_, nrow(data))
    
    for (i in seq_len(nrow(data))) {
      
      current <- data[i, ]
      
      previous <- data %>%
        filter(
          home_team == current$home_team,
          away_team == current$away_team,
          date < current$date,
          season >= current$season - seasons_back,
          season < current$season
        ) %>%
        arrange(desc(date)) %>%
        slice_head(n = n)
      
      if (nrow(previous) > 0) {
        
        if (venue == "home") {
          xg[i] <- mean(previous$home_xG, na.rm = TRUE)
        } else {
          xg[i] <- mean(previous$away_xG, na.rm = TRUE)
        }
        
      }
    }
    
    xg
  }
  
  
  h2h_model <- function(data,
                        n = 3,
                        h2h_seasons = 3,
                        max_h2h = 3,
                        alpha = 0.35,
                        max_goals = 10) {
    
    # --------------------------------------------------
    # Wilkens rolling xG
    # --------------------------------------------------
    
    home_lambda <- rolling_xg(
      data,
      venue = "home",
      n = n
    )
    
    away_lambda <- rolling_xg(
      data,
      venue = "away",
      n = n
    )
    
    
    # --------------------------------------------------
    # Calculate H2H-adjusted lambdas
    # --------------------------------------------------
    
    adjusted_lambdas <- purrr::map_dfr(
      seq_len(nrow(data)),
      function(i) {
        
        current <- data[i, ]
        
        # Historical matches before current match
        historical <- data %>%
          dplyr::slice_head(n = i - 1) %>%
          dplyr::filter(
            home_team == current$home_team,
            away_team == current$away_team,
            season >= current$season - h2h_seasons, 
            season < current$season
            
          ) %>%
          dplyr::arrange(desc(date)) %>%
          dplyr::slice_head(n = max_h2h)
        
        # If no H2H data, use normal Wilkens model
        if (nrow(historical) == 0) {
          
          tibble::tibble(
            home_lambda = home_lambda[i],
            away_lambda = away_lambda[i]
          )
          
        } else {
          
          h2h_home_xg <- mean(historical$home_xG, na.rm = TRUE)
          h2h_away_xg <- mean(historical$away_xG, na.rm = TRUE)
          
          tibble::tibble(
            home_lambda =
              (1 - alpha) * home_lambda[i] +
              alpha * h2h_home_xg,
            
            away_lambda =
              (1 - alpha) * away_lambda[i] +
              alpha * h2h_away_xg
          )
        }
      }
    )
    
    
    # --------------------------------------------------
    # Convert lambdas into outcome probabilities
    # --------------------------------------------------
    
    predictions <- purrr::map2_dfr(
      adjusted_lambdas$home_lambda,
      adjusted_lambdas$away_lambda,
      ~score_matrix(
        home_lambda = .x,
        away_lambda = .y,
        max_goals = max_goals
      )
    )
    
    
    predictions
  }
  
  
  
  poisson_comparison <- function(goals) {
    
    lambda <- mean(goals, na.rm = TRUE)
    
    max_goals <- max(goals, na.rm = TRUE)
    goal_values <- 0:max_goals
    
    observed_freq <- prop.table(
      table(factor(goals, levels = goal_values))
    )
    
    tibble(
      goals = goal_values,
      observed = as.numeric(observed_freq),
      poisson = dpois(goal_values, lambda),
      lambda = lambda
    )
  }
  
  plot_poisson_comparison <- function(data, title) {
    
    ggplot(data, aes(x = goals)) +
      
      geom_col(
        aes(y = observed, fill = "Observed"),
        alpha = 0.6
      ) +
      
      geom_line(
        aes(y = poisson, colour = "Poisson"),
        linewidth = 1.2
      ) +
      
      geom_point(
        aes(y = poisson, colour = "Poisson"),
        size = 2
      ) +
      
      scale_fill_manual(
        values = c("Observed" = "steelblue")
      ) +
      
      scale_colour_manual(
        values = c("Poisson" = "firebrick")
      ) +
      
      labs(
        title = title,
        x = "Goals scored",
        y = "Proportion of matches",
        fill = NULL,
        colour = NULL
      ) +
      
      theme_minimal()
  }
  
 implied_probabilities <- function(home_odds,
                                  draw_odds,
                                  away_odds) {
  
  raw_home <- 1 / home_odds
  raw_draw <- 1 / draw_odds
  raw_away <- 1 / away_odds
  
  overround <- raw_home + raw_draw + raw_away
  
  tibble(
    home_prob = raw_home / overround,
    draw_prob = raw_draw / overround,
    away_prob = raw_away / overround
  )
}

 weighted_h2h_model <- function(data,
                                n = 3,
                                h2h_seasons = 3,
                                max_h2h = 3,
                                alpha = 0.35,
                                half_life = 2,
                                max_goals = 10) {
   
   # --------------------------------------------------
   # Wilkens rolling xG
   # --------------------------------------------------
   
   home_lambda <- rolling_xg(
     data,
     venue = "home",
     n = n
   )
   
   away_lambda <- rolling_xg(
     data,
     venue = "away",
     n = n
   )
   
   
   # --------------------------------------------------
   # Calculate weighted H2H-adjusted lambdas
   # --------------------------------------------------
   
   adjusted_lambdas <- purrr::map_dfr(
     seq_len(nrow(data)),
     function(i) {
       
       current <- data[i, ]
       
       historical <- data %>%
         dplyr::slice_head(n = i - 1) %>%
         dplyr::filter(
           home_team == current$home_team,
           away_team == current$away_team,
           season >= current$season - h2h_seasons,
           season < current$season
         ) %>%
         dplyr::arrange(desc(date)) %>%
         dplyr::slice_head(n = max_h2h)
       
       
       # If no H2H data, use normal Wilkens model
       if (nrow(historical) == 0) {
         
         tibble::tibble(
           home_lambda = home_lambda[i],
           away_lambda = away_lambda[i]
         )
         
       } else {
         
         # Age of each H2H fixture in years
         age <- as.numeric(
           difftime(
             current$date,
             historical$date,
             units = "days"
           )
         ) / 365.25
         
         
         # Exponential decay weights
         weights <- 2^(-age / half_life)
         
         
         # Weighted H2H xG
         h2h_home_xg <- weighted.mean(
           historical$home_xG,
           w = weights,
           na.rm = TRUE
         )
         
         h2h_away_xg <- weighted.mean(
           historical$away_xG,
           w = weights,
           na.rm = TRUE
         )
         
         
         # Blend Wilkens and weighted H2H
         tibble::tibble(
           home_lambda =
             (1 - alpha) * home_lambda[i] +
             alpha * h2h_home_xg,
           
           away_lambda =
             (1 - alpha) * away_lambda[i] +
             alpha * h2h_away_xg
         )
       }
     }
   )
   
   
   # --------------------------------------------------
   # Convert lambdas into outcome probabilities
   # --------------------------------------------------
   
   predictions <- purrr::map2_dfr(
     adjusted_lambdas$home_lambda,
     adjusted_lambdas$away_lambda,
     ~score_matrix(
       home_lambda = .x,
       away_lambda = .y,
       max_goals = max_goals
     )
   )
   
   predictions
 }

 evaluate_alpha <- function(alpha,
                            data,
                            evaluation,
                            eval_rows) {
   
   # Generate H2H predictions using this alpha
   predictions <- h2h_model(
     data = data,
     alpha = alpha
   )
   
   # Restrict to common evaluation sample
   predictions_eval <- predictions[eval_rows, ]
   
   # Evaluate model
   metrics <- evaluate_model(
     evaluation,
     predictions_eval$home_prob,
     predictions_eval$draw_prob,
     predictions_eval$away_prob
   )
   
   metrics %>%
     mutate(
       alpha = alpha
     )
 }
 
 unweighted_h2h_from_history <- function(h2h_history) {
   
   purrr::map_dfr(
     h2h_history,
     function(history) {
       
       if (is.null(history)) {
         return(
           tibble::tibble(
             h2h_home_xg = NA_real_,
             h2h_away_xg = NA_real_
           )
         )
       }
       
       tibble::tibble(
         h2h_home_xg = mean(
           history$home_xG,
           na.rm = TRUE
         ),
         
         h2h_away_xg = mean(
           history$away_xG,
           na.rm = TRUE
         )
       )
     }
   )
 }
 
 evaluate_alpha_fast <- function(alpha,
                                 wilkens_lambdas,
                                 h2h_unweighted,
                                 evaluation,
                                 eval_rows,
                                 max_goals = 10) {
   
   adjusted_lambdas <- tibble::tibble(
     
     home_lambda = dplyr::if_else(
       is.na(h2h_unweighted$h2h_home_xg),
       wilkens_lambdas$home_lambda,
       (1 - alpha) * wilkens_lambdas$home_lambda +
         alpha * h2h_unweighted$h2h_home_xg
     ),
     
     away_lambda = dplyr::if_else(
       is.na(h2h_unweighted$h2h_away_xg),
       wilkens_lambdas$away_lambda,
       (1 - alpha) * wilkens_lambdas$away_lambda +
         alpha * h2h_unweighted$h2h_away_xg
     )
   )
   
   predictions <- purrr::map2_dfr(
     adjusted_lambdas$home_lambda,
     adjusted_lambdas$away_lambda,
     ~score_matrix(
       home_lambda = .x,
       away_lambda = .y,
       max_goals = max_goals
     )
   )
   
   predictions_eval <- predictions[eval_rows, ]
   
   evaluate_model(
     evaluation,
     predictions_eval$home_prob,
     predictions_eval$draw_prob,
     predictions_eval$away_prob
   ) %>%
     dplyr::mutate(
       alpha = alpha
     )
 }

 build_h2h_history <- function(data,
                               h2h_seasons = 3,
                               max_h2h = 3) {
   
   purrr::map(
     seq_len(nrow(data)),
     function(i) {
       
       current <- data[i, ]
       
       historical <- data %>%
         dplyr::slice_head(n = i - 1) %>%
         dplyr::filter(
           home_team == current$home_team,
           away_team == current$away_team,
           season >= current$season - h2h_seasons,
           season < current$season
         ) %>%
         dplyr::arrange(desc(date)) %>%
         dplyr::slice_head(n = max_h2h)
       
       if (nrow(historical) == 0) {
         return(NULL)
       }
       
       historical %>%
         dplyr::transmute(
           home_xG,
           away_xG,
           
           age_years = as.numeric(
             difftime(
               current$date,
               date,
               units = "days"
             )
           ) / 365.25
         )
     }
   )
 }
 
 weighted_h2h_from_history <- function(h2h_history, half_life) {
   
   purrr::map_dfr(
     h2h_history,
     function(history) {
       
       if (is.null(history)) {
         return(
           tibble::tibble(
             h2h_home_xg = NA_real_,
             h2h_away_xg = NA_real_,
             effective_n = 0
           )
         )
       }
       
       weights <- 2^(
         -history$age_years / half_life
       )
       
       effective_n <-
         (sum(weights)^2) /
         sum(weights^2)
       
       tibble::tibble(
         h2h_home_xg = weighted.mean(
           history$home_xG,
           w = weights,
           na.rm = TRUE
         ),
         h2h_away_xg = weighted.mean(
           history$away_xG,
           w = weights,
           na.rm = TRUE
         ),
         effective_n = effective_n
       )
     }
   )
 }
 
 evaluate_half_life <- function(half_life,
                                h2h_history,
                                wilkens_lambdas,
                                evaluation,
                                eval_rows,
                                alpha = 0.35) {
   
   # Calculate weighted H2H xG
   weighted_h2h <- weighted_h2h_from_history(
     h2h_history,
     half_life = half_life
   )
   
   # Combine Wilkens lambdas with weighted H2H xG
   adjusted_lambdas <- tibble(
     
     home_lambda = if_else(
       is.na(weighted_h2h$h2h_home_xg),
       wilkens_lambdas$home_lambda,
       (1 - alpha) * wilkens_lambdas$home_lambda +
         alpha * weighted_h2h$h2h_home_xg
     ),
     
     away_lambda = if_else(
       is.na(weighted_h2h$h2h_away_xg),
       wilkens_lambdas$away_lambda,
       (1 - alpha) * wilkens_lambdas$away_lambda +
         alpha * weighted_h2h$h2h_away_xg
     )
   )
   
   # Convert lambdas to probabilities
   predictions <- purrr::map2_dfr(
     adjusted_lambdas$home_lambda,
     adjusted_lambdas$away_lambda,
     ~score_matrix(
       home_lambda = .x,
       away_lambda = .y
     )
   )
   
   # Restrict to common evaluation sample
   predictions_eval <- predictions[eval_rows, ]
   
   # Evaluate
   metrics <- evaluate_model(
     evaluation,
     predictions_eval$home_prob,
     predictions_eval$draw_prob,
     predictions_eval$away_prob
   )
   
   metrics %>%
     mutate(
       half_life = half_life
     )
 }
 

 
 unweighted_h2h_n <- function(h2h_history, n) {
   
   purrr::map_dfr(h2h_history, function(history) {
     
     if (is.null(history)) {
       return(tibble(
         h2h_home_xg = NA_real_,
         h2h_away_xg = NA_real_,
         n_h2h = 0L
       ))
     }
     
     history_n <- head(history, n)
     
     tibble(
       h2h_home_xg = mean(history_n$home_xG, na.rm = TRUE),
       h2h_away_xg = mean(history_n$away_xG, na.rm = TRUE),
       n_h2h = nrow(history_n)
     )
   })
 }
 
 evaluate_h2h_n <- function(n,
                            alpha = 0.35,
                            max_goals = 10) {
   
   # Get H2H averages
   h2h_values <- unweighted_h2h_n(
     h2h_history = h2h_history_6,
     n = n
   )
   
   # Blend H2H information with Wilkens lambdas
   home_lambda <- ifelse(
     is.na(h2h_values$h2h_home_xg),
     wilkens_lambdas$home_lambda,
     (1 - alpha) * wilkens_lambdas$home_lambda +
       alpha * h2h_values$h2h_home_xg
   )
   
   away_lambda <- ifelse(
     is.na(h2h_values$h2h_away_xg),
     wilkens_lambdas$away_lambda,
     (1 - alpha) * wilkens_lambdas$away_lambda +
       alpha * h2h_values$h2h_away_xg
   )
   
   # Generate probabilities
   predictions <- purrr::map2_dfr(
     home_lambda,
     away_lambda,
     ~ score_matrix(
       home_lambda = .x,
       away_lambda = .y,
       max_goals = max_goals
     )
   )
   
   # Restrict to common evaluation sample
   predictions_eval <- predictions[eval_rows, ]
   
   # Evaluate predictive performance
   metrics <- evaluate_model(
     evaluation,
     predictions_eval$home_prob,
     predictions_eval$draw_prob,
     predictions_eval$away_prob
   )
   
   # Add useful diagnostic information
   metrics %>%
     mutate(
       max_h2h = n,
       mean_h2h_used = mean(h2h_values$n_h2h[eval_rows]),
       pct_using_max = mean(
         h2h_values$n_h2h[eval_rows] == n
       ) * 100
     )
 }
 
 
 
 
 evaluate_half_life_6 <- function(half_life,
                                  alpha = 0.35,
                                  max_goals = 10) {
   
   # Calculate weighted H2H xG
   h2h_values <- weighted_h2h_from_history(
     h2h_history = h2h_history_6,
     half_life = half_life
   )
   
   # Combine Wilkens and H2H estimates
   home_lambda <- ifelse(
     is.na(h2h_values$h2h_home_xg),
     wilkens_lambdas$home_lambda,
     (1 - alpha) * wilkens_lambdas$home_lambda +
       alpha * h2h_values$h2h_home_xg
   )
   
   away_lambda <- ifelse(
     is.na(h2h_values$h2h_away_xg),
     wilkens_lambdas$away_lambda,
     (1 - alpha) * wilkens_lambdas$away_lambda +
       alpha * h2h_values$h2h_away_xg
   )
   
   # Generate match probabilities
   predictions <- purrr::map2_dfr(
     home_lambda,
     away_lambda,
     ~ score_matrix(
       home_lambda = .x,
       away_lambda = .y,
       max_goals = max_goals
     )
   )
   
   # Same evaluation sample as all previous models
   predictions_eval <- predictions[eval_rows, ]
   
   metrics <- evaluate_model(
     evaluation,
     predictions_eval$home_prob,
     predictions_eval$draw_prob,
     predictions_eval$away_prob
   )
   
   metrics %>%
     dplyr::mutate(
       half_life = half_life,
       
       # Average ESS among matches with H2H information
       mean_effective_n = mean(
         h2h_values$effective_n[eval_rows][
           h2h_values$effective_n[eval_rows] > 0
         ],
         na.rm = TRUE
       )
     )
 }
 
 evaluate_alpha_weighted <- function(alpha,
                                     half_life = 2.5,
                                     max_goals = 10) {
   
   h2h_values <- weighted_h2h_from_history(
     h2h_history = h2h_history_6,
     half_life = half_life
   )
   
   home_lambda <- ifelse(
     is.na(h2h_values$h2h_home_xg),
     wilkens_lambdas$home_lambda,
     (1 - alpha) * wilkens_lambdas$home_lambda +
       alpha * h2h_values$h2h_home_xg
   )
   
   away_lambda <- ifelse(
     is.na(h2h_values$h2h_away_xg),
     wilkens_lambdas$away_lambda,
     (1 - alpha) * wilkens_lambdas$away_lambda +
       alpha * h2h_values$h2h_away_xg
   )
   
   predictions <- purrr::map2_dfr(
     home_lambda,
     away_lambda,
     ~ score_matrix(
       home_lambda = .x,
       away_lambda = .y,
       max_goals = max_goals
     )
   )
   
   predictions_eval <- predictions[eval_rows, ]
   
   metrics <- evaluate_model(
     evaluation,
     predictions_eval$home_prob,
     predictions_eval$draw_prob,
     predictions_eval$away_prob
   )
   
   metrics %>%
     dplyr::mutate(alpha = alpha)
 }
 
 # ------------------------------------------------------------
 # 3. Evaluate one alpha / half-life combination
 # ------------------------------------------------------------
 
 evaluate_weighted_combo <- function(h2h_values,
                                     half_life,
                                     alpha,
                                     max_goals = 10) {
   
   home_lambda <- ifelse(
     is.na(h2h_values$h2h_home_xg),
     wilkens_lambdas$home_lambda,
     (1 - alpha) * wilkens_lambdas$home_lambda +
       alpha * h2h_values$h2h_home_xg
   )
   
   away_lambda <- ifelse(
     is.na(h2h_values$h2h_away_xg),
     wilkens_lambdas$away_lambda,
     (1 - alpha) * wilkens_lambdas$away_lambda +
       alpha * h2h_values$h2h_away_xg
   )
   
   predictions <- purrr::map2_dfr(
     home_lambda,
     away_lambda,
     ~ score_matrix(
       home_lambda = .x,
       away_lambda = .y,
       max_goals = max_goals
     )
   )
   
   predictions_eval <- predictions[eval_rows, ]
   
   metrics <- evaluate_model(
     evaluation,
     predictions_eval$home_prob,
     predictions_eval$draw_prob,
     predictions_eval$away_prob
   )
   
   metrics %>%
     mutate(
       half_life = half_life,
       alpha = alpha
     )
 }
 
 evaluate_alpha <- function(alpha,
                            data,
                            evaluation,
                            eval_rows) {
   
   # Generate H2H predictions using this alpha
   predictions <- h2h_model(
     data = data,
     alpha = alpha
   )
   
   # Restrict to common evaluation sample
   predictions_eval <- predictions[eval_rows, ]
   
   # Evaluate model
   metrics <- evaluate_model(
     evaluation,
     predictions_eval$home_prob,
     predictions_eval$draw_prob,
     predictions_eval$away_prob
   )
   
   metrics %>%
     mutate(
       alpha = alpha
     )
 }
 
 unweighted_h2h_from_history <- function(h2h_history) {
   
   purrr::map_dfr(
     h2h_history,
     function(history) {
       
       if (is.null(history)) {
         return(
           tibble::tibble(
             h2h_home_xg = NA_real_,
             h2h_away_xg = NA_real_
           )
         )
       }
       
       tibble::tibble(
         h2h_home_xg = mean(
           history$home_xG,
           na.rm = TRUE
         ),
         
         h2h_away_xg = mean(
           history$away_xG,
           na.rm = TRUE
         )
       )
     }
   )
 }
 
 evaluate_alpha_fast <- function(alpha,
                                 wilkens_lambdas,
                                 h2h_unweighted,
                                 evaluation,
                                 eval_rows,
                                 max_goals = 10) {
   
   adjusted_lambdas <- tibble::tibble(
     
     home_lambda = dplyr::if_else(
       is.na(h2h_unweighted$h2h_home_xg),
       wilkens_lambdas$home_lambda,
       (1 - alpha) * wilkens_lambdas$home_lambda +
         alpha * h2h_unweighted$h2h_home_xg
     ),
     
     away_lambda = dplyr::if_else(
       is.na(h2h_unweighted$h2h_away_xg),
       wilkens_lambdas$away_lambda,
       (1 - alpha) * wilkens_lambdas$away_lambda +
         alpha * h2h_unweighted$h2h_away_xg
     )
   )
   
   predictions <- purrr::map2_dfr(
     adjusted_lambdas$home_lambda,
     adjusted_lambdas$away_lambda,
     ~score_matrix(
       home_lambda = .x,
       away_lambda = .y,
       max_goals = max_goals
     )
   )
   
   predictions_eval <- predictions[eval_rows, ]
   
   evaluate_model(
     evaluation,
     predictions_eval$home_prob,
     predictions_eval$draw_prob,
     predictions_eval$away_prob
   ) %>%
     dplyr::mutate(
       alpha = alpha
     )
 }
 
 final_weighted_h2h_model <- function(
    data,
    n = 3,
    h2h_seasons = 6,
    max_h2h = 6,
    alpha = 0.40,
    half_life = 3,
    max_goals = 10
 ) {
   
   # --------------------------------------------------
   # 1. Wilkens rolling xG lambdas
   # --------------------------------------------------
   
   wilkens_lambdas <- tibble::tibble(
     home_lambda = rolling_xg(
       data,
       venue = "home",
       n = n
     ),
     
     away_lambda = rolling_xg(
       data,
       venue = "away",
       n = n
     )
   )
   
   
   # --------------------------------------------------
   # 2. Build expanded H2H history
   # --------------------------------------------------
   
   h2h_history <- build_h2h_history(
     data = data,
     h2h_seasons = h2h_seasons,
     max_h2h = max_h2h
   )
   
   
   # --------------------------------------------------
   # 3. Apply recency weighting to H2H history
   # --------------------------------------------------
   
   h2h_values <- weighted_h2h_from_history(
     h2h_history = h2h_history,
     half_life = half_life
   )
   
   
   # --------------------------------------------------
   # 4. Blend Wilkens and weighted H2H lambdas
   # --------------------------------------------------
   
   home_lambda <- ifelse(
     is.na(h2h_values$h2h_home_xg),
     wilkens_lambdas$home_lambda,
     (1 - alpha) * wilkens_lambdas$home_lambda +
       alpha * h2h_values$h2h_home_xg
   )
   
   away_lambda <- ifelse(
     is.na(h2h_values$h2h_away_xg),
     wilkens_lambdas$away_lambda,
     (1 - alpha) * wilkens_lambdas$away_lambda +
       alpha * h2h_values$h2h_away_xg
   )
   
   
   # --------------------------------------------------
   # 5. Convert lambdas into outcome probabilities
   # --------------------------------------------------
   
   predictions <- purrr::map2_dfr(
     home_lambda,
     away_lambda,
     ~ score_matrix(
       home_lambda = .x,
       away_lambda = .y,
       max_goals = max_goals
     )
   )
   
   
   # --------------------------------------------------
   # 6. Return predictions
   # --------------------------------------------------
   
   predictions
 }
 
 weighted_h2h_model <- function(
    data,
    n,
    h2h_seasons,
    max_h2h,
    alpha,
    half_life,
    max_goals = 10
 ) {
   
   # Wilkens rolling xG estimates
   home_lambda <- rolling_xg(
     data,
     venue = "home",
     n = n
   )
   
   away_lambda <- rolling_xg(
     data,
     venue = "away",
     n = n
   )
   
   
   # Build H2H history
   h2h_history <- build_h2h_history(
     data = data,
     h2h_seasons = h2h_seasons,
     max_h2h = max_h2h
   )
   
   
   # Apply recency weighting
   h2h_values <- weighted_h2h_from_history(
     h2h_history = h2h_history,
     half_life = half_life
   )
   
   
   # Blend Wilkens and H2H estimates
   adjusted_home_lambda <- ifelse(
     is.na(h2h_values$h2h_home_xg),
     home_lambda,
     (1 - alpha) * home_lambda +
       alpha * h2h_values$h2h_home_xg
   )
   
   adjusted_away_lambda <- ifelse(
     is.na(h2h_values$h2h_away_xg),
     away_lambda,
     (1 - alpha) * away_lambda +
       alpha * h2h_values$h2h_away_xg
   )
   
   
   # Convert lambdas to outcome probabilities
   predictions <- purrr::map2_dfr(
     adjusted_home_lambda,
     adjusted_away_lambda,
     ~ score_matrix(
       home_lambda = .x,
       away_lambda = .y,
       max_goals = max_goals
     )
   )
   
   
   predictions
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
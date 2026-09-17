# ============================================================
# RANDOM-HISTORY CONTROL EXPERIMENT
# ============================================================

library(dplyr)
library(purrr)
library(tibble)


# ------------------------------------------------------------
# 1. Build matched random-history pools
# ------------------------------------------------------------

build_random_control_pools <- function(data,
                                       h2h_seasons = 6,
                                       max_h2h = 6) {
  
  n_matches <- nrow(data)
  
  # ----------------------------------------------------------
  # Pre-index rows by team / venue / season
  # This avoids repeatedly filtering the entire dataframe
  # ----------------------------------------------------------
  
  home_keys <- paste(
    data$home_team,
    data$season,
    sep = "___"
  )
  
  away_keys <- paste(
    data$away_team,
    data$season,
    sep = "___"
  )
  
  home_index <- split(
    seq_len(n_matches),
    home_keys
  )
  
  away_index <- split(
    seq_len(n_matches),
    away_keys
  )
  
  
  # ----------------------------------------------------------
  # Pre-index H2H pairs
  # ----------------------------------------------------------
  
  pair_keys <- paste(
    data$home_team,
    data$away_team,
    sep = "___"
  )
  
  pair_index <- split(
    seq_len(n_matches),
    pair_keys
  )
  
  
  # ----------------------------------------------------------
  # Build control pools for every current match
  # ----------------------------------------------------------
  
  pools <- vector("list", n_matches)
  
  for (i in seq_len(n_matches)) {
    
    current <- data[i, ]
    
    pair_key <- paste(
      current$home_team,
      current$away_team,
      sep = "___"
    )
    
    pair_rows <- pair_index[[pair_key]]
    
    # Previous same-orientation H2Hs
    historical_h2h <- pair_rows[
      pair_rows < i &
        data$season[pair_rows] >= current$season - h2h_seasons &
        data$season[pair_rows] < current$season
    ]
    
    if (length(historical_h2h) == 0) {
      
      pools[[i]] <- NULL
      next
    }
    
    # Keep most recent H2Hs
    historical_h2h <- historical_h2h[
      order(data$date[historical_h2h], decreasing = TRUE)
    ]
    
    historical_h2h <- head(
      historical_h2h,
      max_h2h
    )
    
    # Seasons in which the H2Hs occurred
    target_seasons <- data$season[historical_h2h]
    
    
    # --------------------------------------------------------
    # Candidate pools matched to those exact seasons
    # --------------------------------------------------------
    
    home_pools <- lapply(
      target_seasons,
      function(s) {
        
        key <- paste(
          current$home_team,
          s,
          sep = "___"
        )
        
        home_index[[key]]
      }
    )
    
    
    away_pools <- lapply(
      target_seasons,
      function(s) {
        
        key <- paste(
          current$away_team,
          s,
          sep = "___"
        )
        
        away_index[[key]]
      }
    )
    
    
    pools[[i]] <- list(
      
      target_seasons = target_seasons,
      
      h2h_rows = historical_h2h,
      
      home_pools = home_pools,
      
      away_pools = away_pools
    )
  }
  
  pools
}

# ------------------------------------------------------------
# 2. Generate one random-history sample
# ------------------------------------------------------------

sample_random_history <- function(data,
                                  control_pools,
                                  half_life = 3) {
  
  n_matches <- nrow(data)
  
  random_home_xg <- rep(NA_real_, n_matches)
  random_away_xg <- rep(NA_real_, n_matches)
  
  home_effective_n <- rep(0, n_matches)
  away_effective_n <- rep(0, n_matches)
  
  
  for (i in seq_len(n_matches)) {
    
    pools <- control_pools[[i]]
    
    if (is.null(pools)) {
      next
    }
    
    
    # --------------------------------------------------------
    # Sample one HOME fixture from each matched season
    # --------------------------------------------------------
    
    sampled_home_rows <- vapply(
      pools$home_pools,
      function(rows) {
        
        if (is.null(rows) || length(rows) == 0) {
          return(NA_integer_)
        }
        
        sample(rows, size = 1)
      },
      integer(1)
    )
    
    
    # --------------------------------------------------------
    # Sample one AWAY fixture from each matched season
    # --------------------------------------------------------
    
    sampled_away_rows <- vapply(
      pools$away_pools,
      function(rows) {
        
        if (is.null(rows) || length(rows) == 0) {
          return(NA_integer_)
        }
        
        sample(rows, size = 1)
      },
      integer(1)
    )
    
    
    sampled_home_rows <- sampled_home_rows[
      !is.na(sampled_home_rows)
    ]
    
    sampled_away_rows <- sampled_away_rows[
      !is.na(sampled_away_rows)
    ]
    
    
    # --------------------------------------------------------
    # HOME weighted xG
    # --------------------------------------------------------
    
    if (length(sampled_home_rows) > 0) {
      
      home_age <- as.numeric(
        difftime(
          data$date[i],
          data$date[sampled_home_rows],
          units = "days"
        )
      ) / 365.25
      
      home_weights <- 2^(
        -home_age / half_life
      )
      
      random_home_xg[i] <- weighted.mean(
        data$home_xG[sampled_home_rows],
        w = home_weights,
        na.rm = TRUE
      )
      
      home_effective_n[i] <-
        (sum(home_weights)^2) /
        sum(home_weights^2)
    }
    
    
    # --------------------------------------------------------
    # AWAY weighted xG
    # --------------------------------------------------------
    
    if (length(sampled_away_rows) > 0) {
      
      away_age <- as.numeric(
        difftime(
          data$date[i],
          data$date[sampled_away_rows],
          units = "days"
        )
      ) / 365.25
      
      away_weights <- 2^(
        -away_age / half_life
      )
      
      random_away_xg[i] <- weighted.mean(
        data$away_xG[sampled_away_rows],
        w = away_weights,
        na.rm = TRUE
      )
      
      away_effective_n[i] <-
        (sum(away_weights)^2) /
        sum(away_weights^2)
    }
  }
  
  
  tibble(
    random_home_xg = random_home_xg,
    random_away_xg = random_away_xg,
    home_effective_n = home_effective_n,
    away_effective_n = away_effective_n
  )
}

# ------------------------------------------------------------
# 3. Fast vectorised Poisson probabilities
# ------------------------------------------------------------

score_probabilities_fast <- function(home_lambda,
                                     away_lambda,
                                     max_goals = 10) {
  
  goals <- 0:max_goals
  
  home_goal_probs <- sapply(
    goals,
    function(g) dpois(g, home_lambda)
  )
  
  away_goal_probs <- sapply(
    goals,
    function(g) dpois(g, away_lambda)
  )
  
  
  # Draw probability
  draw_prob <- rowSums(
    home_goal_probs * away_goal_probs
  )
  
  
  # Home win:
  # home scores g and away scores fewer than g
  away_less_than <- sapply(
    goals,
    function(g) ppois(g - 1, away_lambda)
  )
  
  home_prob <- rowSums(
    home_goal_probs * away_less_than
  )
  
  
  # Away win
  home_less_than <- sapply(
    goals,
    function(g) ppois(g - 1, home_lambda)
  )
  
  away_prob <- rowSums(
    away_goal_probs * home_less_than
  )
  
  
  tibble(
    home_prob = home_prob,
    draw_prob = draw_prob,
    away_prob = away_prob
  )
}

# ------------------------------------------------------------
# 4. Evaluate one random-control simulation
# ------------------------------------------------------------

run_random_control_simulation <- function(data,
                                          control_pools,
                                          half_life = 3,
                                          alpha = 0.40,
                                          max_goals = 10) {
  
  # Random matched historical information
  random_history <- sample_random_history(
    data = data,
    control_pools = control_pools,
    half_life = half_life
  )
  
  
  # ----------------------------------------------------------
  # Blend with Wilkens lambdas
  # ----------------------------------------------------------
  
  home_lambda <- ifelse(
    is.na(random_history$random_home_xg),
    wilkens_lambdas$home_lambda,
    (1 - alpha) * wilkens_lambdas$home_lambda +
      alpha * random_history$random_home_xg
  )
  
  
  away_lambda <- ifelse(
    is.na(random_history$random_away_xg),
    wilkens_lambdas$away_lambda,
    (1 - alpha) * wilkens_lambdas$away_lambda +
      alpha * random_history$random_away_xg
  )
  
  
  # ----------------------------------------------------------
  # Generate probabilities
  # ----------------------------------------------------------
  
  predictions <- score_probabilities_fast(
    home_lambda = home_lambda,
    away_lambda = away_lambda,
    max_goals = max_goals
  )
  
  
  predictions_eval <- predictions[
    eval_rows,
  ]
  
  
  # ----------------------------------------------------------
  # Evaluate
  # ----------------------------------------------------------
  
  metrics <- evaluate_model(
    evaluation,
    predictions_eval$home_prob,
    predictions_eval$draw_prob,
    predictions_eval$away_prob
  )
  
  
  metrics %>%
    mutate(
      
      mean_home_effective_n = mean(
        random_history$home_effective_n[eval_rows][
          random_history$home_effective_n[eval_rows] > 0
        ],
        na.rm = TRUE
      ),
      
      mean_away_effective_n = mean(
        random_history$away_effective_n[eval_rows][
          random_history$away_effective_n[eval_rows] > 0
        ],
        na.rm = TRUE
      )
    )
}

sample_one_match_diagnostic <- function(data,
                                        control_pools,
                                        i,
                                        half_life = 3) {
  
  p <- control_pools[[i]]
  
  if (is.null(p)) return(NULL)
  
  home_rows <- vapply(
    p$home_pools,
    function(rows) sample(rows, 1),
    integer(1)
  )
  
  away_rows <- vapply(
    p$away_pools,
    function(rows) sample(rows, 1),
    integer(1)
  )
  
  home_out <- data[home_rows, ] %>%
    transmute(
      side = "home",
      season,
      date,
      team = home_team,
      opponent = away_team,
      xG = home_xG,
      age_years =
        as.numeric(
          difftime(data$date[i], date, units = "days")
        ) / 365.25,
      weight =
        2^(-age_years / half_life)
    )
  
  away_out <- data[away_rows, ] %>%
    transmute(
      side = "away",
      season,
      date,
      team = away_team,
      opponent = home_team,
      xG = away_xG,
      age_years =
        as.numeric(
          difftime(data$date[i], date, units = "days")
        ) / 365.25,
      weight =
        2^(-age_years / half_life)
    )
  
  bind_rows(home_out, away_out)
}
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
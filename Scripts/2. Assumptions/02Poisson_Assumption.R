library(here)
library(dplyr)
library(purrr)
library(ggplot2)
library(patchwork)

source(here("Scripts","2. Assumptions", "Assumptions_Functions.R"))

#----------------------------------------------------------
# Compare means and variances of home and away goals
#----------------------------------------------------------


epl_database %>%
  summarise(
    home_mean = mean(home_goals),
    home_variance = var(home_goals),
    away_mean = mean(away_goals),
    away_variance = var(away_goals)
  )

epl_database %>%
  summarise(
    home_dispersion = var(home_goals) / mean(home_goals),
    away_dispersion = var(away_goals) / mean(away_goals)
  )


#----------------------------------------------------------
# Compare  and plot observed frequencies with Poisson distribution
#----------------------------------------------------------

home_poisson <- poisson_comparison(
  epl_database$home_goals
)

home_poisson

away_poisson <- poisson_comparison(
  epl_database$away_goals
)

away_poisson

home_plot <- plot_poisson_comparison(
  home_poisson,
  "Home Goals: Observed vs Poisson"
)

home_plot

away_plot <- plot_poisson_comparison(
  away_poisson,
  "Away Goals: Observed vs Poisson"
)

away_plot

(home_plot)/
  (away_plot)

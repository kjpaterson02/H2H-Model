#==========================================================
# Test Independence Assumption on Data
#==========================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(here)

source(here("Scripts", "2. Assumptions", "Assumptions_Functions.R"))


#----------------------------------------------------------
# 1. Test correlation between home and away goals
#----------------------------------------------------------

# Test whether home and away goals are linearly associated.
cor_test <- cor.test(
  epl_database$home_goals,
  epl_database$away_goals
)

cor_test


#----------------------------------------------------------
# 2. Examine observed scoreline frequencies
#----------------------------------------------------------

# Calculate the observed frequency of each exact scoreline.
scorelines <- epl_database %>%
  count(home_goals, away_goals) %>%
  mutate(
    observed = n / sum(n)
  )

# Display the 15 most common scorelines.
scorelines %>%
  arrange(desc(n)) %>%
  head(15)

scorelines


#----------------------------------------------------------
# 3. Group scorelines and calculate marginal probabilities
#----------------------------------------------------------

# Group all scores of five or more goals into a single category.
score_data <- epl_database %>%
  mutate(
    home_goals_group = pmin(home_goals, 5),
    away_goals_group = pmin(away_goals, 5)
  )

# Calculate marginal home-goal probabilities.
home_marginal <- score_data %>%
  count(home_goals_group) %>%
  mutate(
    p_home = n / sum(n)
  ) %>%
  select(home_goals_group, p_home)

# Calculate marginal away-goal probabilities.
away_marginal <- score_data %>%
  count(away_goals_group) %>%
  mutate(
    p_away = n / sum(n)
  ) %>%
  select(away_goals_group, p_away)


#----------------------------------------------------------
# 4. Calculate expected probabilities under independence
#----------------------------------------------------------

# Under independence, joint scoreline probabilities equal
# the product of the corresponding marginal probabilities.
expected_independent <- tidyr::crossing(
  home_goals_group = 0:5,
  away_goals_group = 0:5
) %>%
  left_join(home_marginal, by = "home_goals_group") %>%
  left_join(away_marginal, by = "away_goals_group") %>%
  mutate(
    expected = p_home * p_away
  )


#----------------------------------------------------------
# 5. Compare observed and expected scorelines
#----------------------------------------------------------

# Calculate the observed probabilities for grouped scorelines.
observed <- score_data %>%
  count(home_goals_group, away_goals_group) %>%
  mutate(
    observed = n / sum(n)
  )

# Combine observed and independence-based expected probabilities.
score_comparison <- expected_independent %>%
  left_join(
    observed,
    by = c("home_goals_group", "away_goals_group")
  ) %>%
  mutate(
    observed = replace_na(observed, 0)
  )

score_comparison


#----------------------------------------------------------
# 6. Plot deviations from independence
#----------------------------------------------------------

# Plot observed minus expected probability for each scoreline.
ggplot(
  score_comparison,
  aes(
    x = away_goals_group,
    y = home_goals_group,
    fill = observed - expected
  )
) +
  geom_tile() +
  geom_text(
    aes(
      label = sprintf(
        "%.3f",
        observed - expected
      )
    )
  ) +
  scale_x_continuous(
    breaks = 0:5,
    labels = c("0", "1", "2", "3", "4", "5+")
  ) +
  scale_y_continuous(
    breaks = 0:5,
    labels = c("0", "1", "2", "3", "4", "5+")
  ) +
  scale_fill_gradient2(
    low = "steelblue4",
    mid = "white",
    high = "firebrick",
    midpoint = 0
  ) +
  labs(
    title = "Observed vs Independent Scoreline Probabilities",
    x = "Away goals",
    y = "Home goals",
    fill = "Observed − Expected"
  ) +
  theme_minimal()
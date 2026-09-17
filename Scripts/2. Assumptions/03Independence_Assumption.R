library(tidyr)

source(here("Scripts","2. Assumptions", "Assumptions_Functions.R"))


#----------------------------------------------------------
# Test correlation between home and away goals
#----------------------------------------------------------

cor_test <- cor.test(
  epl_database$home_goals,
  epl_database$away_goals
)

cor_test

scorelines <- epl_database %>%
  count(home_goals, away_goals) %>%
  mutate(
    observed = n / sum(n)
  )

scorelines %>%
  arrange(desc(n)) %>%
  head(15)

scorelines

score_data <- epl_database %>%
  mutate(
    home_goals_group = pmin(home_goals, 5),
    away_goals_group = pmin(away_goals, 5)
  )

home_marginal <- score_data %>%
  count(home_goals_group) %>%
  mutate(
    p_home = n / sum(n)
  ) %>%
  select(home_goals_group, p_home)

away_marginal <- score_data %>%
  count(away_goals_group) %>%
  mutate(
    p_away = n / sum(n)
  ) %>%
  select(away_goals_group, p_away)

expected_independent <- tidyr::crossing(
  home_goals_group = 0:5,
  away_goals_group = 0:5
) %>%
  left_join(home_marginal, by = "home_goals_group") %>%
  left_join(away_marginal, by = "away_goals_group") %>%
  mutate(
    expected = p_home * p_away
  )

observed <- score_data %>%
  count(home_goals_group, away_goals_group) %>%
  mutate(
    observed = n / sum(n)
  )

score_comparison <- expected_independent %>%
  left_join(
    observed,
    by = c("home_goals_group", "away_goals_group")
  ) %>%
  mutate(
    observed = replace_na(observed, 0)
  )

score_comparison

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
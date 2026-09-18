#==========================================================
# Cumulative betting results plots
#==========================================================
library(dplyr)
library(ggplot2)
library(here)

source(here("Scripts", "5. Betting", "Betting_Functions.R"))


# ------------------------------------------------------------
# 1. Generate plots for betting results
# ------------------------------------------------------------
plot_end_date <- max(
  rolling_bets_reportable$date,
  na.rm = TRUE
)
# P&L-optimised strategy.
pnl_plot <- plot_cumulative_metric(
  rolling_bets_reportable,
  "pnl",
  plot_end_date
)

# ROI-optimised strategy.
roi_plot <- plot_cumulative_metric(
  rolling_bets_reportable,
  "roi",
  plot_end_date
)

# Sharpe-optimised strategy.
sharpe_plot <- plot_cumulative_metric(
  rolling_bets_reportable,
  "sharpe",
  plot_end_date
)


# ------------------------------------------------------------
# 2. Display plots
# ------------------------------------------------------------

pnl_plot
roi_plot
sharpe_plot
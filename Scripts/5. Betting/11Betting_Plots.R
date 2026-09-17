# ------------------------------------------------------------
# Cumulative P&L plots
# ------------------------------------------------------------

# Set a common end date for all plots.
plot_end_date <- max(
  rolling_bets_reportable$date,
  na.rm = TRUE
)



# ------------------------------------------------------------
# 1. Generate plots for betting results
# ------------------------------------------------------------

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
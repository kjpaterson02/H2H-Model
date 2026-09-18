#==========================================================
# Download Understat Premier League Match Data
# Seasons: 2014/15 - 2025/26
#==========================================================

library(httr2)
library(jsonlite)
library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(lubridate)
library(here)

source(here("Scripts","1. Data Extraction", "ExtractData_Functions.R"))
#----------------------------------------------------------
# Download every EPL season
#----------------------------------------------------------

seasons <- 2014:2025

epl_matches <- map_dfr(
  seasons,
  ~get_understat_matches(
    league = "EPL",
    season = .x
  )
)

epl_matches <- epl_matches %>%
  mutate(
    season = if_else(
      month(date) >= 8,
      year(date),
      year(date) - 1
    )
  )

#----------------------------------------------------------
# Save the dataset
#----------------------------------------------------------

saveRDS(
  epl_matches,
  here("Data", "understat_epl_matches.rds")
)

write.csv(
  epl_matches,
  here("Data", "understat_epl_matches_2014_2025.csv"),
  row.names = FALSE
)


#==========================================================
# Import bookmaker odds
#==========================================================

# Historical EPL odds files should first be downloaded from Football-Data.co.uk and placed in:
# Data/EPL ODDS/
#
# One CSV file is required for each season from 2014/15 onwards.

raw_odds <- here("Data", "EPL ODDS")

files <- list.files(
  path = raw_odds,
  pattern = "\\.csv$",
  full.names = TRUE
)

# Stop with a clear message if the raw odds files are missing.
if (length(files) == 0) {
  stop(
    "No bookmaker odds files found. ",
    "Download the Football-Data EPL CSV files and place them in ",
    "'Data/EPL ODDS/'."
  )
}

epl_odds <- map_dfr(
  files,
  read_odds
)

epl_odds <- map_dfr(files, read_odds)

epl_odds <- epl_odds %>%
  filter(
    !is.na(HomeTeam),
    !is.na(AwayTeam)
  )

#----------------------------------------------------------
# Calculate average  and maximum odds
#----------------------------------------------------------

epl_odds <- epl_odds %>%
  mutate(
    AvgH_calc = rowMeans(select(., B365H, BWH, IWH, PSH, WHH, VCH), na.rm = TRUE),
    AvgD_calc = rowMeans(select(., B365D, BWD, IWD, PSD, WHD, VCD), na.rm = TRUE),
    AvgA_calc = rowMeans(select(., B365A, BWA, IWA, PSA, WHA, VCA), na.rm = TRUE)
  ) %>%
  mutate(
    MaxH_calc = pmax(B365H, BWH, IWH, PSH, WHH, VCH, na.rm = TRUE),
    MaxD_calc = pmax(B365D, BWD, IWD, PSD, WHD, VCD, na.rm = TRUE),
    MaxA_calc = pmax(B365A, BWA, IWA, PSA, WHA, VCA, na.rm = TRUE)
  )

#----------------------------------------------------------
# Keep required variables
#----------------------------------------------------------

epl_odds_clean <- epl_odds %>%
  select(
    season,
    Date,
    
    HomeTeam,
    AwayTeam,
    
    B365H,
    B365D,
    B365A,
    
    AvgH_calc,
    AvgD_calc,
    AvgA_calc,
    
    MaxH_calc,
    MaxD_calc,
    MaxA_calc
  )

#----------------------------------------------------------
# Rename variables
#----------------------------------------------------------

epl_odds_clean <- epl_odds_clean %>%
  rename(
    date = Date,
    home_team = HomeTeam,
    away_team = AwayTeam,
    
    B365_home = B365H,
    B365_draw = B365D,
    B365_away = B365A,
    
    Avg_home = AvgH_calc,
    Avg_draw = AvgD_calc,
    Avg_away = AvgA_calc,
    
    Max_home = MaxH_calc,
    Max_draw = MaxD_calc,
    Max_away = MaxA_calc
  )

#----------------------------------------------------------
# Standardise team names
#----------------------------------------------------------

team_lookup <- c(
  "Man City" = "Manchester City",
  "Man United" = "Manchester United",
  "Newcastle" = "Newcastle United",
  "Nott'm Forest" = "Nottingham Forest",
  "QPR" = "Queens Park Rangers",
  "West Brom" = "West Bromwich Albion",
  "Wolves" = "Wolverhampton Wanderers"
)

epl_odds_clean <- epl_odds_clean %>%
  mutate(
    home_team = recode(home_team, !!!team_lookup),
    away_team = recode(away_team, !!!team_lookup)
  )

#----------------------------------------------------------
# Convert dates
#----------------------------------------------------------

epl_odds_clean <- epl_odds_clean %>%
  mutate(
    date = dmy(date)
  )

#----------------------------------------------------------
# Merge Understat and odds data
#----------------------------------------------------------

# Merge by season and teams rather than date, as match dates
# differ slightly between the two data sources for some fixtures.
epl_database <- epl_matches %>%
  left_join(
    epl_odds_clean,
    by = c(
      "season",
      "home_team",
      "away_team"
    ),
    suffix = c("_understat", "_odds")
  )

# Verify that every Understat match was successfully matched to odds.
stopifnot(
  sum(is.na(epl_database$Avg_home)) == 0
)

epl_database <- epl_database %>%
  rename(date = date_understat) %>%
  select(-date_odds)

epl_database <- epl_database %>%
  arrange(date)

epl_database <- epl_database %>%
  mutate(
    result = case_when(
      home_goals > away_goals ~ "H",
      home_goals == away_goals ~ "D",
      TRUE ~ "A"
    )
  )

epl_database <- epl_database %>%
  mutate(
    obs_home = as.integer(result == "H"),
    obs_draw = as.integer(result == "D"),
    obs_away = as.integer(result == "A")
  )
#----------------------------------------------------------
# Save final dataset
#----------------------------------------------------------

saveRDS(
  epl_database,
  here("Data","epl_database.rds")
)

write_csv(
  epl_database,
  here("Data", "epl_database.csv")
)
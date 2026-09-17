#Retrieve match data from understat
get_understat_matches <- function(league = "EPL", season){
  
  url <- sprintf(
    "https://understat.com/getLeagueData/%s/%s",
    league,
    season
  )
  
  resp <- request(url) |>
    req_headers(
      `X-Requested-With` = "XMLHttpRequest"
    ) |>
    req_perform()
  
  json <- fromJSON(resp |> resp_body_string())
  
  matches <- json$dates |>
    transmute(
      season = paste0(season, "/", substr(season + 1, 3, 4)),
      date = as.Date(datetime),
      home_team = h$title,
      away_team = a$title,
      home_goals = as.integer(goals$h),
      away_goals = as.integer(goals$a),
      home_xG = as.numeric(xG$h),
      away_xG = as.numeric(xG$a)
    )
  
  return(matches)
}

#Read odds data from Odds file downloaded from footballdata.com
read_odds <- function(file) {
  
  df <- read_csv(file, show_col_types = FALSE)
  
  # Extract season from filename
  # 1415.csv -> 2014
  # 2021.csv -> 2020
  code <- str_extract(basename(file), "\\d{4}")
  
  df$season <- 2000 + as.integer(substr(code, 1, 2))
  
  df
}



#Utilize existing R packages

Schedules<-list()

# NFL ---------------------------------------------------------------------
#NFL Schedule Compiler

library(nflreadr)
library(dplyr)
library(lubridate)

today <- Sys.Date()
yr <- as.integer(format(today, "%Y"))
mo <- as.integer(format(today, "%m"))

current_season <- if (mo < 3) yr - 1 else yr

sched_nfl <- load_schedules(seasons = current_season) %>%
  select(
    Date = gameday,
    Time = gametime,
    Away = away_team,
    Home = home_team,
    Location = stadium
  ) %>%
  mutate(
    Time = format(
      with_tz(
        ymd_hm(paste(Date, Time), tz = "America/New_York"),
        "America/Los_Angeles"
      ),
      "%H:%M"
    )
  )

Schedules$NFL <- sched_nfl
rm(sched_nfl)

# NBA ---------------------------------------------------------------------
#NBA Schedule Compiler

library(hoopR)
library(dplyr)
library(lubridate)

today <- Sys.Date()
yr <- as.integer(format(today, "%Y"))
mo <- as.integer(format(today, "%m"))
current_season <- if (mo < 7) yr else yr + 1

sched_nba <- load_nba_schedule(seasons = current_season) %>%
  mutate(
    game_date_time = force_tz(
      as.POSIXct(game_date_time),
      "America/New_York"
    )
  ) %>%
  transmute(
    Date = as.Date(game_date_time),
    Time = format(
      with_tz(game_date_time, "America/Los_Angeles"),
      "%H:%M"
    ),
    Away = away_abbreviation,
    Home = home_abbreviation,
    Location = venue_full_name
  )

Schedules$NBA <- sched_nba
rm(sched_nba)

# NHL ---------------------------------------------------------------------
# NHL Schedule Compiler

library(httr)
library(jsonlite)
library(lubridate)

# 1. Get the data
res <- GET("https://api-web.nhle.com/v1/schedule/now")
raw <- fromJSON(content(res, "text", encoding = "UTF-8"), flatten = FALSE)

target_date <- as.character(Sys.Date())
day_index <- which(raw$gameWeek$date == target_date)
if (length(day_index) == 0) day_index <- 1  

games <- raw$gameWeek$games[[day_index]]

Date     <- as.character(raw$gameWeek$date[day_index])
Time     <- format(with_tz(ymd_hms(games$startTimeUTC), "America/Los_Angeles"), "%H:%M")
Away     <- games$awayTeam$commonName$default
Home     <- games$homeTeam$commonName$default
Location    <- games$venue$default

sched_nhl <- data.frame(
  Date = Date,
  Time = Time,
  Away = Away,
  Home = Home,
  Location = Location,
  stringsAsFactors = FALSE
)

sched_nhl <- sched_nhl[order(sched_nhl$Time), ]

if (nrow(sched_nhl) == 0) {
  sched_nhl <- data.frame(
    Date = as.Date("1900-01-01"),
    Time = "12:00 AM",
    Away = "Place",
    Home = "Holder",
    Location = "Placeholder Arena",
    stringsAsFactors = FALSE
  )
}

Schedules$NHL <-sched_nhl

# MLB ---------------------------------------------------------------------
#MLB Scheduler Compiler

library(baseballr)
library(dplyr)
library(lubridate)

# determine current season
today <- Sys.Date()
yr <- as.integer(format(today, "%Y"))

# pull MLB schedule
mlb_raw <- mlb_schedule(season = yr)

# build flat MLB schedule
sched_mlb <- mlb_raw %>%
  mutate(
    game_datetime = with_tz(
      as.POSIXct(game_date, tz = "UTC"),
      "America/Los_Angeles"
    )
  ) %>%
  transmute(
    Date = as.Date(game_datetime),
    Time = format(game_datetime, "%H:%M"),
    Away = teams_away_team_name,
    Home = teams_home_team_name,
    Location = venue_name
  ) %>%
  as.data.frame(stringsAsFactors = FALSE)

# schema guard (fail fast)
stopifnot(
  nrow(sched_mlb) > 0,
  ncol(sched_mlb) == 5,
  identical(
    names(sched_mlb),
    c("Date", "Time", "Away", "Home", "Location")
  )
)

Schedules$MLB <- sched_mlb
rm(sched_mlb, mlb_raw)

# MLS ---------------------------------------------------------------------
#MLS Schedule Compiler

library(worldfootballR)
library(dplyr)
library(lubridate)

today <- Sys.Date()
yr <- as.integer(format(today, "%Y"))

mls_raw <- tryCatch(
  fb_match_results(
    country = "USA",
    gender = "M",
    tier = "1",
    season_end_year = yr
  ),
  error = function(e) NULL
)

if (is.null(mls_raw) || nrow(mls_raw) == 0) {
  
  # explicit placeholder row
  sched_mls <- data.frame(
    Date = as.Date("1900-01-01"),
    Time = "00:00",
    Away = "Place",
    Home = "Holder",
    Location = "Placeholder Field",
    stringsAsFactors = FALSE
  )
  
} else {
  
  sched_mls <- mls_raw %>%
    mutate(
      game_datetime = suppressWarnings(
        with_tz(
          as.POSIXct(paste(Date, Time), tz = "UTC"),
          "America/Los_Angeles"
        )
      )
    ) %>%
    transmute(
      Date = as.Date(game_datetime),
      Time = format(game_datetime, "%H:%M"),
      Away = Away,
      Home = Home,
      Location = ifelse(
        is.na(Venue) | Venue == "",
        "Unknown",
        Venue
      )
    ) %>%
    as.data.frame(stringsAsFactors = FALSE)
}

stopifnot(
  ncol(sched_mls) == 5,
  identical(
    names(sched_mls),
    c("Date", "Time", "Away", "Home", "Location")
  )
)

Schedules$MLS <- sched_mls
rm(sched_mls, mls_raw)

# Master List and Data Normalization --------------------------------------

Schedules <- lapply(Schedules, function(x) {
  if (!is.data.frame(x)) {
    stop("Non-data.frame object found in Schedules")
  }
  as.data.frame(x, stringsAsFactors = FALSE)
})

nfl_lookup <- readRDS("nfl_team_lookup.rds")
nba_lookup <- readRDS("nba_team_lookup.rds")

Schedules$NFL$Away <- nfl_lookup$Full[
  match(Schedules$NFL$Away, nfl_lookup$Abbr)
] %||% Schedules$NFL$Away

Schedules$NFL$Home <- nfl_lookup$Full[
  match(Schedules$NFL$Home, nfl_lookup$Abbr)
] %||% Schedules$NFL$Home

Schedules$NBA$Away <- nba_lookup$Full[
  match(Schedules$NBA$Away, nba_lookup$Abbr)
] %||% Schedules$NBA$Away

Schedules$NBA$Home <- nba_lookup$Full[
  match(Schedules$NBA$Home, nba_lookup$Abbr)
] %||% Schedules$NBA$Home

nhl_lookup <- readRDS("nhl_team_lookup.rds")

Schedules$NHL$Away <- coalesce(
  nhl_lookup$Full[match(Schedules$NHL$Away, nhl_lookup$Common)],
  Schedules$NHL$Away
)

Schedules$NHL$Home <- coalesce(
  nhl_lookup$Full[match(Schedules$NHL$Home, nhl_lookup$Common)],
  Schedules$NHL$Home
)

Schedules <- lapply(Schedules, function(df) {
  if (!is.data.frame(df)) {
    stop("Non-data.frame object found in Schedules")
  }
  
  df$Date <- as.Date(df$Date)
  df$Time <- as.character(df$Time)
  df$Away <- as.character(df$Away)
  df$Home <- as.character(df$Home)
  df$Location <- as.character(df$Location)
  
  df
})

Schedules$Master <- bind_rows(Schedules, .id = "League")

Schedules$Master$Time <- format(
  strptime(Schedules$Master$Time, "%H:%M"),
  "%I:%M %p"
)

# Today Listing -----------------------------------------------------------

Schedules$Today <- Schedules$Master %>%
  filter(Date == Sys.Date()) %>%
  mutate(
    Time_sort = as.POSIXct(
      paste(Date, Time),
      format = "%Y-%m-%d %I:%M %p",
      tz = "America/Los_Angeles"
    )
  ) %>%
  arrange(League, Time_sort) %>%
  select(-Time_sort)

rm(list = setdiff(ls(), "Schedules"))
todaysListing<-Schedules$Today
print(todaysListing)





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
library(dplyr)


raw <- fromJSON(
  content(
    GET("https://api-web.nhle.com/v1/schedule/now"),
    "text",
    encoding = "UTF-8"
  ),
  flatten = TRUE
)

# IMPORTANT: safely bind all games (handles mismatched columns)
games <- bind_rows(raw$gameWeek$games)


sched_nhl <- data.frame(
  Date = as.Date(
    with_tz(
      as.POSIXct(games$startTimeUTC, tz = "UTC"),
      "America/Los_Angeles"
    )
  ),
  Time = format(
    with_tz(
      as.POSIXct(games$startTimeUTC, tz = "UTC"),
      "America/Los_Angeles"
    ),
    "%H:%M"
  ),
  Away = paste(
    games$awayTeam.placeName.default,
    games$awayTeam.commonName.default
  ),
  Home = paste(
    games$homeTeam.placeName.default,
    games$homeTeam.commonName.default
  ),
  Location = as.character(games$venue.default),
  stringsAsFactors = FALSE
)


stopifnot(
  nrow(sched_nhl) > 0,
  ncol(sched_nhl) == 5,
  identical(
    names(sched_nhl),
    c("Date", "Time", "Away", "Home", "Location")
  )
)

Schedules$NHL <- sched_nhl
rm(sched_nhl, games, raw)


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

# store
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



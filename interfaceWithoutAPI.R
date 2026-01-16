#Utilize existing R packages

Schedules<-list()

# NFL ---------------------------------------------------------------------
#NFL Schedule Compilation

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
#NBA Schedule Compilation

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


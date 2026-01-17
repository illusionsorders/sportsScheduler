#Free Daily sports compiler not beholden to a master API
#January 2026
#Illusions Orders

Schedules<-list()

library(nflreadr)
library(dplyr)
library(lubridate)
library(hoopR)
library(baseballr)
library(worldfootballR)

# NFL ---------------------------------------------------------------------
#NFL Schedule Compiler

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

today <- Sys.Date()
season <- if (month(today) < 7) year(today) else year(today) + 1

nba_raw <- espn_nba_scoreboard(season = season)

# pick venue column safely
venue_col <- intersect(
  c("venue_full_name", "arena_name", "arena.name"),
  names(nba_raw)
)

sched_nba <- nba_raw %>%
  mutate(
    game_dt = as.POSIXct(
      game_date,
      format = "%Y-%m-%dT%H:%MZ",
      tz = "UTC"
    ),
    local_dt = with_tz(game_dt, "America/Los_Angeles")
  ) %>%
  filter(as.Date(local_dt) == today) %>%
  transmute(
    Date = as.Date(local_dt),
    Time = format(local_dt, "%H:%M"),
    Away = away_team_abb,
    Home = home_team_abb,
    Location = if (length(venue_col) == 1)
      .data[[venue_col]]
    else
      "Unknown Arena"
  )

Schedules$NBA<-sched_nba

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
stopifnot(length(day_index) == 1)

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
    Time = "00:00",
    Away = "Place",
    Home = "Holder",
    Location = "Placeholder Arena",
    stringsAsFactors = FALSE
  )
}

Schedules$NHL <-sched_nhl

# MLB ---------------------------------------------------------------------
#MLB Scheduler Compiler

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

Schedules$NFL$Away <- coalesce(
  nfl_lookup$Full[match(Schedules$NFL$Away, nfl_lookup$Abbr)],
  Schedules$NFL$Away
)

Schedules$NFL$Home <- coalesce(
  nfl_lookup$Full[match(Schedules$NFL$Home, nfl_lookup$Abbr)],
  Schedules$NFL$Home
)

Schedules$NBA$Home <- trimws(as.character(Schedules$NBA$Home))
Schedules$NBA$Away <- trimws(as.character(Schedules$NBA$Away))

stopifnot("ESPN_Abbr" %in% names(nba_lookup))

Schedules$NBA$Home <- coalesce(
  nba_lookup$Full[
    match(Schedules$NBA$Home, nba_lookup$ESPN_Abbr)
  ],
  Schedules$NBA$Home
)

Schedules$NBA$Away <- coalesce(
  nba_lookup$Full[
    match(Schedules$NBA$Away, nba_lookup$ESPN_Abbr)
  ],
  Schedules$NBA$Away
)
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

stopifnot(
  all(sapply(Schedules, function(x) {
    identical(names(x), c("Date", "Time", "Away", "Home", "Location"))
  }))
)

Schedules$Master <- bind_rows(Schedules, .id = "League")

Schedules$Master$Time <- ifelse(
  grepl("AM|PM", Schedules$Master$Time),
  Schedules$Master$Time,
  format(strptime(Schedules$Master$Time, "%H:%M"), "%I:%M %p")
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



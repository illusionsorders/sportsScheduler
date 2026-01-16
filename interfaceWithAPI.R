# Daily “what’s on” schedule for:
# EPL, Bundesliga, UEFA Champions League, MLB, NBA, NFL, NHL

API_KEY <- "123"
TZ_OUT  <- "America/Los_Angeles"

base_url <- function(path) sprintf("https://www.thesportsdb.com/api/v1/json/%s/%s", API_KEY, path)

tsdb_get <- function(path, query = list()) {
  req  <- request(base_url(path)) |> req_url_query(!!!query)
  resp <- req |> req_perform()
  fromJSON(resp_body_string(resp), flatten = TRUE)
}

# --- today in your timezone
today_local <- as.Date(with_tz(Sys.time(), TZ_OUT))
today_str   <- format(today_local, "%Y-%m-%d")

# Pull adjacent API days to avoid UTC boundary issues
api_days <- format(c(today_local - 1, today_local, today_local + 1), "%Y-%m-%d")
sports_to_pull <- c("Soccer", "Baseball", "Basketball", "American Football", "Ice Hockey")

get_events_day_sport <- function(day_yyyy_mm_dd, sport) {
  x <- tsdb_get("eventsday.php", query = list(d = day_yyyy_mm_dd, s = sport))
  if (is.null(x$events)) return(tibble())
  as_tibble(x$events) |>
    mutate(api_day = day_yyyy_mm_dd, request_sport = sport)
}

raw <- map_dfr(api_days, \(d) map_dfr(sports_to_pull, \(s) get_events_day_sport(d, s)))

if (nrow(raw) == 0) stop("No events returned by TheSportsDB for: ", paste(api_days, collapse = ", "))

# --- keep only your leagues (tolerant matching)
keep_league <- function(x) {
  x <- str_to_lower(str_squish(x))
  str_detect(x, "premier league") |
    str_detect(x, "bundesliga") |
    str_detect(x, "uefa.*champions league|\\bchampions league\\b") |
    str_detect(x, "\\bnfl\\b|national football league") |
    str_detect(x, "\\bnba\\b|national basketball association") |
    str_detect(x, "\\bnhl\\b|national hockey league") |
    str_detect(x, "\\bmlb\\b|major league baseball")
}

raw <- raw |>
  mutate(strLeague = if ("strLeague" %in% names(raw)) strLeague else NA_character_) |>
  filter(!is.na(strLeague) & keep_league(strLeague))

if (nrow(raw) == 0) stop("No matching league events found for local date: ", today_str)

# --- Parse datetime as UTC, then convert to TZ_OUT
dt_utc <- rep(as.POSIXct(NA, tz = "UTC"), nrow(raw))

if ("strTimestamp" %in% names(raw)) {
  tmp <- ymd_hms(raw$strTimestamp, tz = "UTC", quiet = TRUE)
  dt_utc[!is.na(tmp)] <- tmp[!is.na(tmp)]
}

if (all(is.na(dt_utc)) && all(c("dateEvent", "strTime") %in% names(raw))) {
  tmp <- ymd_hms(paste(raw$dateEvent, raw$strTime), tz = "UTC", quiet = TRUE)
  dt_utc <- tmp
}

dt_local   <- with_tz(dt_utc, TZ_OUT)
date_local <- as.Date(dt_local, tz = TZ_OUT)

schedule_today <- tibble(
  league   = raw$strLeague,
  sport    = if ("strSport" %in% names(raw)) raw$strSport else raw$request_sport,
  datetime = dt_local,
  date     = date_local,
  time     = ifelse(is.na(dt_local), NA_character_, format(dt_local, "%H:%M")),
  home     = if ("strHomeTeam" %in% names(raw)) raw$strHomeTeam else NA_character_,
  away     = if ("strAwayTeam" %in% names(raw)) raw$strAwayTeam else NA_character_,
  event    = coalesce(if ("strEvent" %in% names(raw)) raw$strEvent else NA_character_,
                      paste(home, "vs", away)),
  venue    = if ("strVenue" %in% names(raw)) raw$strVenue else NA_character_,
  idEvent  = if ("idEvent" %in% names(raw)) raw$idEvent else NA_character_
) |>
  filter(date == today_local) |>
  arrange(league, datetime, event) |>
  distinct(coalesce(idEvent, paste(league, datetime, event)), .keep_all = TRUE) |>
  select(league, sport, date, time, home, away, event, venue) |>
  arrange(league, time, event)

print(schedule_today, n = 500)

# =========================================================
# EXCEL OUTPUT (formatted)
# - auto-sized columns
# - bold header + filters
# - freeze header row
# =========================================================
out_file <- paste0("schedule_", today_str, ".xlsx")

wb <- createWorkbook()
addWorksheet(wb, "Schedule")

writeData(wb, "Schedule", schedule_today, withFilter = TRUE)

# Header styling
header_style <- createStyle(textDecoration = "bold")
addStyle(
  wb, "Schedule", header_style,
  rows = 1, cols = 1:ncol(schedule_today),
  gridExpand = TRUE, stack = TRUE
)

# Freeze header row
freezePane(wb, "Schedule", firstRow = TRUE)

# Auto-size columns
setColWidths(wb, "Schedule", cols = 1:ncol(schedule_today), widths = "auto")

saveWorkbook(wb, out_file, overwrite = TRUE)
message("Wrote: ", out_file)

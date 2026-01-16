#Utilize existing R packages

Schedules<-list()


#NFL Schedule Compilation

if(!require(nflreadr)){ 
  install.packages("nflreadr")
  library(nflreadr)
}

today <- Sys.Date()
yr <- as.integer(format(today, "%Y"))
mo <- as.integer(format(today, "%m"))

current_season <- if (mo < 3) yr - 1 else yr

sched <- load_schedules(seasons = current_season)

Schedules$NFL<-sched
rm(sched)
# load data ---------------------------------------------------------------
trips_bikers <- read.csv("data/Bikingdata_nofilter_SegmentCount.csv")
trips_hikers <- read.csv("data/Hikingdata_nofilter_SegmentCount.csv")

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
#normalize total_trips columns 
trips_bikers$total_bikers_normalized <- normalize(trips_bikers$total_bikers)
trips_hikers$total_hikers_normalized <- normalize(trips_hikers$total_hikers)

trips <- inner_join(trips_bikers, trips_hikers, by = "edgeuid")
#trips<- rename(trips, edgeUID = edgeuid)
trips$X.x <- NULL 
trips$X.y <- NULL

weighted_geo_mean <- function(a, b, weight=100) {
  return(weight * (a * b)^(1/3))
}

trips$conflict_index <- weighted_geo_mean(trips$total_bikers_normalized, trips$total_hikers_normalized)
# noramlize index to be from 0 to 100 again 
trips$conflict_index <- normalize(trips$conflict_index) * 100

write_feather(trips, "data/AllTrips_nofilter_SegmentCount.feather")


# ------------------------------------------------------------------------------
hours_bikers <- read.csv("data/Hourlystats_bikers_Bundesland.csv") 
write_feather(hours_bikers, "data/Hourlystats_bikers_Bundesland.feather")

hours_hikers <- read.csv("data/Hourlystats_hikers_Bundesland.csv") 
write_feather(hours_hikers, "data/Hourlystats_hikers_Bundesland.feather")

weekdays_bikers <- read.csv("data/Weekdaystats_bikers_Bundesland.csv") 
write_feather(weekdays_bikers, "data/Weekdaystats_bikers_Bundesland.feather")

weekdays_hikers <- read.csv("data/Weekdaystats_hikers_Bundesland.csv") 
write_feather(weekdays_hikers, "data/Weekdaystats_hikers_Bundesland.feather")

months_bikers <- read.csv("data/Monthlystats_bikers_Bundsland.csv") 
write_feather(months_bikers, "data/Monthlystats_bikers_Bundsland.feather")

months_hikers <- read.csv("data/Monthlystats_hikers_Bundesland.csv")
write_feather(months_hikers, "data/Monthlystats_hikers_Bundsland.feather")

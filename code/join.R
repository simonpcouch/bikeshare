# Joining the Bikeshare and Opportunity Insights Data

# This file serves to join the data we harvested in scrape.R
# to the data downloaded from Opportunity Insights

# Load libraries and data
library(tigris)
library(tidyverse)
bikeshare_raw <- load("data/bikeshare_raw.Rda")
tract_covariates <- read_csv("data/tract_covariates.csv")

# Uncount source data on num_bikes_available
bikeshare_raw <- bikeshare_raw %>%
  uncount(num_bikes_available)

# We're going to break this process up into less sizable chunks
# so that we can give the geolocator a break occasionally, as well
# as check on in where the function is at by printing the index
program_ids <- unique(bikeshare_raw$program_id)

station_census <- list()
free_bike_census <- list()

for (i in program_ids) {
  # print the current index
  print(i)
  
  # reverse geocode the stationed bikes from the given program
  station_census[[i]] <- map2(.x = bikeshare_raw$lat[bikeshare_raw$type == "station" & bikeshare_raw$program_id == i], 
                              .y = bikeshare_raw$lon[bikeshare_raw$type == "station" & bikeshare_raw$program_id == i],
                              .f = call_geolocator_latlon)
  
  # sleep for 10 seconds to give the geocoder a break
  Sys.sleep(10)
  
  # reverse geocode the free bikes from the given program
  free_bike_census[[i]] <- map2(.x = bikeshare_raw$lat[bikeshare_raw$type == "bike" & bikeshare_raw$program_id == i], 
                                .y = bikeshare_raw$lon[bikeshare_raw$type == "bike" & bikeshare_raw$program_id == i],
                                .f = call_geolocator_latlon)
  
  # sleep for 10 seconds to give the geocoder a break
  Sys.sleep(10)
}

# Break up the string into state, county, and tract.
station_locations <- as.data.frame(list(
  state = substr(unlist(station_census), 1, 2),
  county = substr(unlist(station_census), 3, 5),
  tract = substr(unlist(station_census), 6, 11)
))

free_bike_locations <- as.data.frame(list(
  state = substr(unlist(free_bike_census), 1, 2),
  county = substr(unlist(free_bike_census), 3, 5),
  tract = substr(unlist(free_bike_census), 6, 11)
))

# Make a table of counts by location.
station_locations_table <- station_locations %>%
  group_by(state, county, tract) %>%
  summarize(station_freq = n())

bike_locations_table <- free_bike_locations %>%
  group_by(state, county, tract) %>%
  summarize(bike_freq = n())

# Every entry in tract_covariates$state should have the same length
# so that it can be effectively joined with station_locations_table.
# The same goes for county and tract.
standardize_length <- function(x, length) {
  while (str_length(unlist(x)) < length) {
    x <- paste0(0, x)
  }
  unlist(x)
}

# Standardize the length of the variables so that they correctly
# match up with station_locations_table
tract_covariates$state <- as.factor(unlist(lapply(tract_covariates$state, 
                                                  standardize_length, 
                                                  2)))

tract_covariates$county <- as.factor(unlist(lapply(tract_covariates$county, 
                                                   standardize_length, 
                                                   3)))

tract_covariates$tract <- as.factor(unlist(lapply(tract_covariates$tract, 
                                                   standardize_length, 
                                                   6)))

# Left join the data so that every census tract is kept.
bikeshare <- left_join(tract_covariates, 
                       station_locations_table, 
                       by = c("tract", "state", "county")) %>%
             left_join(.,
                       bike_locations_table,
                       by = c("tract", "state", "county"))

# Mutate a new column, bike_stations, that is in integer measuring
# the number of bikeshare stations in a given census tract. (This operation
# essentially recodes NAs as zeroes.)
bikeshare <- bikeshare %>%
  mutate(num_stations = case_when(
           as.integer(station_freq) > 0 ~ as.integer(station_freq),
           is.na(station_freq) ~ as.integer(0)),
         num_bikes = case_when(
           as.integer(bike_freq) > 0 ~ as.integer(bike_freq),
           is.na(bike_freq) ~ as.integer(0))) %>%
  select(-station_freq, -bike_freq)

# Look at the distribution of counts.
bikeshare %>%
  group_by(num_stations) %>%
  summarize(n = n())

bikeshare %>%
  group_by(num_bikes) %>%
  summarize(n = n())

# We'd also like to mutate a variable indicating whether a bikeshare program
# operates in the county---we'd expect that a large part of the variation in
# in predicting the number of bikeshare stations in a census block can be
# accounted for but controlling for whether a bikeshare program actually operates
# in that county.
bikeshare_counties <- bikeshare %>%
  # the level of observation here is the state and county
  mutate(state_county = paste(state, county, sep = "_")) %>%
  # for each county..
  group_by(state_county) %>%
  # add up the number of bikes and stations
  summarize(tot_bikes = sum(num_bikes),
            tot_stations = sum(num_stations)) %>%
  # only keep the ones with an operating bikeshare system
  filter(tot_bikes != 0 | tot_stations != 0)

# make a new variable that is TRUE if a bikeshare program operates
# in the county, and FALSE otherwise. 
bikeshare <- bikeshare %>%
  mutate(state_county = paste(state, county, sep = "_")) %>%
  left_join(., bikeshare_counties, by = c("state_county")) %>%
  mutate(county_has_bikeshare = case_when(
    tot_bikes != 0 | tot_stations != 0 ~ TRUE,
    TRUE ~ FALSE
  )) %>%
  select(-state_county, -tot_bikes, -tot_stations)

# Save the dataset.
save(bikeshare, file = "data/bikeshare.Rda")

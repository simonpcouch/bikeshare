# GBFS Bikeshare Information Scraping

# This file serves to query gbfs feeds of all operating bikeshare
# programs in the US that utilize gbfs.

# Load libraries
library(tidyverse)
library(gbfs)
library(jsonlite)

# Read in the current cities releasing gbfs feeds
cities <- read_csv("https://raw.githubusercontent.com/NABSA/gbfs/master/systems.csv") %>%
  filter(`Country Code` == "US")

# Extract the URL from that dataset
cities <- cities$`Auto-Discovery URL`

# Each of these URLs is a json file containing, among other things, the link
# to the relevant dataset. For each city in cities, pull down the
# JSON at the given link, and find the link we want inside of it.
find_URL <- function(feed, index, type) {
  # let the user know where the function is at
  print(index)
  # Pull down the JSON
  city_info <- tryCatch(fromJSON(feed), error=function(e) Sys.sleep(5))
  # Extract the dataframe
  df <- city_info$data$en$feeds
  
  if (type %in% df$name) {
    # Extract the url
    df %>%
      filter(name == type) %>%
      select(url) %>%
      pull()
  } else {
    NA
  }
}

# Pull out the station information link for every city
station_information_URLs <- unlist(map2(cities,
                                        1:length(cities),
                                        find_URL,
                                        type = "station_information"))

station_status_URLs <- unlist(map2(cities,
                                   1:length(cities),
                                   find_URL,
                                   "station_status"))

# We now want to extract the dataframe in every one of these links
# and bind them all together. This function can be applied over
# the vector of links we now have.
get_station_info <- function(i) {
  # Let the user know where the function is at
  print(i)
  # Grab the url at the given index
  stat_info_url <- station_information_URLs[i]
  stat_status_url <- station_status_URLs[i]
  # Only use the URL if it's a valid .json file
  if (grepl(".json", stat_info_url) & !grepl("lyft-last", stat_info_url)) {
    # Grab the datasets using 'gbfs'
    get_station_information(stat_info_url, "data")
    station_status <- fromJSON(stat_status_url)$data$stations
    # Load up the datasets
    station_information <- readRDS("data/station_information.rds")
    # Just pull out the number of available bikes from the station status feed
    # and join it with the station information
    if (length(station_status$num_bikes_available) != 0) { 
      station_status <- station_status %>%
        select(station_id, num_bikes_available)
      station_information <- left_join(station_information, 
                                       station_status)
    }
    # Make sure there actually *are* stations to extract data about
    if (length(station_information$lat) != 0) {
      # Select only the relevant columns and standardize
      # formatting between station info and free bikes
      station_information <- station_information %>%
        flatten() %>%
        dplyr::select(lon, lat, station_id, num_bikes_available) %>%
        mutate(program_id = i,
               type = "station") %>%
        rename(obs_id = station_id)
      # Make station_id a character vector for consistency
      station_information$obs_id <- as.character(station_information$obs_id)
      # Return station_information
      station_information  
    }
  }
}

# Run get_station_info on all cities
full_station_information_list <- lapply(1:length(station_information_URLs), 
                                        get_station_info)

# Bind the rows of the resulting dataset together
full_station_information <- bind_rows(full_station_information_list)

# We'd also like to extract all of the information for free bikes.
# This framework will look similar to that for station information, except
# that not all programs are required to report this information.

# Extract all of the free_bike_status URLs
free_bike_status_URLs <- unlist(map2(cities,
                                     1:length(cities),
                                     find_URL,
                                     "free_bike_status"))

# Write a function that, for a given index, grabs the data
# at that index in the list of URLs and tidies it up for binding
get_free_bikes <- function(i) {
  # Let the user know where the function is at
  print(i)
  # Grab the url at the given index
  url <- free_bike_status_URLs[i]
  # Only use the URL if it's a valid .json file
  if (grepl(".json", url) & !grepl("lyft-last", url)) {
    # Grab the dataset
    free_bike_status <- fromJSON(url)$data$bikes
    # Make sure there actually *are* bikes to extract data about
    if ("lat" %in% colnames(free_bike_status)) {
        # Select only the relevant columns
        free_bike_status <- free_bike_status %>%
          flatten() %>%
          dplyr::select(lon, lat, bike_id)
        # Standardize columns between the two datasets
        free_bike_status <- free_bike_status %>%
          mutate(program_id = i,
                 type = "bike",
                 num_bikes_available = 1)  %>%
          rename(obs_id = bike_id)
        # Make station_id a character vector for consistency
        free_bike_status$obs_id <- as.character(free_bike_status$obs_id)
        # Return free_bike_status
        free_bike_status  
      }
    }
  }

# Run get_free_bikes on all cities
full_free_bike_status_list <- lapply(1:length(free_bike_status_URLs), 
                                        get_free_bikes)

# Bind the rows of the resulting dataset together
full_free_bike_status <- bind_rows(full_free_bike_status_list)

# Bind the two datasets together
bikeshare_raw <- rbind(full_station_information, full_free_bike_status)

# Save the dataset
save(bikeshare_raw, file = "data/bikeshare_raw.Rda")

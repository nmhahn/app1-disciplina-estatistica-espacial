library(tidyverse)
library(lubridate)
library(RSocrata)

years_ago <- today() - years(2)
crash_url <- glue::glue("https://data.cityofchicago.org/Transportation/Traffic-Crashes-Crashes/85ca-t3if?$where=CRASH_DATE > '{years_ago}'")
crash_raw <- as_tibble(read.socrata(crash_url))

crash <- crash_raw %>%
  arrange(desc(crash_date)) %>%
  transmute(
    injuries = if_else(injuries_total > 0, "injuries", "none"),
    crash_date,
    crash_hour,
    report_type = if_else(report_type == "", "UNKNOWN", report_type),
    num_units,
    posted_speed_limit,
    weather_condition,
    lighting_condition,
    roadway_surface_cond,
    first_crash_type,
    trafficway_type,
    prim_contributory_cause,
    latitude, longitude
  ) %>%
  na.omit()

library(leaflet)
leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addMarkers(lng = crash$longitude[1:10], 
             lat = crash$latitude[1:10])


leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addMarkers(lng = crash$longitude, 
             lat = crash$latitude,
             clusterOptions = markerClusterOptions())


getColor <- function(crash) {
  sapply(crash$injuries, function(injuries) {
    if(injuries == "none") {
      "green"
    } else {
      "red"
    } })
}

icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = getColor(crash)
)
a = crash[1:10,]
leaflet() %>% 
  addAwesomeMarkers(lng = a$longitude, a$latitude, icon=icons)

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addAwesomeMarkers(lng = a$longitude,
                    lat = a$latitude,
                    icon = icons,
                    clusterOptions = markerClusterOptions())

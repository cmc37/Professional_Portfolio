#Laod packages
library(scales)
library(gt)
library(sf)
library(tmap)
library(tigris)
library(tidygeocoder)
library(httr)
library(jsonlite)
library(shiny)
library(flexdashboard)
library(reactable)
library(dplyr)
library(DT)
library(ggplot2)

#Census API - run lines 16-17 to initiate connection
library(tidyverse)
library(tidycensus)
#my_key <- readLines("/Users/catheacarey/Documents/KCRHA Work/census_key.txt")
#census_api_key(my_key, install = TRUE)

#Data is on external drive
#check for files
#list.files("/Volumes/")
# Corrected file path 
#file_path1 <-

# Read CSV
#ahrf_data <- read.csv(file_path1, stringsAsFactors = FALSE)



# Corrected file path 
#file_path2 <-

# Read CSV
#placescdc_data <- read.csv(file_path2, stringsAsFactors = FALSE)
library(readr)

#Template
#url0 <- "https://data.cdc.gov/resource/eav7-hnsx.csv?$limit=50000"
#placescdc_data <- read_csv(url0)
#WA State ONLY
  url1 <- paste0(
    "https://data.cdc.gov/resource/eav7-hnsx.csv?",
    "$where=stateabbr='WA'",
    "&$limit=50000"
  )
  wa_places <- read_csv(url1)

#King County ONLY - filter from wa_places
  #read as shape file
  wa_places_sf <- st_as_sf(
    wa_places,
    wkt = "geolocation",
    crs = 4326
  )
  # King County geometry is valid
  ## confirm sf worked
  st_geometry_type(wa_places_sf)
  #King County Boundary
  king <- counties(state = "WA", cb = TRUE, class = "sf") %>%
    filter(NAME == "King")
  st_geometry_type(king)
  king_places <- wa_places_sf[king, , op = st_within]
 # CRS mismatch
  st_crs(wa_places_sf)
  st_crs(king)
  
  # IF CRS FAILS AKA MISMATCH RUN THIS:
  # wa_places_sf <- st_transform(wa_places_sf, 4326)
  # king <- st_transform(king, 4326)
  
  king_places <- st_join(
    wa_places_sf,
    king,
    join = st_within
  ) %>%
    filter(!is.na(NAME))
  

### END - NOW HAVE CONNECTED DATA FILES FOR CLEANING AND ANALYSIS
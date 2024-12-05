# Code description --------------------------------------------------------
#* Author: Vivian
#* Date: 06/10/2024
#* Goal: create chord diagram of multiple severe weather events - county-days (not counties)
#* Facet this by us census region
#* Note: this double counts whenever there is a 3 type combination 
#* (e.g., anomppt, anoomheat, cyc = 90 days total but this chart identifies this as anomppt appearing 180 times)

# set up ------------------------------------------------------------------
rm(list=ls(all=TRUE))
options(scipen = 999)
library(dplyr)
library(lubridate)
library(tidyverse)
library(fst)
library(sf)
library(ggthemes)
library(usmap)
library(patchwork)
library(MetBrewer)
library(tidycensus)
library(stringr)
library(PNWColors)
library(circlize) # used for chord diagram

path_data_raw <- "/Users/vivian/Desktop/0_PhD/0_Research Projects/power_outage-severe_weather-coocurrence/analysis/data/1_raw/"
path_data_processed <- "/Users/vivian/Desktop/0_PhD/0_Research Projects/power_outage-severe_weather-coocurrence/analysis/data/3_processed/"
path_map <- "/Users/vivian/Desktop/0_PhD/0_Research Projects/power_outage-severe_weather-coocurrence/analysis/output/figures/"

# read in data ------------------------------------------------------------
po8_sw_multi_sw_state_cat_scatter <- read_csv(paste0(path_data_processed, "multi_po_sw_4.0.csv"))

# data wrangling ----------------------------------------------------------
# separate by us census region
sw_raw <- po8_sw_multi_sw_state_cat_scatter %>% 
  select(clean_state_name, cyclone, anomhot, anomcold, anomppt, wf, snowfall)

# classify by us census region
sw_raw <- sw_raw %>% 
  mutate(us_census_region = case_when(
    clean_state_name %in% c("connecticut", "maine", "massachusetts", "newhampshire", "rhodeisland", "vermont", 
                            "newjersey", "newyork", "pennsylvania") ~ "northeast",
    
    clean_state_name %in% c("illinois", "indiana", "michigan", "ohio", "wisconsin", 
                            "iowa", "kansas", "minnesota", "missouri", "nebraska", "northdakota", "southdakota") ~ "midwest",
    
    clean_state_name %in% c("delaware", "florida", "georgia", "maryland", "northcarolina", "southcarolina", 
                            "virginia", "westvirginia", "districtofcolumbia", "alabama", "kentucky", 
                            "mississippi", "tennessee", "arkansas", "louisiana", "oklahoma", "texas") ~ "south",
    
    clean_state_name %in% c("arizona", "colorado", "idaho", "montana", "nevada", "newmexico", "utah", "wyoming", 
                            "alaska", "california", "hawaii", "oregon", "washington") ~ "west"
  )) %>% 
  select(-clean_state_name)

# functions to process data -----------------------------------------------
process_combinations <- function(data, severe_weather_types) {
  sw_df <- data.frame()
  
  for (i in 1:(length(severe_weather_types) - 1)) {
    for (j in (i + 1):length(severe_weather_types)) {
      print(paste0("Processing: ", severe_weather_types[i], " and ", severe_weather_types[j]))
      
      # For each combination, select the corresponding columns and remove the rows with NAs
      sw <- data %>%
        select(severe_weather_types[i], severe_weather_types[j]) %>%
        filter(!is.na(.[[1]]) & !is.na(.[[2]])) %>%
        rename(sw1 = severe_weather_types[i], sw2 = severe_weather_types[j])
      
      # If the data frame is not empty, append it to the result data frame
      if (nrow(sw) > 0) {
        # Add a column to identify the pair
        sw$pair <- paste(severe_weather_types[i], severe_weather_types[j], sep = "_")
        
        # Append the data frame to the result data frame
        sw_df <- rbind(sw_df, sw)
      }
    }
  }
  
  return(sw_df)
}

replace_severe_weather_names <- function(data) {
  data %>%
    select(-pair) %>%
    mutate_all(~str_replace_all(., "cyclone", "Tropical Cyclone")) %>% 
    mutate_all(~str_replace_all(., "anomhot", "Anomalous Heat")) %>% 
    mutate_all(~str_replace_all(., "anomcold", "Anomalous Cold")) %>%
    mutate_all(~str_replace_all(., "anomppt", "Anomalous Precipitation")) %>%
    mutate_all(~str_replace_all(., "snowfall", "Snowfall")) %>% 
    mutate_all(~ifelse(. == "wf", "Wildfire", .))
}

# Loop through census regions ---------------------------------------------
# Create a vector of the severe weather types
severe_weather_types <- c("cyclone", "anomhot", "anomcold", "anomppt", "snowfall", "wf")

# Initialize an empty list to store the results for each region
sw_df3_list <- list()

# Get the unique US Census regions
us_census_regions <- unique(sw_raw$us_census_region)

# Loop through each US Census region
for (region in us_census_regions) {
  print(paste0("Processing region: ", region))
  
  # Filter the data for the current region
  # Remove the us_census_region column
  sw_raw_region <- sw_raw %>% filter(us_census_region == region) %>% 
    select(-us_census_region)
  
  # Process combinations of severe weather types
  sw_df <- process_combinations(sw_raw_region, severe_weather_types)
  
  # Replace severe weather type codes with full names
  sw_df2 <- replace_severe_weather_names(sw_df)
  
  # Create a table for the current region
  sw_df3 <- table(sw_df2)
  
  # Store the result in the list with the region name as the key
  sw_df3_list[[region]] <- sw_df3
}

# Make chord diagram by us census region ----------------------------------
# Define the sectors for the chord diagram (aka each severe weather event)
sectors <- c("Anomalous Cold", "Anomalous Heat", "Anomalous Precipitation", "Cyclone", "Snowfall", "Wildfire")

# Create a named vector of colors for the sectors
values <- c(
  "Anomalous Cold" = met.brewer("Johnson", 6)[6], # cold
  "Anomalous Heat" = met.brewer("Johnson", 6)[1], # heat
  "Anomalous Precipitation" = met.brewer("Archambault", 5)[2], # precip
  "Tropical Cyclone" = met.brewer("Archambault", 5)[3], # cyclone
  "Snowfall" = met.brewer("Johnson", 6)[5], # snow
  "Wildfire" = met.brewer("Johnson", 12)[5] # wf
)

# Ensure the named vector covers all sectors
values <- values[sectors]

# Note: chord diagram function requires specific steps, specifically:
# 1. Open a device (e.g., pdf)
# 2. Plot the chord diagram
# 3. Stop the device
# It will not plot otherwise

# Midwest region
pdf(file = "/Users/vivian/Desktop/0_PhD/0_Research Projects/power_outage-severe_weather-coocurrence/analysis/output/figures/multi_sw_chord_diag_cbfriendly_midwest_4.0.pdf",
    width = 10,
    height = 7)
chordDiagram(sw_df3_list$midwest, transparency = 0.5, grid.col = values)
title("Midwest")
dev.off()

# Northeast region
pdf(file = "/Users/vivian/Desktop/0_PhD/0_Research Projects/power_outage-severe_weather-coocurrence/analysis/output/figures/multi_sw_chord_diag_cbfriendly_northeast_4.0.pdf",
    width = 10,
    height = 7)
chordDiagram(sw_df3_list$northeast, transparency = 0.5, grid.col = values)
title("Northeast")
dev.off()

# South region
pdf(file = "/Users/vivian/Desktop/0_PhD/0_Research Projects/power_outage-severe_weather-coocurrence/analysis/output/figures/multi_sw_chord_diag_cbfriendly_south_4.0.pdf",
    width = 10,
    height = 7)
chordDiagram(
  sw_df3_list$south,
  transparency = 0.5,
  grid.col = values,
  order = c(
    "Anomalous Cold",
    "Anomalous Heat",
    "Snowfall",
    "Anomalous Precipitation",
    "Tropical Cyclone"
  )
)
title("South")
dev.off()

# West region
pdf(file = "/Users/vivian/Desktop/0_PhD/0_Research Projects/power_outage-severe_weather-coocurrence/analysis/output/figures/multi_sw_chord_diag_cbfriendly_west_4.0.pdf",
    width = 10,
    height = 7)
chordDiagram(sw_df3_list$west, transparency = 0.5, grid.col = values)
title("West")
dev.off()
















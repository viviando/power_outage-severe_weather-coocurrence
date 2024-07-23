# Code description --------------------------------------------------------
#* Author: Vivian
#* Date: 06/10/2024
#* Goal: create chord diagram of multiple severe weather events - county-days (not counties)
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

path_data <- "/Users/vivian/Desktop/0_PhD/0_Research Projects/spatial_po_severe_weather/project code/data/"
path_map <- "/Users/vivian/Desktop/0_PhD/0_Research Projects/spatial_po_severe_weather/project code/output/figures/"


# read in data ------------------------------------------------------------
po8_sw_multi_sw_state_cat_scatter <- read_csv(paste0(path_data, "processed/po8_sw_multi_sw_state_cat_scatter.csv"))

# data wrangling ----------------------------------------------------------
test <- po8_sw_multi_sw_state_cat_scatter %>% 
  select(cyclone, anomhot, anomcold, anomppt, wf, snowfall)

# create separate datasets
sw1 <- test %>% 
  select(cyclone, anomhot) %>% 
  filter(!is.na(cyclone) & !is.na(anomhot))

# Create a vector of the severe weather types
severe_weather_types <- c("cyclone", "anomhot", "anomcold", "anomppt", "snowfall", "wf")

# Initialize an empty data frame to store the results
sw_df <- data.frame()

# Use a nested loop to iterate over the different combinations of severe weather types
for (i in 1:(length(severe_weather_types)-1)) {
  for (j in (i+1):length(severe_weather_types)) {
    print(paste0("Processing: ", severe_weather_types[i], " and ", severe_weather_types[j]))
    
    # For each combination, select the corresponding columns and remove the rows with NAs
    sw <- test %>%
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

sw_df2 <- sw_df %>%
  select(-pair) %>%
  mutate_all(~str_replace_all(., "cyclone", "Cyclone")) %>% 
  mutate_all(~str_replace_all(., "anomhot", "Anomalous Heat")) %>% 
  mutate_all(~str_replace_all(., "anomcold", "Anomalous Cold")) %>%
  mutate_all(~str_replace_all(., "anomppt", "Anomalous Precipitation")) %>%
  mutate_all(~str_replace_all(., "snowfall", "Snowfall")) %>% 
  mutate_all(~ifelse(. == "wf", "Wildfire", .))

sw_df3 <- table(sw_df2)
  
# Define a custom color palette
values <- c(met.brewer("Johnson", 6)[6], #cold
            met.brewer("Johnson", 6)[1], #heat
            met.brewer("Archambault", 5)[2], #precip
            met.brewer("Archambault", 5)[3], #cyclone
            met.brewer("Johnson", 6)[5], #snow
            met.brewer("Johnson", 12)[5]) #wf

# Create the chord diagram with the specified sector colors and hide the default track
par(cex = 5) # increase font size
chordDiagram(sw_df3, transparency = 0.5, grid.col = values)

# # Create a PNG file
# png(
#   paste0(
#     "/Users/vivian/Desktop/0_PhD/0_Research Projects/spatial_po_severe_weather/project code/output/multi_sw_chord_diag_cbfriendly.png"
#   ),
#   width = 1500,
#   height = 1000,
#   res = 150
# )
# 
# # Increase font size
# par(cex = 5)
# 
# # Create the chord diagram with the specified sector colors and hide the default track
# chordDiagram(sw_df3, transparency = 0.5, grid.col = values)
# 
# # Stop the device, this saves the file
# dev.off()

# Start PDF device
pdf(file = "/Users/vivian/Desktop/0_PhD/0_Research Projects/spatial_po_severe_weather/project code/output/figures/multi_sw_chord_diag_cbfriendly.pdf",
    width = 10,
    height = 7)


# Assuming 'values' and 'sw_df3' are already defined and prepared as per your previous code
chordDiagram(sw_df3, transparency = 0.5, grid.col = values)

# Stop the device, this saves the file
dev.off()









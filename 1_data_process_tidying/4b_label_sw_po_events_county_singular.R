# Code description --------------------------------------------------------
#* Author: Vivian
#* Date: 03/20/2024
#* Goal: create and label quartiles of each severe weather type
#* Please note that I did have to play around with the cutpoints because sometimes quartiles did not 
#* make sense visually when displaying them on a map
#* For figure 1

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

path_data_raw <- "/Users/vivian/Desktop/0_PhD/0_Research Projects/power_outage-severe_weather-coocurrence/analysis/data/1_raw/"
path_data_processed <- "/Users/vivian/Desktop/0_PhD/0_Research Projects/power_outage-severe_weather-coocurrence/analysis/data/3_processed/"

# read in data ------------------------------------------------------------

# po sw data with different po thresholds
cty_days_sw_po_0.1 <- read_csv(paste0(path_data_processed, "/cty_days_sw_po_0.1.csv"))
cty_days_sw_po_2.5 <- read_csv(paste0(path_data_processed, "/cty_days_sw_po_2.5.csv"))
cty_days_sw_po_4.0 <- read_csv(paste0(path_data_processed, "/cty_days_sw_po_4.0.csv"))

# shapefile
us_county_shape_2020 <- st_read(paste0(path_data_raw, "nhgis0044_shapefile_tl2020_us_county_2020/US_county_2020.shp")) %>% 
  filter(!STATEFP %in% c("02", "15", "72", "66", "60", "69", "78")) %>% 
  rename(fips = GEOID)

# Helper functions --------------------------------------------------------
# function to join shape and po-sw data
join_shapefile <- function(po_sw_data) {
  us_county_shape_2020 %>%
    left_join(
      po_sw_data %>%
        select(fips, starts_with("tot")),
      by = "fips"
    )
}

# cyclone label
label_cyclone_0.1 <- function(data) {
  data %>%
    mutate(
      label_tot_days_cyc = case_when(
        tot_days_cyc == 0 ~ "0",
        tot_days_cyc == 1 ~ "1",
        tot_days_cyc == 2 ~ "2",
        tot_days_cyc == 3 ~ "3",
        tot_days_cyc == 4 ~ "4",
        tot_days_cyc == 5 ~ "5",
        tot_days_cyc > 5 ~ "6"
      ),
      label_tot_days_cyc = as.character(label_tot_days_cyc)
    )
}

# cyclone label
label_cyclone_4.0 <- function(data) {
  data %>%
    mutate(
      label_tot_days_cyc = case_when(
        tot_days_cyc == 0 ~ "0",
        tot_days_cyc == 1 ~ "1",
        tot_days_cyc == 2 ~ "2",
        tot_days_cyc == 3 ~ "3",
        tot_days_cyc == 4 ~ "4",
        tot_days_cyc == 5 ~ "5",
        tot_days_cyc > 5 ~ "6+"
      ),
      label_tot_days_cyc = as.character(label_tot_days_cyc)
    )
}

# anomalous precipitation label
label_anomppt_0.1 <- function(data) {
  data %>%
    mutate(
      label_tot_days_anomppt = case_when(
        tot_days_anomppt == 0 ~ "0",
        tot_days_anomppt > 0 & tot_days_anomppt <= 5 ~ "1-5",
        tot_days_anomppt > 5 & tot_days_anomppt <= 10 ~ "6-10",
        tot_days_anomppt > 10 & tot_days_anomppt <= 15 ~ "11-15",
        tot_days_anomppt > 15 & tot_days_anomppt <= 20 ~ "16-20",
        tot_days_anomppt > 20 ~ "21+"
      ),
      label_tot_days_anomppt = as.character(label_tot_days_anomppt)
    )
}

# anomalous precipitation label with smaller distribution range
label_anomppt_4.0 <- function(data) {
  data %>%
    mutate(
      label_tot_days_anomppt = case_when(
        tot_days_anomppt == 0 ~ "0",
        tot_days_anomppt == 1 ~ "1",
        tot_days_anomppt == 2 ~ "2",
        tot_days_anomppt == 3 ~ "3",
        tot_days_anomppt == 4 ~ "4",
        tot_days_anomppt == 5 ~ "5",
        tot_days_anomppt >=6 ~ "6+"
      ),
      label_tot_days_anomppt = as.character(label_tot_days_anomppt)
    )
}

# anomalous heat label
label_anomhot_0.1 <- function(data) {
  data %>%
    mutate(
      label_tot_days_anomhot = case_when(
        tot_days_anomhot == 0 ~ "0",
        tot_days_anomhot > 0 & tot_days_anomhot <= 5 ~ "1-5",
        tot_days_anomhot > 5 & tot_days_anomhot <= 10 ~ "6-10",
        tot_days_anomhot > 10 & tot_days_anomhot <= 15 ~ "11-15",
        tot_days_anomhot > 15 & tot_days_anomhot <= 20 ~ "16-20",
        tot_days_anomhot > 20 ~ "21+"
      )
    )
}

# anomalous heat label smaller dist
label_anomhot_4.0 <- function(data) {
  data %>%
    mutate(
      label_tot_days_anomhot = case_when(
        tot_days_anomhot == 0 ~ "0",
        tot_days_anomhot == 1 ~ "1",
        tot_days_anomhot == 2 ~ "2",
        tot_days_anomhot == 3 ~ "3",
        tot_days_anomhot == 4 ~ "4",
        tot_days_anomhot == 5 ~ "5",
        tot_days_anomhot == 6 ~ "6+"
      )
    )
}

# anomalous cold label
label_anomcold_0.1 <- function(data) {
  data %>%
    mutate(
      label_tot_days_anomcold = case_when(
        tot_days_anomcold == 0 ~ "0",
        tot_days_anomcold == 1 ~ "1",
        tot_days_anomcold == 2 ~ "2",
        tot_days_anomcold == 3 ~ "3",
        tot_days_anomcold == 4 ~ "4",
        tot_days_anomcold == 5 ~ "5",
        tot_days_anomcold > 5 ~ "6+"
      ),
      label_tot_days_anomcold = as.character(label_tot_days_anomcold)
    )
}

# wildfire label
label_wf_0.1 <- function(data) {
  data %>%
    mutate(
      label_tot_days_wf = case_when(
        tot_days_wf == 0 ~ "0",
        tot_days_wf == 1 ~ "1",
        tot_days_wf == 2 ~ "2",
        tot_days_wf == 3 ~ "3",
        tot_days_wf == 4 ~ "4",
        tot_days_wf == 5 ~ "5",
        tot_days_wf > 5 ~ "6+"
      ),
      label_tot_days_wf = as.character(label_tot_days_wf)
    )
}

# snowfall label
label_snowfall_0.1 <- function(data) {
  data %>%
    mutate(
      label_tot_days_snowfall = case_when(
        tot_days_snowfall == 0 ~ "0",
        tot_days_snowfall > 0 & tot_days_snowfall <= 5 ~ "1-5",
        tot_days_snowfall > 5 & tot_days_snowfall <= 10 ~ "6-10",
        tot_days_snowfall > 10 & tot_days_snowfall <= 15 ~ "11-15",
        tot_days_snowfall > 15 & tot_days_snowfall <= 20 ~ "16-20",
        tot_days_snowfall > 20 ~ "21+"
      ),
      label_tot_days_snowfall = as.character(label_tot_days_snowfall)
    )
}

# snowfall label for smaller distribution range
label_snowfall_4.0 <- function(data) {
  data %>%
    mutate(
      label_tot_days_snowfall = case_when(
        tot_days_snowfall == 0 ~ "0",
        tot_days_snowfall == 1 ~ "1",
        tot_days_snowfall == 2 ~ "2",
        tot_days_snowfall == 3 ~ "3",
        tot_days_snowfall == 4 ~ "4",
        tot_days_snowfall == 5 ~ "5",
        tot_days_snowfall == 6 ~ "6+"
      ),
      label_tot_days_snowfall = as.character(label_tot_days_snowfall)
    )
}

# Join po-sw with shapefile and label sw -------------------------------------
# store the datasets for loop
cty_days_sw_po <- list(cty_days_sw_po_0.1, cty_days_sw_po_2.5, cty_days_sw_po_4.0)

# check that they have names
names(cty_days_sw_po) <- c("0.1", "2.5", "4.0")

# start loop
cty_days_sw_po_labeled <- list()

for (i in seq_along(cty_days_sw_po)) {
  
  data <- cty_days_sw_po[[i]]
  
  if (!grepl("4.0", names(cty_days_sw_po)[i])) {
    data <- join_shapefile(data)
    data <- label_cyclone_0.1(data)
    data <- label_anomppt_0.1(data)
    data <- label_anomhot_0.1(data)
    data <- label_anomcold_0.1(data)
    data <- label_wf_0.1(data)
    data <- label_snowfall_0.1(data)
  }
  
  else{
    data <- join_shapefile(data)
    data <- label_cyclone_4.0(data)
    data <- label_anomppt_4.0(data)
    data <- label_anomhot_4.0(data)
    data <- label_anomcold_0.1(data)
    data <- label_wf_0.1(data)
    data <- label_snowfall_4.0(data)
  }

  cty_days_sw_po_labeled[[i]] <- data
}

write_csv(cty_days_sw_po_labeled[[1]], paste0(path_data_processed, "/cty_days_sw_po_labeled_0.1.csv"))
write_csv(cty_days_sw_po_labeled[[2]], paste0(path_data_processed, "/cty_days_sw_po_labeled_2.5.csv"))
write_csv(cty_days_sw_po_labeled[[3]], paste0(path_data_processed, "/cty_days_sw_po_labeled_4.0.csv"))


# Code description --------------------------------------------------------
#* Author: Vivian
#* Date: 6/3/24
#* Goal: subset data for nina

# set up ------------------------------------------------------------------
rm(list=ls(all=TRUE))
options(scipen = 999)
library(dplyr)
library(lubridate)
library(tidyverse)
library(fst)

path_data <- "/Users/vivian/Desktop/0_PhD/0_Research Projects/spatial_po_severe_weather/project code/data/"


# read in data ------------------------------------------------------------

# 8+ hour outages (county-day)
po8 <- readRDS("/Volumes/My Passport Vivian/pous_updated/data_proc/data/days_exposed_unexposed_expansion_w_all_fips.RDS")

# severe weather
sw <- read_fst("/Volumes/My Passport Vivian/pous_updated/data_proc/data/weather/sw_po_events.fst") %>% 
  filter(year(day) != 2021)


# subset ------------------------------------------------------------------
po8_subset <- po8[1:250000,]
sw_subset <- sw[1:250000,]

# save --------------------------------------------------------------------
write_csv(po8_subset, paste0(path_data, "processed/po8_subset.csv"))
write_csv(sw_subset, paste0(path_data, "processed/sw_subset.csv"))



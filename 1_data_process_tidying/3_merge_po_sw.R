# Code description --------------------------------------------------------
#* Author: Vivian
#* Date: 9/26/24
#* Goal: merge power outage - severe weather data
#* Apply the following to counties
#*      - 2018-2020
#*      - pc >= 50%
#*      - reliable years == 3

# set up ------------------------------------------------------------------
rm(list=ls(all=TRUE))
options(scipen = 999)
library(dplyr)
library(lubridate)
library(tidyverse)
library(fst)

path_data_processed <- "/Users/vivian/Desktop/0_PhD/0_lead_research-projects/power_outage-severe_weather-coocurrence/analysis/data/3_processed/"
path_data_ext_drive <- "/Volumes/My Passport Vivian/pous_updated/data_proc/data/"

# read in data and light tidying ------------------------------------------------------------

# 8+ hour outages (county-day)
po8 <- read_fst(paste0(path_data_processed, "po8_pct_all.fst"))

# severe weather
sw <- read_fst(paste0(path_data_ext_drive, "weather/sw_po_events.fst")) %>% 
  filter(year(day) != 2021)

# join data ---------------------------------------------------------------
# limit data to those with percent coverage > 50%
# limit to counties with 3 years of full data
# limit to CONUS because weather data available in CONUS
po8_sw <- left_join(po8, sw) %>% 
  filter(pc >= 0.5,
         n_reliable_years_available == 3,
         !clean_state_name %in% c("alaska", "hawaii")) %>% 
  mutate(year = year(day))

# Save --------------------------------------------------------------------
write_csv(po8_sw, paste0(path_data_processed, "/po8_sw_reliable.csv"))





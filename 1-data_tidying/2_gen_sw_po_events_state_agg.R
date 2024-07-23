# Code description --------------------------------------------------------
#* Author: Vivian
#* Date: 03/20/2024
#* Goal: read in and clean sw_po_events.fst data, aggregating to state
#* Note that to save data space on computer, the raw po and sw data are on external hard drive
#* For figure 2
 
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


# join data ---------------------------------------------------------------
# limit data to those with percent coverage > 50%
# limit to counties with 3 years of full data
# limit to CONUS because weather data available in CONUS
po8_sw <- left_join(po8, sw) %>% 
  rename(i_po8 = exposed) %>% 
  filter(i_po8 == 1,
         pc >= 0.5,
         n_reliable_years_available == 3,
         !clean_state_name %in% c("alaska", "hawaii"))

# aggregate to state, year ------------------------------------------------
po8_sw_state_year <- po8_sw %>% 
  group_by(clean_state_name, year) %>% 
  mutate(state_tot_days_cyc = sum(cyclone),
         state_tot_days_anomppt = sum(anomppt),
         state_tot_days_anomhot = sum(anomhot),
         state_tot_days_anomcold = sum(anomcold),
         state_tot_days_wf = sum(wf),
         state_tot_days_snowfall = sum(snowfall)) %>%
  filter(row_number() == 1) %>% 
  select(clean_state_name, year, starts_with("state"))

# save --------------------------------------------------------------------
write_csv(po8_sw_state_year, paste0(path_data, "processed/po8_sw_state_year.csv"))












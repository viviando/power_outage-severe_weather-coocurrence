# Code description --------------------------------------------------------
#* Author: Vivian
#* Date: 03/20/2024 
#* Goal: read in and clean sw_po_events.fst data, aggregating to county (get # days per county)
#* Note that to save data space on computer, the raw po and sw data are on external hard drive
#* For figure 1

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

# join data
# limit data to those with percent coverage > 50%
# no limit on number of years
# note that i do not limit to 3 years of reliable data here but in 1b_label_sw_po_events_county.R code
po8_sw <- left_join(po8, sw) %>% 
  rename(i_po8 = exposed) %>% 
  filter(i_po8 == 1,
         pc >= 0.5)


# po - singular severe weather event --------------------------------------
# calculate total days with power outage and severe weather event per county
po8_sw_tot_days_singular <- po8_sw %>% 
  group_by(fips) %>% 
  mutate(tot_days_cyc = sum(cyclone),
         tot_days_anomppt = sum(anomppt),
         tot_days_anomhot = sum(anomhot),
         tot_days_anomcold = sum(anomcold),
         tot_days_wf = sum(wf),
         tot_days_snowfall = sum(snowfall)) %>% 
  filter(row_number() == 1) %>% 
  select(-c(9:18))

write_csv(po8_sw_tot_days_singular, paste0(path_data, "processed/po8_sw_tot_days_singular.csv"))




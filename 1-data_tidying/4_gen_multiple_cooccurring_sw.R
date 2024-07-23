# Code description --------------------------------------------------------
#* Author: Vivian
#* Date: 05/02/2024
#* Goal: create data on multiple simultaneous severe weather events, aggregating to state
#* Note that to save data space on computer, the raw po and sw data are on external hard drive
#* For figure 4

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
po8_sw <- left_join(po8, sw) %>% 
  rename(i_po8 = exposed) %>% 
  filter(i_po8 == 1,
         pc >= 0.5,
         n_reliable_years_available == 3)
# add in 3 year


# po - MULTIPLE severe weather event --------------------------------------

# identify multiple severe weather events
sw_types <- c("cyclone", "anomhot", "anomcold", "anomppt", "wf", "snowfall")

po8_sw.5 <- po8_sw %>%
  mutate(tot_ind_sw = rowSums(select(., all_of(sw_types)))) %>%
  filter(tot_ind_sw > 1) %>% 
  mutate(across(all_of(sw_types), ~ifelse(. == 1, as.character(sw_types[which(sw_types == cur_column())]), ""), .names = "{.col}")) %>%
  mutate(multi_sw = paste(cyclone, anomhot, anomcold, anomppt, wf, snowfall, sep = ""))

write_csv(po8_sw.5, paste0(path_data, "processed/po8_sw_multi_sw_state_cat_scatter.csv"))

# aggregate to multi_sw, clean_state_name
po8_sw2 <- po8_sw.5 %>% 
  group_by(clean_state_name, multi_sw) %>%
  mutate(tot_sw = n()) %>% 
  filter(row_number() == 1)

# filter out rows where multi_sw is empty
po8_sw3 <- po8_sw2 %>% 
  filter(multi_sw != "",
         multi_sw != "NANANANANANA")

# get the top 3 tot_sw by state
po8_sw4 <- po8_sw3 %>% 
  group_by(clean_state_name) %>% 
  arrange(desc(tot_sw)) %>% 
  filter(row_number() <= 1) %>% 
  select(clean_state_name, multi_sw, tot_sw)

write_csv(po8_sw4, paste0(path_data, "processed/po8_sw_multi_sw_state.csv"))








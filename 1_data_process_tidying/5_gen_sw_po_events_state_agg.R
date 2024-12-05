# Code description --------------------------------------------------------
#* Author: Vivian
#* Date: 03/20/2024
#* Goal: read in and clean sw_po_events.fst data, aggregating to state
#* Note that to save data space on computer, the raw po and sw data are on external hard drive
#* For figure 2 - state level and year
 
# set up ------------------------------------------------------------------
rm(list=ls(all=TRUE))
options(scipen = 999)
library(dplyr)
library(lubridate)
library(tidyverse)
library(fst)

path_data_processed <- "/Users/vivian/Desktop/0_PhD/0_Research Projects/power_outage-severe_weather-coocurrence/analysis/data/3_processed/"
path_data_ext_drive <- "/Volumes/My Passport Vivian/pous_updated/data_proc/data/"

# read in data and light tidying ------------------------------------------------------------
# reliable po-sw data
po8_sw_reliable <- read_csv(paste0(path_data_processed, "/po8_sw_reliable.csv"))

# Aggregate to state-year -------------------------------------------------
agg_state_year <- function(po_threshold) {
  po8_sw_reliable %>%
    rename(i_po8 = !!sym(po_threshold)) %>%
    group_by(clean_state_name, year) %>% 
    filter(i_po8 == 1) %>%
    mutate(state_tot_days_cyc = sum(cyclone),
           state_tot_days_anomppt = sum(anomppt),
           state_tot_days_anomhot = sum(anomhot),
           state_tot_days_anomcold = sum(anomcold),
           state_tot_days_wf = sum(wf),
           state_tot_days_snowfall = sum(snowfall)) %>%
    filter(row_number() == 1) %>%
    select(clean_state_name, year, starts_with("state"))
}

po_vars <- c("i_exp_0.1", "i_exp_2.5", "i_exp_4.0")

po_sw_all_thresh <- list()

for (po_var in po_vars) {
  po_sw_all_thresh[[po_var]] <- agg_state_year(po_var)
}

po8_sw_state_year_0.1 <- po_sw_all_thresh[["i_exp_0.1"]]
po8_sw_state_year_2.5 <- po_sw_all_thresh[["i_exp_2.5"]]
po8_sw_state_year_4.0 <- po_sw_all_thresh[["i_exp_4.0"]]

# save --------------------------------------------------------------------
write_csv(po8_sw_state_year_0.1, paste0(path_data_processed, "/po8_sw_state_year_0.1.csv"))
write_csv(po8_sw_state_year_2.5, paste0(path_data_processed, "/po8_sw_state_year_2.5.csv"))
write_csv(po8_sw_state_year_4.0, paste0(path_data_processed, "/po8_sw_state_year_4.0.csv"))












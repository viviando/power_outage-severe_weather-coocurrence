# Code description --------------------------------------------------------
#* Author: Vivian
#* Date: 04/05/2024
#* Goal: read in and clean sw_po_events.fst data, aggregating to county month
#* Note that to save data space on computer, the raw po and sw data are on external hard drive
#* For figure 3

# set up ------------------------------------------------------------------
rm(list=ls(all=TRUE))
options(scipen = 999)
library(dplyr)
library(lubridate)
library(tidyverse)
library(fst)

path_data_processed <- "/Users/vivian/Desktop/0_PhD/0_Research Projects/power_outage-severe_weather-coocurrence/analysis/data/3_processed/"

# read in data ------------------------------------------------------------
# reliable po-sw data
po8_sw_reliable <- read_csv(paste0(path_data_processed, "/po8_sw_reliable.csv"))

# calculate total days with power outage and severe weather event per county --------------------------------------
calc_cty_tot_po_sw <- function(po_threshold){
  po8_sw_reliable %>% 
    rename(i_po8 = !!sym(po_threshold)) %>%
    filter(i_po8 == 1) %>% 
    mutate(month = month(day)) %>% 
    group_by(fips, month) %>% 
    mutate(tot_days_cyc = sum(cyclone),
           tot_days_anomppt = sum(anomppt),
           tot_days_anomhot = sum(anomhot),
           tot_days_anomcold = sum(anomcold),
           tot_days_wf = sum(wf),
           tot_days_snowfall = sum(snowfall)) %>% 
    filter(row_number() == 1) %>% 
    select(clean_state_name, clean_county_name, fips, day, month, i_po8, starts_with("tot"))
}

po_vars <- c("i_exp_0.1", "i_exp_2.5", "i_exp_4.0")
po_sw_all_thresh <- list()

for (po_var in po_vars) {
  po_sw_all_thresh[[po_var]] <- calc_cty_tot_po_sw(po_var)
}

cty_tot_po_sw_0.1 <- po_sw_all_thresh[["i_exp_0.1"]]
cty_tot_po_sw_2.5 <- po_sw_all_thresh[["i_exp_2.5"]]
cty_tot_po_sw_4.0 <- po_sw_all_thresh[["i_exp_4.0"]]

# save --------------------------------------------------------------------
write_csv(cty_tot_po_sw_0.1, paste0(path_data_processed, "/cty_tot_po_sw_0.1.csv"))
write_csv(cty_tot_po_sw_2.5, paste0(path_data_processed, "/cty_tot_po_sw_2.5.csv"))
write_csv(cty_tot_po_sw_4.0, paste0(path_data_processed, "/cty_tot_po_sw_4.0.csv"))





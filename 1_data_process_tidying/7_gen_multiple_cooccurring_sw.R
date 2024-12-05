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

path_data_processed <- "/Users/vivian/Desktop/0_PhD/0_Research Projects/power_outage-severe_weather-coocurrence/analysis/data/3_processed/"

# read in data ------------------------------------------------------------
# reliable po-sw data
po8_sw_reliable <- read_csv(paste0(path_data_processed, "/po8_sw_reliable.csv"))

# Helper functions --------------------------------------------------------
sw_types <- c("cyclone", "anomhot", "anomcold", "anomppt", "wf", "snowfall")

# id power outages of different thresholds with multiple sw
id_po_multi_sw <- function(po_threshold){
  po8_sw_reliable %>% 
    rename(i_po8 = !!sym(po_threshold)) %>% 
    filter(i_po8 == 1) %>% 
    mutate(tot_ind_sw = rowSums(select(., all_of(sw_types)))) %>%
    filter(tot_ind_sw > 1) %>% 
    mutate(across(all_of(sw_types), ~ifelse(. == 1, as.character(sw_types[which(sw_types == cur_column())]), ""), .names = "{.col}")) %>%
    mutate(multi_sw = paste(cyclone, anomhot, anomcold, anomppt, wf, snowfall, sep = ""))
}

# id top sw combination per state
id_state_top_multi_sw_po <- function(data){
  data %>% 
    group_by(clean_state_name, multi_sw) %>%
    mutate(tot_sw = n()) %>% 
    filter(row_number() == 1) %>% 
    filter(multi_sw != "",
           multi_sw != "NANANANANANA") %>% 
    group_by(clean_state_name) %>% 
    arrange(desc(tot_sw)) %>% 
    filter(row_number() <= 1) %>% 
    select(clean_state_name, multi_sw, tot_sw)
  return(data)
}


po_vars <- c("i_exp_0.1", "i_exp_2.5", "i_exp_4.0")
po_sw_all_thresh <- list()
top_po_sw_all_thresh <- list()

for (po_var in po_vars) {
  po_sw_all_thresh[[po_var]] <- id_po_multi_sw(po_var)
  
  top_po_sw_all_thresh[[po_var]] <- id_state_top_multi_sw_po(id_po_multi_sw(po_var))
}

# Extract datasets
multi_po_sw_0.1 <- po_sw_all_thresh[["i_exp_0.1"]]
multi_po_sw_2.5 <- po_sw_all_thresh[["i_exp_2.5"]]
multi_po_sw_4.0 <- po_sw_all_thresh[["i_exp_4.0"]]

top_multi_po_sw_0.1 <- top_po_sw_all_thresh[["i_exp_0.1"]]
top_multi_po_sw_2.5 <- top_po_sw_all_thresh[["i_exp_2.5"]]
top_multi_po_sw_4.0 <- top_po_sw_all_thresh[["i_exp_4.0"]]

# Save 
write_csv(multi_po_sw_0.1, paste0(path_data_processed, "/multi_po_sw_0.1.csv"))
write_csv(multi_po_sw_2.5, paste0(path_data_processed, "/multi_po_sw_2.5.csv"))
write_csv(multi_po_sw_4.0, paste0(path_data_processed, "/multi_po_sw_4.0.csv"))

write_csv(top_multi_po_sw_0.1, paste0(path_data_processed, "/top_multi_po_sw_0.1.csv"))
write_csv(top_multi_po_sw_2.5, paste0(path_data_processed, "/top_multi_po_sw_2.5.csv"))
write_csv(top_multi_po_sw_4.0, paste0(path_data_processed, "/top_multi_po_sw_4.0.csv"))


# Old ---------------------------------------------------------------------


# po - MULTIPLE severe weather event --------------------------------------
# identify multiple severe weather events
sw_types <- c("cyclone", "anomhot", "anomcold", "anomppt", "wf", "snowfall")

po8_sw_reliable_sw_list <- po8_sw_reliable %>%
  mutate(tot_ind_sw = rowSums(select(., all_of(sw_types)))) %>%
  filter(tot_ind_sw > 1) %>% 
  mutate(across(all_of(sw_types), ~ifelse(. == 1, as.character(sw_types[which(sw_types == cur_column())]), ""), .names = "{.col}")) %>%
  mutate(multi_sw = paste(cyclone, anomhot, anomcold, anomppt, wf, snowfall, sep = ""))

write_csv(po8_sw_reliable_sw_list, paste0(path_data, "processed/po8_sw_multi_sw_state_cat_scatter.csv"))

# aggregate to multi_sw, clean_state_name
po8_sw_reliable_multi_sw_by_state <- po8_sw_reliable_sw_list %>% 
  group_by(clean_state_name, multi_sw) %>%
  mutate(tot_sw = n()) %>% 
  filter(row_number() == 1)

# filter out rows where multi_sw is empty
po8_sw_reliable_multi_sw_by_state <- po8_sw_reliable_multi_sw_by_state %>% 
  filter(multi_sw != "",
         multi_sw != "NANANANANANA")

# get the top tot_sw by state
po8_sw_multi_sw_state <- po8_sw_reliable_multi_sw_by_state %>% 
  group_by(clean_state_name) %>% 
  arrange(desc(tot_sw)) %>% 
  filter(row_number() <= 1) %>% 
  select(clean_state_name, multi_sw, tot_sw)

write_csv(po8_sw_multi_sw_state, paste0(path_data_processed, "processed/po8_sw_multi_sw_state.csv"))

po8_sw_multi_sw_state <- read_csv(paste0(path_data_processed, "processed/po8_sw_multi_sw_state.csv"))







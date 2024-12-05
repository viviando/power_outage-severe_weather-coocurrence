# Code description --------------------------------------------------------
#* Author: Vivian
#* Date: 06/26/2024 
#* Goal: create data of county days with ONLY power outage
#* for figure 3

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

# eval power outages ------------------------------------------------------
table(po8_sw_reliable$)











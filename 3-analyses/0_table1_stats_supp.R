# Code description --------------------------------------------------------
#* Author: Vivian
#* Date: 05/14/2024
#* Goal: generate data for table 1 SUPPLEMENTARY and manuscript
#* for example,
#* how many counties have 3 years of reliable data by state?
#*

# set up ------------------------------------------------------------------
rm(list = ls(all = TRUE))
options(scipen = 999)
library(dplyr)
library(lubridate)
library(tidyverse)
library(fst)
library(sf)
library(ggthemes)
library(usmap)
library(patchwork)
library(MetBrewer)
library(tidycensus)
library(stringr)
library(PNWColors)

path_data <-
  "/Users/vivian/Desktop/0_PhD/0_Research Projects/spatial_po_severe_weather/project code/data/"
path_map <-
  "/Users/vivian/Desktop/0_PhD/0_Research Projects/spatial_po_severe_weather/project code/output/figures/"
path_tables <-
  "/Users/vivian/Desktop/0_PhD/0_Research Projects/spatial_po_severe_weather/project code/output/tables/"

# read in data; some prep ------------------------------------------------------------
# 8+ hour outages (county-day)
po8 <- readRDS("/Volumes/My Passport Vivian/pous_updated/data_proc/data/days_exposed_unexposed_expansion_w_all_fips.RDS")
denom_electrical_customers <-
  read_rds(("/Volumes/My Passport Vivian/pous_updated/data_proc/data/data_with_coverage_exclusions_sample.RDS")) %>% 
  group_by(fips) %>%
  filter(row_number() == 1) %>% 
  select(fips, customers_served_total)

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
         !clean_state_name %in% c("alaska", "hawaii")) %>% 
  left_join(., denom_electrical_customers)

# national county data; use ACS 5 year survey 2016-2020
population <-
  read_csv(
    paste0(
      path_data,
      "/raw/Population/nhgis0021_csv/nhgis0021_ds249_20205_county.csv"
    )
  ) %>%
  mutate(fips = paste0(STATEA, COUNTYA)) %>%
  select(STATE, COUNTY, fips, AMPVE001) %>%
  rename(pop = AMPVE001) %>%
  filter(!STATE %in% c("Hawaii", "Alaska", "Puerto Rico"))


# n counties/county-days with po8 (DO THIS AFTER I GET BACK TO NYC) -------



# n study counties and electrical customers affected --------------------------------------------------------
# number of counties with 1, 2, or 3 years of reliable data
length(unique(po8$fips))

# number of counties with 3 years of reliable data
# 1657
po8 %>%
  filter(n_reliable_years_available == 3) %>%
  pull(fips) %>%
  unique() %>%
  length()

elec_cust_counties_reliable <- po8 %>%
  left_join(., denom_electrical_customers) %>% 
  filter(n_reliable_years_available == 3) %>%
  group_by(fips) %>% 
  filter(row_number() == 1) %>% 
  select(fips, customers_served_total) 
sum(elec_cust_counties_reliable$customers_served_total) #156,770,930 electrical customers served in these 1,657 counties

# number of counties with 3 years of reliable data AND individual sw
# 1229
po_sw_single <-
  read_csv(paste0(path_data, "processed/po8_sw_tot_days_singular.csv")) %>%
  filter(n_reliable_years_available == 3) %>%
  group_by(fips) %>%
  filter(row_number() == 1) %>%
  select(clean_state_name, clean_county_name, starts_with("tot")) %>% 
  left_join(., denom_electrical_customers)
nrow(po_sw_single)
sum(po_sw_single$customers_served_total) #116,767,905 electrical customers served in these 1,229 counties


# number of counties with 3 years of reliable data AND multiple sw
# 880
po_sw_multi <-
  read_csv(paste0(
    path_data,
    "processed/po8_sw_multi_sw_state_cat_scatter.csv"
  )) %>%
  filter(n_reliable_years_available == 3) %>%
  group_by(fips) %>%
  filter(row_number() == 1) %>%
  select(clean_state_name, clean_county_name, fips, multi_sw) %>% 
  left_join(., denom_electrical_customers)
nrow(po_sw_multi)
sum(po_sw_multi$customers_served_total) # 96,204,715 electrical customers served in these 880 counties
nrow(po_sw_multi <-
       read_csv(paste0(
         path_data,
         "processed/po8_sw_multi_sw_state_cat_scatter.csv"
       ))) # 2,202 county days with 8+ hour outages and multiple severe weather types and 3 years of reliable data

# n counties affected by each sw type -------------------------------------
# identify counties that every experienced a given sw type
n_counties_affected_by_sw <- po_sw_single %>% 
  group_by(fips) %>% 
  mutate(i_cyc = ifelse(sum(tot_days_cyc) > 0, 1, 0),
         i_anomhot = ifelse(sum(tot_days_anomhot) > 0, 1, 0),
         i_anomppt = ifelse(sum(tot_days_anomppt) > 0, 1, 0),
         i_anomcold = ifelse(sum(tot_days_anomcold) > 0, 1, 0),
         i_anomwf = ifelse(sum(tot_days_wf) > 0, 1, 0),
         i_snowfall = ifelse(sum(tot_days_snowfall) > 0, 1, 0)) %>% 
  select(fips, starts_with("i_")) %>% 
  filter(row_number() == 1) 

# create table
n_fips_w_cyc <- nrow(n_counties_affected_by_sw %>% filter(i_cyc == 1))
n_fips_w_anomhot <- nrow(n_counties_affected_by_sw %>% filter(i_anomhot == 1))
n_fips_w_anomppt <- nrow(n_counties_affected_by_sw %>% filter(i_anomppt == 1))
n_fips_w_anomcold <- nrow(n_counties_affected_by_sw %>% filter(i_anomcold == 1))
n_fips_w_anomwf <- nrow(n_counties_affected_by_sw %>% filter(i_anomwf == 1))
n_fips_w_snowfall <- nrow(n_counties_affected_by_sw %>% filter(i_snowfall == 1))

# create a table with sw_type and n_counties_sing
sw_type <-
  c(
    "Cyclone",
    "Anomalous Precipitation",
    "Anomalous Hot",
    "Anomalous Cold",
    "Wildfire",
    "Snowfall"
  )
n_counties_sing <-
  c(
    n_fips_w_cyc,
    n_fips_w_anomppt,
    n_fips_w_anomhot,
    n_fips_w_anomcold,
    n_fips_w_anomwf,
    n_fips_w_snowfall
  )

table_sing <- data.frame(sw_type, n_counties_sing) %>%
  mutate(
    pct = n_counties_sing / po8 %>%
      filter(n_reliable_years_available == 3) %>%
      pull(fips) %>%
      unique() %>%
      length(),
    n_pct = paste0(n_counties_sing, " (", round(pct * 100, 1), "%)")
  ) %>%
  arrange(desc(n_counties_sing)) %>%
  select(sw_type, n_pct) %>%
  write_csv(paste0(path_tables, "table_counties_w_single_sw.csv"))

# counties with severe weather by state -----------------------------------
# single
n_counties_state_single <- po_sw_single %>%
  group_by(clean_state_name) %>%
  mutate(n_counties_single = n()) %>%
  filter(row_number() == 1) %>%
  select(clean_state_name, n_counties_single)

# multi
n_counties_state_multi <- po_sw_multi %>%
  group_by(clean_state_name) %>%
  mutate(n_counties_multi = n()) %>%
  filter(row_number() == 1) %>%
  select(clean_state_name, n_counties_multi)

# number of counties per state with 3+ years of reliable power outage data
# this will be our denominator for manuscript/supplementary tables
n_counties_state_3yr_reliable_po_data <- po8 %>%
  filter(n_reliable_years_available == 3) %>% 
  group_by(clean_state_name, fips) %>%
  filter(row_number() == 1) %>%
  group_by(clean_state_name) %>% 
  mutate(n_counties = n()) %>%
  filter(row_number() == 1) %>% 
  select(clean_state_name, n_counties) %>% 
  mutate(clean_state_name = ifelse(clean_state_name == "districtofcolumbia", "District of Columbia", clean_state_name),
         clean_state_name = ifelse(clean_state_name == "westvirginia", "West Virginia", clean_state_name),
         clean_state_name = ifelse(clean_state_name == "southdakota", "South Dakota", clean_state_name),
         clean_state_name = ifelse(clean_state_name == "northdakota", "North Dakota", clean_state_name),
         clean_state_name = ifelse(clean_state_name == "newhampshire", "New Hampshire", clean_state_name),
         clean_state_name = ifelse(clean_state_name == "newjersey", "New Jersey", clean_state_name),
         clean_state_name = ifelse(clean_state_name == "newmexico", "New Mexico", clean_state_name),
         clean_state_name = ifelse(clean_state_name == "newyork", "New York", clean_state_name),
         clean_state_name = ifelse(clean_state_name == "northcarolina", "North Carolina", clean_state_name),
         clean_state_name = ifelse(clean_state_name == "southcarolina", "South Carolina", clean_state_name),
         clean_state_name = ifelse(clean_state_name == "rhodeisland", "Rhode Island", clean_state_name),
         clean_state_name = str_to_title(clean_state_name))

# join and clean
n_counties_state_sw <-
  left_join(n_counties_state_single, n_counties_state_multi, by = "clean_state_name") %>% 
  mutate(n_counties_multi = ifelse(is.na(n_counties_multi), 0, n_counties_multi)) %>% 
  filter(!clean_state_name %in% c("alaska", "hawaii")) %>% 
  mutate(clean_state_name = ifelse(clean_state_name == "districtofcolumbia", "District of Columbia", clean_state_name),
         clean_state_name = ifelse(clean_state_name == "westvirginia", "West Virginia", clean_state_name),
         clean_state_name = ifelse(clean_state_name == "southdakota", "South Dakota", clean_state_name),
         clean_state_name = ifelse(clean_state_name == "northdakota", "North Dakota", clean_state_name),
         clean_state_name = ifelse(clean_state_name == "newhampshire", "New Hampshire", clean_state_name),
         clean_state_name = ifelse(clean_state_name == "newjersey", "New Jersey", clean_state_name),
         clean_state_name = ifelse(clean_state_name == "newmexico", "New Mexico", clean_state_name),
         clean_state_name = ifelse(clean_state_name == "newyork", "New York", clean_state_name),
         clean_state_name = ifelse(clean_state_name == "northcarolina", "North Carolina", clean_state_name),
         clean_state_name = ifelse(clean_state_name == "southcarolina", "South Carolina", clean_state_name),
         clean_state_name = ifelse(clean_state_name == "rhodeisland", "Rhode Island", clean_state_name),
         clean_state_name = str_to_title(clean_state_name)) %>% 
  left_join(n_counties_state_3yr_reliable_po_data) %>% 
  mutate(n_counties = ifelse(clean_state_name == "District Of Columbia", 1, n_counties),
         pct_counties_single = round(n_counties_single/n_counties * 100, 1),
         pct_counties_multi = round(n_counties_multi/n_counties * 100, 1),
         n_pct_counties_single = paste0(n_counties_single, " (", pct_counties_single, ")"),
         n_pct_counties_multi = paste0(n_counties_multi, " (", pct_counties_multi, ")")) %>% 
  select(clean_state_name, n_counties, n_pct_counties_single, n_pct_counties_multi) 
  
# save
write_csv(n_counties_state_sw, paste0(path_tables, "stable_n_counties_state_sw.csv"))
n_counties_state_sw <- read_csv(paste0(path_tables, "stable_n_counties_state_sw.csv"))


# top multi sw combination per state --------------------------------------
top_multi_sw_state <- po_sw_multi %>% 
  group_by(clean_state_name, multi_sw) %>% 
  mutate(n_state_sw = n()) %>% 
  filter(row_number() == 1) %>% 
  group_by(clean_state_name) %>% 
  filter(n_state_sw == max(n_state_sw)) %>% 
  select(clean_state_name, multi_sw, n_state_sw) %>% 
  mutate(clean_state_name = ifelse(clean_state_name == "districtofcolumbia", "District of Columbia", clean_state_name),
         clean_state_name = ifelse(clean_state_name == "westvirginia", "West Virginia", clean_state_name),
         clean_state_name = ifelse(clean_state_name == "southdakota", "South Dakota", clean_state_name),
         clean_state_name = ifelse(clean_state_name == "northdakota", "North Dakota", clean_state_name),
         clean_state_name = ifelse(clean_state_name == "newhampshire", "New Hampshire", clean_state_name),
         clean_state_name = ifelse(clean_state_name == "newjersey", "New Jersey", clean_state_name),
         clean_state_name = ifelse(clean_state_name == "newmexico", "New Mexico", clean_state_name),
         clean_state_name = ifelse(clean_state_name == "newyork", "New York", clean_state_name),
         clean_state_name = ifelse(clean_state_name == "northcarolina", "North Carolina", clean_state_name),
         clean_state_name = ifelse(clean_state_name == "southcarolina", "South Carolina", clean_state_name),
         clean_state_name = ifelse(clean_state_name == "rhodeisland", "Rhode Island", clean_state_name),
         clean_state_name = str_to_title(clean_state_name)) %>% 
  mutate(multi_sw = case_when(multi_sw == "cycloneanomppt" ~ "Cyclone and Anomalous Precipitation",
                              multi_sw == "anomcoldanomppt" ~ "Anomalous Precipitation and Cold",
                              multi_sw == "anomhotanomppt" ~ "Anomalous Precipitation and Heat",
                              multi_sw == "anomcoldsnowfall" ~ "Anomalous Cold and Snowfall",
                              multi_sw == "anompptwf" ~ "Anomalous Precipitation and Wildfire",
                              multi_sw == "cycloneanomhotanomppt" ~ "Cyclone, Anomalous Heat, and Anomalous Precipitation",
                              multi_sw == "anomhotwf" ~ "Anomalous Heat and Wildfire",
                              multi_sw == "anomhotanompptwf" ~ "Anomalous Heat, Anomalous Precipitation, and Wildfire",
                              multi_sw == "wfsnowfall" ~ "Wildfire and Snowfall",
                              multi_sw == "anomcoldwfsnowfall" ~ "Anomalous Cold and Snowfall",
                              multi_sw == "anomcoldwf" ~ "Anomalous Cold and Wildfire",
                              multi_sw == "cycloneanomhot" ~ "Cyclone and Anomalous Heat"))
  
# save
write_csv(top_multi_sw_state, paste0(path_tables, "stable_top_multi_sw_state.csv"))
top_multi_sw_state <- read_csv(paste0(path_tables, "stable_top_multi_sw_state.csv"))


# multi sw for total county days and states ------------------------------
multi_sw_tot_county_days <- read_csv(paste0(path_data, "processed/po8_sw_multi_sw_state_cat_scatter.csv")) %>% 
  select(multi_sw, clean_state_name) %>%
  group_by(multi_sw) %>% 
  mutate(n_county_days = n(),
         n_states_affected = n_distinct(clean_state_name)) %>% 
  filter(row_number() == 1) %>% 
  select(-clean_state_name)
  
# wf estimates from above
# filter multi_sw contains "wf"
multi_sw_tot_county_days_wf <- read_csv(paste0(path_data, "processed/po8_sw_multi_sw_state_cat_scatter.csv")) %>% 
  select(multi_sw, clean_state_name) %>% 
  filter(multi_sw %in% c("anompptwf",
                         "anomhotwf",
                         "wfsnowfall",
                         "anomcoldwfsnowfall",
                         "anomcoldwf")) %>% 
  mutate(n_county_days = n(),
         n_states_affected = n_distinct(clean_state_name)) %>% 
  filter(row_number() == 1) %>% 
  select(starts_with("n_"))



# anomalous heat and anomalous precipitation ------------------------------
anomheat_anompcp <- po_sw_single %>% 
  select(fips, clean_state_name, tot_days_anomppt, tot_days_anomhot) %>% 
  filter(tot_days_anomppt > 0 & tot_days_anomhot > 0)

length(unique(anomheat_anompcp$clean_state_name)) # 44 states affected by either anom heat, anom pcp
sum(anomheat_anompcp$tot_days_anomppt, anomheat_anompcp$tot_days_anomhot) # 10,783 county days



# we no longer want the below after 6/10/24 so we can focus on electrical customers
# # population included in study
# # all counties
# n_counties_state <- population %>% 
#   group_by(STATE) %>% 
#   mutate(n_counties = n()) %>%
#   filter(row_number() == 1) %>% 
#   select(STATE, n_counties) %>% 
#   rename(clean_state_name = STATE)
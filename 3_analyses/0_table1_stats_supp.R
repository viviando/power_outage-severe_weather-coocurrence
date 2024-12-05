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
library(totalcensus)
library(usdata)

path_data_processed <-
  "/Users/vivian/Desktop/0_PhD/0_Research Projects/power_outage-severe_weather-coocurrence/analysis/data/3_processed/"
path_map <-
  "/Users/vivian/Desktop/0_PhD/0_Research Projects/power_outage-severe_weather-coocurrence/analysis/output/figures/"
path_tables <-
  "/Users/vivian/Desktop/0_PhD/0_Research Projects/power_outage-severe_weather-coocurrence/analysis/output/tables/"

# read in data; some prep ------------------------------------------------------------
# reliable po-sw data
po8_sw_reliable <- read_csv(paste0(path_data_processed, "/po8_sw_reliable.csv"))

# electrical customers
denom_electrical_customers <-
  read_rds(("/Volumes/My Passport Vivian/pous_updated/data_proc/data/data_with_coverage_exclusions_sample.RDS")) %>% 
  group_by(fips) %>%
  filter(row_number() == 1) %>% 
  select(fips, customers_served_total)

# add electrical customers
po8_sw_reliable <- po8_sw_reliable %>% 
  left_join(., denom_electrical_customers)

# data for number of counties per state (for supplementary table)
# convert state fips to state name
n_counties_per_state <- st_read("/Users/vivian/Desktop/0_PhD/0_Research Projects/spatial_po_severe_weather/project code/data/raw/nhgis0044_shapefile_tl2020_us_county_2020/US_county_2020.shp") %>% 
  select(STATEFP, NAME) %>%
  group_by(STATEFP) %>% 
  mutate(n_counties_total = n()) %>%
  filter(row_number() == 1) %>% 
  mutate(state_abbr = convert_fips_to_names(STATEFP, states = NULL, geo_header = "STATE", in_states = NULL)) %>% 
  select(n_counties_total, state_abbr) %>% 
  st_drop_geometry() %>% 
  mutate(clean_state_name = abbr2state(state_abbr)) %>% 
  mutate(clean_state_name = ifelse(clean_state_name == "District of Columbia", "District Of Columbia", clean_state_name))

# national population county data; use ACS 5 year survey 2016-2020
population <-
  read_csv(
    paste0(
      path_data_processed,
      "/raw/Population/nhgis0021_csv/nhgis0021_ds249_20205_county.csv"
    )
  ) %>%
  mutate(fips = paste0(STATEA, COUNTYA)) %>%
  select(STATE, COUNTY, fips, AMPVE001) %>%
  rename(pop = AMPVE001) %>%
  filter(!STATE %in% c("Hawaii", "Alaska", "Puerto Rico"))

# number of study counties, county-days, electrical customers served --------------------------------------------------------
# reliability only - number of counties with 1, 2, or 3 years of reliable data
length(unique(po8$fips))

# reliability only - number of counties with 3 years of reliable data
# 1657
po8 %>%
  filter(n_reliable_years_available == 3) %>%
  pull(fips) %>%
  unique() %>%
  length()

# reliability only - number of electrical customers served in counties with 3 years of reliable data
# 156,770,930 electrical customers served in these 1,657 counties
elec_cust_counties_reliable <- po8 %>%
  left_join(., denom_electrical_customers) %>% 
  filter(n_reliable_years_available == 3) %>%
  group_by(fips) %>% 
  filter(row_number() == 1) %>% 
  select(fips, customers_served_total) 
sum(elec_cust_counties_reliable$customers_served_total) 

# reliability & power outage 
# 1,229 counties 
# 23,076 county-days 
po_3_years_reliable_data <- po8 %>%
  filter(n_reliable_years_available == 3,
         exposed == 1)  
length(unique(po_3_years_reliable_data$fips)) # counties with 3 years of reliable data and 8+ hour outages
nrow(po_3_years_reliable_data) # county-days with 3 years of reliable data and 8+ hour outages

# reliability & power outage & individual severe weather
# 1,201 counties 
# 14,226 county-days 
n_countdays_w_po8_ind_sw <- po8_sw %>% 
  filter(n_reliable_years_available == 3) %>% 
  mutate(i_sw = ifelse(cyclone + anomhot + anomppt + anomcold + wf + snowfall > 0, 1, 0)) %>%
  filter(i_po8 == 1, i_sw == 1)
length(unique(n_countdays_w_po8_ind_sw$fips))
nrow(n_countdays_w_po8_ind_sw)
nrow(n_countdays_w_po8_ind_sw)/nrow(po_3_years_reliable_data) # 61.7% of county-days with 3 years of reliable data and 8+ hour outages have severe weather

# reliability & power outage & individual severe weather
# 116,060,934 electrical customers served in these 1,201 counties
po_sw_ind <-
  read_csv(paste0(path_data_processed, "processed/po8_sw_tot_days_singular.csv")) %>%
  filter(n_reliable_years_available == 3) %>%
  mutate(i_sw = ifelse(tot_days_cyc + tot_days_anomppt + tot_days_anomhot + tot_days_anomcold + tot_days_wf + tot_days_snowfall > 0, 1, 0)) %>%
  filter(i_sw == 1, i_po8 == 1) %>% 
  group_by(fips) %>%
  filter(row_number() == 1) %>%
  select(clean_state_name, clean_county_name, i_sw, starts_with("tot")) %>% 
  left_join(., denom_electrical_customers)
sum(po_sw_ind$customers_served_total) 

# reliability & power outage & multiple severe weather
# 2,202 county-days 
n_countdays_w_po8_multi_sw <- n_countdays_w_po8_ind_sw %>% 
  mutate(i_multi_sw = ifelse(cyclone + anomhot + anomppt + anomcold + wf + snowfall > 1, 1, 0)) %>% 
  filter(i_multi_sw == 1)
nrow(n_countdays_w_po8_multi_sw)

# reliability & power outage & multiple severe weather
# 880 counties
# 96,204,715 electrical customers served in these 880 counties
po_sw_multi <-
  read_csv(paste0(
    path_data_processed,
    "processed/po8_sw_multi_sw_state_cat_scatter.csv"
  )) %>%
  filter(n_reliable_years_available == 3) %>%
  group_by(fips) %>%
  filter(row_number() == 1) %>%
  select(clean_state_name, clean_county_name, fips, multi_sw) %>% 
  left_join(., denom_electrical_customers)
nrow(po_sw_multi)
sum(po_sw_multi$customers_served_total) 
nrow(po_sw_multi <-
       read_csv(paste0(
         path_data_processed,
         "processed/po8_sw_multi_sw_state_cat_scatter.csv"
       ))) 

# number of counties and severe weather events -------------------------------------
# number of counties with individual severe weather event
n_counties_affected_by_sw <- po_sw_ind %>% 
  group_by(fips) %>% 
  mutate(i_cyc = ifelse(sum(tot_days_cyc) > 0, 1, 0),
         i_anomhot = ifelse(sum(tot_days_anomhot) > 0, 1, 0),
         i_anomppt = ifelse(sum(tot_days_anomppt) > 0, 1, 0),
         i_anomcold = ifelse(sum(tot_days_anomcold) > 0, 1, 0),
         i_anomwf = ifelse(sum(tot_days_wf) > 0, 1, 0),
         i_snowfall = ifelse(sum(tot_days_snowfall) > 0, 1, 0)) %>% 
  select(fips, starts_with("i_")) %>% 
  filter(row_number() == 1) 

# create table of above
n_fips_w_cyc <- nrow(n_counties_affected_by_sw %>% filter(i_cyc == 1))
n_fips_w_anomhot <- nrow(n_counties_affected_by_sw %>% filter(i_anomhot == 1))
n_fips_w_anomppt <- nrow(n_counties_affected_by_sw %>% filter(i_anomppt == 1))
n_fips_w_anomcold <- nrow(n_counties_affected_by_sw %>% filter(i_anomcold == 1))
n_fips_w_anomwf <- nrow(n_counties_affected_by_sw %>% filter(i_anomwf == 1))
n_fips_w_snowfall <- nrow(n_counties_affected_by_sw %>% filter(i_snowfall == 1))

# create a table with sw_type and n_counties_ind
sw_type <-
  c(
    "Cyclone",
    "Anomalous Precipitation",
    "Anomalous Hot",
    "Anomalous Cold",
    "Wildfire",
    "Snowfall"
  )
n_counties_ind <-
  c(
    n_fips_w_cyc,
    n_fips_w_anomppt,
    n_fips_w_anomhot,
    n_fips_w_anomcold,
    n_fips_w_anomwf,
    n_fips_w_snowfall
  )

table_ind <- data.frame(sw_type, n_counties_ind) %>%
  mutate(
    pct = n_counties_ind / po8 %>%
      filter(n_reliable_years_available == 3) %>%
      pull(fips) %>%
      unique() %>%
      length(),
    n_pct = paste0(n_counties_ind, " (", round(pct * 100, 1), "%)")
  ) %>%
  arrange(desc(n_counties_ind)) %>%
  select(sw_type, n_pct) %>%
  write_csv(paste0(path_tables, "table_counties_w_ind_sw.csv"))

# state-level number of counties with severe weather event -----------------------------------
# ind
n_counties_state_ind <- po_sw_ind %>%
  group_by(clean_state_name) %>%
  mutate(n_counties_ind = n()) %>%
  filter(row_number() == 1) %>%
  select(clean_state_name, n_counties_ind)

# multi
n_counties_state_multi <- po_sw_multi %>%
  group_by(clean_state_name, fips) %>%
  filter(row_number() == 1) %>% 
  group_by(clean_state_name) %>%
  mutate(n_counties_multi = n()) %>%
  filter(row_number() == 1) %>%
  select(clean_state_name, n_counties_multi)

# state-level number of counties with 3+ years of reliable power outage data
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

# join and clean following datasets
# state-level totals of individual severe weather events
# state-level totals of multiple simultaneous severe weather events
n_counties_state_sw <-
  left_join(n_counties_state_ind, n_counties_state_multi, by = "clean_state_name") %>% 
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
         pct_counties_ind = round(n_counties_ind/n_counties * 100, 1),
         pct_counties_multi = round(n_counties_multi/n_counties * 100, 1),
         n_pct_counties_ind = paste0(n_counties_ind, " (", pct_counties_ind, ")"),
         n_pct_counties_multi = paste0(n_counties_multi, " (", pct_counties_multi, ")")) %>% 
  select(clean_state_name, n_counties, n_pct_counties_ind, n_pct_counties_multi) 
  
# add in state-level total counties 
n_counties_state_sw <- n_counties_state_sw %>% 
  left_join(., n_counties_per_state) %>% 
  select(clean_state_name, n_counties_total, everything()) %>%
  select(-state_abbr)

# save
write_csv(n_counties_state_sw, paste0(path_tables, "stable_n_counties_state_sw.csv"))
n_counties_state_sw <- read_csv(paste0(path_tables, "stable_n_counties_state_sw.csv"))

# top individual severe weather combination per state --------------------------------------
# among variables starting with "tot", identify the one with the greatest value
top_ind_sw_state <- po_sw_ind %>% 
  group_by(clean_state_name) %>% 
  mutate(state_tot_days_cyc = sum(tot_days_cyc),
         state_tot_days_anomppt = sum(tot_days_anomppt),
         state_tot_days_anomhot = sum(tot_days_anomhot),
         state_tot_days_anomcold = sum(tot_days_anomcold),
         state_tot_days_wf = sum(tot_days_wf),
         state_tot_days_snowfall = sum(tot_days_snowfall)) %>% 
  filter(row_number() == 1) %>%
  select(clean_state_name, starts_with("state_tot_days")) %>% 
  pivot_longer(cols = starts_with("state_tot_days"), names_to = "sw_type", values_to = "days") %>%
  filter(days == max(days)) %>% 
  select(clean_state_name, sw_type, days) %>%
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
  mutate(sw_type = case_when(sw_type == "state_tot_days_cyc" ~ "Cyclone",
                             sw_type == "state_tot_days_anomppt" ~ "Anomalous Precipitation",
                             sw_type == "state_tot_days_anomhot" ~ "Anomalous Hot",
                             sw_type == "state_tot_days_anomcold" ~ "Anomalous Cold",
                             sw_type == "state_tot_days_wf" ~ "Wildfire",
                             sw_type == "state_tot_days_snowfall" ~ "Snowfall"))
  
# save
write_csv(top_ind_sw_state, paste0(path_tables, "stable_top__sw_state.csv"))

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


# multi severe weather for total county days and states ------------------------------
# anomalous precipitation-anomalous heat affect 1,003 county days and 39 states
multi_sw_tot_county_days <- read_csv(paste0(path_data_processed, "processed/po8_sw_multi_sw_state_cat_scatter.csv")) %>% 
  select(multi_sw, clean_state_name) %>%
  group_by(multi_sw) %>% 
  mutate(n_county_days = n(),
         n_states_affected = n_distinct(clean_state_name)) %>% 
  filter(row_number() == 1) %>% 
  select(-clean_state_name)


# correlation for cyclones - precipitation --------------------------------
po8_sw %>%
  filter(n_reliable_years_available == 3) %>%
  mutate(i_cyc = ifelse(cyclone > 0, 1, 0),
         i_anomppt = ifelse(anomppt > 0, 1, 0)) %>%
  select(i_cyc, i_anomppt) %>%
  filter(i_cyc == 1) %>% 
  summarise(corr = cor(i_cyc, i_anomppt, method = "pearson")) %>%
  pull(corr) # 0.176 correlation

test <- po8_sw %>%
  filter(n_reliable_years_available == 3) %>%
  mutate(i_cyc = ifelse(cyclone > 0, 1, 0),
         i_anomppt = ifelse(anomppt > 0, 1, 0)) %>%
  select(i_cyc, i_anomppt) %>%
  filter(i_cyc == 1)

table(test)[1]/nrow(test)



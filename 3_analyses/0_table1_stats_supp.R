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
  "/Users/vivian/Desktop/0_PhD/0_lead_research-projects/power_outage-severe_weather-coocurrence/analysis/data/3_processed/"
path_map <-
  "/Users/vivian/Desktop/0_PhD/0_lead_research-projects/power_outage-severe_weather-coocurrence/analysis/output/figures/"
path_tables <-
  "/Users/vivian/Desktop/0_PhD/0_lead_research-projects/power_outage-severe_weather-coocurrence/analysis/output/tables/"

# read in data; some prep ------------------------------------------------------------
# power outage data - contains unreliable and reliable data
po8 <- read_fst(paste0(path_data_processed, "po8_pct_all.fst"))

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

# number of counties per state 
# for supplementary table
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

# separate data by po threshold
po8_sw_reliable_0.1 <- po8_sw_reliable %>% 
  filter(i_exp_0.1 == 1) %>% 
  select(-i_exp_2.5, -i_exp_4.0)

po8_sw_reliable_2.5 <- po8_sw_reliable %>% 
  filter(i_exp_2.5 == 1) %>% 
  select(-i_exp_0.1, -i_exp_4.0)

po8_sw_reliable_4.0 <- po8_sw_reliable %>% 
  filter(i_exp_4.0 == 1) %>% 
  select(-i_exp_0.1, -i_exp_2.5)

# number of study counties, county-days, electrical customers served --------------------------------------------------------

##### reliability only (does not rely on threshold) -----
# number of counties in our data with unreliable pc and unreliable full years of data
length(unique(po8$fips)) # 2836
 
# number of counties with 3 years of reliable data and pc 
# 1657
po8 %>%
  filter(n_reliable_years_available == 3,
         pc >= 0.5) %>%
  pull(fips) %>%
  unique() %>%
  length()

# number of county days, 1,802,873
po8 %>%
  filter(n_reliable_years_available == 3,
         pc >= 0.5,
         !clean_state_name %in% c("alaska", "hawaii")) %>%
  nrow()

# number of electrical customers served in counties with 3 years of reliable data
# 156,770,930 electrical customers served in these 1,657 counties
elec_cust_counties_reliable <- po8 %>%
  left_join(., denom_electrical_customers) %>% 
  filter(n_reliable_years_available == 3,
         pc >= 0.5) %>%
  group_by(fips) %>% 
  filter(row_number() == 1) %>% 
  select(fips, customers_served_total) 
sum(elec_cust_counties_reliable$customers_served_total) 
nrow(elec_cust_counties_reliable)


# reliability and power outage thresholds ---------------------------------
# reliable and exposed to po 0.1
length(unique(po8_sw_reliable_0.1$fips)) #1226
nrow(po8_sw_reliable_0.1) #28259 cty days

# reliable and exposed to po 2.5
length(unique(po8_sw_reliable_2.5$fips)) #865
nrow(po8_sw_reliable_2.5) #5275 cty days

# reliable and exposed to po 4.0
length(unique(po8_sw_reliable_4.0$fips)) #739
nrow(po8_sw_reliable_4.0) #3857 cty days

# reliability and power outage thresholds and individual severe weather ---------------------------------
# po 0.1
cty_days_w_po_ind_sw_0.1 <- po8_sw_reliable_0.1 %>% 
  mutate(i_sw = ifelse(cyclone + anomhot + anomppt + anomcold + wf + snowfall > 0, 1, 0)) %>%
  filter(i_exp_0.1 == 1, i_sw == 1)
length(unique(cty_days_w_po_ind_sw_0.1$fips)) #1205
nrow(cty_days_w_po_ind_sw_0.1) #16757 cty days
nrow(cty_days_w_po_ind_sw_0.1)/nrow(po8_sw_reliable_0.1) #59.3% of county days with 3 years of reliable data and 8+ hour outages have severe weather

cty_days_w_po_ind_sw_0.1 %>%
  left_join(denom_electrical_customers) %>% 
  group_by(fips) %>% 
  mutate(avg_customers_served = mean(customers_served_total, na.rm = TRUE)) %>% 
  filter(row_number() == 1) %>% 
  ungroup() %>% 
  summarise(total_customers_served = sum(avg_customers_served, na.rm = TRUE)) %>%
  pull(total_customers_served) #116154002 customers served in counties with outage and individual severe weather

# po 2.5
cty_days_w_po_ind_sw_2.5 <- po8_sw_reliable_2.5 %>% 
  mutate(i_sw = ifelse(cyclone + anomhot + anomppt + anomcold + wf + snowfall > 0, 1, 0)) %>%
  filter(i_exp_2.5 == 1, i_sw == 1)
length(unique(cty_days_w_po_ind_sw_2.5$fips)) #844
nrow(cty_days_w_po_ind_sw_2.5)
nrow(cty_days_w_po_ind_sw_2.5)/nrow(po8_sw_reliable_2.5) #61.9% of county days with 3 years of reliable data and 8+ hour outages have severe weather

cty_days_w_po_ind_sw_2.5 %>%
  left_join(denom_electrical_customers) %>% 
  group_by(fips) %>% 
  mutate(avg_customers_served = mean(customers_served_total, na.rm = TRUE)) %>% 
  filter(row_number() == 1) %>% 
  ungroup() %>% 
  summarise(total_customers_served = sum(customers_served_total, na.rm = TRUE)) %>%
  pull(total_customers_served) #79512633 customers served in counties with outage and individual severe weather

# po 4.0
cty_days_w_po_ind_sw_4.0 <- po8_sw_reliable_4.0 %>% 
  mutate(i_sw = ifelse(cyclone + anomhot + anomppt + anomcold + wf + snowfall > 0, 1, 0)) %>%
  filter(i_exp_4.0 == 1, i_sw == 1)
length(unique(cty_days_w_po_ind_sw_4.0$fips)) #719
nrow(cty_days_w_po_ind_sw_4.0)
nrow(cty_days_w_po_ind_sw_4.0)/nrow(po8_sw_reliable_4.0) #61.3% of county days with 3 years of reliable data and 8+ hour outages have severe weather

cty_days_w_po_ind_sw_4.0 %>%
  left_join(denom_electrical_customers) %>% 
  group_by(fips) %>% 
  mutate(avg_customers_served = mean(customers_served_total, na.rm = TRUE)) %>% 
  filter(row_number() == 1) %>% 
  ungroup() %>% 
  summarise(total_customers_served = sum(customers_served_total, na.rm = TRUE)) %>%
  pull(total_customers_served) #56021069 customers served in counties with outage and individual severe weather

# reliability and power outage thresholds and multiple severe weather ---------------------------------
# po 0.1
cty_days_w_po_multi_sw_0.1 <- cty_days_w_po_ind_sw_0.1 %>% 
  mutate(i_multi_sw = ifelse(cyclone + anomhot + anomppt + anomcold + wf + snowfall > 1, 1, 0)) %>% 
  filter(i_multi_sw == 1)
nrow(cty_days_w_po_multi_sw_0.1) # 2389 county days

po_sw_multi_0.1 <-
  read_csv(paste0(
    path_data_processed,
    "multi_po_sw_0.1.csv"
  )) %>%
  filter(n_reliable_years_available == 3) %>%
  group_by(fips) %>%
  filter(row_number() == 1) %>%
  select(clean_state_name, clean_county_name, fips, multi_sw) %>% 
  left_join(., denom_electrical_customers)
nrow(po_sw_multi_0.1) #904 counties
sum(po_sw_multi_0.1$customers_served_total) # 97752077 electrical customers

# po 2.5
cty_days_w_po_multi_sw_2.5 <- cty_days_w_po_ind_sw_2.5 %>% 
  mutate(i_multi_sw = ifelse(cyclone + anomhot + anomppt + anomcold + wf + snowfall > 1, 1, 0)) %>% 
  filter(i_multi_sw == 1)
nrow(cty_days_w_po_multi_sw_2.5) # 741 county days

po_sw_multi_2.5 <-
  read_csv(paste0(
    path_data_processed,
    "multi_po_sw_2.5.csv"
  )) %>%
  filter(n_reliable_years_available == 3) %>%
  group_by(fips) %>%
  filter(row_number() == 1) %>%
  select(clean_state_name, clean_county_name, fips, multi_sw) %>% 
  left_join(., denom_electrical_customers)
nrow(po_sw_multi_2.5) #452 counties
sum(po_sw_multi_2.5$customers_served_total) # 36108354 electrical customers


# po 4.0
cty_days_w_po_multi_sw_4.0 <- cty_days_w_po_ind_sw_4.0 %>% 
  mutate(i_multi_sw = ifelse(cyclone + anomhot + anomppt + anomcold + wf + snowfall > 1, 1, 0)) %>% 
  filter(i_multi_sw == 1)
nrow(cty_days_w_po_multi_sw_4.0) # 607 county days

po_sw_multi_4.0 <-
  read_csv(paste0(
    path_data_processed,
    "multi_po_sw_4.0.csv"
  )) %>%
  filter(n_reliable_years_available == 3) %>%
  group_by(fips) %>%
  filter(row_number() == 1) %>%
  select(clean_state_name, clean_county_name, fips, multi_sw) %>% 
  left_join(., denom_electrical_customers)
nrow(po_sw_multi_4.0) #380 counties
sum(po_sw_multi_4.0$customers_served_total) # 30261229 electrical customers


# number of counties and severe weather events -------------------------------------
n_cty_w_sw <- function(input_name, output_name) {
  # Number of counties with individual severe weather event
  input_data <- read_csv(paste0(path_data_processed, input_name, ".csv"))
  
  n_counties_affected_by_sw <- input_data %>% 
    group_by(fips) %>% 
    mutate(i_cyc = ifelse(sum(tot_days_cyc) > 0, 1, 0),
           i_anomhot = ifelse(sum(tot_days_anomhot) > 0, 1, 0),
           i_anomppt = ifelse(sum(tot_days_anomppt) > 0, 1, 0),
           i_anomcold = ifelse(sum(tot_days_anomcold) > 0, 1, 0),
           i_anomwf = ifelse(sum(tot_days_wf) > 0, 1, 0),
           i_snowfall = ifelse(sum(tot_days_snowfall) > 0, 1, 0)) %>% 
    select(fips, starts_with("i_")) %>% 
    filter(row_number() == 1) 
  
  # Create table of above
  n_fips_w_cyc <- nrow(n_counties_affected_by_sw %>% filter(i_cyc == 1))
  n_fips_w_anomhot <- nrow(n_counties_affected_by_sw %>% filter(i_anomhot == 1))
  n_fips_w_anomppt <- nrow(n_counties_affected_by_sw %>% filter(i_anomppt == 1))
  n_fips_w_anomcold <- nrow(n_counties_affected_by_sw %>% filter(i_anomcold == 1))
  n_fips_w_anomwf <- nrow(n_counties_affected_by_sw %>% filter(i_anomwf == 1))
  n_fips_w_snowfall <- nrow(n_counties_affected_by_sw %>% filter(i_snowfall == 1))
  
  # Create a table with sw_type and n_counties_ind
  sw_type <- c("Tropical Cyclone", "Anomalous Precipitation", "Anomalous Hot", "Anomalous Cold", "Wildfire", "Snowfall")
  n_counties_ind <- c(n_fips_w_cyc, n_fips_w_anomhot, n_fips_w_anomppt, n_fips_w_anomcold, n_fips_w_anomwf, n_fips_w_snowfall)
  
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
    select(sw_type, n_pct)
  
  # Save the table to a CSV file
  write_csv(table_ind, paste0(path_tables, output_name, ".csv"))
}

# fix this to be all 3 years of data and reliability without power outage considerations
n_cty_w_sw("cty_tot_po_sw_0.1", "table_counties_w_ind_sw_0.1")
n_cty_w_sw("cty_tot_po_sw_2.5", "table_counties_w_ind_sw_2.5")
n_cty_w_sw("cty_tot_po_sw_4.0", "table_counties_w_ind_sw_4.0")

# state-level number of counties with severe weather event -----------------------------------
# ind - 0.1%
n_counties_state_ind <- cty_days_w_po_ind_sw_0.1 %>%
  group_by(fips) %>% 
  filter(row_number() == 1) %>% 
  ungroup() %>% 
  group_by(clean_state_name) %>%
  mutate(n_counties_ind = n()) %>%
  filter(row_number() == 1) %>%
  select(clean_state_name, n_counties_ind)

# multi - 0.1%
# 45 states have at least one county with multiple simultaneous severe weather events
n_counties_state_multi <- cty_days_w_po_multi_sw_0.1 %>%
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
# can check at least 80% here
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
         n_pct_counties_multi = paste0(n_counties_multi, " (", pct_counties_multi, ")")) 

n_counties_state_sw <- n_counties_state_sw %>% 
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
cty_tot_po_sw_0.1 <- read_csv(paste0(path_data_processed, "cty_tot_po_sw_0.1.csv"))

top_ind_sw_state <- cty_tot_po_sw_0.1 %>% 
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
po_sw_multi <- read_csv(paste0(path_data_processed, "multi_po_sw_0.1.csv"))

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
# anomalous precipitation-anomalous heat affect 1,155 county days and 40 states
multi_sw_tot_county_days <- read_csv(paste0(path_data_processed, "multi_po_sw_0.1.csv")) %>%
  select(multi_sw, clean_state_name) %>%
  group_by(multi_sw) %>%
  mutate(n_county_days = n(),
         n_states_affected = n_distinct(clean_state_name)) %>%
  filter(row_number() == 1) %>%
  select(-clean_state_name) %>% 
  arrange(desc(n_county_days))
multi_sw_tot_county_days

# additional estimates ----------------------------------------------------

# how many counties were affected by ppt and outages
cty_days_sw_po_0.1 <- read_csv(paste0(path_data_processed, "/cty_days_sw_po_0.1.csv"))

cty_days_sw_po_0.1 %>% 
  filter(i_po == 1 & tot_days_anomppt > 0) %>% 
  group_by(fips) %>% 
  pull() %>% 
  length() #1170 counties with po and anomalous precipitation



# which state have the most diversity of multi sw
# ca, ga
n_multi_sw <- read_csv(paste0(path_data_processed, "/multi_po_sw_0.1.csv")) %>% 
  group_by(clean_state_name, multi_sw) %>%
  filter(row_number() == 1) %>% 
  group_by(state = clean_state_name) %>%
  mutate(n_multi_sw = n()) %>% 
  filter(row_number() == 1) %>% 
  select(clean_state_name, n_multi_sw)
  
# which states exp wildfire multi 
n_multi_sw <- read_csv(paste0(path_data_processed, "/multi_po_sw_0.1.csv")) %>% 
  select(clean_state_name, multi_sw) %>% 
  filter(str_detect(multi_sw, "wf")) %>% 
  group_by(clean_state_name, multi_sw) %>%
  filter(row_number() == 1) %>% 
  filter(multi_sw != "anomcoldsnowfall") 
length(unique(n_multi_sw$clean_state_name)) # 9 states

n_multi_sw <- read_csv(paste0(path_data_processed, "/multi_po_sw_0.1.csv")) %>% 
  select(clean_state_name, multi_sw) %>% 
  filter(str_detect(multi_sw, "wf")) %>% 
  filter(multi_sw != "anomcoldsnowfall") 
nrow(n_multi_sw) #26

# most common triple occurrence
n_multi_sw <- read_csv(paste0(path_data_processed, "/multi_po_sw_0.1.csv")) %>% 
  select(clean_state_name, multi_sw) 
table(n_multi_sw$multi_sw)
n_multi_sw <- n_multi_sw %>% 
  filter(multi_sw == "cycloneanomhotanomppt")
table(n_multi_sw$clean_state_name)

  


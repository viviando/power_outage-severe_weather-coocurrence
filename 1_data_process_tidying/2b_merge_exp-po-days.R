#* Vivian Do
#* 09/26/24
#* Goal - Merge datasets of exposed power outages with different %cust thresholds into one
#* Include pc and n_reliable_years_available from previously generated data, which has info on county-day reliability

# Libraries ---------------------------------------------------------------
library(tidyverse)
library(here)
library(lubridate)
library(countytimezones)
library(fst)

path_data_processed <- "/Users/vivian/Desktop/0_PhD/0_Research Projects/power_outage-severe_weather-coocurrence/analysis/data/3_processed/"
path_data_ext_drive <- "/Volumes/My Passport Vivian/pous_updated/data_proc/data/"

# Read and light prep --------------------------------------------------------------------

# power outage data with different thresholds
po8_pct_0.1 <- read_fst(paste0(path_data_processed, "po8_pct_0-1.fst")) %>% 
  select(contains("name"), fips, day, i_exposed) %>% 
  mutate(i_exposed = ifelse(is.na(i_exposed), 0, i_exposed)) %>% 
  rename(i_exp_0.1 = i_exposed)
po8_pct_2.5 <- read_fst(paste0(path_data_processed, "po8_pct_2-5.fst")) %>% 
  select(contains("name"), fips, day, i_exposed) %>% 
  mutate(i_exposed = ifelse(is.na(i_exposed), 0, i_exposed)) %>% 
  rename(i_exp_2.5 = i_exposed)
po8_pct_4.0 <- read_fst(paste0(path_data_processed, "po8_pct_4-0.fst")) %>% 
  select(contains("name"), fips, day, i_exposed) %>% 
  mutate(i_exposed = ifelse(is.na(i_exposed), 0, i_exposed)) %>% 
  rename(i_exp_4.0 = i_exposed)

# reliability metrics (pc, n_reliable_years_available) for county-days
reli_metrics <- read_rds(paste0(path_data_ext_drive, "days_exposed_unexposed_expansion.RDS")) %>% 
  select(fips, day, pc, n_reliable_years_available)

# Merge -------------------------------------------------------------
# join the above by fips, day
po8_pct_all <- full_join(po8_pct_0.1, po8_pct_2.5, by = c("clean_state_name", "clean_county_name", "fips", "day")) %>% 
  full_join(., po8_pct_4.0, by = c("clean_state_name", "clean_county_name", "fips", "day"))

# join po data with reliability metrics
po8_pct_all <- left_join(po8_pct_all, reli_metrics, by = c("fips", "day"))

# Save --------------------------------------------------------------------
write_fst(po8_pct_all, paste0(path_data_processed, "po8_pct_all.fst"))

# Check number of county days with po at various thresholds ------------------------------------------------------------------
test <- po8_pct_all %>% 
  filter(pc >= 0.5,
         n_reliable_years_available == 3,
         !clean_state_name %in% c("alaska", "hawaii"))
  
po_vars <- c("i_exp_0.1", "i_exp_2.5", "i_exp_4.0")

for (po_var in po_vars) {
  total_po_days <- sum(test[[po_var]], na.rm = TRUE)
  total_days <- nrow(test)
  percentage_po_days <- (total_po_days / total_days) * 100
  
  cat(sprintf("Total number of power outage days for %s: %d (%.2f%%)\n", po_var, total_po_days, percentage_po_days))
}

nrow(test)


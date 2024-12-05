#* Vivian Do
#* 09/24/24
#* Goal - Evaluate higher percentiles of % customers out
#* To identify severe power outages in addition to small-scale outages at 0.1%

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(here)
library(lubridate)
library(countytimezones)

options(scipen = 999)

# paths
path_pous_data <- "/Volumes/My Passport Vivian/pous_updated/data_proc/data/"

# Read --------------------------------------------------------------------
counties <- read_rds(paste0(path_pous_data, "data_with_coverage_exclusions_sample.RDS"))

# Evaluate percentile distribution  --------------------------------------------------------------------
# must first filter to 
# (1) counties with pc > 50%
# (2) counties with >0 customers without power
counties_cleaned <- counties %>%
  filter(!is.na(pc),
         pc > 0.5,
         customers_out_total > 0,
         customers_served_total > 0) %>% 
  mutate(pct = (customers_out_total/customers_served_total)*100)

quantile(counties_cleaned$pct, probs = (0.9)) #0.1% threshold

# check different quanrtiles at 92.5, 95, 97.5, 99 at the same time
quantile(counties_cleaned$pct, c(0.9, 0.925, 0.95, 0.985, 0.99), na.rm = TRUE)/100

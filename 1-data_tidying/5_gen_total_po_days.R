# Code description --------------------------------------------------------
#* Author: Vivian
#* Date: 06/12/2024 
#* Goal: read in and clean sw_po_events.fst data, aggregating to county (get # days per county)
#* Get the total number of power outages days (regardless of severe weather presence)
#* Note that to save data space on computer, the raw po and sw data are on external hard drive
#* For figure 3 (and possibly others)

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
# limit to 3 years of reliable data 
po8_sw <- left_join(po8, sw) %>% 
  rename(i_po8 = exposed) %>% 
  filter(pc >= 0.5,
         n_reliable_years_available == 3) %>% 
  select(-lightning, nth_po_day)


# produce data ------------------------------------------------------------
#* data should be reliable counties with all days regardless of severe weather or power outage
write_csv(po8_sw, paste0(path_data, "processed/reliable_counties_w_all_days_regardless_of_sw_po.csv"))


# eval monthly dist of po ---------------------------------------------------------

# generate the followingL
# tot_po_only, monthly sum of i_po8
# tot_po_sw, monthly sum of i_po8 AND any severe weather variable == 1 like cyclone
test <- po8_sw %>% 
  mutate(i_sw = ifelse(cyclone + anomhot + anomcold + anomppt + snowfall + wf > 0, 1, 0),
         i_po8_sw = ifelse(i_po8 == 1 & i_sw == 1, 1, 0)) %>%
  group_by(month = month(day)) %>% 
  mutate(tot_po8 = sum(i_po8, na.rm = TRUE),
         tot_po8_sw = sum(i_po8_sw, na.rm = TRUE)) %>% 
  select(month, tot_po8, tot_po8_sw) %>%
  filter(row_number() == 1)

# Subtract tot_po8_sw from tot_po8
test <- test %>%
  mutate(tot_po8 = tot_po8 - tot_po8_sw)

# Reshape the data to long format
test_long <- test %>%
  pivot_longer(cols = c(tot_po8, tot_po8_sw), names_to = "total_type", values_to = "value")

# Create the bar plot
ggplot(data = test_long, aes(x = as.factor(month), y = value, fill = total_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("tot_po8" = "black", "tot_po8_sw" = "red"),
                    labels = c("tot_po8" = "Power outage only", "tot_po8_sw" = "Power outage +\n severe weather")) +
  scale_x_discrete(labels = c("1" = "January", "2" = "February", "3" = "March", "4" = "April", "5" = "May", 
                              "6" = "June", "7" = "July", "8" = "August", "9" = "September", "10" = "October", 
                              "11" = "November", "12" = "December")) +
  labs(title = "",
       x = "Month",
       y = "Total county-days",
       fill = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))





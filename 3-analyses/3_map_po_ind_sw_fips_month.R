# Code description --------------------------------------------------------
#* Author: Vivian
#* Date: 04/05/2024
#* Goal: create maps of po and cumulative severe weather as scatterplot
#* I have data that is number of days in a county co-occurring with a severe weather event
#* Aggregate this to a scatterplot illustrating counties with most burden

# set up ------------------------------------------------------------------
rm(list=ls(all=TRUE))
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
library(geofacet)
# hrbrthemes::import_roboto_condensed()
library(hrbrthemes)
library(GGally)
library(viridis)

path_data <- "/Users/vivian/Desktop/0_PhD/0_Research Projects/spatial_po_severe_weather/project code/data/"
path_map <- "/Users/vivian/Desktop/0_PhD/0_Research Projects/spatial_po_severe_weather/project code/output/figures/"

# read in data ------------------------------------------------------------
po8_county_monthly <- read_csv(paste0(path_data, "processed/po8_county_monthly.csv")) 
glimpse(po8_county_monthly)

reliable_counties_w_all_days_regardless_of_sw_po <- read_csv(paste0(path_data, "processed/reliable_counties_w_all_days_regardless_of_sw_po.csv"))
glimpse(reliable_counties_w_all_days_regardless_of_sw_po)

# data wrangling ----------------------------------------------------------
# severe weather data to create the bubble plot
po8_county_monthly_bubbleplot <- po8_county_monthly %>% 
  mutate_at(vars(starts_with("tot_")), replace_na, replace = 0) %>% 
  select(fips, day, starts_with("tot")) %>% 
  mutate(month = month(day)) %>% 
  group_by(fips, month) %>%
  mutate(tot_days_cyc_per_month = sum(tot_days_cyc),
         tot_days_anomppt_per_month = sum(tot_days_anomppt),
         tot_days_anomhot_per_month = sum(tot_days_anomhot),
         tot_days_anomcold_per_month = sum(tot_days_anomcold),
         tot_days_wf_per_month = sum(tot_days_wf),
         tot_days_snowfall_per_month = sum(tot_days_snowfall)) %>% 
  filter(row_number() == 1) %>%
  mutate(i_fips_w_cyc = ifelse(tot_days_cyc_per_month > 0, 1, 0),
         i_fips_w_anomppt = ifelse(tot_days_anomppt_per_month > 0, 1, 0),
         i_fips_w_anomhot = ifelse(tot_days_anomhot_per_month > 0, 1, 0),
         i_fips_w_anomcold = ifelse(tot_days_anomcold_per_month > 0, 1, 0),
         i_fips_w_wf = ifelse(tot_days_wf_per_month > 0, 1, 0),
         i_fips_w_snowfall = ifelse(tot_days_snowfall_per_month > 0, 1, 0)) %>%
  group_by(month, tot_days_cyc_per_month) %>% 
  mutate(n_fips_per_month_w_cyc = sum(i_fips_w_cyc)) %>% 
  group_by(month, tot_days_anomppt_per_month) %>%
  mutate(n_fips_per_month_w_anomppt = sum(i_fips_w_anomppt)) %>%
  group_by(month, tot_days_anomhot_per_month) %>%
  mutate(n_fips_per_month_w_anomhot = sum(i_fips_w_anomhot)) %>%
  group_by(month, tot_days_anomcold_per_month) %>%
  mutate(n_fips_per_month_w_anomcold = sum(i_fips_w_anomcold)) %>%
  group_by(month, tot_days_wf_per_month) %>%
  mutate(n_fips_per_month_w_wf = sum(i_fips_w_wf)) %>%
  group_by(month, tot_days_snowfall_per_month) %>%
  mutate(n_fips_per_month_w_snowfall = sum(i_fips_w_snowfall)) %>% 
  ungroup() %>% 
  select(fips, month, contains("per_month")) 


# create data for bubbleplot - tot zips by sw type, faceted by month ------------------------------------------------
# x axis = severe weather type
# y axis = total days of sw
# size = number of counties with sw
# color = severe weather type

##### cyc ---------------------------------------------------------------
tot_days_n_fips_cyc <- po8_county_monthly_bubbleplot %>% 
  select(month, contains("cyc")) %>% 
  group_by_all() %>% 
  filter(row_number() == 1) %>% 
  rename_with(~"tot_days", starts_with("tot_days")) %>% 
  rename_with(~"n_fips", starts_with("n_fips")) %>% 
  mutate(sw = "Cyclone")

##### anomppt ---------------------------------------------------------------
tot_days_n_fips_anomppt <- po8_county_monthly_bubbleplot %>% 
  select(month, contains("ppt")) %>% 
  group_by_all() %>% 
  filter(row_number() == 1) %>% 
  rename_with(~"tot_days", starts_with("tot_days")) %>% 
  rename_with(~"n_fips", starts_with("n_fips")) %>% 
  mutate(sw = "Anomalous Precipitation")

##### anomhot ---------------------------------------------------------------
tot_days_n_fips_anomhot <- po8_county_monthly_bubbleplot %>% 
  select(month, contains("hot")) %>% 
  group_by_all() %>% 
  filter(row_number() == 1) %>% 
  rename_with(~"tot_days", starts_with("tot_days")) %>% 
  rename_with(~"n_fips", starts_with("n_fips")) %>% 
  mutate(sw = "Anomalous Heat")

##### anomcold ---------------------------------------------------------------
tot_days_n_fips_anomcold <- po8_county_monthly_bubbleplot %>% 
  select(month, contains("cold")) %>% 
  group_by_all() %>% 
  filter(row_number() == 1) %>% 
  rename_with(~"tot_days", starts_with("tot_days")) %>% 
  rename_with(~"n_fips", starts_with("n_fips")) %>% 
  mutate(sw = "Anomalous Cold")

##### wf ---------------------------------------------------------------
tot_days_n_fips_wf <- po8_county_monthly_bubbleplot %>% 
  select(month, contains(c("wf_", "_wf"))) %>% 
  group_by_all() %>% 
  filter(row_number() == 1) %>% 
  rename_with(~"tot_days", starts_with("tot_days")) %>% 
  rename_with(~"n_fips", starts_with("n_fips")) %>% 
  mutate(sw = "Wildfire")

##### snowfall ---------------------------------------------------------------
tot_days_n_fips_snowfall <- po8_county_monthly_bubbleplot %>% 
  select(month, contains("snowfall")) %>% 
  group_by_all() %>% 
  filter(row_number() == 1) %>% 
  rename_with(~"tot_days", starts_with("tot_days")) %>% 
  rename_with(~"n_fips", starts_with("n_fips")) %>% 
  mutate(sw = "Snowfall")

##### power outage only data ---------------------------------------------------------------
# tot_days_n_fips_po_only <- reliable_counties_w_all_days_regardless_of_sw_po %>% 
#   select(fips, day, i_po8) %>%
#   group_by(fips, month = month(day)) %>% 
#   mutate(tot_days = sum(i_po8, na.rm = TRUE)) %>% 
#   filter(row_number() == 1) %>% 
#   select(fips, month, tot_days) %>% 
#   group_by(month, tot_days) %>%
#   mutate(n_fips = n()) %>% 
#   filter(row_number() == 1) %>% 
#   select(-fips) %>% 
#   mutate(sw = "po_only")

##### stack all tot_days_n_fips ---------------------------------------------------------------
tot_days_n_fips <-
  bind_rows(
    tot_days_n_fips_cyc,
    tot_days_n_fips_anomppt,
    tot_days_n_fips_anomhot,
    tot_days_n_fips_anomcold,
    tot_days_n_fips_wf,
    tot_days_n_fips_snowfall #,
    # tot_days_n_fips_po_only
  ) %>% 
  filter(tot_days > 0) 

# rename months from digits to full month names
tot_days_n_fips$month <- factor(tot_days_n_fips$month, levels = 1:12, labels = month.name)

# create bubbleplot - tot zips by sw type, faceted by month ------------------------------------------------
# x = severe weather type
# y = total days of sw
# size = n_fips
# faceted by month

ggplot(tot_days_n_fips) +
  geom_point(aes(
    x = sw,
    y = tot_days,
    size = n_fips,
    color = sw
  )) +
  scale_x_discrete(labels = c(
    "Cyclone" = "Cyclone",
    "Snowfall" = "Snowfall",
    "Anomalous Heat" = "Anomalous\nHeat",
    "Anomalous Cold" = "Anomalous\nCold",
    "Anomalous Precipitation" = "Anomalous\nPrecipitation",
    "Wildfire" = "Wildfire"
  )) +
  scale_color_manual(values = c(
    "Cyclone" = met.brewer("Archambault", 5)[3],
    "Snowfall" = met.brewer("Johnson", 6)[5],
    "Anomalous Heat" = met.brewer("Johnson", 6)[1],
    "Anomalous Cold" = met.brewer("Johnson", 6)[6],
    "Anomalous Precipitation" = met.brewer("Archambault", 5)[2],
    "Wildfire" = met.brewer("Johnson", 12)[5]
  )) +
  scale_y_continuous(breaks = c(1, 5, 10, 15)) +  
  scale_size(breaks = c(1, 50, 100, 150, 200, 250, 300, 350, 400)) +
  theme_bw() +
  xlab("") +
  ylab("Total days") +
  labs(size = "Total counties",
       color = "Severe weather type") +
  theme(text = element_text(size = 16),  # Increase text size
        axis.text.x = element_text(size = 16, angle = 90, hjust = 0.5, vjust = 0.5)) +  
  guides(color = guide_legend(override.aes = list(size = 6))) + # increase size of severe weather type legend
  facet_wrap( ~ month) 

# save po + sw co-occurrence --------------------------------------------------------------------
ggsave(paste0(path_map, "bubble_monthly_po_sw_tot_days_n_zip.png"), width = 15, height = 10, dpi = 300)

# create separate plot of only power outage county days -------------------
tot_days_n_fips_po_only <- reliable_counties_w_all_days_regardless_of_sw_po %>%
  select(fips, day, i_po8) %>%
  group_by(fips, month = month(day)) %>%
  mutate(tot_days = sum(i_po8, na.rm = TRUE)) %>%
  filter(row_number() == 1) %>%
  select(fips, month, tot_days) %>%
  group_by(month, tot_days) %>%
  mutate(n_fips = n()) %>%
  filter(row_number() == 1) %>%
  select(-fips) %>%
  mutate(sw = "Power outage only") %>% 
  filter(tot_days > 0)

# rename months from digits to full month names
tot_days_n_fips_po_only$month <- factor(tot_days_n_fips_po_only$month, levels = 1:12, labels = month.name)

# create plot
ggplot(tot_days_n_fips_po_only) +
  geom_point(aes(
    x = month,
    y = tot_days,
    size = n_fips
  )) +
  scale_color_manual(values = c(
    "Power outage only" = "grey"
  )) +
  scale_y_continuous(breaks = c(1, 10, 20, 30, 40, 50, 60, 70, 80)) +
  scale_size(breaks = c(1, 50, 100, 150, 200, 250, 300, 350, 400)) +
  theme_bw() +
  xlab("") +
  ylab("Total days") +
  labs(size = "Total counties") +
  theme(text = element_text(size = 16),  # Increase text size
        axis.text.x = element_text(size = 16, angle = 90, hjust = 0.5, vjust = 0.5)) +  
  guides(color = guide_legend(override.aes = list(size = 6))) 

# save po only co-occurrence --------------------------------------------------------------------
ggsave(paste0(path_map, "bubble_monthly_po_only_tot_days_n_zip.png"), width = 15, height = 10, dpi = 300)





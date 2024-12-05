# Code description --------------------------------------------------------
#* Author: Vivian
#* Date: 09/19/2024
#* Goal: create maps of po and cumulative severe weather as scatterplot by US census region and season
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

path_data_raw <- "/Users/vivian/Desktop/0_PhD/0_Research Projects/power_outage-severe_weather-coocurrence/analysis/data/1_raw/"
path_data_processed <- "/Users/vivian/Desktop/0_PhD/0_Research Projects/power_outage-severe_weather-coocurrence/analysis/data/3_processed/"
path_map <- "/Users/vivian/Desktop/0_PhD/0_Research Projects/power_outage-severe_weather-coocurrence/analysis/output/figures/"

# read in data ------------------------------------------------------------
po8_county_monthly <- read_csv(paste0(path_data_processed, "cty_tot_po_sw_2.5.csv"))

reliable_counties_w_all_days_regardless_of_sw_po <- read_fst(paste0(path_data_processed, "po8_pct_all.fst")) %>% 
  filter(pc >= 0.5,
         n_reliable_years_available == 3)

glimpse(reliable_counties_w_all_days_regardless_of_sw_po)

# data wrangling ----------------------------------------------------------
# identify us census regions
us_census_region <- list(
  "northeast" = c("connecticut", "maine", "massachusetts", "newhampshire", "rhodeisland", "vermont", 
                  "newjersey", "newyork", "pennsylvania"),
  
  "midwest" = c("illinois", "indiana", "michigan", "ohio", "wisconsin", 
                "iowa", "kansas", "minnesota", "missouri", "nebraska", "northdakota", "southdakota"),
  
  "south" = c("delaware", "florida", "georgia", "maryland", "northcarolina", "southcarolina", 
              "virginia", "westvirginia", "districtofcolumbia", "alabama", "kentucky", 
              "mississippi", "tennessee", "arkansas", "louisiana", "oklahoma", "texas"),
  
  "west" = c("arizona", "colorado", "idaho", "montana", "nevada", "newmexico", "utah", "wyoming", 
             "alaska", "california", "hawaii", "oregon", "washington")
)

# heat season is defined by climate central (May-September)
# https://www.climatecentral.org/climate-matters/heat-season-power-outages
# cool season is complement of above
season <- list(
  "warm" = c(5, 6, 7, 8, 9),
  "cool" = c(10, 11, 12, 1, 2, 3, 4)
)

# if a state is in one of the us_census_regions list, then assign the region to the state
po8_county_monthly <- po8_county_monthly %>%
  mutate(
    us_census_region = case_when(
      clean_state_name %in% us_census_region$northeast ~ "Northeast",
      clean_state_name %in% us_census_region$midwest ~ "Midwest",
      clean_state_name %in% us_census_region$south ~ "South",
      clean_state_name %in% us_census_region$west ~ "West",
      TRUE ~ "other"
    ),
    season = case_when(
      month %in% season$warm ~ "Warm",
      month %in% season$cool ~ "Cool",
      TRUE ~ "other"
    )
  )

# severe weather data to create the bubble plot
# aggregate to us census regions as well
po8_county_season_bubbleplot <- po8_county_monthly %>% 
  mutate_at(vars(starts_with("tot_")), replace_na, replace = 0) %>% 
  select(fips, day, season, us_census_region, starts_with("tot")) %>% 
  group_by(fips, season, us_census_region) %>%
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
  group_by(season, us_census_region, tot_days_cyc_per_month) %>% 
  mutate(n_fips_per_month_w_cyc = sum(i_fips_w_cyc)) %>% 
  group_by(season, us_census_region, tot_days_anomppt_per_month) %>%
  mutate(n_fips_per_month_w_anomppt = sum(i_fips_w_anomppt)) %>%
  group_by(season, us_census_region, tot_days_anomhot_per_month) %>%
  mutate(n_fips_per_month_w_anomhot = sum(i_fips_w_anomhot)) %>%
  group_by(season, us_census_region, tot_days_anomcold_per_month) %>%
  mutate(n_fips_per_month_w_anomcold = sum(i_fips_w_anomcold)) %>%
  group_by(season, us_census_region, tot_days_wf_per_month) %>%
  mutate(n_fips_per_month_w_wf = sum(i_fips_w_wf)) %>%
  group_by(season, us_census_region, tot_days_snowfall_per_month) %>%
  mutate(n_fips_per_month_w_snowfall = sum(i_fips_w_snowfall)) %>% 
  ungroup() %>% 
  select(season, us_census_region, contains("per_month")) 


# create data for bubbleplot - tot zips by sw type, faceted by month ------------------------------------------------
# x axis = severe weather type
# y axis = total days of sw
# size = number of counties with sw
# color = severe weather type

##### cyc ---------------------------------------------------------------
tot_days_n_fips_cyc <- po8_county_season_bubbleplot %>% 
  select(season, us_census_region, contains("cyc")) %>% 
  group_by_all() %>% 
  filter(row_number() == 1) %>% 
  rename_with(~"tot_days", starts_with("tot_days")) %>% 
  rename_with(~"n_fips", starts_with("n_fips")) %>% 
  mutate(sw = "Tropical Cyclone")

##### anomppt ---------------------------------------------------------------
tot_days_n_fips_anomppt <- po8_county_season_bubbleplot %>% 
  select(season, us_census_region, contains("ppt")) %>% 
  group_by_all() %>% 
  filter(row_number() == 1) %>% 
  rename_with(~"tot_days", starts_with("tot_days")) %>% 
  rename_with(~"n_fips", starts_with("n_fips")) %>% 
  mutate(sw = "Anomalous Precipitation")

##### anomhot ---------------------------------------------------------------
tot_days_n_fips_anomhot <- po8_county_season_bubbleplot %>% 
  select(season, us_census_region, contains("hot")) %>% 
  group_by_all() %>% 
  filter(row_number() == 1) %>% 
  rename_with(~"tot_days", starts_with("tot_days")) %>% 
  rename_with(~"n_fips", starts_with("n_fips")) %>% 
  mutate(sw = "Anomalous Heat")

##### anomcold ---------------------------------------------------------------
tot_days_n_fips_anomcold <- po8_county_season_bubbleplot %>% 
  select(season, us_census_region, contains("cold")) %>% 
  group_by_all() %>% 
  filter(row_number() == 1) %>% 
  rename_with(~"tot_days", starts_with("tot_days")) %>% 
  rename_with(~"n_fips", starts_with("n_fips")) %>% 
  mutate(sw = "Anomalous Cold")

##### wf ---------------------------------------------------------------
tot_days_n_fips_wf <- po8_county_season_bubbleplot %>% 
  select(season, us_census_region, contains(c("wf_", "_wf"))) %>% 
  group_by_all() %>% 
  filter(row_number() == 1) %>% 
  rename_with(~"tot_days", starts_with("tot_days")) %>% 
  rename_with(~"n_fips", starts_with("n_fips")) %>% 
  mutate(sw = "Wildfire")

##### snowfall ---------------------------------------------------------------
tot_days_n_fips_snowfall <- po8_county_season_bubbleplot %>% 
  select(season, us_census_region, contains("snowfall")) %>% 
  group_by_all() %>% 
  filter(row_number() == 1) %>% 
  rename_with(~"tot_days", starts_with("tot_days")) %>% 
  rename_with(~"n_fips", starts_with("n_fips")) %>% 
  mutate(sw = "Snowfall")

##### stack all tot_days_n_fips ---------------------------------------------------------------
tot_days_n_fips <-
  bind_rows(
    tot_days_n_fips_cyc,
    tot_days_n_fips_anomppt,
    tot_days_n_fips_anomhot,
    tot_days_n_fips_anomcold,
    tot_days_n_fips_wf,
    tot_days_n_fips_snowfall
  ) %>% 
  filter(tot_days > 0) 

# create bubbleplot - tot zips by sw type, faceted by season and us census region ------------------------------------------------
# x = severe weather type
# y = total days of sw
# size = n_fips
# faceted by season and us census region

ggplot(tot_days_n_fips) +
  geom_point(aes(
    x = sw,
    y = tot_days,
    size = n_fips,
    color = sw
  )) +
  scale_x_discrete(labels = c(
    "Tropical Cyclone" = "Tropical\nCyclone",
    "Snowfall" = "Snowfall",
    "Anomalous Heat" = "Anomalous\nHeat",
    "Anomalous Cold" = "Anomalous\nCold",
    "Anomalous Precipitation" = "Anomalous\nPrecipitation",
    "Wildfire" = "Wildfire"
  )) +
  scale_color_manual(values = c(
    "Tropical Cyclone" = met.brewer("Archambault", 5)[3],
    "Snowfall" = met.brewer("Johnson", 6)[5],
    "Anomalous Heat" = met.brewer("Johnson", 6)[1],
    "Anomalous Cold" = met.brewer("Johnson", 6)[6],
    "Anomalous Precipitation" = met.brewer("Archambault", 5)[2],
    "Wildfire" = met.brewer("Johnson", 12)[5]
  )) +
  # scale_y_continuous(breaks = seq(1, 60, by = 12)) +  
  scale_size(breaks = c(1, 50, 100, 150, 200, 250, 300, 350, 400)) +
  theme_bw() +
  xlab("") +
  ylab("Total days") +
  labs(size = "Total counties",
       color = "Severe weather type") +
  theme(text = element_text(size = 16),  # Increase text size
        axis.text.x = element_text(size = 16, angle = 90, hjust = 0.5, vjust = 0.5)) +  
  guides(color = guide_legend(override.aes = list(size = 6))) + # increase size of severe weather type legend
  facet_wrap(us_census_region ~ season, nrow = 4, ncol = 2)

# save po + sw co-occurrence --------------------------------------------------------------------
ggsave(paste0(path_map, "bubble_cty_tot_po_sw_census_region_2.5.png"), width = 15, height = 10, dpi = 300)



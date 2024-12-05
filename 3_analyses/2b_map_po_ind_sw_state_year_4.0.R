# Code description --------------------------------------------------------
#* Author: Vivian
#* Date: 04/02/2024
#* Goal: create maps of po and cumulative severe weather
#* Figure 2

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

path_data_raw <- "/Users/vivian/Desktop/0_PhD/0_Research Projects/power_outage-severe_weather-coocurrence/analysis/data/1_raw/"
path_data_processed <- "/Users/vivian/Desktop/0_PhD/0_Research Projects/power_outage-severe_weather-coocurrence/analysis/data/3_processed/"
path_map <- "/Users/vivian/Desktop/0_PhD/0_Research Projects/power_outage-severe_weather-coocurrence/analysis/output/figures/"

# read in data ------------------------------------------------------------
po8_sw_state_year <- read_csv(paste0(path_data_processed, "po8_sw_state_year_4.0.csv"))

glimpse(po8_sw_state_year)
po8_sw_state_year_test <- po8_sw_state_year %>% 
  filter(clean_state_name == "california")

# prep data for mapping ---------------------------------------------------

# convert from wide to long
po8_sw_state_year_test <- po8_sw_state_year %>% 
  pivot_longer(cols = starts_with("state"), names_to = "sw_type", values_to = "days") %>% 
  mutate(sw_type = str_remove(sw_type, "state_tot_days_")) 

# update statenames for geofacet
state_replacements <- c(
  "districtofcolumbia" = "district of columbia",
  "newhampshire" = "new hampshire",
  "newjersey" = "new jersey",
  "newmexico" = "new mexico",
  "newyork" = "new york",
  "northcarolina" = "north carolina",
  "northdakota" = "north dakota",
  "rhodeisland" = "rhode island",
  "southcarolina" = "south carolina",
  "southdakota" = "south dakota",
  "westvirginia" = "west virginia"
)

po8_sw_state_year_test <- po8_sw_state_year_test %>% 
  mutate(clean_state_name = str_replace_all(clean_state_name, state_replacements),
         clean_state_name = str_to_title(clean_state_name),
         clean_state_name = ifelse(clean_state_name == "District Of Columbia", "District of Columbia", clean_state_name))

# Produce map of proportions per year --------------------------------------------------------------
po8_sw_state_year_prop <- po8_sw_state_year_test %>% 
  group_by(clean_state_name, year) %>% 
  mutate(tot_days = sum(days),
         prop_days = days / tot_days) 

# replace District of Columbia with DC for map
us_state_grid1$name <- ifelse(us_state_grid1$name == "District of Columbia", "DC", us_state_grid1$name)
po8_sw_state_year_prop$clean_state_name <- ifelse(po8_sw_state_year_prop$clean_state_name == "District of Columbia", "DC", po8_sw_state_year_prop$clean_state_name)

met.brewer("Archambault", 5)[2]
met.brewer("Archambault", 5)[3]

# map
map_po_ind_sw_state_year_4.0 <- ggplot(po8_sw_state_year_prop, aes(x = year, y = prop_days, fill = sw_type)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(legend.position = "bottom",
        text = element_text(size = 16),
        legend.title = element_text(size = 22),
        legend.text = element_text(size = 22)) +
  labs(title = "",
       x = "",
       y = "") +
  scale_fill_manual(
    values = c(anomcold = met.brewer("Johnson", 6)[6],
               anomhot = met.brewer("Johnson", 6)[1],
               anomppt = met.brewer("Archambault", 5)[2],
               cyc = met.brewer("Archambault", 5)[3],
               snowfall = met.brewer("Johnson", 6)[5],
               wf = met.brewer("Johnson", 12)[5]),
    labels = c(
      "anomcold" = "Anomalous Cold",
      "anomhot" = "Anomalous Hot",
      "anomppt" = "Anomalous Precipitation",
      "cyc" = "Tropical Cyclone",
      "snowfall" = "Snowfall",
      "wf" = "Wildfire"
    ),
    name = "Severe weather type"
  ) +
  scale_x_continuous(breaks = seq(2018, 2020, 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_geo(~ clean_state_name, grid = us_state_grid1[c(-2,-11),])



# save --------------------------------------------------------------------
ggsave(paste0(path_map, "sfig2_map_po8_sw_state_year_4.0.tiff"), map_po_ind_sw_state_year_4.0, width = 15, height = 10, dpi = 300)


# Eval: get numbers from ppo8_sw_tot_days_singularo8_sw_state_year_prop ---------------------------------
sw_type_max_yearly <- po8_sw_state_year_prop %>% 
  group_by(clean_state_name, year) %>% 
  filter(prop_days == max(prop_days))

sw_type_max_yearly_2018 <- sw_type_max_yearly %>% 
  filter(year == 2018)

table(sw_type_max_yearly_2018$sw_type)/nrow(sw_type_max_yearly_2018)


# discussion - wildfire on west coast prop --------------------------------
prop_west_coast_wf <- po8_sw_state_year_prop %>% 
  filter(clean_state_name %in% c("California", "Oregon", "Washington"),
         sw_type == "wf")








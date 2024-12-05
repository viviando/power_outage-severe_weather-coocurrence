# Code description --------------------------------------------------------
#* Author: Vivian
#* Date: 05/06/2024
#* Goal: read in and clean sw_po_events.fst data
#* Note that to save data space on computer, the raw po and sw data are on external hard drive


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
library(tidycensus)
library(stringr)
library(PNWColors)

path_data_raw <- "/Users/vivian/Desktop/0_PhD/0_lead_research-projects/power_outage-severe_weather-coocurrence/analysis/data/1_raw/"
path_data_processed <- "/Users/vivian/Desktop/0_PhD/0_lead_research-projects/power_outage-severe_weather-coocurrence/analysis/data/3_processed/"
path_map <- "/Users/vivian/Desktop/0_PhD/0_lead_research-projects/power_outage-severe_weather-coocurrence/analysis/output/figures/"
path_public_data <- "/Users/vivian/Desktop/0_PhD/0_lead_research-projects/power_outage-severe_weather-coocurrence/analysis/output/public_data/"

# read in data ------------------------------------------------------------
po8_sw_multi_sw_state_cat_scatter <- read_csv(paste0(path_data_processed, "multi_po_sw_0.1.csv"))

# data wrangling ----------------------------------------------------------
po8_sw_multi_sw_state_cat_scatter <- po8_sw_multi_sw_state_cat_scatter %>% 
  group_by(clean_state_name, multi_sw) %>% 
  mutate(n_county_days = n()) %>%
  filter(row_number() == 1) %>% 
  select(clean_state_name, multi_sw, n_county_days)

po8_sw_multi_sw_state_cat_scatter <- po8_sw_multi_sw_state_cat_scatter %>% 
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

# re order levels so we have descending x axis multi_sw categories
multi_sw_order <- po8_sw_multi_sw_state_cat_scatter %>% 
  group_by(multi_sw) %>% 
  mutate(order_events = sum(n_county_days)) %>%
  filter(row_number() == 1) %>% 
  select(multi_sw, order_events) %>%
  arrange(desc(order_events))

po8_sw_multi_sw_state_cat_scatter$multi_sw <- fct_relevel(po8_sw_multi_sw_state_cat_scatter$multi_sw, rev(multi_sw_order$multi_sw[order(multi_sw_order$order_events)]))

# calculate the total number of county-days per severe weather combination (column totals)
tot_sw_all_states <- po8_sw_multi_sw_state_cat_scatter %>% 
  group_by(multi_sw) %>%
  mutate(clean_state_name = "All States",
         tot_sw_county_days = sum(n_county_days)) %>% 
  filter(row_number() == 1) %>% 
  select(-n_county_days) %>% 
  rename(n_county_days = tot_sw_county_days)

po8_sw_multi_sw_state_cat_scatter <- rbind(po8_sw_multi_sw_state_cat_scatter, tot_sw_all_states)

# calculate the total number of county-days of multi_sw events per state (row totals)
tot_sw_all_multi_sw <- po8_sw_multi_sw_state_cat_scatter %>% 
  group_by(clean_state_name) %>%
  mutate(multi_sw = "All Multi-SW",
         tot_sw_county_days = sum(n_county_days)) %>% 
  filter(row_number() == 1) %>% 
  select(-n_county_days) %>% 
  rename(n_county_days = tot_sw_county_days)

po8_sw_multi_sw_state_cat_scatter <- rbind(po8_sw_multi_sw_state_cat_scatter, tot_sw_all_multi_sw)

# Reorder the factor levels of multi_sw and clean_state_name
po8_sw_multi_sw_state_cat_scatter$multi_sw <- fct_relevel(po8_sw_multi_sw_state_cat_scatter$multi_sw, rev(multi_sw_order$multi_sw[order(multi_sw_order$order_events)]))
po8_sw_multi_sw_state_cat_scatter$multi_sw <- fct_relevel(po8_sw_multi_sw_state_cat_scatter$multi_sw, "All Multi-SW", after = 0L)
po8_sw_multi_sw_state_cat_scatter$clean_state_name <- fct_relevel(po8_sw_multi_sw_state_cat_scatter$clean_state_name, "All States", after = 0L)

levels(po8_sw_multi_sw_state_cat_scatter$multi_sw)
levels(po8_sw_multi_sw_state_cat_scatter$clean_state_name)

# create a categorical scatterplot such that
# each point is the size of the number of multi_sw events
# y axis is state
# x axis is multi_sw 
# include tot county days with sw per state (row totals) and tot county days with multi_sw per state (column totals))
# make an alternating grey color scheme for easier viewing 
po8_sw_multi_sw_state_cat_scatter %>% 
  mutate(row = as.numeric(as.factor(clean_state_name))) %>%
  ggplot(aes(y = clean_state_name, x = multi_sw)) +
  geom_tile(aes(y = row, fill = as.factor(row %% 2)), 
            width = Inf, height = 1, color = NA) +
  scale_fill_manual(values = c("grey90", "grey95"), guide = FALSE) +  # Remove this legend related to row colors
  geom_point(aes(size = n_county_days), alpha = 0.6, 
             data = . %>% filter(multi_sw != "All Multi-SW" & clean_state_name != "All States")) +
  geom_text(aes(label = n_county_days), data = . %>% filter(multi_sw == "All Multi-SW" | clean_state_name == "All States"), size = 4) +  # increase size of numbers
  scale_size(range = c(1, 10)) +
  scale_y_discrete(limits = rev(levels(po8_sw_multi_sw_state_cat_scatter$clean_state_name))) +
  scale_x_discrete(labels = c("cycloneanomppt" = "Tropical Cyclone,\nAnomalous\n Precipitation",
                              "anomcoldanomppt" = "Anomalous Cold,\nPrecipitation",
                              "anomhotanomppt" = "Anomalous Heat,\nPrecipitation",
                              "anomcoldsnowfall" = "Anomalous Cold,\nSnowfall",
                              "anompptwf" = "Wildfire,\nAnomalous\nPrecipitation",
                              "cycloneanomhotanomppt" = "Tropical Cyclone\nAnomalous\n Heat,\nPrecipitation",
                              "anomhotwf" = "Wildfire,\nAnomalous\nHeat",
                              "anomhotanompptwf" = "Wildfire,\nAnomalous\nHeat,\nPrecipitation",
                              "wfsnowfall" = "Wildfire,\nSnowfall",
                              "anomcoldwfsnowfall" = "Anomalous cold,\nWildfire,\nSnowfall",
                              "anomcoldwf" = "Wildfire,\nAnomalous\nCold",
                              "cycloneanomhot" = "Tropical Cyclone,\n Anomalous\n Heat",
                              "All Multi-SW" = "Any severe\nweather event"),
                   limits = (levels(po8_sw_multi_sw_state_cat_scatter$multi_sw))) +
  theme_minimal() +
  xlab("") +
  ylab("") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 14),
        axis.text.y = element_text(size = 14),
        text = element_text(size = 16),
        legend.position = "right") +
  scale_size(breaks = c(1, 25, 50, 75, 100, 125, 150)) +
  guides(size = guide_legend(title = "Total county-days"))

# save output for public data
write_fst(po8_sw_multi_sw_state_cat_scatter, paste0(path_public_data, "po8_sw_multi_sw_state_cat_scatter.fst"))

ggsave(paste0(path_map, "scatter_cty_tot_po_sw_0.1.png"), width = 15, height = 10, dpi = 300)








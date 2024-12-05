# Code description --------------------------------------------------------
#* Author: Vivian
#* Date: 03/20/2024
#* Goal: create maps of po and singular severe weather 
#* Figure 1

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
po8_sw_county <- read_csv(paste0(path_data_processed, "cty_days_sw_po_labeled_0.1.csv")) %>% 
  mutate(label_tot_days_cyc = as.character(label_tot_days_cyc)) %>% 
  filter(!is.na(label_tot_days_cyc)) # only keep study counties (if any label_tot_days_* is NA, then the rest are NA. Just chose cyclone)
glimpse(po8_sw_county)

# prep data for mapping ---------------------------------------------------

# US state map 2018, convert to albers equal area
us_states <- st_read(paste0(path_data_raw, "nhgis0049_state_2018/US_state_2018.shp")) %>% 
  filter(!STUSPS %in% c("AK", "HI", "PR"))
us_states <- st_transform(us_states, crs = 2163)

state_fip <- us_states %>% 
  select(STATEFP, STUSPS) %>% 
  st_drop_geometry()

# US counties map 2018, convert to albers equal area
us_counties <- st_read(paste0(path_data_raw, "nhgis0047_county_2018/US_county_2018.shp")) %>% 
  left_join(., state_fip, by = c("STATEFP")) %>% 
  filter(!STUSPS %in% c("AK", "HI", "PR"))
us_counties <- st_transform(us_counties, crs = 2163)

# join us_counties with po_sw data
# keep counties if they exist in our dataset
us_counties <- us_counties %>%
  right_join(
    .,
    po8_sw_county %>%
      select(fips, starts_with(c(
        "tot_days", "label_tot_days"
      ))) %>%
      st_drop_geometry(),
    by = c("GEOID" = "fips")
  )

# save output for public data
write_fst(
  us_counties %>% select(GEOID, contains("label")) %>% st_drop_geometry(),
  paste0(
    path_public_data,
    "us_counties_cty_days_sw_po_labeled_0.1.fst"
  )
)

# make maps by severe weather type ---------------------------------------------------------------

### cyclone ------------------
map_cyc <- ggplot() +
  geom_sf(
    data = us_states,
    fill = "white",
    color = "black",
    size = 0.1
  ) +
  geom_sf(
    data = us_counties,
    aes(fill = as.factor(label_tot_days_cyc)),
    color = "black",
    size = 0.1
  ) +
  scale_fill_manual(
    values = c(
      "0" = "lightgrey",
      "1" = viridisLite::viridis(6)[1],
      "2" = viridisLite::viridis(6)[2],
      "3" = viridisLite::viridis(6)[3],
      "4" = viridisLite::viridis(6)[4],
      "5" = viridisLite::viridis(6)[5],
      "6" = viridisLite::viridis(6)[6],
      "Not calculated" = "white"
    ),
    name = "Cumulative days"
  ) +
  theme_map() +
  coord_sf(crs = st_crs(2163)) +
  theme(legend.position = "none") +
  labs(x = "", y = "") +
  ggtitle("Tropical Cyclone") +
  theme(plot.title.position = "plot",  
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 14)) 
# map_cyc

### anomalous ppt ------------------
# format so legend is in order
us_counties$label_tot_days_anomppt <- factor(
  us_counties$label_tot_days_anomppt,
  levels = c("0", "1-5", "6-10", "11-15", "16-20", "21+", "Not calculated")
)
map_anomppt <- ggplot() +
  geom_sf(
    data = us_states,
    fill = "white",
    color = "black",
    size = 0.1
  ) +
  geom_sf(
    data = us_counties,
    aes(fill = as.factor(label_tot_days_anomppt)),
    color = "black",
    size = 0.1
  ) +
  scale_fill_manual(
    values = c(
      "0" = "lightgrey",
      "1-5" = viridisLite::plasma(5)[1],
      "6-10" = viridisLite::plasma(5)[2], 
      "11-15" = viridisLite::plasma(5)[3], 
      "16-20" = viridisLite::plasma(5)[4], 
      "21+" = viridisLite::plasma(5)[5], 
      "Not calculated" = "white"
    ),
    name = "Cumulative days"
  ) +
  theme_map() +
  coord_sf(crs = st_crs(2163)) +
  theme(legend.position = "none") +
  labs(x = "", y = "") +
  ggtitle("Anomalous Precipitation") +
  theme(plot.title.position = "plot",  
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 14)) 

# map_anomppt

### anomalous heat ------------------
# format so legend is in order
us_counties$label_tot_days_anomhot <- factor(
  us_counties$label_tot_days_anomhot,
  levels = c("0", "1-5", "6-10", "11-15", "16-20", "21+", "Not calculated")
)
map_anomheat <- ggplot() +
  geom_sf(
    data = us_states,
    fill = "white",
    color = "black",
    size = 0.1
  ) +
  geom_sf(
    data = us_counties,
    aes(fill = as.factor(label_tot_days_anomhot)),
    color = "black",
    size = 0.1
  ) +
  scale_fill_manual(
    values = c(
      "0" = "lightgrey",
      "1-5" = viridisLite::viridis(5)[1],
      "6-10" = viridisLite::viridis(5)[2],
      "11-15" = viridisLite::viridis(5)[3],
      "16-20" = viridisLite::viridis(5)[4],
      "21+" = viridisLite::viridis(5)[5],
      "Not calculated" = "white"
    ),
    name = "Cumulative days"
  ) +
  theme_map() +
  coord_sf(crs = st_crs(2163)) +
  theme(legend.position = "none") +
  labs(x = "", y = "") +
  ggtitle("Anomalous Heat") +
  theme(plot.title.position = "plot",  
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 14)) 

# map_anomheat

### anomalous cold ------------------
map_anomcold <- ggplot() +
  geom_sf(
    data = us_states,
    fill = "white",
    color = "black",
    size = 0.1
  ) +
  geom_sf(
    data = us_counties,
    aes(fill = as.factor(label_tot_days_anomcold)),
    color = "black",
    size = 0.1
  ) +
  scale_fill_manual(
    values = c(
      "0" = "lightgrey",
      "1" = viridisLite::plasma(6)[1],
      "2" = viridisLite::plasma(6)[2],
      "3" = viridisLite::plasma(6)[3],
      "4" = viridisLite::plasma(6)[4],
      "5" = viridisLite::plasma(6)[5],
      "6+" = viridisLite::plasma(6)[6],
      "Not calculated" = "white"
    ),
    name = "Cumulative days"
  ) +
  theme_map() +
  coord_sf(crs = st_crs(2163)) +
  theme(legend.position = "none") +
  labs(x = "", y = "") +
  ggtitle("Anomalous Cold") +
  theme(plot.title.position = "plot",  
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 14)) 

# map_anomcold

### wf ------------------
# format so legend is in order
map_wf <- ggplot() +
  geom_sf(
    data = us_states,
    fill = "white",
    color = "black",
    size = 0.1
  ) +
  geom_sf(
    data = us_counties,
    aes(fill = as.factor(label_tot_days_wf)),
    color = "black",
    size = 0.1
  ) +
  scale_fill_manual(
    values = c(
      "0" = "lightgrey",
      "1" = viridisLite::viridis(6)[1],
      "2" = viridisLite::viridis(6)[2],
      "3" = viridisLite::viridis(6)[3],
      "4" = viridisLite::viridis(6)[4],
      "5" = viridisLite::viridis(6)[5],
      "6+" = viridisLite::viridis(6)[6],
      "Not calculated" = "white"
    ),
    name = "Cumulative days"
  ) +
  theme_map() +
  coord_sf(crs = st_crs(2163)) +
  labs(x = "", y = "") +
  ggtitle("Wildfire") +
  theme(plot.title.position = "plot",  
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 14)) +
  theme(
    legend.position = "bottom",  # Position legend at bottom
    legend.direction = "horizontal",  # Make legend horizontal
    legend.justification = "center"  # Center the legend
  ) +
  guides(fill = guide_legend(nrow = 1, direction = "horizontal", order = 1))  

# map_wf

### snowfall ------------------
# format so legend is in order
us_counties$label_tot_days_snowfall <- factor(
  us_counties$label_tot_days_snowfall,
  levels = c("0", "1-5", "6-10", "11-15", "16-20", "21+", "Not calculated")
)
map_snowfall <- ggplot() +
  geom_sf(
    data = us_states,
    fill = "white",
    color = "black",
    size = 0.1
  ) +
  geom_sf(
    data = us_counties,
    aes(fill = as.factor(label_tot_days_snowfall)),
    color = "black",
    size = 0.1
  ) +
  scale_fill_manual(
    values = c(
      "0" = "lightgrey",
      "1-5" = viridisLite::plasma(5)[1],
      "6-10" = viridisLite::plasma(5)[2],
      "11-15" = viridisLite::plasma(5)[3],
      "16-20" = viridisLite::plasma(5)[4],
      "21+" = viridisLite::plasma(5)[5],
      "Not calculated" = "white"
    ),
    name = "Cumulative days"
  ) +
  theme_map() +
  coord_sf(crs = st_crs(2163)) +
  labs(x = "", y = "") +
  ggtitle("Snowfall") +
  theme(plot.title.position = "plot",  
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 14)) +
  theme(
    legend.position = "bottom",  # Position legend at bottom
    legend.direction = "horizontal",  # Make legend horizontal
    legend.justification = "center"  # Center the legend
  ) +
  guides(fill = guide_legend(nrow = 1, direction = "horizontal", order = 1))  

# map_snowfall

# put maps together -------------------------------------------------------
# put maps together in a 3x2 grid
maps <-
  (map_anomheat / map_cyc / map_wf) | 
  (map_anomcold / map_anomppt / map_snowfall) 

# maps

# save --------------------------------------------------------------------
ggsave(paste0(path_map, "fig1_maps_cty_days_sw_po_labeled_0.1.tiff"), width = 12, height = 8, dpi = 300)


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
# 
# map_po_ind_sw_state_year_0.1_4.0 <- map_po_ind_sw_state_year_0.1 / map_po_ind_sw_state_year_4.0 +
#   plot_annotation(tag_levels = 'A')
# ggsave(paste0(path_map, "map_po_ind_sw_state_year_0.1_4.0.png"), map_po_ind_sw_state_year_0.1_4.0, width = 20, height = 25, dpi = 300)
# 

library(magick)
library(ggplot2)
library(patchwork)
library(grid)
img1 <- image_read(paste0(path_map, "map_po8_sw_state_year_0.1.png"))
img2 <- image_read(paste0(path_map, "map_po8_sw_state_year_4.0.png"))

plot1 <- ggplot() + 
  annotation_custom(rasterGrob(img1), -Inf, Inf, -Inf, Inf) + 
  theme_void()

plot2 <- ggplot() + 
  annotation_custom(rasterGrob(img2), -Inf, Inf, -Inf, Inf) + 
  theme_void()

combined_plot <- plot1 / plot2 + plot_annotation(tag_levels = 'A')
combined_plot
ggsave(paste0(path_map, "map_po_ind_sw_state_year_0.1_4.0.png"), combined_plot, width = 20, height = 25, dpi = 300)

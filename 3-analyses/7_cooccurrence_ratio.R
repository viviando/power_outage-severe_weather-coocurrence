# Code description --------------------------------------------------------
#* Author: Vivian
#* Date: 7/1/2024
#* Goal: create co-occurrence ratio for
#* (1) individual severe weather
#* (2) multiple simultaneous severe weather

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

path_data <-
  "/Users/vivian/Desktop/0_PhD/0_Research Projects/spatial_po_severe_weather/project code/data/"
path_map <-
  "/Users/vivian/Desktop/0_PhD/0_Research Projects/spatial_po_severe_weather/project code/output/figures/"
path_tables <-
  "/Users/vivian/Desktop/0_PhD/0_Research Projects/spatial_po_severe_weather/project code/output/tables/"

# read in data ------------------------------------------------------------
#* data should be reliable counties with all days regardless of severe weather or power outage
all_po8_sw <-
  read_csv(
    paste0(
      path_data,
      "processed/reliable_counties_w_all_days_regardless_of_sw_po.csv"
    )
  )


# Co-occurrence ratio for single severe weather ---------------------------
##### data prep ---------------------------------------------------------------
# create new variable as indicator for any sw
all_po8_sw_single <- all_po8_sw %>%
  mutate(i_any_sw = ifelse(cyclone + anomhot + anomcold + anomppt + snowfall + wf > 0, 1, 0),
         no_sw = ifelse(i_any_sw == 0, 1, 0))

nrow(all_po8_sw)
##### calculate co-occurrence ratio -------------------------------------------
#* do this for each weather type
#* numerator: total number of county days with power outage and sw_i
#* denominator: total number of county days with sw_i

# set up loop with severe weather types
sw_types <-
  c("cyclone", "anomhot", "anomcold", "anomppt", "snowfall", "wf", "no_sw")
ratios <- data.frame()

# Use a loop to iterate over the severe weather types
for (i in seq_along(sw_types)) {
  # numerator
  numerator <- all_po8_sw_single %>%
    filter(i_po8 == 1 & get(sw_types[i]) == 1) %>%
    nrow()
  
  # denominator
  denominator <- all_po8_sw_single %>%
    filter(get(sw_types[i]) == 1) %>%
    nrow()
  
  # ratio
  ratio <- numerator / denominator
  
  # calculate proportion of county days (sw county-days/all county-days)
  sw_prop = denominator/nrow(all_po8_sw)
  
  # update the ratios data frame
  ratios <-
    rbind(ratios,
          data.frame(
            sw_type = sw_types[i],
            sw_freq = denominator,
            sw_prop = sw_prop,
            ratio = ratio
          ))
}

ratios

# clean up table
ratios <- ratios %>%
  mutate(
    sw_type = case_when(
      sw_type == "cyclone" ~ "Cyclone",
      sw_type == "anomhot" ~ "Anomalous Heat",
      sw_type == "anomcold" ~ "Anomalous Cold",
      sw_type == "anomppt" ~ "Anomalous Precipitation",
      sw_type == "snowfall" ~ "Snowfall",
      sw_type == "wf" ~ "Wildfire",
      sw_type == "no_sw" ~ "No severe weather"
    )
  ) %>%
 # Use factor to control the order, placing "No severe weather" last
  mutate(sw_type = factor(sw_type, levels = c("Cyclone", "Anomalous Heat", "Anomalous Cold", "Anomalous Precipitation", "Snowfall", "Wildfire", "No severe weather"))) %>%
  arrange(sw_type) %>%
  mutate(
    ratio = round(ratio, 2),
    sw_prop = round(sw_prop * 100, 1),
    sw_freq_prop = paste0(sw_freq, " (", sw_prop, "%)")
  ) %>%
  select(sw_type, sw_freq_prop, ratio) %>%
  rename(
    "Severe weather type" = sw_type,
    "Count of county-days with severe weather type (%)" = sw_freq_prop,
    "Probability of 8+ hour power outage given severe weather type" = ratio
  )

# save
write_csv(ratios, paste0(path_tables, "table_po_sw_ratio_single.csv"))

# Co-occurrence ratio for multiple simultaneous severe weather ------------
##### data prep ----
sw_types <-
  c("cyclone", "anomhot", "anomcold", "anomppt", "wf", "snowfall")

all_po8_sw_multiple <- all_po8_sw %>%
  mutate(i_any_sw = ifelse(cyclone + anomhot + anomcold + anomppt + snowfall + wf > 0, 1, 0),
         no_sw = ifelse(i_any_sw == 0, 1, 0)) %>%
  mutate(tot_ind_sw = rowSums(select(., all_of(sw_types)))) %>%
  # filter(tot_ind_sw > 1) %>%
  mutate(across(all_of(sw_types), ~ ifelse(. == 1, as.character(sw_types[which(sw_types == cur_column())]), ""), .names = "{.col}")) %>%
  mutate(multi_sw = paste(cyclone, anomhot, anomcold, anomppt, wf, snowfall, sep = ""),
         multi_sw = ifelse(no_sw == 1, "no_sw", multi_sw))

##### calculate co-occurrence ratio -------------------------------------------
#* do this for each weather type
#* numerator: total number of county days with power outage and multiple sw_i
#* denominator: total number of county days with multiple sw_i

# set up loop with severe weather types, removing single weather types
sw_types <- c(na.omit(setdiff(unique(all_po8_sw_multiple$multi_sw), c("", "NANANANANANA", "anomcold", "anomhot", "anomppt", "cyclone", "snowfall", "wf"))))
ratios <- data.frame()

# Use a loop to iterate over the severe weather types
for (i in seq_along(sw_types)) {
  print(sw_types[i])
  
  # numerator
  numerator <- all_po8_sw_multiple %>%
    filter(i_po8 == 1 & multi_sw == sw_types[i]) %>%
    nrow()
  
  # denominator
  denominator <- all_po8_sw_multiple %>%
    filter(multi_sw == sw_types[i]) %>%
    nrow()
  
  # ratio
  ratio <- numerator / denominator
  
  # update the ratios data frame
  ratios <-
    rbind(ratios,
          data.frame(
            sw_type = sw_types[i],
            sw_freq = denominator,
            sw_prop = denominator/nrow(all_po8_sw),
            ratio = ratio
          ))
}

ratios

unique(ratios$sw_type)
# clean up table
ratios <- ratios %>%
  mutate(
    sw_type = case_when(
      sw_type == "anomcoldsnowfall" ~ "Anomalous cold-snowfall",
      sw_type == "cycloneanomppt" ~ "Anomalous precipitation-cyclone",
      sw_type == "anomhotanomppt" ~ "Anomalous heat-anomalous precipitation",
      sw_type == "cycloneanomhotanomppt" ~ "Anomalous heat-anomalous precipitation-cyclone",
      sw_type == "anompptwf" ~ "Anomalous precipitation-wildfire",
      sw_type == "anomhotwf" ~ "Anomalous heat-wildfire",
      sw_type == "anomcoldanomppt" ~ "Anomalous cold-anomalous precipitation",
      sw_type == "anomcoldwf" ~ "Anomalous cold-wildfire",
      sw_type == "wfsnowfall" ~ "Snowfall-wildfire",
      sw_type == "anomhotanompptwf" ~ "Anomalous heat-anomalous precipitation",
      sw_type == "anomcoldwfsnowfall" ~ "Anomalous cold-snowfall-wildfire",
      sw_type == "cycloneanomhot" ~ "Anomalous heat-cyclone",
      sw_type == "cycloneanomhotwf" ~ "Anomalous heat-cyclone-wildfire",
      sw_type == "anomcoldanompptwf" ~ "Anomalous cold-anomalous precipitation-wildfire",
      sw_type == "no_sw" ~ "No severe weather"
    )
  ) %>%
  mutate(
    ratio = round(ratio, 2),
    sw_prop = round(sw_prop * 100, 1),
    sw_freq_prop = paste0(sw_freq, " (", sw_prop, "%)")
  ) %>%
  arrange(match(sw_type, c(unique(sw_type)[!unique(sw_type) == "No severe weather"], "No severe weather"))) %>%
  select(sw_type, sw_freq_prop, ratio) %>%
  rename(
    `Multiple simultaneous severe weather type` = sw_type,
    `Count of county-days with multiple simultaneous severe weather type (%)` = sw_freq_prop,
    `Probability of 8+ hour power outage given multiple simultaneous severe weather type` = ratio
  )

# save
write_csv(ratios,
          paste0(path_tables, "table_po_sw_ratio_multiple.csv"))

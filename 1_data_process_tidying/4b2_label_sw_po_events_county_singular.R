# Code description --------------------------------------------------------
#* Author: Vivian
#* Date: 03/20/2024
#* Goal: create and label quartiles of each severe weather type
#* Please note that I did have to play around with the cutpoints because sometimes quartiles did not 
#* make sense visually when displaying them on a map
#* For figure 1

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

path_data_raw <- "/Users/vivian/Desktop/0_PhD/0_Research Projects/power_outage-severe_weather-coocurrence/analysis/data/1_raw/"
path_data_processed <- "/Users/vivian/Desktop/0_PhD/0_Research Projects/power_outage-severe_weather-coocurrence/analysis/data/3_processed/"

# read in data ------------------------------------------------------------

# po sw data with different po thresholds
cty_days_sw_po_0.1 <- read_csv(paste0(path_data_processed, "/cty_days_sw_po_0.1.csv"))
cty_days_sw_po_2.5 <- read_csv(paste0(path_data_processed, "/cty_days_sw_po_2.5.csv"))
cty_days_sw_po_4.0 <- read_csv(paste0(path_data_processed, "/cty_days_sw_po_4.0.csv"))

# shapefile
us_county_shape_2020 <- st_read(paste0(path_data_raw, "nhgis0044_shapefile_tl2020_us_county_2020/US_county_2020.shp")) %>% 
  filter(!STATEFP %in% c("02", "15", "72", "66", "60", "69", "78")) %>% 
  rename(fips = GEOID)

# Helper functions --------------------------------------------------------
# function to join shape and po-sw data
join_shapefile <- function(po_sw_data) {
  us_county_shape_2020 %>%
    left_join(
      po_sw_data %>%
        select(fips, starts_with("tot")),
      by = "fips"
    )
}

# cyclone label
label_cyclone <- function(data) {
  data %>%
    mutate(
      label_tot_days_cyc = case_when(
        tot_days_cyc == 0 ~ "0",
        tot_days_cyc == 1 ~ "1",
        tot_days_cyc == 2 ~ "2",
        tot_days_cyc == 3 ~ "3",
        tot_days_cyc == 4 ~ "4",
        tot_days_cyc == 5 ~ "5",
        tot_days_cyc > 5 ~ "6"
      ),
      label_tot_days_cyc = as.character(label_tot_days_cyc)
    )
}

# anomalous precipitation label
label_anomppt <- function(data) {
  data %>%
    mutate(
      label_tot_days_anomppt = case_when(
        tot_days_anomppt == 0 ~ "0",
        tot_days_anomppt > 0 & tot_days_anomppt <= 5 ~ "1-5",
        tot_days_anomppt > 5 & tot_days_anomppt <= 10 ~ "6-10",
        tot_days_anomppt > 10 & tot_days_anomppt <= 15 ~ "11-15",
        tot_days_anomppt > 15 & tot_days_anomppt <= 20 ~ "16-20",
        tot_days_anomppt > 20 ~ "21+"
      ),
      label_tot_days_anomppt = as.character(label_tot_days_anomppt)
    )
}

# anomalous heat label
label_anomhot <- function(data) {
  data %>%
    mutate(
      label_tot_days_anomhot = case_when(
        tot_days_anomhot == 0 ~ "0",
        tot_days_anomhot > 0 & tot_days_anomhot <= 5 ~ "1-5",
        tot_days_anomhot > 5 & tot_days_anomhot <= 10 ~ "6-10",
        tot_days_anomhot > 10 & tot_days_anomhot <= 15 ~ "11-15",
        tot_days_anomhot > 15 & tot_days_anomhot <= 20 ~ "16-20",
        tot_days_anomhot > 20 ~ "21+"
      )
    )
}

# anomalous cold label
label_anomcold <- function(data) {
  data %>%
    mutate(
      label_tot_days_anomcold = case_when(
        tot_days_anomcold == 0 ~ "0",
        tot_days_anomcold == 1 ~ "1",
        tot_days_anomcold == 2 ~ "2",
        tot_days_anomcold == 3 ~ "3",
        tot_days_anomcold == 4 ~ "4",
        tot_days_anomcold == 5 ~ "5",
        tot_days_anomcold > 5 ~ "6+"
      ),
      label_tot_days_anomcold = as.character(label_tot_days_anomcold)
    )
}

# wildfire label
label_wf <- function(data) {
  data %>%
    mutate(
      label_tot_days_wf = case_when(
        tot_days_wf == 0 ~ "0",
        tot_days_wf == 1 ~ "1",
        tot_days_wf == 2 ~ "2",
        tot_days_wf == 3 ~ "3",
        tot_days_wf == 4 ~ "4",
        tot_days_wf == 5 ~ "5",
        tot_days_wf > 5 ~ "6+"
      ),
      label_tot_days_wf = as.character(label_tot_days_wf)
    )
}

# snowfall label
label_snowfall <- function(data) {
  data %>%
    mutate(
      label_tot_days_snowfall = case_when(
        tot_days_snowfall == 0 ~ "0",
        tot_days_snowfall > 0 & tot_days_snowfall <= 5 ~ "1-5",
        tot_days_snowfall > 5 & tot_days_snowfall <= 10 ~ "6-10",
        tot_days_snowfall > 10 & tot_days_snowfall <= 15 ~ "11-15",
        tot_days_snowfall > 15 & tot_days_snowfall <= 20 ~ "16-20",
        tot_days_snowfall > 20 ~ "21+"
      ),
      label_tot_days_snowfall = as.character(label_tot_days_snowfall)
    )
}


# Join po-sw with shapefile and label sw -------------------------------------
# store the datasets for loop
cty_days_sw_po <- list(cty_days_sw_po_0.1, cty_days_sw_po_2.5, cty_days_sw_po_4.0)

# start loop
cty_days_sw_po_labeled <- list()

for (i in seq_along(cty_days_sw_po)) {
  data <- cty_days_sw_po[[i]]
  data <- join_shapefile(data)
  data <- label_cyclone(data)
  data <- label_anomppt(data)
  data <- label_anomhot(data)
  data <- label_anomcold(data)
  data <- label_wf(data)
  data <- label_snowfall(data)
  cty_days_sw_po_labeled[[i]] <- data
}

write_csv(cty_days_sw_po_labeled[[1]], paste0(path_data_processed, "/cty_days_sw_po_labeled_0.1.csv"))
write_csv(cty_days_sw_po_labeled[[2]], paste0(path_data_processed, "/cty_days_sw_po_labeled_2.5.csv"))
write_csv(cty_days_sw_po_labeled[[3]], paste0(path_data_processed, "/cty_days_sw_po_labeled_4.0.csv"))



# Old ---------------------------------------------------------------------



# join shape and po-sw data -------------------------------------------------------------
# focus on counties with 3 years of data
po8_sw_county <- us_county_shape_2020 %>%
  left_join(
    .,
    po8_sw_tot_days_singular %>%
      select(fips, n_reliable_years_available, starts_with("tot")),
    by = ("fips")
  ) 

# label breaks -----------------------------------------------
# cyclone - label each category to be 0, 1, 2, 3, 4, 5, 6
po8_sw_county <- po8_sw_county %>%
  mutate(
    label_tot_days_cyc = case_when(
      tot_days_cyc == 0 ~ "0",
      tot_days_cyc == 1 ~ "1",
      tot_days_cyc == 2 ~ "2",
      tot_days_cyc == 3 ~ "3",
      tot_days_cyc == 4 ~ "4",
      tot_days_cyc == 5 ~ "5",
      tot_days_cyc > 5 ~ "6",
    ),
    label_tot_days_cyc = as.character(label_tot_days_cyc)
  )

# anomalous ppt - label each category: 0, 1-5, 6-10, 11-15, 16-20, 20+
po8_sw_county <- po8_sw_county %>%
  mutate(
    label_tot_days_anomppt = case_when(
      tot_days_anomppt == 0 ~ "0",
      tot_days_anomppt > 0 & tot_days_anomppt <= 5 ~ "1-5",
      tot_days_anomppt > 5 & tot_days_anomppt <= 10 ~ "6-10",
      tot_days_anomppt > 10 & tot_days_anomppt <= 15 ~ "11-15",
      tot_days_anomppt > 15 & tot_days_anomppt <= 20 ~ "16-20",
      tot_days_anomppt > 20 ~ "21+"
    ),
    label_tot_days_anomppt = as.character(label_tot_days_anomppt)
  )

table(po8_sw_county$label_tot_days_anomppt)

# anomlous hot - label each category: 0, 1-5, 6-10, 11-15, 16-20, 21+
po8_sw_county <- po8_sw_county %>%
  mutate(
    label_tot_days_anomhot = case_when(
      tot_days_anomhot == 0 ~ "0",
      tot_days_anomhot > 0 & tot_days_anomhot <= 5 ~ "1-5",
      tot_days_anomhot > 5 & tot_days_anomhot <= 10 ~ "6-10",
      tot_days_anomhot > 10 & tot_days_anomhot <= 15 ~ "11-15",
      tot_days_anomhot > 15 & tot_days_anomhot <= 20 ~ "16-20",
      tot_days_anomhot > 20 ~ "21+"
    ),
  )

table(po8_sw_county$label_tot_days_anomhot)

# anomalous cold - label each category to be 0, 1, 2, 3, 4, 5, 6+
po8_sw_county <- po8_sw_county %>%
  mutate(
    label_tot_days_anomcold = case_when(
      tot_days_anomcold == 0 ~ "0",
      tot_days_anomcold == 1 ~ "1",
      tot_days_anomcold == 2 ~ "2",
      tot_days_anomcold == 3 ~ "3",
      tot_days_anomcold == 4 ~ "4",
      tot_days_anomcold == 5 ~ "5",
      tot_days_anomcold > 5 ~ "6+"
    ),
    label_tot_days_anomcold = as.character(label_tot_days_anomcold)
  )

table(po8_sw_county$label_tot_days_anomcold)


# wildfire - label each category to be below: 0, 1, 2, 3, 4, 5, 6+
po8_sw_county <- po8_sw_county %>%
  mutate(
    label_tot_days_wf = case_when(
      tot_days_wf == 0 ~ "0",
      tot_days_wf == 1 ~ "1",
      tot_days_wf == 2 ~ "2",
      tot_days_wf == 3 ~ "3",
      tot_days_wf == 4 ~ "4",
      tot_days_wf == 5 ~ "5",
      tot_days_wf > 5 ~ "6+"
    ),
    label_tot_days_wf = as.character(label_tot_days_wf)
  )

table(po8_sw_county$label_tot_days_wf)


# snowfall - label each category to be 0, 1-5, 6-10, 11-15, 16-20, 21-27
po8_sw_county <- po8_sw_county %>%
  mutate(
    label_tot_days_snowfall = case_when(
      tot_days_snowfall == 0 ~ "0",
      tot_days_snowfall > 0 & tot_days_snowfall <= 5 ~ "1-5",
      tot_days_snowfall > 5 & tot_days_snowfall <= 10 ~ "6-10",
      tot_days_snowfall > 10 & tot_days_snowfall <= 15 ~ "11-15",
      tot_days_snowfall > 15 & tot_days_snowfall <= 20 ~ "16-20",
      tot_days_snowfall > 20 ~ "21+"
    ),
    label_tot_days_snowfall = as.character(label_tot_days_snowfall)
  )

table(po8_sw_county$label_tot_days_snowfall)


# save output
write_csv(po8_sw_county, paste0(path_data, "processed/po8_sw_county_w_labels.csv"))



# Check -------------------------------------------------------------------
label_quintiles <- function(df, var_name, label_name) {
  
  non_zero_values <- df[[var_name]][df[[var_name]] != 0]
  breaks <- quantile(non_zero_values, probs = seq(0, 1, 1/5), na.rm = TRUE)
  
  df <- df %>%
    mutate(!!label_name := case_when(
      .data[[var_name]] == 0 ~ "Q0",
      .data[[var_name]] > 0 & .data[[var_name]] <= breaks[2] ~ "Q1",
      .data[[var_name]] > breaks[2] & .data[[var_name]] <= breaks[3] ~ "Q2",
      .data[[var_name]] > breaks[3] & .data[[var_name]] <= breaks[4] ~ "Q3",
      .data[[var_name]] > breaks[4] & .data[[var_name]] <= breaks[5] ~ "Q4",
      .data[[var_name]] > breaks[5] ~ "Q5"
    ))
  
  return(df)
}

po8_sw_county <- label_quintiles(po8_sw_county, "tot_days_anomppt", "label_tot_days_anomppt")
po8_sw_county <- label_quintiles(po8_sw_county, "tot_days_anomhot", "label_tot_days_anomhot")
po8_sw_county <- label_quintiles(po8_sw_county, "tot_days_wf", "label_tot_days_wf")
po8_sw_county <- label_quintiles(po8_sw_county, "tot_days_snowfall", "label_tot_days_snowfall")

unique(po8_sw_county$label_tot_days_anomppt)
unique(po8_sw_county$label_tot_days_anomhot)
unique(po8_sw_county$label_tot_days_wf)
unique(po8_sw_county$label_tot_days_snowfall)

non_zero_values <- po8_sw_county$tot_days_anomppt[po8_sw_county$tot_days_anomppt != 0]
hist(non_zero_values, main = "Histogram of tot_days_anomppt excluding 0", xlab = "tot_days_anomppt", breaks = 20)
hist(po8_sw_county$tot_days_anomhot)
hist(po8_sw_county$tot_days_wf)
hist(po8_sw_county$tot_days_snowfall)

# evaluate distribution ---------------------------------------------------
unique(po8_sw_county$tot_days_cyc) # 6 categories --> have a label for each 
unique(po8_sw_county$tot_days_anomppt) #ok for quartiles
summary(po8_sw_county$tot_days_anomhot) #ok for quartiles
unique(po8_sw_county$tot_days_anomcold) # categories --> 1-5, 6-10, 11-15, 16-21
unique(po8_sw_county$tot_days_wf) # ok
unique(po8_sw_county$tot_days_snowfall) # only 3 quartiles
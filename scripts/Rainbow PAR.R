#Import libraries
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(lubridate)
library(stringr)


# Import data
tank_1 <- read.csv("controller 39 - Tank 1 - Photosynthetic Active Radiation (PAR).csv") %>% separate_wider_delim(cols = Time, delim = " ", names = c("date", "time")) %>% select(-PAR1m, -PAR5m)
tank_2 <- read.csv("controller 43 - Tank 2 - Photosynthetic Active Radiation (PAR).csv") %>% separate_wider_delim(cols = Time, delim = " ", names = c("date", "time")) %>% select(-PAR1m, -PAR5m)
tank_3 <- read.csv("controller 41 - Tank 3 - Photosynthetic Active Radiation (PAR).csv") %>% separate_wider_delim(cols = Time, delim = " ", names = c("date", "time")) %>% select(-PAR1m, -PAR5m)
tank_4 <- read.csv("controller 35 - Tank 4 - Photosynthetic Active Radiation (PAR).csv") %>% separate_wider_delim(cols = Time, delim = " ", names = c("date", "time")) %>% select(-PAR1m, -PAR5m)
tank_5 <- read.csv("controller 36 - Tank 5 - Photosynthetic Active Radiation (PAR).csv") %>% separate_wider_delim(cols = Time, delim = " ", names = c("date", "time")) %>% select(-PAR1m, -PAR5m)
tank_6 <- read.csv("controller 31 - Tank 6 - Photosynthetic Active Radiation (PAR).csv") %>% separate_wider_delim(cols = Time, delim = " ", names = c("date", "time")) %>% select(-PAR1m, -PAR5m)
tank_7 <- read.csv("controller 38 - Tank 7 - Photosynthetic Active Radiation (PAR).csv") %>% separate_wider_delim(cols = Time, delim = " ", names = c("date", "time")) %>% select(-PAR1m, -PAR5m)
tank_8 <- read.csv("controller 42 - Tank 8 - Photosynthetic Active Radiation (PAR).csv") %>% separate_wider_delim(cols = Time, delim = " ", names = c("date", "time")) %>% select(-PAR1m, -PAR5m)
tank_9 <- read.csv("controller 40 - Tank 9 - Photosynthetic Active Radiation (PAR).csv") %>% separate_wider_delim(cols = Time, delim = " ", names = c("date", "time")) %>% select(-PAR1m, -PAR5m)
tank_10 <- read.csv("controller 37 - Tank 10 - Photosynthetic Active Radiation (PAR).csv") %>% separate_wider_delim(cols = Time, delim = " ", names = c("date", "time")) %>% select(-PAR1m, -PAR5m)
tank_11 <- read.csv("controller 32 - Tank 11 - Photosynthetic Active Radiation (PAR).csv") %>% separate_wider_delim(cols = Time, delim = " ", names = c("date", "time")) %>% select(-PAR1m, -PAR5m)
tank_12 <- read.csv("controller 30 - Tank 12 - Photosynthetic Active Radiation (PAR).csv") %>% separate_wider_delim(cols = Time, delim = " ", names = c("date", "time")) %>% select(-PAR1m, -PAR5m)

# Make list of all dataframes
df_list <- list(tank_1, tank_2, tank_3, tank_4, tank_5, tank_6, tank_7, tank_8,
                tank_9, tank_10, tank_11, tank_12)

# Merge all dataframes sequentially by 2 columns
merged_df <- df_list %>% 
  reduce(left_join, by = c("date", "time"))

# Rename columns
merged_df <- merged_df %>% rename(
  tank_1 = PAR.x,
  tank_2 = PAR.y,
  tank_3 = PAR.x.x,
  tank_4 = PAR.y.y,
  tank_5 = PAR.x.x.x,
  tank_6 = PAR.y.y.y,
  tank_7 = PAR.x.x.x.x,
  tank_8 = PAR.y.y.y.y,
  tank_9 = PAR.x.x.x.x.x,
  tank_10 = PAR.y.y.y.y.y,
  tank_11 = PAR.x.x.x.x.x.x,
  tank_12 = PAR.y.y.y.y.y.y)

# Remove "PAR" from all columns
Rainbow_PAR_merged <- merged_df %>% 
  mutate(across(everything(), ~ str_remove_all(.x, "PAR")))

# Export new merged dataframe
write_csv(Rainbow_PAR_merged, "Rainbow_PAR_merged.csv")

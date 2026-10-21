#Import libraries
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(lubridate)


# Import data
tank_1 <- read.csv("controller 39 - Tank 1 - Temperature.csv") %>% separate_wider_delim(cols = Time, delim = " ", names = c("date", "time")) %>% select(-SP)
tank_2 <- read.csv("controller 43 - Tank 2 - Temperature.csv") %>% separate_wider_delim(cols = Time, delim = " ", names = c("date", "time")) %>% select(-SP)
tank_3 <- read.csv("controller 41 - Tank 3 - Temperature.csv") %>% separate_wider_delim(cols = Time, delim = " ", names = c("date", "time")) %>% select(-SP)
tank_4 <- read.csv("controller 35 - Tank 4 - Temperature.csv") %>% separate_wider_delim(cols = Time, delim = " ", names = c("date", "time")) %>% select(-SP)
tank_5 <- read.csv("controller 36 - Tank 5 - Temperature.csv") %>% separate_wider_delim(cols = Time, delim = " ", names = c("date", "time")) %>% select(-SP)
tank_6 <- read.csv("controller 31 - Tank 6 - Temperature.csv") %>% separate_wider_delim(cols = Time, delim = " ", names = c("date", "time")) %>% select(-SP)
tank_7 <- read.csv("controller 38 - Tank 7 - Temperature.csv") %>% separate_wider_delim(cols = Time, delim = " ", names = c("date", "time")) %>% select(-SP)
tank_8 <- read.csv("controller 42 - Tank 8 - Temperature.csv") %>% separate_wider_delim(cols = Time, delim = " ", names = c("date", "time")) %>% select(-SP)
tank_9 <- read.csv("controller 40 - Tank 9 - Temperature.csv") %>% separate_wider_delim(cols = Time, delim = " ", names = c("date", "time")) %>% select(-SP)
tank_10 <- read.csv("controller 37 - Tank 10 - Temperature.csv") %>% separate_wider_delim(cols = Time, delim = " ", names = c("date", "time")) %>% select(-SP)
tank_11 <- read.csv("controller 32 - Tank 11 - Temperature.csv") %>% separate_wider_delim(cols = Time, delim = " ", names = c("date", "time")) %>% select(-SP)
tank_12 <- read.csv("controller 30 - Tank 12 - Temperature.csv") %>% separate_wider_delim(cols = Time, delim = " ", names = c("date", "time")) %>% select(-SP)

# Make list of all dataframes
df_list <- list(tank_1, tank_2, tank_3, tank_4, tank_5, tank_6, tank_7, tank_8,
                tank_9, tank_10, tank_11, tank_12)

# Merge all dataframes sequentially by 2 columns
merged_df <- df_list %>% 
  reduce(left_join, by = c("date", "time"))

# Rename columns
merged_df <- merged_df %>% rename(
  tank_1 = PV.x,
  tank_2 = PV.y,
  tank_3 = PV.x.x,
  tank_4 = PV.y.y,
  tank_5 = PV.x.x.x,
  tank_6 = PV.y.y.y,
  tank_7 = PV.x.x.x.x,
  tank_8 = PV.y.y.y.y,
  tank_9 = PV.x.x.x.x.x,
  tank_10 = PV.y.y.y.y.y,
  tank_11 = PV.x.x.x.x.x.x,
  tank_12 = PV.y.y.y.y.y.y)


# Remove "degrees C" from all columns
Rainbow_PAR_merged <- merged_df %>%
  mutate(across(everything(), ~ str_remove_all(.x, "¬∞")))

# Export new merged dataframe
write_csv(Rainbow_PAR_merged, "Rainbow_Temp_merged.csv")


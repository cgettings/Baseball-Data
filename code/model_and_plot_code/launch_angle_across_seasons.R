###########################################-
###########################################-
##
## Launch angle across seasons ----
##
###########################################-
###########################################-

#=========================#
# Setting up ----
#=========================#

#-------------------------#
# Loading libraries ----
#-------------------------#

library(lubridate)
library(stringdist)
library(magrittr)
library(dbplyr)
library(DBI)
library(tibbletime)
library(glue)
library(tidyverse)

source("./code/functions/se_funs.R")

#--------------------------------#
# Connecting to database ----
#--------------------------------#

statcast_db <- dbConnect(RSQLite::SQLite(), "./data/statcast_db.sqlite3")

#--------------------------------#
# Pulling data from database
#--------------------------------#

statcast_data <- 
    tbl(statcast_db, "statcast_data_updated") %>% 
    filter(!is.na(events) & game_year %in% 2015:2018) %>% 
    select(game_date, 
           game_year,
           events,
           type,
           player_name,
           batter_id_sc,
           launch_speed_sc,
           launch_angle_sc,
           estimated_woba_using_speedangle) %>% 
    collect()

#--------------------------------#
# Pulling data from database ----
#--------------------------------#

hr_data <- 
    statcast_data %>%
    mutate(game_date = as_date(game_date)) %>% 
    arrange(game_date) %>% 
    mutate(
        HR_v_other_pa = case_when(
            events == "home_run" ~ 1L, 
            TRUE ~ 0L),
        HR_v_other_bb = case_when(
            events == "home_run" ~ 1L, 
            events != "home_run" & type == "X" ~ 0L,
            TRUE ~ NA_integer_)
    )


hr_data %>% 
    group_by(game_year, HR_v_other_pa) %>% 
    summarise(
        n = n(),
        launch_angle_sc = mean(launch_angle_sc, na.rm = TRUE)
        )

## BY WEEK ##

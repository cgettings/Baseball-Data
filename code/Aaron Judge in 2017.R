###########################################
###########################################
##
## Aaron Judge in 2017
##
###########################################
###########################################

#=========================#
#### Loading packages ####
#=========================#

library(readr)
library(dplyr)
library(tidyr)
library(dbplyr)
library(stringr)
library(lubridate)
library(stringr)
library(tibble)
library(magrittr)
library(DBI)
library(pryr)
library(ggplot2)

#=====================#
#### Loading Data ####
#=====================#

setwd("~/BASP/R analyses/Baseball Data/Data Files")

#--------------------------------#
# ---- Initializing database ----
#--------------------------------#

pitchRx_db <- dbConnect(RSQLite::SQLite(), "pitchRx_db.sqlite3")
statcast_db <- dbConnect(RSQLite::SQLite(), "statcast_db.sqlite3")

###===###===###===###===###===###===###===###===###===###
#
## Pulling out data ----
#
###===###===###===###===###===###===###===###===###===###


tbl(pitchRx_db, "player") %>% glimpse()

tbl(pitchRx_db, "player") %>% filter(last == "Judge" & team_abbrev == "NYY")
    
tbl(statcast_db, "statcast_data_updated") %>% glimpse()


judge_data <-
    tbl(statcast_db, "statcast_data_updated") %>%
    filter(batter_id_sc == 592450 & game_year == 2017) %>%
    select(game_date, type, events, babip_value, zone, plate_x, plate_z, sz_top, sz_bot) %>%
    collect() %>%
    # mutate(babip_value = as.factor(babip_value)) %>% 
    mutate(
        half_season = case_when(
            game_date < "2017-07-11" ~ "first",
            game_date > "2017-07-11" ~ "second"), 
        game_date = game_date %>% as_date(),
        game_month = game_date %>% month() %>% as.integer(),
        game_month_by_year_date = floor_date(game_date, "month"),
        game_month_by_year_chr = floor_date(game_date, "month") %>% str_sub(1, 7)        
    ) %>%
    arrange(game_date)

write_csv(judge_data, "judge_data.csv")


judge_data %>% 
    filter(type == "X") %>% 
    ggplot(aes(plate_x, plate_z, color = babip_value, group = half_season)) + 
    geom_point(aes(shape = babip_value), size = 2.5) +
    facet_grid(~ half_season) +
    theme_bw()


###===###===###===###===###===###===###===###===###===###
#
## Fooling around ----
#
###===###===###===###===###===###===###===###===###===###

judge_data %>% summarise(BA = mean(babip_value, na.rm = TRUE))

judge_data_2 <- judge_data %>% filter(!is.na(babip_value))

judge_data_2 %>% summarise(BA = mean(babip_value))




glm.1 <- glm(babip_value ~ 1, judge_data_2, family = "binomial")

summary(glm.1)

tidy(glm.1, exponentiate = TRUE)

exp(coef(glm.1)[[1]]) / (1 + exp(coef(glm.1)[[1]]))




glmer.1 <-
    glmer(
        babip_value ~ 1 + (1 | game_month_by_year_date),
        judge_data_2,
        family = "binomial")

summary(glmer.1)

tidy(glmer.1)

exp(coef(glmer.1)[[1]]) / (1 + exp(coef(glmer.1)[[1]]))
exp(fixef(glmer.1)[[1]]) / (1 + exp(fixef(glmer.1)[[1]]))



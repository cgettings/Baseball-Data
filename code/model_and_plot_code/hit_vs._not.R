###########################################-
###########################################-
##
## Hit vs. not (aka batting average) ----
##
###########################################-
###########################################-

#=========================================#
# Setting up ----
#=========================================#

#-----------------------------------------#
# Loading libraries ----
#-----------------------------------------#

library(lubridate)
library(stringdist)
library(magrittr)
library(dbplyr)
library(DBI)
library(tidyquant)
library(tibbletime)
library(glue)
library(tidyverse)
library(lme4)
library(pROC)
library(broom)

source("./code/functions/se_funs.R")

#-----------------------------------------#
# Connecting to database ----
#-----------------------------------------#

statcast_db <- dbConnect(RSQLite::SQLite(), "./data/statcast_db.sqlite3")

#-----------------------------------------#
# Pulling data from database ----
#-----------------------------------------#

hit_data <- 
    tbl(statcast_db, "statcast_data_updated") %>% 
    filter(!is.na(events) & game_type == "R") %>% 
    filter(game_year == 2017) %>% 
    select(
        game_date,
        game_year,
        game_pk,
        player_name,
        batter_id_sc,
        pitcher_id_sc,
        p_throws,
        at_bat_number,
        events,
        type,
        bb_type,
        description,
        inning,
        babip_value
    ) %>% 
    collect()


#-----------------------------------------#
# Cleaning data ----
#-----------------------------------------#

hit_data_2 <- 
    hit_data %>%
    mutate(
        game_date = as_date(game_date),
        p_throws = as.factor(p_throws)
        ) %>% 
    arrange(game_date) %>% 
    as_tbl_time(index = game_date) %>% 
    mutate(
        hit_vs_not = case_when(
            type == "B" | events == "sac_fly" | events == "sac_bunt" ~ NA_integer_,
            babip_value == 1 | events == "home_run" ~ 1L, 
            TRUE ~ 0L)
    )

contrasts(hit_data_2$p_throws) <- "contr.sdif"

hit_data_ja <- hit_data_2 %>% filter(batter_id_sc == 514888)


#=========================================#
# Calculating batting average ----
#=========================================#

#-----------------------------------------#
# Unweighted average ----
#-----------------------------------------#

hit_data_summary <- 
    hit_data_2 %>% 
    group_by(batter_id_sc) %>% 
    summarise(
        BA = mean(hit_vs_not, na.rm = TRUE), 
        n = n()
    ) %>% 
    filter(n > 10) %>% 
    arrange(-BA) %>% 
    left_join(
        ., 
        hit_data_2 %>% 
            select(batter_id_sc, player_name) %>% 
            distinct(batter_id_sc, .keep_all = TRUE)) %>% 
    select(player_name, everything())

### Jose Altuve ----

hit_data_summary %>% filter(player_name == "Jose Altuve")
hit_data_summary %>% filter(batter_id_sc == 514888)


hit_data_2 %>% 
    filter(batter_id_sc == 514888) %>% 
    summarise(
        BA = mean(hit_vs_not, na.rm = TRUE), 
        n = n()
    ) %>% 
    mutate(player_name = "Jose Altuve", batter_id_sc = 514888L) %>% 
    select(player_name, batter_id_sc, everything())



#-----------------------------------------#
# Average calculated by GLM ----
#-----------------------------------------#

ja.ba.glm.mod.1 <- 
    glm(
        hit_vs_not ~ 1, 
        family = binomial(link = "logit"), 
        data   = hit_data_ja)

ja.ba.glm.mod.1 %>% 
    tidy() %>% 
    mutate(BA = exp(estimate)/(1 + exp(estimate))) %>% 
    mutate_if(is.numeric, round, digits = 3)


#-----------------------------------------#
# Average calculated by mixed effects model ----
#-----------------------------------------#

ja.ba.lmer.mod.1 <- 
    glmer(
        hit_vs_not ~ 1 + (1 | game_date), 
        family = binomial(link = "logit"), 
        data   = hit_data_ja)

ja.ba.lmer.mod.1 %>% 
    tidy() %>% 
    mutate(BA = exp(estimate)/(1 + exp(estimate))) %>% 
    mutate_if(is.numeric, round, digits = 3)

plotREsim(REsim(ja.ba.lmer.mod.1, n.sims = 500, seed = 1), labs = FALSE)



ja.ba.lmer.mod.2 <- 
    glmer(
        hit_vs_not ~ p_throws + (1 | game_date), 
        family = binomial(link = "logit"), 
        data   = hit_data_ja)

ja.ba.lmer.mod.2 %>% 
    tidy() %>% 
    mutate(BA = exp(estimate)/(1 + exp(estimate))) %>% 
    mutate_if(is.numeric, round, digits = 3)

plotREsim(REsim(ja.ba.lmer.mod.2, n.sims = 500, seed = 1), labs = FALSE)



ja.ba.lmer.mod.3 <- 
    glmer(
        hit_vs_not ~ p_throws + (p_throws | game_date), 
        family = binomial(link = "logit"), 
        data   = hit_data_ja)

ja.ba.lmer.mod.3 %>% 
    tidy() %>% 
    mutate(BA = exp(estimate)/(1 + exp(estimate))) %>% 
    mutate_if(is.numeric, round, digits = 3)

plotREsim(REsim(ja.ba.lmer.mod.3, n.sims = 500, seed = 1), labs = FALSE)


anova(ja.ba.glm.mod.1, ja.ba.lmer.mod.1)
anova(ja.ba.lmer.mod.1, ja.ba.lmer.mod.2)
anova(ja.ba.lmer.mod.2, ja.ba.lmer.mod.3)
anova(ja.ba.lmer.mod.1, ja.ba.lmer.mod.3)


###########################################
###########################################
##
## babip
##
###########################################
###########################################

#=========================#
#### Loading packages ####
#=========================#


library(MASS)
library(readxl)
library(readr)
library(QuantPsyc)
library(dplyr)
library(dbplyr)
library(ggplot2)
library(cowplot)
library(tidyr)
library(visreg)
library(broom)
library(stringr)
library(lme4)
library(merTools)
library(viridis)
library(pitchRx)
library(lubridate)
library(pryr)
library(rstanarm)
library(pROC)
library(DBI)


source("~/BASP/R analyses/omegaFunctions.R")
source("~/BASP/R analyses/tidy.lmerTest.R")
source("~/BASP/R analyses/ICC_comp.R")
source("~/BASP/R analyses/se_funs.R")


#=====================#
#### Loading Data ####
#=====================#

# setwd("~/BASP/R analyses/Baseball Data/Data Files")
setwd("D:/Files/BASP/R analyses/Baseball Data/Data Files/")

#------------------------#
# ---- From database ----
#------------------------#

statcast_db <- dbConnect(RSQLite::SQLite(), "statcast_db.sqlite3")


#----------------------------------#
# ---- Selecting and filtering ----
#----------------------------------#


statcast_data <- 
    
    tbl(statcast_db, "statcast_data_updated") %>%
    
    select(
        game_date,
        game_year,
        game_type,
        babip_value,
        type,
        events,
        bb_type,
        game_type
        ) %>%
    
    filter(type == "X") %>%
    
    filter(game_type == "R") %>%
    
    collect() %>% 
    
    mutate(
        game_date = game_date %>% as_date(),
        game_month = game_date %>% month() %>% as.integer(),
        game_month_by_year_date = floor_date(game_date, "month"),
        game_month_by_year_chr = floor_date(game_date, "month") %>% str_sub(1, 7)
        ) %>% 
    
    arrange(desc(game_date))


#----------------------------------#
# ---- Selecting and filtering ----
#----------------------------------#

#### Hit distance ####

statcast_data %>% 
    # filter(game_type == "R") %>% 
    group_by(game_year, game_month) %>% 
    summarise(
        n = n(),
        babip = mean(babip_value)
        ) %>% 
    mutate_if(is.double, round, digits = 4) %>% 
    print(n = Inf)


statcast_data %>% 
    # filter(game_type == "R") %>%
    group_by(game_year) %>% 
    summarise(
        n = n(),
        babip = mean(babip_value)
        ) %>% 
    mutate_if(is.double, round, digits = 4) %>% 
    print(n = Inf)


statcast_data %>% 
    group_by(game_year) %>% 
    summarise(
        n = n(),
        babip = mean(babip_value)
        ) %>% 
    mutate_if(is.double, round, digits = 4) %>% 
    print(n = Inf)



#----------------------------------#
# ---- Fitting models ----
#----------------------------------#



glm.fit.1 <- glm(babip_value ~ 1, family = "binomial", statcast_data)

summary(glm.fit.1)

exp(coef(glm.fit.1)[[1]]) / (1 + exp(coef(glm.fit.1)[[1]]))




glmer.fit.1 <- 
    glmer(babip_value ~ 1 + (1 | game_year), 
        family = "binomial", 
        statcast_data,
        glmerControl(optimizer = "optim")
        )

summary(glmer.fit.1)

plotREsim(REsim(glmer.fit.1, n.sims = 1000, seed = 726), labs = TRUE)

exp(coef(glmer.fit.1)[[1]]) / (1 + exp(coef(glmer.fit.1)[[1]]))
exp(fixef(glmer.fit.1)[[1]]) / (1 + exp(fixef(glmer.fit.1)[[1]]))




fit.2 <- lmer(hit_distance_sc ~ 1 + (1 | game_month_by_year_chr), statcast_data)

summary(fit.2)

plotREsim(REsim(fit.2, n.sims = 1000, seed = 726), labs = TRUE)

fit.2.coef <-
    coef(fit.2)[[1]] %>% 
    as.data.frame(.) %>% 
    rownames_to_column() %>% 
    rename(year_month = rowname, distance = `(Intercept)`) %>% 
    # mutate(year_month = as.Date(year_month)) %>% 
    as_tibble() 

fit.2.coef %>% 
    ggplot(aes(x = year_month, y = distance)) + 
    geom_point() + 
    # geom_path() + 
    # scale_x_date(date_breaks = "1 month") +
    theme_bw()


#### Launch speed ####

statcast_data %>% 
    filter(game_type == "R") %>% 
    group_by(game_year, game_month) %>% 
    summarise(
        n = n(),
        speed = mean(launch_speed_sc),
        sd = sd(launch_speed_sc),
        se = se(launch_speed_sc)
        ) %>% 
    mutate_if(is.double, round, digits = 2)


statcast_data %>% 
    filter(game_type == "R") %>% 
    group_by(game_year) %>% 
    summarise(
        n = n(),
        speed = mean(launch_speed_sc),
        sd = sd(launch_speed_sc),
        se = se(launch_speed_sc)
        ) %>% 
    mutate_if(is.double, round, digits = 2)


statcast_data %>% 
    group_by(game_year) %>% 
    summarise(
        n = n(),
        speed = mean(launch_speed_sc),
        sd = sd(launch_speed_sc),
        se = se(launch_speed_sc)
        ) %>% 
    mutate_if(is.double, round, digits = 2)



fit.1 <- lmer(launch_speed_sc ~ 1 + (1 | game_year), statcast_data)

summary(fit.1)

plotREsim(REsim(fit.1, n.sims = 1000, seed = 726), labs = TRUE)



fit.2 <- lmer(launch_speed_sc ~ 1 + (1 | game_month_by_year_chr), statcast_data)

summary(fit.2)

plotREsim(REsim(fit.2, n.sims = 1000, seed = 726), labs = TRUE)

fit.2.coef <-
    coef(fit.2)[[1]] %>% 
    as.data.frame(.) %>% 
    rownames_to_column() %>% 
    rename(year_month = rowname, speed = `(Intercept)`) %>% 
    # mutate(year_month = as.Date(year_month)) %>% 
    as_tibble() 

fit.2.coef %>% 
    ggplot(aes(x = year_month, y = speed)) + 
    geom_point() + 
    # geom_path() + 
    # scale_x_date(date_breaks = "1 month") +
    theme_bw()


#### Launch angle ####

statcast_data %>% 
    filter(game_type == "R") %>% 
    group_by(game_year, game_month) %>% 
    summarise(
        n = n(),
        angle = mean(launch_angle_sc),
        sd = sd(launch_angle_sc),
        se = se(launch_angle_sc)
        ) %>% 
    mutate_if(is.double, round, digits = 2)


statcast_data %>% 
    filter(game_type == "R") %>% 
    group_by(game_year) %>% 
    summarise(
        n = n(),
        angle = mean(launch_angle_sc),
        sd = sd(launch_angle_sc),
        se = se(launch_angle_sc)
        ) %>% 
    mutate_if(is.double, round, digits = 2)


statcast_data %>% 
    group_by(game_year) %>% 
    summarise(
        n = n(),
        angle = mean(launch_angle_sc),
        sd = sd(launch_angle_sc),
        se = se(launch_angle_sc)
        ) %>% 
    mutate_if(is.double, round, digits = 2)



#----------------------------------#
# ---- Selecting and filtering ----
#----------------------------------#

statcast_data_1 <- 
    
    tbl(statcast_db, "statcast_data_updated") %>%
    
    select(
        game_date,
        game_year,
        game_type,
        launch_speed_sc,
        hit_distance_sc,
        launch_angle_sc,
        type,
        events,
        bb_type) %>%
    
    filter(type == "X" & !is.na(hit_distance_sc)) %>%
    
    collect() %>% 
    
    mutate(
        game_date = game_date %>% as_date(),
        game_month = game_date %>% month() %>% as.integer()
        ) %>% 
    
    arrange(desc(game_date))


#----------------------------------#
# ---- Selecting and filtering ----
#----------------------------------#




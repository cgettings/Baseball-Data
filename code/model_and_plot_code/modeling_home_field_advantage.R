###########################################
###########################################
##
## Modeling home field advantage
##
###########################################
###########################################

#=========================#
#### Loading packages ####
#=========================#

library(MASS)
library(car)
library(readr)
library(QuantPsyc)
library(dplyr)
library(ggplot2)
library(cowplot)
library(viridis)
library(visreg)
library(tidyr)
library(broom)
library(stringr)
library(lme4)
library(merTools)
library(lubridate)
library(pryr)
library(rstanarm)
library(pROC)
library(RLRsim)

setwd("~/BASP/R analyses/Baseball Data/Data Files")

#==========================#
#### Loading data ####
#==========================#

standings    <- read_rds("baseball_standings.RDS")
team_results_raw <- read_rds("team_results.RDS")


#--------------------------------------------#
#---- Long format ----
#--------------------------------------------#

standings_long <- 
    
    standings %>% 
    select(
        year, 
        year_date, 
        team, 
        league, 
        R, 
        RA, 
        Rdiff, 
        SOSx10, 
        SRSx10, 
        home_wins, 
        road_wins) %>%
    gather(
        key = "home_v_road",
        value = "wins",
        road_wins,
        home_wins,
        factor_key = TRUE
    ) %>% 
    mutate(
        home_v_road = recode_factor(
            home_v_road,
            "road_wins" = "road",
            "home_wins" = "home"
            )
    )


standings_long <- 
    
    standings %>% 
    select(
        year, 
        year_date, 
        team, 
        league, 
        home_losses, 
        road_losses) %>%
    gather(
        key = "home_v_road",
        value = "losses",
        road_losses,
        home_losses,
        factor_key = TRUE
    ) %>% 
    mutate(
        home_v_road = recode_factor(
            home_v_road,
            "road_losses" = "road",
            "home_losses" = "home"
            )
    ) %>% 
    left_join(standings_long, .)


standings_long <- 
    
    standings %>% 
    select(
        year,
        year_date, 
        team,
        league, 
        win_percent_home, 
        win_percent_road) %>%
    gather(
        key = "home_v_road",
        value = "win_percent",
        win_percent_road,
        win_percent_home,
        factor_key = TRUE
    ) %>% 
    mutate(
        home_v_road = recode_factor(
            home_v_road,
            "win_percent_road" = "road",
            "win_percent_home" = "home"
            )
    ) %>% 
    left_join(standings_long, .)


standings_long <- 
    standings_long %>% 
    mutate(league = as.factor(league))

# contrasts(standings_long$home_v_road) <- "contr.sdif"
contrasts(standings_long$home_v_road) <- "contr.treatment"

contrasts(standings_long$league) <- "contr.sdif"


contrasts(standings_long$home_v_road)
contrasts(standings_long$league)


#----------------------------------------------------------------------#
#---- Prepping team results ----
#----------------------------------------------------------------------#

team_results <-
    team_results_raw %>% 
    filter(win_loss != "T") %>% 
    mutate(
        win_loss = recode(win_loss, "W" = 1, "L" = 0),
        win_loss = as.integer(win_loss),
        home_v_road = as.factor(home_v_road)
        )

# contrasts(team_results$home_v_road) <- "contr.sdif"
contrasts(team_results$home_v_road) <- "contr.Treatment"

contrasts(team_results$home_v_road)


#----------------------------------------------------------------------#
#---- Adding season-level variables from standings to team_results ----
#----------------------------------------------------------------------#

team_results_standings <-
    
    left_join(
        team_results,
        standings %>%
            select(
                year,
                team,
                league,
                win_percent,
                SRS),
        by = c(
            "game_year" = "year", 
            "team")
    ) %>% 
    mutate(
        team      = as.factor(team),
        league    = as.factor(league),
        day_night = case_when(
            day_night == "" & game_year %in% 1900:1912 ~ "D",
            day_night == "" & !game_year %in% 1900:1912 ~ NA_character_,
            TRUE ~ identity(day_night)),
        day_night = as.factor(day_night),
        SRS       = as.integer(SRS)
        
    ) %>% 
    mutate(
        league      = `attr<-`(league, "var_level", "L2"),
        win_percent = `attr<-`(win_percent, "var_level", "L2"),
        SRS         = `attr<-`(SRS, "var_level", "L2")
    ) %>% 
    select(
        -end_inn,
        -record,
        -rank,
        -GB,
        -win,
        -loss,
        -save,
        -game_time,
        -attendance)


# contrasts(team_results_standings$home_v_road) <- "contr.sdif"
contrasts(team_results_standings$home_v_road) <- "contr.Treatment"
contrasts(team_results_standings$league) <- "contr.sdif"
contrasts(team_results_standings$day_night) <- "contr.sdif"

contrasts(team_results_standings$home_v_road)
contrasts(team_results_standings$league)
contrasts(team_results_standings$day_night)


#=============================#
#### Summarizing the data ####
#=============================#

## Overall ##

standings %>% 
    summarise(
        win_percent =      mean(win_percent),
        win_percent_home = mean(win_percent_home), 
        win_percent_road = mean(win_percent_road)
        ) %>% 
    mutate_if(is.double, round, digits = 4)


### By year ###

standings %>% 
    group_by(year) %>% 
    summarise(
        win_percent =      mean(win_percent),
        win_percent_home = mean(win_percent_home), 
        win_percent_road = mean(win_percent_road)
        ) %>% 
    mutate_if(is.double, round, digits = 3) %>% 
    arrange(-year) %>% 
    print(n = 30)


### By team ###

standings %>% 
    group_by(team) %>% 
    summarise(
        win_percent =      mean(win_percent),
        win_percent_home = mean(win_percent_home), 
        win_percent_road = mean(win_percent_road)
        ) %>% 
    mutate_if(is.double, round, digits = 3) %>% 
    arrange(team) %>% 
    print(n = 50)

standings %>% 
    group_by(year, team) %>% 
    summarise(
        win_percent =      mean(win_percent),
        win_percent_home = mean(win_percent_home), 
        win_percent_road = mean(win_percent_road)
        ) %>% 
    mutate_if(is.double, round, digits = 3) %>% 
    arrange(team)

standings %>% 
    group_by(team) %>% 
    summarise(
        wins =      mean(win),
        home_wins = mean(home_wins), 
        road_wins = mean(road_wins)
        ) %>% 
    mutate_if(is.double, round, digits = 3) %>% 
    arrange(team)


### By league ###

standings %>% 
    group_by(league) %>% 
    summarise(
        win_percent =      mean(win_percent),
        win_percent_home = mean(win_percent_home), 
        win_percent_road = mean(win_percent_road)
        ) %>% 
    mutate_if(is.double, round, digits = 3)

#--------------------------------------------#
#---- Covariates ----
#--------------------------------------------#

standings %>% 
    group_by(team) %>% 
    summarise(
        Rdiff = mean(Rdiff)
        ) %>% 
    mutate_if(is.double, round, digits = 3)


#=============================#
#### Models ####
#=============================#

# Intercept is estimated

Hw_Rw_mtx <- standings %>% select(home_wins, road_wins) %>% as.matrix()

Hw_Hl_mtx <- standings %>% select(home_wins, home_losses) %>% as.matrix()


#--------------------------------------------#
#---- Treating everything as independent ----
#--------------------------------------------#

glm.model.1 <- 
    glm(
        Hw_Rw_mtx ~ 1, 
        family = "binomial", 
        standings)


summary(glm.model.1)
tidy(glm.model.1) %>% mutate_if(is.double, round, digits = 3)
tidy(glm.model.1, exponentiate = TRUE) %>% mutate_if(is.double, round, digits = 3)

exp(coef(glm.model.1)[[1]]) / (1 + exp(coef(glm.model.1)[[1]]))


#--------------------------------------------#
#---- Random intercepts for year ----
#--------------------------------------------#

glmer.model.2 <- 
    glmer(
        Hw_Rw_mtx ~ 
            1 + 
            (1 | year),
        family = binomial("logit"),
        standings#,
        # glmerControl(optimizer = "optim")
        )

summary(glmer.model.2)

tidy(glmer.model.2) %>% mutate_if(is.double, round, digits = 3)

exp(coef(glmer.model.2)[[1]]) / (1 + exp(coef(glmer.model.2)[[1]]))
exp(fixef(glmer.model.2)[[1]]) / (1 + exp(fixef(glmer.model.2)[[1]]))

VarCorr(glmer.model.2)

plotREsim(REsim(glmer.model.2, n.sims = 1000, seed = 726), labs = TRUE)



#--------------------------------------------#
#---- Random intercepts for team ----
#--------------------------------------------#

glmer.model.3 <- 
    glmer(
        Hw_Hl_mtx ~ 
            1 + 
            (1 | team),
        family = "binomial",
        standings#,
        # glmerControl(optimizer = "optim")
        )

summary(glmer.model.3)

tidy(glmer.model.3) %>% mutate_if(is.double, round, digits = 3)

exp(coef(glmer.model.3)[[1]]) / (1 + exp(coef(glmer.model.3)[[1]]))
exp(fixef(glmer.model.3)[[1]]) / (1 + exp(fixef(glmer.model.3)[[1]]))
exp(ranef(glmer.model.3)[[1]]) / (1 + exp(ranef(glmer.model.3)[[1]]))

VarCorr(glmer.model.3)

plotREsim(REsim(glmer.model.3, n.sims = 1000, seed = 726), labs = TRUE)


#--------------------------------------------#
#---- Linear model for home % - road % ----
#--------------------------------------------#

lm.model.4 <- lm(HvR_percent ~ 1, standings)

summary(lm.model.4)
tidy(lm.model.4) %>% mutate_if(is.double, round, digits = 3)



#--------------------------------------------#
#---- Random intercepts for year ----
#--------------------------------------------#

lmer.model.5 <-
    lmer(
        HvR_percent ~ 1 + (1 | year),
        
        standings,
        REML = FALSE)


tidy(lmer.model.5) %>% mutate_if(is.double, round, digits = 4)

VarCorr(lmer.model.5) %>% as.data.frame() %>% mutate_if(is.double, round, digits = 5)

exactRLRT(lmer.model.5)

exactLRT(
    m =  lmer.model.5, 
    m0 = lm.model.4)

plotREsim(REsim(lmer.model.5))


#--------------------------------------------#
#---- Random intercepts for team ----
#--------------------------------------------#

lmer.model.6 <-
    lmer(
        HvR_percent ~ 1 + (1 | team),
        
        standings,
        REML = FALSE)


tidy(lmer.model.6) %>% mutate_if(is.double, round, digits = 4)

VarCorr(lmer.model.6) %>% as.data.frame() %>% mutate_if(is.double, round, digits = 5)

exactRLRT(lmer.model.6)

exactLRT(
    m =  lmer.model.6, 
    m0 = lm.model.4)

plotREsim(REsim(lmer.model.6))


#--------------------------------------------#
#---- Team nested withn year ----
#--------------------------------------------#

lmer.model.7 <-
    lmer(
        HvR_percent ~ 1 + (1 | year/team),
        
        standings,
        REML = FALSE)


tidy(lmer.model.7) %>% mutate_if(is.double, round, digits = 4)

VarCorr(lmer.model.7) %>% as.data.frame() %>% mutate_if(is.double, round, digits = 5)

exactRLRT(lmer.model.7)

exactLRT(
    m =  lmer.model.7, 
    m0 = lm.model.4)

plotREsim(REsim(lmer.model.7))


#--------------------------------------------#
#---- Team nested withn year ----
#--------------------------------------------#


lme.model.7 <-
    lme(
        fixed = HvR_percent ~ 1, 
        random = ~ 1 | year/team,
        
        standings,
        method = "ML")


summary(lme.model.7)

coef(lme.model.7)
fixef(lme.model.7)

ranef(lme.model.7) %>% 
    extract("year") %>% 
    as.data.frame() %>% 
    rownames_to_column() %>% 
    rename(year = rowname, intercept = X.Intercept.) %>% 
    as_tibble()

ranef(lme.model.7) %>% 
    extract("team") %>% 
    as.data.frame() %>% 
    rownames_to_column() %>% 
    rename(year_team = rowname, intercept = X.Intercept.) %>% 
    as_tibble()


year_ranef <-
    ranef(lme.model.7) %>% 
    extract("year") %>% 
    as.data.frame() %>% 
    rownames_to_column() %>% 
    rename(year = rowname, intercept = X.Intercept.) %>% 
    as_tibble()

year_team_ranef <-
    ranef(lme.model.7) %>% 
    extract("team") %>% 
    as.data.frame() %>% 
    rownames_to_column() %>% 
    rename(year_team = rowname, intercept = X.Intercept.) %>% 
    as_tibble()


year_ranef %>% 
    ggplot(aes(year, intercept)) + 
    geom_point() + 
    geom_hline(yintercept = 0) +
    theme(axis.text.x = element_text(angle = 45, vjust = .5, size = 8))

year_team_ranef %>% 
    ggplot(aes(year_team, intercept)) + 
    geom_point() + 
    geom_hline(yintercept = 0) +
    theme(axis.text.x = element_text(angle = 90, vjust = .5, size = 8))



#==============================#
#### Models with long data ####
#==============================#

# Intercept is estimated

WL_mtx <- standings_long %>% select(wins, losses) %>% as.matrix()



#--------------------------------------------#
#---- Treating everything as independent ----
#--------------------------------------------#

glm.model.8 <- 
    
    glm(
        WL_mtx ~
            home_v_road, 
        
        family = "binomial", 
        standings_long)


summary(glm.model.8)
tidy(glm.model.8) %>% mutate_if(is.double, round, digits = 3)
tidy(glm.model.8, exponentiate = TRUE) %>% mutate_if(is.double, round, digits = 3)

exp(coef(glm.model.8)) / (1 + exp(coef(glm.model.8)))


#--------------------------------------------#
#---- Random intercepts for year ----
#--------------------------------------------#

glmer.model.9 <- 
    
    glmer(
        WL_mtx ~
            home_v_road +
            (1 | year),
        family = binomial("logit"), 
        standings_long
        # glmerControl(optimizer = "Nelder_Mead")
        )

summary(glmer.model.9)

tidy(glmer.model.9) %>% mutate_if(is.double, round, digits = 3)

exp(fixef(glmer.model.9)) / (1 + exp(fixef(glmer.model.9)))
exp(coef(glmer.model.9)$year) / (1 + exp(coef(glmer.model.9)$year))
exp(ranef(glmer.model.9)$year) / (1 + exp(ranef(glmer.model.9)$year))

VarCorr(glmer.model.9)

plotREsim(REsim(glmer.model.9, n.sims = 1000, seed = 726), labs = TRUE)


coef.9 <- exp(coef(glmer.model.9)$year) / (1 + exp(coef(glmer.model.9)$year))

fixef.9 <- exp(fixef(glmer.model.9)) / (1 + exp(fixef(glmer.model.9)))

coef.diffs.9 <- 
    coef.9 %>% 
    rownames_to_column(var = "year") %>% 
    mutate(
        year = as.integer(year),
        coef.diff = home_v_roadhome - .5
        ) %>% 
    as_tibble()

#######

coef.diffs.9 %>% 
    mutate(coef.diff.100 = coef.diff * 100) %>% 
    ggplot(aes(year, coef.diff.100)) + 
    geom_hline(
        yintercept = (fixef.9[2] - .5)*100, 
        size = 1, 
        color = "gray30", 
        linetype = 1) + 
    geom_smooth(method = "loess", color = "red3", span = .8) +
    geom_line(color = "gray50", size = .5) + 
    geom_point(size = 2) + 
    scale_x_continuous(name = "Year", breaks = seq(1900, 2016, 10)) +
    scale_y_continuous(name = "% more games won at home", breaks = 4:13) +
    labs(
        title = "Home field advantage", 
        subtitle = paste0("Model: ", glmer.model.9@call$formula %>% deparse())) +
    coord_cartesian(xlim = c(1899, 2017), ylim = c(3.5, 13.5), expand = FALSE) +
    theme_light() +
    theme(
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 14)
        )

#######


#---------------------------------------------------------------------#
#---- Random intercepts for year, random slopes for home vs. road ----
#---------------------------------------------------------------------#

glmer.model.10 <- 
    
    glmer(
        WL_mtx ~
            home_v_road +
            (home_v_road | year),
        family = binomial("logit"), 
        standings_long
        # glmerControl(optimizer = "Nelder_Mead")
        )

summary(glmer.model.10)

tidy(glmer.model.10) %>% mutate_if(is.double, round, digits = 3)
exp(fixef(glmer.model.10))

exp(fixef(glmer.model.10)) / (1 + exp(fixef(glmer.model.10)))
exp(coef(glmer.model.10)$year) / (1 + exp(coef(glmer.model.10)$year))
exp(ranef(glmer.model.10)$year) / (1 + exp(ranef(glmer.model.10)$year))

VarCorr(glmer.model.10)

plotREsim(REsim(glmer.model.10, n.sims = 1000, seed = 726), labs = TRUE)

######

coef.10 <- exp(coef(glmer.model.10)$year) / (1 + exp(coef(glmer.model.10)$year))

fixef.10 <- exp(fixef(glmer.model.10)) / (1 + exp(fixef(glmer.model.10)))

coef.diffs.10 <- 
    coef.10 %>% 
    rownames_to_column(var = "year") %>% 
    mutate(
        year = as.integer(year),
        coef.diff = home_v_roadhome - .5
        ) %>% 
    as_tibble()

#######

coef.diffs.10 %>% 
    mutate(coef.diff.100 = coef.diff * 100) %>% 
    ggplot(aes(year, coef.diff.100)) + 
    geom_hline(
        yintercept = (fixef.10[2] - .5)*100, 
        size = 1, 
        color = "gray30", 
        linetype = 1) + 
    geom_smooth(method = "loess", color = "red3", span = .8) +
    geom_line(color = "gray50", size = .5) + 
    geom_point(size = 2) + 
    scale_x_continuous(name = "Year", breaks = seq(1900, 2016, 10)) +
    scale_y_continuous(name = "% more games won at home", breaks = 4:13) +
    labs(
        title = "Home field advantage", 
        subtitle = paste0(
            "Model: ", 
            glmer.model.10@call$formula %>% deparse(),
            " [",
            glmer.model.10@call$family %>% deparse(),
            "]")) +
    coord_cartesian(xlim = c(1899, 2017), ylim = c(3.5, 13.5), expand = FALSE) +
    theme_light() +
    theme(
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 14)
        )

#######

#---------------------------------------------------------------#
#---- Random intercepts and slopes with team nested in year ----
#---------------------------------------------------------------#

glmer.model.11 <- 
    
    glmer(
        WL_mtx ~
            home_v_road +
            (home_v_road | year/team),
        family = binomial("logit"), 
        standings_long,
        # glmerControl(optimizer = "Nelder_Mead")
        glmerControl(check.conv.grad = .makeCC("warning", tol = 0.006, relTol = NULL))
        )

summary(glmer.model.11)

tidy(glmer.model.11) %>% mutate_if(is.double, round, digits = 3)

exp(fixef(glmer.model.11)) / (1 + exp(fixef(glmer.model.11)))
exp(coef(glmer.model.11)$year) / (1 + exp(coef(glmer.model.11)$year))
exp(coef(glmer.model.11)$`team:year`) / (1 + exp(coef(glmer.model.11)$`team:year`))
exp(ranef(glmer.model.11)$year) / (1 + exp(ranef(glmer.model.11)$year))
exp(ranef(glmer.model.11)$`team:year`) / (1 + exp(ranef(glmer.model.11)$`team:year`))

VarCorr(glmer.model.11)

plotREsim(REsim(glmer.model.11, n.sims = 1000, seed = 726))

######

coef.11.1 <- exp(coef(glmer.model.11)$year) / (1 + exp(coef(glmer.model.11)$year))
coef.11.2 <- exp(coef(glmer.model.11)$`team:year`) / (1 + exp(coef(glmer.model.11)$`team:year`))

fixef.11 <- exp(fixef(glmer.model.11)) / (1 + exp(fixef(glmer.model.11)))

coef.diffs.11 <- 
    coef.11.1 %>% 
    rownames_to_column(var = "year") %>% 
    mutate(
        year = as.integer(year),
        coef.diff = home_v_roadhome - .5
        ) %>% 
    as_tibble()

######

coef.diffs.11 %>% 
    mutate(coef.diff.100 = coef.diff * 100) %>% 
    ggplot(aes(year, coef.diff.100)) + 
    geom_hline(
        yintercept = (fixef.11[2] - .5)*100, 
        size = 1, 
        color = "gray30", 
        linetype = 1) + 
    geom_smooth(method = "loess", color = "red3", span = .8) +
    geom_line(color = "gray50", size = .5) + 
    geom_point(size = 2) + 
    scale_x_continuous(name = "Year", breaks = seq(1900, 2016, 10)) +
    scale_y_continuous(name = "% more games won at home", breaks = 4:13) +
    labs(
        title = "Home field advantage", 
        subtitle = paste0(
            "Model: ", 
            glmer.model.11@call$formula %>% deparse(),
            " [",
            glmer.model.11@call$family %>% deparse(),
            "]")) +
    coord_cartesian(xlim = c(1899, 2017), ylim = c(3.5, 13.5), expand = FALSE) +
    theme_light() +
    theme(
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 14)
        )

######


#--------------------------------------------#
#---- model 9 + run diff fixed effect ----
#--------------------------------------------#

glmer.model.12 <- 
    
    glmer(
        WL_mtx ~
            Rdiff +
            home_v_road +
            (1 | year),
        family = binomial("logit"), 
        standings_long
        # glmerControl(optimizer = "Nelder_Mead")
        )

summary(glmer.model.12)

tidy(glmer.model.12) %>% mutate_if(is.double, round, digits = 3)

exp(fixef(glmer.model.12)) / (1 + exp(fixef(glmer.model.12)))
exp(coef(glmer.model.12)$year) / (1 + exp(coef(glmer.model.12)$year))
exp(ranef(glmer.model.12)$year) / (1 + exp(ranef(glmer.model.12)$year))

VarCorr(glmer.model.12)

plotREsim(REsim(glmer.model.12, n.sims = 1000, seed = 726), labs = TRUE)


coef.12 <- exp(coef(glmer.model.12)$year) / (1 + exp(coef(glmer.model.12)$year))

fixef.12 <- exp(fixef(glmer.model.12)) / (1 + exp(fixef(glmer.model.12)))

coef.diffs.12 <- 
    coef.12 %>% 
    rownames_to_column(var = "year") %>% 
    mutate(
        year = as.integer(year),
        coef.diff = home_v_roadhome - .5
        ) %>% 
    as_tibble()

#######

coef.diffs.12 %>% 
    mutate(coef.diff.100 = coef.diff * 100) %>% 
    ggplot(aes(year, coef.diff.100)) + 
    geom_hline(
        yintercept = (fixef.12[2] - .5)*100, 
        size = 1, 
        color = "gray30", 
        linetype = 1) + 
    geom_smooth(method = "loess", color = "red3", span = .8) +
    geom_line(color = "gray50", size = .5) + 
    geom_point(size = 2) + 
    scale_x_continuous(name = "Year", breaks = seq(1900, 2016, 10)) +
    scale_y_continuous(name = "% more games won at home", breaks = 4:13) +
    labs(
        title = "Home field advantage", 
        subtitle = paste0("Model: ", glmer.model.12@call$formula %>% deparse())) +
    coord_cartesian(xlim = c(1899, 2017), ylim = c(3.5, 13.5), expand = FALSE) +
    theme_light() +
    theme(
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 14)
        )

#######



#---------------------------------------------------------------------#
#---- Model 10 + Rdiff fixed effect ----
#---------------------------------------------------------------------#

glmer.model.13 <- 
    
    glmer(
        WL_mtx ~
            Rdiff +
            home_v_road +
            (home_v_road | year),
        family = binomial("logit"), 
        standings_long
        # glmerControl(optimizer = "Nelder_Mead")
        )

summary(glmer.model.13)

tidy(glmer.model.13) %>% mutate_if(is.double, round, digits = 3)
exp(fixef(glmer.model.13))

exp(fixef(glmer.model.13)) / (1 + exp(fixef(glmer.model.13)))
exp(coef(glmer.model.13)$year) / (1 + exp(coef(glmer.model.13)$year))
exp(ranef(glmer.model.13)$year) / (1 + exp(ranef(glmer.model.13)$year))

VarCorr(glmer.model.13)

plotREsim(REsim(glmer.model.13, n.sims = 1000, seed = 726), labs = TRUE)

######

coef.13 <- exp(coef(glmer.model.13)$year) / (1 + exp(coef(glmer.model.13)$year))

fixef.13 <- exp(fixef(glmer.model.13)) / (1 + exp(fixef(glmer.model.13)))

coef.diffs.13 <- 
    coef.13 %>% 
    rownames_to_column(var = "year") %>% 
    mutate(
        year = as.integer(year),
        coef.diff = home_v_roadhome - .5
        ) %>% 
    as_tibble()

#######

coef.diffs.13 %>% 
    mutate(coef.diff.100 = coef.diff * 100) %>% 
    ggplot(aes(year, coef.diff.100)) + 
    geom_hline(
        yintercept = (fixef.13[2] - .5)*100, 
        size = 1, 
        color = "gray30", 
        linetype = 1) + 
    geom_smooth(method = "loess", color = "red3", span = .8) +
    geom_line(color = "gray50", size = .5) + 
    geom_point(size = 2) + 
    scale_x_continuous(name = "Year", breaks = seq(1900, 2016, 10)) +
    scale_y_continuous(name = "% more games won at home", breaks = 4:13) +
    labs(
        title = "Home field advantage", 
        subtitle = paste0(
            "Model: ", 
            glmer.model.13@call$formula %>% deparse(),
            " [",
            glmer.model.13@call$family %>% deparse(),
            "]")) +
    coord_cartesian(xlim = c(1899, 2017), ylim = c(3.5, 13.5), expand = FALSE) +
    theme_light() +
    theme(
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 14)
        )

#######

#---------------------------------------------------------------#
#---- Model 11 + Rdiff fixed effect ----
#---------------------------------------------------------------#

glmer.model.14 <- 
    
    glmer(
        WL_mtx ~
            Rdiff *
            home_v_road +
            (home_v_road | year/team),
        family = binomial("logit"), 
        standings_long,
        # glmerControl(optimizer = "Nelder_Mead")
        glmerControl(check.conv.grad = .makeCC("warning", tol = 1e-2, relTol = NULL))
        )

summary(glmer.model.14)

tidy(glmer.model.14) %>% mutate_if(is.double, round, digits = 3)

exp(fixef(glmer.model.14)) / (1 + exp(fixef(glmer.model.14)))
exp(coef(glmer.model.14)$year) / (1 + exp(coef(glmer.model.14)$year))
exp(coef(glmer.model.14)$`team:year`) / (1 + exp(coef(glmer.model.14)$`team:year`))
exp(ranef(glmer.model.14)$year) / (1 + exp(ranef(glmer.model.14)$year))
exp(ranef(glmer.model.14)$`team:year`) / (1 + exp(ranef(glmer.model.14)$`team:year`))

VarCorr(glmer.model.14)

plotREsim(REsim(glmer.model.14, n.sims = 1000, seed = 726))

######

coef.14.1 <- exp(coef(glmer.model.14)$year) / (1 + exp(coef(glmer.model.14)$year))
coef.14.2 <- exp(coef(glmer.model.14)$`team:year`) / (1 + exp(coef(glmer.model.14)$`team:year`))

fixef.14 <- exp(fixef(glmer.model.14)) / (1 + exp(fixef(glmer.model.14)))

coef.diffs.14 <- 
    coef.14.1 %>% 
    rownames_to_column(var = "year") %>% 
    mutate(
        year = as.integer(year),
        coef.diff = home_v_roadhome - .5
        ) %>% 
    as_tibble()

######

coef.diffs.14 %>% 
    mutate(coef.diff.100 = coef.diff * 100) %>% 
    ggplot(aes(year, coef.diff.100)) + 
    geom_hline(
        yintercept = (fixef.14[2] - .5)*100, 
        size = 1, 
        color = "gray30", 
        linetype = 1) + 
    geom_smooth(method = "loess", color = "red3", span = .8) +
    geom_line(color = "gray50", size = .5) + 
    geom_point(size = 2) + 
    scale_x_continuous(name = "Year", breaks = seq(1900, 2016, 10)) +
    scale_y_continuous(name = "% more games won at home", breaks = 4:13) +
    labs(
        title = "Home field advantage", 
        subtitle = paste0(
            "Model: ", 
            glmer.model.14@call$formula %>% deparse(),
            " [",
            glmer.model.14@call$family %>% deparse(),
            "]")) +
    coord_cartesian(xlim = c(1899, 2017), ylim = c(3.5, 13.5), expand = FALSE) +
    theme_light() +
    theme(
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 14)
        )

######

#--------------------------------------------#
#---- Random intercepts for year ----
#--------------------------------------------#


#--------------------------------------------#
#---- model 9 + SOS fixed effect ----
#--------------------------------------------#

glmer.model.15 <- 
    
    glmer(
        WL_mtx ~
            SOSx10 +
            home_v_road +
            (1 | year),
        family = binomial("logit"), 
        standings_long
        # glmerControl(optimizer = "Nelder_Mead")
        )

summary(glmer.model.15)

tidy(glmer.model.15) %>% mutate_if(is.double, round, digits = 3)

exp(fixef(glmer.model.15)) / (1 + exp(fixef(glmer.model.15)))
exp(coef(glmer.model.15)$year) / (1 + exp(coef(glmer.model.15)$year))
exp(ranef(glmer.model.15)$year) / (1 + exp(ranef(glmer.model.15)$year))

VarCorr(glmer.model.15)

plotREsim(REsim(glmer.model.15, n.sims = 1000, seed = 726), labs = TRUE)


coef.15 <- exp(coef(glmer.model.15)$year) / (1 + exp(coef(glmer.model.15)$year))

fixef.15 <- exp(fixef(glmer.model.15)) / (1 + exp(fixef(glmer.model.15)))

coef.diffs.15 <- 
    coef.15 %>% 
    rownames_to_column(var = "year") %>% 
    mutate(
        year = as.integer(year),
        coef.diff = home_v_roadhome - .5
        ) %>% 
    as_tibble()

#######

coef.diffs.15 %>% 
    mutate(coef.diff.100 = coef.diff * 100) %>% 
    ggplot(aes(year, coef.diff.100)) + 
    geom_hline(
        yintercept = (fixef.15[2] - .5)*100, 
        size = 1, 
        color = "gray30", 
        linetype = 1) + 
    geom_smooth(method = "loess", color = "red3", span = .8) +
    geom_line(color = "gray50", size = .5) + 
    geom_point(size = 2) + 
    scale_x_continuous(name = "Year", breaks = seq(1900, 2016, 10)) +
    scale_y_continuous(name = "% more games won at home", breaks = 4:13) +
    labs(
        title = "Home field advantage", 
        subtitle = paste0("Model: ", glmer.model.15@call$formula %>% deparse())) +
    coord_cartesian(xlim = c(1899, 2017), ylim = c(3.5, 13.5), expand = FALSE) +
    theme_light() +
    theme(
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 14)
        )

#######



#---------------------------------------------------------------------#
#---- Model 10 + SOS fixed effect ----
#---------------------------------------------------------------------#

glmer.model.16 <- 
    
    glmer(
        WL_mtx ~
            SOSx10 +
            home_v_road +
            (home_v_road | year),
        family = binomial("logit"), 
        standings_long
        # glmerControl(optimizer = "Nelder_Mead")
        )

summary(glmer.model.16)

tidy(glmer.model.16) %>% mutate_if(is.double, round, digits = 3)
exp(fixef(glmer.model.16))

exp(fixef(glmer.model.16)) / (1 + exp(fixef(glmer.model.16)))
exp(coef(glmer.model.16)$year) / (1 + exp(coef(glmer.model.16)$year))
exp(ranef(glmer.model.16)$year) / (1 + exp(ranef(glmer.model.16)$year))

VarCorr(glmer.model.16)

plotREsim(REsim(glmer.model.16, n.sims = 1000, seed = 726), labs = TRUE)

######

coef.16 <- exp(coef(glmer.model.16)$year) / (1 + exp(coef(glmer.model.16)$year))

fixef.16 <- exp(fixef(glmer.model.16)) / (1 + exp(fixef(glmer.model.16)))

coef.diffs.16 <- 
    coef.16 %>% 
    rownames_to_column(var = "year") %>% 
    mutate(
        year = as.integer(year),
        coef.diff = home_v_roadhome - .5
        ) %>% 
    as_tibble()

#######

coef.diffs.16 %>% 
    mutate(coef.diff.100 = coef.diff * 100) %>% 
    ggplot(aes(year, coef.diff.100)) + 
    geom_hline(
        yintercept = (fixef.16[2] - .5)*100, 
        size = 1, 
        color = "gray30", 
        linetype = 1) + 
    geom_smooth(method = "loess", color = "red3", span = .8) +
    geom_line(color = "gray50", size = .5) + 
    geom_point(size = 2) + 
    scale_x_continuous(name = "Year", breaks = seq(1900, 2016, 10)) +
    scale_y_continuous(name = "% more games won at home", breaks = 4:13) +
    labs(
        title = "Home field advantage", 
        subtitle = paste0(
            "Model: ", 
            glmer.model.16@call$formula %>% deparse(),
            " [",
            glmer.model.16@call$family %>% deparse(),
            "]")) +
    coord_cartesian(xlim = c(1899, 2017), ylim = c(3.5, 13.5), expand = FALSE) +
    theme_light() +
    theme(
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 14)
        )

#######

#---------------------------------------------------------------#
#---- Model 11 + SOS fixed effect ----
#---------------------------------------------------------------#

glmer.model.17 <- 
    
    glmer(
        WL_mtx ~
            SOSx10 +
            home_v_road +
            (home_v_road | year/team),
        family = binomial("logit"), 
        standings_long,
        # glmerControl(optimizer = "Nelder_Mead")
        glmerControl(
            optimizer = "Nelder_Mead",
            check.conv.grad = .makeCC("warning", tol = 0.03, relTol = NULL)
        )
    )

summary(glmer.model.17)

tidy(glmer.model.17) %>% mutate_if(is.double, round, digits = 3)

exp(fixef(glmer.model.17)) / (1 + exp(fixef(glmer.model.17)))
exp(coef(glmer.model.17)$year) / (1 + exp(coef(glmer.model.17)$year))
exp(coef(glmer.model.17)$`team:year`) / (1 + exp(coef(glmer.model.17)$`team:year`))
exp(ranef(glmer.model.17)$year) / (1 + exp(ranef(glmer.model.17)$year))
exp(ranef(glmer.model.17)$`team:year`) / (1 + exp(ranef(glmer.model.17)$`team:year`))

VarCorr(glmer.model.17)

plotREsim(REsim(glmer.model.17, n.sims = 1000, seed = 726))

######

coef.17.1 <- exp(coef(glmer.model.17)$year) / (1 + exp(coef(glmer.model.17)$year))
coef.17.2 <- exp(coef(glmer.model.17)$`team:year`) / (1 + exp(coef(glmer.model.17)$`team:year`))

fixef.17 <- exp(fixef(glmer.model.17)) / (1 + exp(fixef(glmer.model.17)))

coef.diffs.17 <- 
    coef.17.1 %>% 
    rownames_to_column(var = "year") %>% 
    mutate(
        year = as.integer(year),
        coef.diff = home_v_roadhome - .5
        ) %>% 
    as_tibble()

######

coef.diffs.17 %>% 
    mutate(coef.diff.100 = coef.diff * 100) %>% 
    ggplot(aes(year, coef.diff.100)) + 
    geom_hline(
        yintercept = (fixef.17[3] - .5)*100, 
        size = 1, 
        color = "gray30", 
        linetype = 1) + 
    geom_smooth(method = "loess", color = "red3", span = .8) +
    geom_line(color = "gray50", size = .5) + 
    geom_point(size = 2) + 
    scale_x_continuous(name = "Year", breaks = seq(1900, 2016, 10)) +
    scale_y_continuous(name = "% more games won at home", breaks = 4:13) +
    labs(
        title = "Home field advantage", 
        subtitle = paste0(
            "Model: ", 
            glmer.model.17@call$formula %>% deparse(),
            " [",
            glmer.model.17@call$family %>% deparse(),
            "]")) +
    coord_cartesian(xlim = c(1899, 2017), ylim = c(3.5, 13.5), expand = FALSE) +
    theme_light() +
    theme(
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 14)
        )

######

################################################################################
################################################################################
##
##
## Team results
##
##
################################################################################
################################################################################


#=============================#
#### Summarizing the data ####
#=============================#

## Overall ##

team_results %>% 
    mutate(win_loss = recode(win_loss, "W" = 1L, "L" = 0L)) %>% 
    summarise(
        win_percent = mean(win_loss)
        ) %>% 
    mutate_if(is.double, round, digits = 4)


## Home vs. Away ##

team_results %>% 
    group_by(home_v_road) %>% 
    mutate(win_loss = recode(win_loss, "W" = 1L, "L" = 0L)) %>% 
    summarise(
        win_percent = mean(win_loss)
        ) %>% 
    mutate_if(is.double, round, digits = 4)


### By year ###

team_results %>% 
    group_by(game_year, home_v_road) %>% 
    mutate(win_loss = recode(win_loss, "W" = 1L, "L" = 0L)) %>% 
    summarise(
        win_percent = mean(win_loss)
        ) %>% 
    mutate_if(is.double, round, digits = 3) %>% 
    arrange(-game_year) %>% 
    print(n = 30)


### By team ###

team_results %>% 
    group_by(team, home_v_road) %>% 
    mutate(win_loss = recode(win_loss, "W" = 1L, "L" = 0L)) %>% 
    summarise(
        win_percent = mean(win_loss)
        ) %>% 
    mutate_if(is.double, round, digits = 3) %>% 
    arrange(team) %>% 
    print(n = 30)


#=============================#
#### Models ####
#=============================#


#--------------------------------------------#
#---- Treating everything as independent ----
#--------------------------------------------#

glm.model.1 <- 
    glm(
        win_loss ~ home_v_road, 
        family = "binomial", 
        team_results_standings)


summary(glm.model.1)
tidy(glm.model.1) %>% mutate_if(is.double, round, digits = 3)
tidy(glm.model.1, exponentiate = TRUE) %>% mutate_if(is.double, round, digits = 3)

exp(coef(glm.model.1)[[2]]) / (1 + exp(coef(glm.model.1)[[2]]))


#--------------------------------------------#
#---- Random intercepts for year ----
#--------------------------------------------#

glmer.model.1 <- 
    glmer(
        win_loss ~ 
            1 + 
            (1 | game_year),
        family = binomial("logit"),
        team_results_standings,
        glmerControl(
            optimizer = "Nelder_Mead",
            check.conv.grad = .makeCC("warning", tol = 0.0023, relTol = NULL)
        )
    )

summary(glmer.model.1)

tidy(glmer.model.1) %>% mutate_if(is.double, round, digits = 3)

exp(coef(glmer.model.1)[[1]]) / (1 + exp(coef(glmer.model.1)[[1]]))
exp(fixef(glmer.model.1)[[2]]) / (1 + exp(fixef(glmer.model.1)[[2]]))

VarCorr(glmer.model.1)

plotREsim(REsim(glmer.model.1, n.sims = 1000, seed = 726), labs = TRUE)


#--------------------------------------------#
#---- Random intercepts for year ----
#--------------------------------------------#

glmer.model.21 <- 
    glmer(
        win_loss ~ 
            home_v_road + 
            (1 | game_year),
        family = binomial("logit"),
        team_results_standings,
        glmerControl(
            optimizer = "Nelder_Mead",
            check.conv.grad = .makeCC("warning", tol = 0.0017, relTol = NULL)
        )
    )

summary(glmer.model.21)

tidy(glmer.model.21) %>% mutate_if(is.double, round, digits = 3)

exp(fixef(glmer.model.21)) / (1 + exp(fixef(glmer.model.21)))
### WRITE A FUNCTION FOR THIS ###


exp(coef(glmer.model.21)[[1]]) / (1 + exp(coef(glmer.model.21)[[1]]))
exp(ranef(glmer.model.21)[[1]]) / (1 + exp(ranef(glmer.model.21)[[1]]))

VarCorr(glmer.model.21)

plotREsim(REsim(glmer.model.21, n.sims = 1000, seed = 726), labs = TRUE)



#--------------------------------------------#
#---- Random intercepts for year ----
#--------------------------------------------#

glmer.model.22 <- 
    glmer(
        win_loss ~ 
            home_v_road + 
            SRS +
            (1 | game_year),
        family = binomial("logit"),
        team_results_standings,
        glmerControl(
            optimizer = "Nelder_Mead",
            check.conv.grad = .makeCC("warning", tol = 0.0011, relTol = NULL)
        )
    )

summary(glmer.model.22)

tidy(glmer.model.22) %>% mutate_if(is.double, round, digits = 3)

exp(fixef(glmer.model.22)) / (1 + exp(fixef(glmer.model.22)))
### WRITE A FUNCTION FOR THIS ###


exp(coef(glmer.model.22)[[1]]) / (1 + exp(coef(glmer.model.22)[[1]]))
exp(ranef(glmer.model.22)[[1]]) / (1 + exp(ranef(glmer.model.22)[[1]]))

VarCorr(glmer.model.22)

plotREsim(REsim(glmer.model.22, n.sims = 1000, seed = 726), labs = TRUE)


anova(glmer.model.21, glmer.model.22)


#--------------------------------------------#
#---- Random intercepts for year ----
#--------------------------------------------#

glmer.model.31 <- 
    glmer(
        win_loss ~ 
            home_v_road + 
            (home_v_road | game_year),
        family = binomial("logit"),
        team_results_standings
        # glmerControl(
            # optimizer = "Nelder_Mead"
            # check.conv.grad = .makeCC("warning", tol = 0.0017, relTol = NULL)
            # )
        )


summary(glmer.model.31)

tidy(glmer.model.31) %>% mutate_if(is.double, round, digits = 3)

exp(fixef(glmer.model.31)) / (1 + exp(fixef(glmer.model.31)))
### WRITE A FUNCTION FOR THIS ###


exp(coef(glmer.model.31)[[1]]) / (1 + exp(coef(glmer.model.31)[[1]]))
exp(ranef(glmer.model.31)[[1]]) / (1 + exp(ranef(glmer.model.31)[[1]]))

VarCorr(glmer.model.31)

plotREsim(REsim(glmer.model.31, n.sims = 1000, seed = 726), labs = TRUE)


#--------------------------------------------#
#---- Random intercepts for year ----
#--------------------------------------------#

glmer.model.32 <- 
    glmer(
        win_loss ~ 
            home_v_road + 
            SRS +
            (home_v_road | game_year),
        family = binomial("logit"),
        team_results_standings
        # glmerControl(
            # optimizer = "Nelder_Mead"
            # check.conv.grad = .makeCC("warning", tol = 0.0017, relTol = NULL)
            # )
        )


summary(glmer.model.32)

tidy(glmer.model.32) %>% mutate_if(is.double, round, digits = 3)

exp(fixef(glmer.model.32)) / (1 + exp(fixef(glmer.model.32)))
### WRITE A FUNCTION FOR THIS ###


exp(coef(glmer.model.32)[[1]]) / (1 + exp(coef(glmer.model.32)[[1]]))
exp(ranef(glmer.model.32)[[1]]) / (1 + exp(ranef(glmer.model.32)[[1]]))

VarCorr(glmer.model.32)

plotREsim(REsim(glmer.model.32, n.sims = 1000, seed = 726), labs = TRUE)


anova(glmer.model.31, glmer.model.32)


#--------------------------------------------#
#---- Team nested inside year ----
#--------------------------------------------#

glmer.model.4 <- 
    glmer(
        win_loss ~ 
            1 + 
            (1 | game_year/team),
        family = "binomial",
        team_results_standings
        # glmerControl(
            # optimizer = "Nelder_Mead"
            # check.conv.grad = .makeCC("warning", tol = 0.0017, relTol = NULL)
            # )
        )

summary(glmer.model.4)

tidy(glmer.model.4) %>% mutate_if(is.double, round, digits = 3)

exp(fixef(glmer.model.4)) / (1 + exp(fixef(glmer.model.4)))
### WRITE A FUNCTION FOR THIS ###


exp(coef(glmer.model.4)[[1]]) / (1 + exp(coef(glmer.model.4)[[1]]))
exp(ranef(glmer.model.4)[[1]]) / (1 + exp(ranef(glmer.model.4)[[1]]))

VarCorr(glmer.model.4)

plotREsim(REsim(glmer.model.4, n.sims = 1000, seed = 726), labs = TRUE)



#--------------------------------------------#
#---- Random intercepts for team ----
#--------------------------------------------#

now()
glmer.model.51 <- 
    glmer(
        win_loss ~ 
            home_v_road + 
            (1 | game_year/team),
        family = "binomial",
        team_results_standings
        # glmerControl(
            # optimizer = "Nelder_Mead"
            # check.conv.grad = .makeCC("warning", tol = 0.0017, relTol = NULL)
            # )
        )
now()

summary(glmer.model.51)

tidy(glmer.model.51) %>% mutate_if(is.double, round, digits = 3)

exp(fixef(glmer.model.51)) / (1 + exp(fixef(glmer.model.51)))
### WRITE A FUNCTION FOR THIS ###


exp(coef(glmer.model.51)[[1]]) / (1 + exp(coef(glmer.model.51)[[1]]))
exp(ranef(glmer.model.51)[[1]]) / (1 + exp(ranef(glmer.model.51)[[1]]))

VarCorr(glmer.model.51)

plotREsim(REsim(glmer.model.51, n.sims = 1000, seed = 726), labs = TRUE)



#--------------------------------------------#
#---- Random intercepts for team ----
#--------------------------------------------#

now()
glmer.model.52 <- 
    glmer(
        win_loss ~ 
            SRS +
            home_v_road + 
            (1 | game_year/team),
        family = "binomial",
        team_results_standings
        # glmerControl(
            # optimizer = "Nelder_Mead"
            # check.conv.grad = .makeCC("warning", tol = 0.0017, relTol = NULL)
            # )
        )
now()


summary(glmer.model.52)

tidy(glmer.model.52) %>% mutate_if(is.double, round, digits = 3)

exp(fixef(glmer.model.52)) / (1 + exp(fixef(glmer.model.52)))
### WRITE A FUNCTION FOR THIS ###


exp(coef(glmer.model.52)[[1]]) / (1 + exp(coef(glmer.model.52)[[1]]))
exp(ranef(glmer.model.52)[[1]]) / (1 + exp(ranef(glmer.model.52)[[1]]))

VarCorr(glmer.model.52)

plotREsim(REsim(glmer.model.52, n.sims = 1000, seed = 726), labs = TRUE)

anova(glmer.model.51, glmer.model.52)
anova(glmer.model.22, glmer.model.52)


#--------------------------------------------#
#---- Random intercepts for team ----
#--------------------------------------------#

now()
glmer.model.61 <- 
    glmer(
        win_loss ~ 
            home_v_road + 
            (home_v_road | game_year/team),
        family = "binomial",
        team_results_standings,
        glmerControl(
            optimizer = "Nelder_Mead",
            check.conv.grad = .makeCC("warning", tol = 0.001, relTol = NULL)
        )
    )

now()

summary(glmer.model.61)

tidy(glmer.model.61) %>% mutate_if(is.double, round, digits = 3)

exp(fixef(glmer.model.61)) / (1 + exp(fixef(glmer.model.61)))
### WRITE A FUNCTION FOR THIS ###


exp(coef(glmer.model.61)[[1]]) / (1 + exp(coef(glmer.model.61)[[1]]))
exp(ranef(glmer.model.61)[[1]]) / (1 + exp(ranef(glmer.model.61)[[1]]))

VarCorr(glmer.model.61)

plotREsim(REsim(glmer.model.61, n.sims = 1000, seed = 726), labs = TRUE)



# write_rds(glmer.model.61, "glmer.model.61.RDS")



anova(glmer.model.1, glmer.model.61)
anova(glmer.model.31, glmer.model.61)
anova(glmer.model.4,  glmer.model.61)
anova(glmer.model.51, glmer.model.61)
anova(glmer.model.52, glmer.model.61)

anova(glmer.model.6, glmer.model.52)
anova(glmer.model.52, glmer.model.61)



roccurve.1.1 <-
    roc(
        model.frame(glmer.model.61)$win_loss ~ predict(glmer.model.61, type = "response"),
        grid = TRUE,
        plot = TRUE,
        print.thres = TRUE,
        print.auc = TRUE,
        main = "glmer.model.61",
        algorithm = 1
    )



#--------------------------------------------#
#---- Random intercepts for team ----
#--------------------------------------------#

now()
glmer.model.62 <- 
    glmer(
        win_loss ~ 
            SRS +
            home_v_road + 
            (home_v_road | game_year/team),
        family = "binomial",
        team_results_standings,
        glmerControl(
            optimizer = "Nelder_Mead",
            check.conv.grad = .makeCC("warning", tol = 0.001, relTol = NULL)
        )
    )

now()

summary(glmer.model.62)

tidy(glmer.model.62) %>% mutate_if(is.double, round, digits = 3)

exp(fixef(glmer.model.62)) / (1 + exp(fixef(glmer.model.62)))
### WRITE A FUNCTION FOR THIS ###


exp(coef(glmer.model.62)[[1]]) / (1 + exp(coef(glmer.model.62)[[1]]))
exp(ranef(glmer.model.62)[[1]]) / (1 + exp(ranef(glmer.model.62)[[1]]))

VarCorr(glmer.model.62)

plotREsim(REsim(glmer.model.62, n.sims = 1000, seed = 726), labs = TRUE)



# write_rds(glmer.model.62, "glmer.model.62.RDS")



anova(glmer.model.1, glmer.model.6)
anova(glmer.model.32, glmer.model.62)

anova(glmer.model.6, glmer.model.52)
anova(glmer.model.52, glmer.model.62)



roccurve.1.1 <-
    roc(
        model.frame(glmer.model.62)$win_loss ~ predict(glmer.model.62, type = "response"),
        grid = TRUE,
        plot = TRUE,
        print.thres = TRUE,
        print.auc = TRUE,
        main = "glmer.model.62",
        algorithm = 1
    )




#--------------------------------------------#
#---- Random intercepts for team ----
#--------------------------------------------#

now()
stan_glmer.model.22 <- 
    stan_glmer(
        win_loss ~ 
            SRS +
            home_v_road + 
            (1 | game_year),
        family = "binomial",
        data = team_results_standings, chains = 1, iter = 100
    )

now()

######################

now()
stan_glmer.model.22 <- 
    stan_glmer(
        win_loss ~ 
            SRS +
            home_v_road + 
            (1 | game_year),
        family = "binomial",
        data = team_results_standings
    )

now()

######################


glmer.model.32 <- 
    glmer(
        win_loss ~ 
            SRS +
            home_v_road + 
            (home_v_road | game_year),
        family = binomial("logit"),
        team_results_standings
        # glmerControl(
            # optimizer = "Nelder_Mead"
            # check.conv.grad = .makeCC("warning", tol = 0.0017, relTol = NULL)
            # )
        )

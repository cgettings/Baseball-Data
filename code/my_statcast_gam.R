#######################################################-
#######################################################-
##
## Statcast GAMs using 2017 data ----
##
#######################################################-
#######################################################-

#========================================#
#### Setting up ####
#========================================#

#---------------------------------#
# Loading libraries ----
#---------------------------------#

library(readr)
library(dplyr)
library(tidyr)
library(dbplyr)
library(stringr)
library(lubridate)
library(stringdist)
library(stringr)
library(tibble)
library(magrittr)
library(DBI)
library(pryr)
library(ggplot2)
library(mgcv)
# library(gam)
library(broom)
library(pROC)

invlogit <- function(x) exp(x) / (1 + exp(x))

#=============================#
# Connecting to Database ----
#=============================#

statcast_db <- dbConnect(RSQLite::SQLite(), "./data/statcast_db.sqlite3")

#----------------------------------#
# Selecting and filtering ----
#----------------------------------#

statcast_data <- 
    tbl(statcast_db, "statcast_data_updated") %>% 
    filter(game_year == 2017 & type == "X") %>% 
    mutate(outcome = as.integer(ifelse(events %in% c("single", "double", "triple", "home_run"), 1, 0))) %>% 
    collect()

#===============================#
# basic exploratory plots ----
#===============================#

#--------------------------------------------#
# Speed and angle for different outcomes ----
#--------------------------------------------#

set.seed(726)

statcast_data %>% 
    sample_n(2000) %>% 
    ggplot(aes(launch_angle_sc, launch_speed_sc, color = as.factor(outcome))) +
    geom_point() +
    xlim(-25, 50) +
    ylim(40, 120) +
    ggtitle("2000 Balls Put In Play") + 
    scale_color_manual(values = c("blue4", "skyblue")) +
    theme_gray()

#-----------------------------------------------#
# Speed distribution for different outcomes ----
#-----------------------------------------------#

set.seed(726)

statcast_data %>% 
    sample_n(2000) %>% 
    ggplot(aes(y = launch_speed_sc, x = outcome)) +
    geom_violin(aes(group = outcome)) +
    geom_point(
        aes(group = outcome),
        shape = 16,
        alpha = .125,
        position = position_jitter(width = 0.0125)
    ) +
    # xlim(-25, 50) +
    # ylim(40, 120) +
    # ggtitle("2000 Balls Put In Play") + 
    # scale_color_manual(values = c("blue4", "skyblue")) +
    theme_gray() +
    coord_flip()

#-----------------------------------------------#
# Angle distribution for different outcomes ----
#-----------------------------------------------#

set.seed(726)

statcast_data %>% 
    sample_n(2000) %>% 
    ggplot(aes(y = launch_angle_sc, x = outcome)) +
    geom_violin(aes(group = outcome)) +
    geom_point(
        aes(group = outcome),
        shape = 16,
        alpha = .125,
        position = position_jitter(width = 0.0125)
    ) +
    # xlim(-25, 50) +
    # ylim(40, 120) +
    # ggtitle("2000 Balls Put In Play") + 
    # scale_color_manual(values = c("blue4", "skyblue")) +
    theme_gray() +
    coord_flip()

#=============================#
# GAMs ----
#=============================#

#-------------------------------------------------#
# Splitting data into training and test sets ----
#-------------------------------------------------#

set.seed(726)

statcast_data_sample <- sample_frac(statcast_data, 0.5)
statcast_data_test   <- anti_join(statcast_data, statcast_data_sample)

### Checking the exclusivity of each set ###

# semi_join(statcast_data_sample, statcast_data_test) %>% nrow()
# semi_join(statcast_data_test, statcast_data_sample) %>% nrow()


#-----------------------------------#
# ---- Fitting models for angle ----
#-----------------------------------#

#### GLM ----

fit.glm.angle <-
    glm(
        outcome ~ launch_angle_sc,
        
        family = binomial,
        data = statcast_data_sample)

summary(fit.glm.angle)
tidy(fit.glm.angle)

plot(fit.glm.angle)


#### GAM ----

fit.gam.angle <-
    gam(
        outcome ~ 
            s(launch_angle_sc),
        
        family = binomial,
        data = statcast_data_sample)

summary(fit.gam.angle)
tidy(fit.gam.angle)

plot(fit.gam.angle)


#### Comparing ----

### Not nested, but can still get a sense of relative performance ###

anova(fit.glm.angle, fit.gam.angle, test = "Chisq")
AIC(fit.glm.angle, fit.gam.angle)

## (the angle-only glm is pretty terrible!)

### Creating tbl of fitted values ###

fitted_comb_angle <-
    data_frame(
        outcome = model.frame(fit.gam.angle)$outcome,
        launch_angle_sc = model.frame(fit.gam.angle)$launch_angle_sc,
        glm_fitted = fitted(fit.glm.angle),
        gam_fitted = fitted(fit.gam.angle)
    ) %>% 
    as_tibble()


### Plotting fitted values agaist each other ###

fitted_comb_angle %>% 
    ggplot(aes(x = launch_angle_sc)) + 
    geom_point(aes(y = glm_fitted), color = "black") + 
    geom_point(aes(y = gam_fitted), color = "red") +
    theme_minimal()


#### predicting over a grid of angle values ----

angle_p <-
    expand.grid(
        launch_angle_sc = seq(-20, 50, length = 50)
    ) %>%
    as_tibble()

fit.gam.angle_pred <- predict(fit.gam.angle, newdata = angle_p, type = "link") %>% invlogit()
fit.glm.angle_pred <- predict(fit.glm.angle, newdata = angle_p, type = "link") %>% invlogit()


### Checking the density of the fitted values ###

plot(density(fit.gam.angle_pred))
plot(density(fit.glm.angle_pred))


#----------------------------------#
# ---- Fitting models for speed ----
#----------------------------------#

#### GLM ----

fit.glm.speed <-
    glm(
        outcome ~ launch_speed_sc,
        
        family = binomial,
        data = statcast_data_sample)

summary(fit.glm.speed)
tidy(fit.glm.speed)

plot(fit.glm.speed)


#### GAM ----

## Trying a (default) thin-plate spline with k = 50

fit.gam.speed <-
    gam(
        outcome ~ 
            s(launch_speed_sc, k = 50
        
        family = binomial,
        data = statcast_data_sample)

summary(fit.gam.speed)
tidy(fit.gam.speed)

plot(fit.gam.speed)


#### Comparing ----

### Not nested, but can still get a sense of relative performance ###

anova(fit.glm.speed, fit.gam.speed, test = "Chisq")
AIC(fit.glm.speed, fit.gam.speed)


### Creating tbl of fitted values ###

fitted_comb_speed <-
    data_frame(
        outcome = model.frame(fit.gam.speed)$outcome,
        launch_speed_sc = model.frame(fit.gam.speed)$launch_speed_sc,
        glm_fitted = fitted(fit.glm.speed),
        gam_fitted = fitted(fit.gam.speed)
    ) %>% 
    as_tibble()


### Plotting fitted values agaist each other ###

fitted_comb_speed %>% 
    ggplot(aes(x = launch_speed_sc)) + 
    geom_line(aes(y = glm_fitted), color = "black") + 
    geom_line(aes(y = gam_fitted), color = "red") +
    geom_point(aes(y = glm_fitted), color = "black", shape = 1) + 
    geom_point(aes(y = gam_fitted), color = "red", shape = 1) +
    theme_minimal()


#### predicting over a grid of speed values ----

speed_p <-
    expand.grid(
        launch_speed_sc = seq(40, 120, length = 50)
    ) %>%
    as_tibble()

fit.gam.speed_pred <- predict(fit.gam.speed, newdata = speed_p, type = "link") %>% invlogit()
fit.glm.speed_pred <- predict(fit.glm.speed, newdata = speed_p, type = "link") %>% invlogit()


### Checking the density of the fitted values ###

plot(density(fit.gam.speed_pred))
plot(density(fit.glm.speed_pred))


### Plotting predicted values agaist each other ###

bind_cols(speed_p, data.frame(fit.gam.speed_pred, fit.glm.speed_pred)) %>% 
    ggplot(aes(x = launch_speed_sc)) +
    geom_line(aes(y = fit.glm.speed_pred), color = "black") + 
    geom_line(aes(y = fit.gam.speed_pred), color = "red") +
    # geom_point(aes(y = fit.glm.speed_pred), color = "black") +
    # geom_point(aes(y = fit.gam.speed_pred), color = "red") +
    theme_minimal()


#--------------------------------------#
# ---- Fitting more complex models ----
#--------------------------------------#


### GLM ----

## Adding a quadratic term for launch angle

fit.glm <-
    glm(
        outcome ~ 
            launch_angle_sc +
            launch_speed_sc +
            I(launch_angle_sc^2),
        
        family = binomial,
        data = statcast_data_sample)

summary(fit.glm)
tidy(fit.glm)

plot(fit.glm)


### GAM ----

## Trying a (default) thin-plate spline with k = 50

fit.gam.1 <-
    gam(
        outcome ~ 
            s(launch_angle_sc, launch_speed_sc, k = 50,
        
        family = binomial,
        data = statcast_data_sample)

summary(fit.gam.1)
tidy(fit.gam.1)

plot(fit.gam.1)

gam.check(fit.gam.1)


### GAM ----

## Seeing if specifying separate, non-interactive smooth terms is any different

fit.gam.2 <-
    gam(
        outcome ~ 
            s(launch_angle_sc, k = 50) + s(launch_speed_sc, k = 50),
        
        family = binomial,
        data = statcast_data_sample)

summary(fit.gam.2)
tidy(fit.gam.2)

plot(fit.gam.2)


### GAM ----

## Trying a full tensor product smooth, which (basically) captures the interaction
        
fit.gam.3 <-
    bam(
        outcome ~ 
            te(launch_angle_sc, launch_speed_sc, k = 50),
        
        family = binomial,
        data = statcast_data_sample)

summary(fit.gam.3)
tidy(fit.gam.3)

plot(fit.gam.3)


#----------------------------------#
# Model Predictions ----
#----------------------------------#

#### Constructing grid of angle and speed values ----

speed_angle_p <-
    expand.grid(
        launch_angle_sc = seq(-20, 50, length = 50),
        launch_speed_sc = seq(40, 120, length = 50)
    ) %>%
    as_tibble()


#### GLM predicted probabilities ----

### Computing predicted values ###

fit.glm_pred <- predict(fit.glm, newdata = speed_angle_p, type = "link") %>% invlogit()

### Plotting predicted probability of a batted ball being a hit ###

data.frame(speed_angle_p, fit.glm_pred) %>% 
    
    ggplot(aes(x = launch_angle_sc, y = launch_speed_sc, z = fit.glm_pred))  +
    stat_contour(
        geom = "polygon",
        breaks = seq(0, 1, length.out = 21),
        size = 1.5,
        aes(fill = ..level..)
        # aes(fill = fit.glm_pred)
    ) +
    scale_fill_viridis_c(option = "viridis") +
    geom_vline(xintercept = 0, color = "black") +
    # xlim(-25, 50) +
    # ylim(40, 120) +
    labs(title = "Contour Plot of Probability of Hit", subtitle = deparse(fit.glm$call)) +
    theme_bw()

#### GAM 1 predicted probabilities ----

### Computing predicted values ###

fit.gam.1_pred <- predict(fit.gam.1, newdata = speed_angle_p, type = "link") %>% invlogit()

### Plotting predicted probability of a batted ball being a hit ###

data.frame(speed_angle_p, fit.gam.1_pred) %>% 

    ggplot(aes(x = launch_angle_sc, y = launch_speed_sc, z = fit.gam.1_pred))  +
    stat_contour(
        geom = "polygon",
        breaks = seq(0, 1, length.out = 21),
        size = 1.5,
        aes(fill = ..level..)
    ) +
    scale_fill_viridis_c(option = "viridis") +
    geom_vline(xintercept = 0, color = "black") +
    xlim(-25, 50) +
    ylim(40, 120) +
    labs(title = "Contour Plot of Probability of Hit", subtitle = deparse(fit.gam.1$call)) +
    theme_bw()


#### GAM 3 predicted probabilities ----

### Computing predicted values ###

fit.gam.3_pred <- predict(fit.gam.3, newdata = speed_angle_p, type = "link") %>% invlogit()

### Plotting predicted probability of a batted ball being a hit ###

data.frame(speed_angle_p, fit.gam.3_pred) %>% 

    ggplot(aes(x = launch_angle_sc, y = launch_speed_sc, z = fit.gam.3_pred))  +
    stat_contour(
        geom = "polygon",
        breaks = seq(0, 1, length.out = 21),
        size = 1.5,
        aes(fill = ..level..)
    ) +
    scale_fill_viridis_c(option = "viridis") +
    geom_vline(xintercept = 0, color = "black") +
    xlim(-25, 50) +
    ylim(40, 120) +
    labs(title = "Contour Plot of Probability of Hit", subtitle = deparse(fit.gam.3$call)) +
    theme_bw()


#### Plotting ROC curves to examine model performance ----

roccurve.glm <-
    roc(
        model.frame(fit.glm)$outcome ~ predict(fit.glm, type = "response"),
        grid = TRUE,
        plot = TRUE,
        print.thres = TRUE,
        print.auc = TRUE,
        main = "fit.glm.1"
    )

roccurve.gam.1 <-
    roc(
        model.frame(fit.gam.1)$outcome ~ predict(fit.gam.1, type = "response"),
        grid = TRUE,
        plot = TRUE,
        print.thres = TRUE,
        print.auc = TRUE,
        main = "fit.gam.1"
    )

roccurve.gam.2 <-
    roc(
        model.frame(fit.gam.2)$outcome ~ predict(fit.gam.2, type = "response"),
        grid = TRUE,
        plot = TRUE,
        print.thres = TRUE,
        print.auc = TRUE,
        main = "fit.gam.2"
    )

roccurve.gam.3 <-
    roc(
        model.frame(fit.gam.3)$outcome ~ predict(fit.gam.3, type = "response"),
        grid = TRUE,
        plot = TRUE,
        print.thres = TRUE,
        print.auc = TRUE,
        main = "fit.gam.3"
    )

##########################################################################################
##########################################################################################
        

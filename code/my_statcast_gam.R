#######################################################-
#######################################################-
##
## Statcast GAM using 2017 data ----
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
#### Initilizing Database ####
#=============================#

# setwd("D:/Files/BASP/R analyses/Baseball Data/Data Files/")
setwd("~/BASP/R analyses/Baseball Data/Data Files")

statcast_db <- dbConnect(RSQLite::SQLite(), "statcast_db.sqlite3")

#----------------------------------#
# ---- Selecting and filtering ----
#----------------------------------#

statcast_data <- 
    tbl(statcast_db, "statcast_data_updated") %>% 
    filter(game_year == 2017 & type == "X") %>% 
    mutate(outcome = as.integer(ifelse(events %in% c("single", "double", "triple", "home_run"), 1, 0))) %>% 
    collect()

#===============================#
#### basic exploratory plot ####
#===============================#

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
    # theme(plot.title = element_text(hjust = 0.5, size = 18))

#===============================#

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
    # theme(plot.title = element_text(hjust = 0.5, size = 18))


#===============================#

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
    # theme(plot.title = element_text(hjust = 0.5, size = 18))


#=============================#
#### GAMs ####
#=============================#

#----------------------------------#
# ---- Splitting data ----
#----------------------------------#

set.seed(726)

statcast_data_sample <- sample_frac(statcast_data, 0.5)
statcast_data_test   <- anti_join(statcast_data, statcast_data_sample)

# semi_join(statcast_data_sample, statcast_data_test) %>% nrow()
# semi_join(statcast_data_test, statcast_data_sample) %>% nrow()


#----------------------------------#
# ---- Fitting models for angle ----
#----------------------------------#


### GLM ----

fit.glm.angle <-
    glm(
        outcome ~ launch_angle_sc,
        
        family = binomial,
        data = statcast_data_sample)

summary(fit.glm.angle)
tidy(fit.glm.angle)

plot(fit.glm.angle)


### GAM ----

fit.gam.angle <-
    gam(
        outcome ~ 
            s(launch_angle_sc),
        
        family = binomial,
        data = statcast_data_sample)

summary(fit.gam.angle)
tidy(fit.gam.angle)

plot(fit.gam.angle)


### Comparing ----

anova(fit.glm.angle, fit.gam.angle, test = "Chisq")

fitted_comb_angle <-
    data_frame(
        outcome = model.frame(fit.gam.angle)$outcome,
        launch_angle_sc = model.frame(fit.gam.angle)$launch_angle_sc,
        glm_fitted = fitted(fit.glm.angle),
        gam_fitted = fitted(fit.gam.angle)
    ) %>% 
    as_tibble()

fitted_comb_angle %>% 
    ggplot(aes(x = launch_angle_sc)) + 
    geom_point(aes(y = glm_fitted), color = "black") + 
    geom_point(aes(y = gam_fitted), color = "red") +
    theme_minimal()


### predict over grid ----

angle_p <-
    expand.grid(
        launch_angle_sc = seq(-20, 50, length = 50)
    ) %>%
    as_tibble()


fit.gam.angle_pred <- predict(fit.gam.angle, newdata = angle_p, type = "link") %>% invlogit()
fit.glm.angle_pred <- predict(fit.glm.angle, newdata = angle_p, type = "link") %>% invlogit()

plot(density(fit.gam.angle_pred))
plot(density(fit.glm.angle_pred))


#----------------------------------#
# ---- Fitting models for speed ----
#----------------------------------#


### GLM ----

fit.glm.speed <-
    glm(
        outcome ~ launch_speed_sc,
        
        family = binomial,
        data = statcast_data_sample)

summary(fit.glm.speed)
tidy(fit.glm.speed)

plot(fit.glm.speed)


### GAM ----

fit.gam.speed <-
    gam(
        outcome ~ 
            s(launch_speed_sc),
        
        family = binomial,
        data = statcast_data_sample)

summary(fit.gam.speed)
tidy(fit.gam.speed)

plot(fit.gam.speed)


### Comparing ----

anova(fit.glm.speed, fit.gam.speed, test = "Chisq")

fitted_comb_speed <-
    data_frame(
        outcome = model.frame(fit.gam.speed)$outcome,
        launch_speed_sc = model.frame(fit.gam.speed)$launch_speed_sc,
        glm_fitted = fitted(fit.glm.speed),
        gam_fitted = fitted(fit.gam.speed)
    ) %>% 
    as_tibble()

fitted_comb_speed %>% 
    ggplot(aes(x = launch_speed_sc)) + 
    geom_line(aes(y = glm_fitted), color = "black") + 
    geom_line(aes(y = gam_fitted), color = "red") +
    geom_point(aes(y = glm_fitted), color = "black", shape = 1) + 
    geom_point(aes(y = gam_fitted), color = "red", shape = 1) +
    theme_minimal()



### predict over grid ----

speed_p <-
    expand.grid(
        launch_speed_sc = seq(40, 120, length = 50)
    ) %>%
    as_tibble()


fit.gam.speed_pred <- predict(fit.gam.speed, newdata = speed_p, type = "link") %>% invlogit()
fit.glm.speed_pred <- predict(fit.glm.speed, newdata = speed_p, type = "link") %>% invlogit()

plot(density(fit.gam.speed_pred))
plot(density(fit.glm.speed_pred))


bind_cols(speed_p, data.frame(fit.gam.speed_pred, fit.glm.speed_pred)) %>% 
    ggplot(aes(x = launch_speed_sc)) +
    geom_line(aes(y = fit.glm.speed_pred), color = "black") + 
    geom_line(aes(y = fit.gam.speed_pred), color = "red") +
    # geom_point(aes(y = fit.glm.speed_pred), color = "black") +
    # geom_point(aes(y = fit.gam.speed_pred), color = "red") +
    theme_minimal()


#----------------------------------#
# ---- Fitting models ----
#----------------------------------#


### GLM ----

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

fit.gam.1 <-
    gam(
        outcome ~ 
            s(launch_angle_sc, launch_speed_sc, k = 100),
        
        family = binomial,
        data = statcast_data_sample)

summary(fit.gam.1)
tidy(fit.gam.1)

plot(fit.gam.1)

gam.check(fit.gam.1)


### GAM ----

fit.gam.2 <-
    gam(
        outcome ~ 
            s(launch_angle_sc) + s(launch_speed_sc),
        
        family = binomial,
        data = statcast_data_sample)

summary(fit.gam.2)
tidy(fit.gam.2)

plot(fit.gam.2)



### GAM ----

fit.gam.3.1 <-
    bam(
        outcome ~ 
            te(launch_angle_sc, launch_speed_sc, k = 100),
            # te(launch_angle_sc, launch_speed_sc, bs = "tp"),
        
        family = binomial,
        data = statcast_data_sample)

summary(fit.gam.3)
tidy(fit.gam.3.1)

plot(fit.gam.3.1)



### GAM ----

fit.gam.3.1 <-
    bam(
        outcome ~ 
            te(launch_angle_sc, launch_speed_sc, k = 50),
            # te(launch_angle_sc, launch_speed_sc, bs = "tp"),
        
        family = binomial,
        data = statcast_data_sample)

summary(fit.gam.3)
tidy(fit.gam.3.1)

plot(fit.gam.3.1)



#----------------------------------#
# ---- GAM Predictions ----
#----------------------------------#


### predict over grid ----

speed_angle_p <-
    expand.grid(
        launch_angle_sc = seq(-20, 50, length = 50),
        launch_speed_sc = seq(40, 120, length = 50)
    ) %>%
    as_tibble()



### Plotting predictions ----

fit.glm_pred <- predict(fit.glm, newdata = speed_angle_p, type = "link") %>% invlogit()


data.frame(speed_angle_p, fit.glm_pred) %>% 
    
    # gam_df_p %>% 
    ggplot(aes(x = launch_angle_sc, y = launch_speed_sc, z = fit.glm_pred))  +
    stat_contour(
        geom = "polygon",
        breaks = seq(0, 1, length.out = 21),
        size = 1.5,
        aes(fill = ..level..)
        # aes(fill = fit.glm_pred)
    ) +
    # scale_fill_gradientn(colours = heat.colors(10)) +
    scale_fill_viridis_c(option = "viridis") +
    # scale_fill_viridis_c(option = "magma") +
    # scale_fill_viridis_c(option = "inferno") +
    # scale_fill_viridis_c(option = "plasma") +
    geom_vline(xintercept = 0, color = "black") +
    # xlim(-25, 50) +
    # ylim(40, 120) +
    ggtitle("Contour Plot of Probability of Hit") + 
    theme(plot.title = element_text(hjust = 0.5, size = 18)) 


### Plotting predictions ----

fit.gam.1_pred <- predict(fit.gam.1, newdata = speed_angle_p, type = "link") %>% invlogit()


data.frame(speed_angle_p, fit.gam.1_pred) %>% 

# gam_df_p %>% 
    ggplot(aes(x = launch_angle_sc, y = launch_speed_sc, z = fit.gam.1_pred))  +
    stat_contour(
        geom = "polygon",
        breaks = seq(0, 1, length.out = 21),
        size = 1.5,
        aes(fill = ..level..)
    ) +
    # scale_fill_gradientn(colours = heat.colors(10)) +
    scale_fill_viridis_c(option = "viridis") +
    # scale_fill_viridis_c(option = "magma") +
    # scale_fill_viridis_c(option = "inferno") +
    # scale_fill_viridis_c(option = "plasma") +
    geom_vline(xintercept = 0, color = "black") +
    xlim(-25, 50) +
    ylim(40, 120) +
    ggtitle("Contour Plot of Probability of Hit") + 
    theme(plot.title = element_text(hjust = 0.5, size = 18)) 


### Plotting predictions ----

fit.gam.3_pred <- predict(fit.gam.3, newdata = speed_angle_p, type = "link") %>% invlogit()


data.frame(speed_angle_p, fit.gam.3_pred) %>% 

# gam_df_p %>% 
    ggplot(aes(x = launch_angle_sc, y = launch_speed_sc, z = fit.gam.3_pred))  +
    stat_contour(
        geom = "polygon",
        breaks = seq(0, 1, length.out = 21),
        size = 1.5,
        aes(fill = ..level..)
    ) +
    # scale_fill_gradientn(colours = heat.colors(10)) +
    scale_fill_viridis_c(option = "viridis") +
    # scale_fill_viridis_c(option = "magma") +
    # scale_fill_viridis_c(option = "inferno") +
    # scale_fill_viridis_c(option = "plasma") +
    geom_vline(xintercept = 0, color = "black") +
    xlim(-25, 50) +
    ylim(40, 120) +
    ggtitle("Contour Plot of Probability of Hit") + 
    theme(plot.title = element_text(hjust = 0.5, size = 18)) 



### Plotting predictions ----


roccurve.1 <-
    roc(
        model.frame(fit.glm)$outcome ~ predict(fit.glm, type = "response"),
        grid = TRUE,
        plot = TRUE,
        print.thres = TRUE,
        print.auc = TRUE,
        main = "fit.glm.1"
    )

roccurve.2.1 <-
    roc(
        model.frame(fit.gam.1)$outcome ~ predict(fit.gam.1, type = "response"),
        grid = TRUE,
        plot = TRUE,
        print.thres = TRUE,
        print.auc = TRUE,
        main = "fit.gam"
    )

roccurve.3 <-
    roc(
        model.frame(fit.gam.2)$outcome ~ predict(fit.gam.2, type = "response"),
        grid = TRUE,
        plot = TRUE,
        print.thres = TRUE,
        print.auc = TRUE,
        main = "fit.gam.2"
    )

roccurve.3.1 <-
    roc(
        model.frame(fit.gam.3.1)$outcome ~ predict(fit.gam.3.1, type = "response"),
        grid = TRUE,
        plot = TRUE,
        print.thres = TRUE,
        print.auc = TRUE,
        main = "fit.gam.3.1"
    )

#=============================#
#### GAMs ####
#=============================#

fit1 <- lm(wage ~ ns(year, df = 5) + ns(age, df = 5) + education, data = Wage)

fit2 <- gam(wage ~ ns(year, df = 5) + ns(age, df = 5) + education, data = Wage)

plot(fit2, se = TRUE)

fit3 <- gam(wage ~ s(year, df = 5) + s(age, df = 5) + education, data = Wage)

plot(fit3, se = TRUE)




fit.gam.s2 <-
    gam::gam(
        outcome ~ 
            ns(launch_angle_sc) + ns(launch_speed_sc),
        
        family = binomial,
        data = statcast_data_sample)

summary(fit.gam.s2)
tidy(fit.gam.s2)




fit.glm.s <-
    glm(
        outcome ~ 
            s(launch_angle_sc) + s(launch_speed_sc),
        
        family = binomial,
        data = statcast_data_sample)

summary(fit.gam.s2)
tidy(fit.gam.s2)


ns(statcast_data_sample$launch_speed_sc)
bs(statcast_data_sample$launch_speed_sc)
s(statcast_data_sample$launch_speed_sc)

######################################

anova(fit.gam.2, freq = FALSE)

gam.check(fit.gam.2)

######################################

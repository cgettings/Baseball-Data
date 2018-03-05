###########################################
###########################################
##
## HR vs. not
##
###########################################
###########################################

#=========================#
#### Loading packages ####
#=========================#


library(MASS)
library(car)
library(readxl)
library(readr)
library(QuantPsyc)
library(dplyr)
library(ggplot2)
library(cowplot)
library(visreg)
library(broom)
library(stringr)
library(lme4)
library(merTools)
library(viridis)
library(pitchRx)
library(lubridate)
library(tidyr)
library(stringdist)
library(tibble)
library(rstanarm)
library(pROC)
library(DBI)


setwd("~/BASP/R analyses")
source("omegaFunctions.R")
source("spec_dec.R")
source("tidy.lmerTest.R")
source("ICC_comp.R")



#=====================#
#### Loading Data ####
#=====================#


#######################
   ## From files ##
#######################

setwd("~/BASP/R analyses/Baseball Data/Data Files")

statcast.data <- read_rds("statcast_data_updated.RDS")


# Optimal launch angle for each speed

geten <- function(df, x) get(x, envir = as.environment(df))

statcast.data %>% geten("events") %>% str()

statcast.data %>% count(events) %>% arrange(desc(n))
statcast.data %>% count(bb_type) %>% arrange(desc(n))
statcast.data %>% count(description) %>% arrange(desc(n))

statcast.data %>% count(events, bb_type) %>% arrange(desc(n)) %>% filter(events == "home_run")
statcast.data %>% count(events, bb_type) %>% arrange(desc(bb_type), desc(n)) %>% filter(bb_type %in% c("fly_ball", "line_drive")) %>% print(n = 50)



# statcast.data.2 <-
#     statcast.data %>%  
#     mutate(HR_v_other = with(.,
#     case_when(
#         events == "home_run"  & bb_type %in% c("fly_ball", "line_drive") ~ "home_run",
#         events == "field_out" & bb_type %in% c("fly_ball", "line_drive") ~ "field_out",
#         bb_type %in% c("fly_ball", "line_drive") ~ "in_play"
#         ))) %>% 
#     filter(!is.na(HR_v_other) & !is.na(launch_angle_sc)) %>% 
#     filter(launch_angle_sc > 0 & launch_speed_sc >= 75)



statcast.data.2 <-
    statcast.data %>% 
    filter(!is.na(hit_distance_sc) | (is.na(hit_distance_sc) & is.na(launch_speed_sc))) %>%
    # filter(bb_type %in% c("fly_ball", "line_drive")) %>% 
    filter(bb_type %in% c("fly_ball")) %>% 
    filter(launch_angle_sc > 0) %>%
    # filter(!is.na(launch_angle_sc)) %>%
    # filter((launch_angle_sc > 0) | is.na(launch_angle_sc)) %>%
    mutate(HR_v_other = with(.,
        case_when(
            events == "home_run" ~ "home_run",
            TRUE ~ "other"
        ))) %>% 
    mutate(HR_v_other = recode_factor(HR_v_other, `0` = "other", `1` = "home_run")) %>% 
    
    mutate(HR_v_out = with(.,
        case_when(
            events == "home_run" ~ "home_run",
            events %in% c(
                "field_out",
                "sac_fly",
                "double_play",
                "field_error",
                "force_out",
                "sac_fly_double_play",
                "triple_play",
                "fielders_choice_out"
            ) ~ "out"
        ))) %>% 
    mutate(HR_v_out = recode_factor(HR_v_out, `0` = "out", `1` = "home_run")) %>% 
    
    mutate(HR_v_hit = with(.,
        case_when(
            events == "home_run" ~ "home_run",
            events %in% c("single", "double", "triple") ~ "hit"
        ))) %>% 
    mutate(HR_v_hit = recode_factor(HR_v_hit, `0` = "hit", `1` = "home_run")) %>% 
    
    mutate(HR_v_out_hit = with(.,
        case_when(
            events == "home_run" ~ "home_run",
            events %in% c("single", "double", "triple") ~ "hit",
            events %in% c(
                "field_out",
                "sac_fly",
                "double_play",
                "field_error",
                "force_out",
                "sac_fly_double_play",
                "triple_play",
                "fielders_choice_out"
            ) ~ "out"
        )))

statcast.data.2 %>% count(events) %>% arrange(desc(n))
statcast.data.2 %>% count(bb_type) %>% arrange(desc(n))
statcast.data.2 %>% count(description) %>% arrange(desc(n))
statcast.data.2 %>% count(HR_v_other) %>% arrange(desc(n))
statcast.data.2 %>% count(HR_v_out) %>% arrange(desc(n))
statcast.data.2 %>% count(HR_v_hit) %>% arrange(desc(n))
statcast.data.2 %>% count(HR_v_out_hit) %>% arrange(desc(n))


# outcome <- "HR_v_other"
# outcome <- "HR_v_out"
# outcome <- "HR_v_hit"
outcome <- "HR_v_out_hit"


hr_range <- statcast.data.2 %>% filter(events == "home_run") %>% with(., range(launch_angle_sc))



statcast.data.2 %>%
    filter(!is.na(geten(., outcome))) %>% 
    # filter(launch_angle_sc >= hr_range[1] &
    #         launch_angle_sc <= hr_range[2]) %>%
    group_by(outcome = geten(., outcome)) %>%
    summarise(hit_mean = median(launch_angle_sc, na.rm = TRUE)) %>% mutate_if(is.numeric, round, digits = 2)

hit_mean1 <-
    statcast.data.2 %>%
    filter(!is.na(HR_v_out_hit)) %>% 
    # filter(launch_angle_sc >= hr_range[1] &
    #         launch_angle_sc <= hr_range[2]) %>%     
    group_by(outcome = HR_v_out_hit, bb_type) %>%
    summarise(hit_mean = median(launch_angle_sc, na.rm = TRUE)) %>% mutate_if(is.numeric, round, digits = 2)

# clrs <- c("home_run" = "dodgerblue3", "other" = "green2")
# clrs <- c("home_run" = "dodgerblue3", "out" = "green2")
# clrs <- c("home_run" = "dodgerblue3", "hit" = "green2")
# clrs <- c("home_run" = "blue3", "hit" = "gray40", "out" = "red3")
clrs <- c("home_run" = "gray30", "hit" = "deeppink1", "out" = "blue3")


statcast.data.2 %>% 
    filter(!is.na(HR_v_out_hit)) %>% 
    ggplot(aes(launch_angle_sc)) + 
    geom_density(aes(fill = HR_v_out_hit, color = HR_v_out_hit), alpha = .325) +
    # coord_cartesian(xlim = c(0, 70), ylim = c(0, .08), expand = FALSE) +
    coord_cartesian(xlim = c(0, 70), ylim = c(0, .12), expand = FALSE) +
    geom_vline(
        aes(xintercept = hit_mean, color = outcome),
        data = hit_mean1,
        size = 1) +
    geom_text(
        # aes(label = hit_mean, color = outcome, x = c(35, 35), y = c(.061, .059)),
        # aes(label = hit_mean, color = outcome, x = c(35, 35, 35), y = c(.061, .059, .057)),
        aes(label = hit_mean, color = outcome, x = c(40, 40, 40, 40, 40, 40), y = c(.061, .059, .057, .061, .059, .057)),
        data = hit_mean1,
        inherit.aes = FALSE) +
    # scale_color_manual(values = clrs) +
    # scale_fill_manual(values = clrs)
    scale_color_manual(values = rep(clrs, 2)) +
    scale_fill_manual(values = rep(clrs, 2)) +
    facet_grid(~bb_type)




hit_mean2 <-
    statcast.data.2 %>%
    filter(!is.na(HR_v_out_hit)) %>% 
    # filter(bb_type == "fly_ball") %>% 
    group_by(outcome = HR_v_out_hit, bb_type) %>%
    summarise(hit_mean = median(launch_speed_sc, na.rm = TRUE)) %>% mutate_if(is.numeric, round, digits = 2)

statcast.data.2 %>% 
    # filter(bb_type == "fly_ball") %>% 
    filter(!is.na(HR_v_out_hit)) %>% 
    ggplot(aes(launch_speed_sc)) + 
    geom_density(aes(fill = HR_v_out_hit, color = HR_v_out_hit), alpha = .325) +
    coord_cartesian(xlim = c(60, 120), ylim = c(0, .1), expand = FALSE) +
    geom_vline(
        aes(xintercept = hit_mean, color = outcome),
        data = hit_mean2,
        size = 1) +
    geom_text(
        # aes(label = hit_mean, color = outcome, x = c(35, 35), y = c(.061, .059)),
        # aes(label = hit_mean, color = outcome, x = c(85, 85, 85), y = c(.075, .073, .071)),
        aes(label = hit_mean, color = outcome, x = c(85, 85, 85, 85, 85, 85), y = c(.075, .073, .071, .075, .073, .071)),
        data = hit_mean2,
        inherit.aes = FALSE) +
    # scale_color_manual(values = clrs) +
    # scale_fill_manual(values = clrs) +
    scale_color_manual(values = rep(clrs, 2)) +
    scale_fill_manual(values = rep(clrs, 2)) +
    facet_grid(facets = ~ bb_type)



statcast.data.df.2 %>% 
    filter(!is.na(launch_angle_sc)) %>% 
    filter(!is.na(hit_distance_sc)) %>%
    ggplot(aes(launch_angle_sc, launch_speed_sc)) +
    geom_point(
        alpha = .01,
        color = "blue",
        shape = 16,
        size = 2) + 
    theme_bw() + 
    scale_x_continuous(breaks = seq(-70, 70, 5)) + 
    scale_y_continuous(breaks = seq(35, 110, 5)) + 
    coord_cartesian(
        xlim = c(-70, 70),
        ylim = c(35, 110),
        expand = FALSE) + 
    # ggsave("with hit distance.png")
    ggsave("with no hit distance.png")



hit_mean3 <-
    statcast.data.df.2 %>%
    filter(!is.na(HR_v_out_hit)) %>% 
    group_by(outcome = HR_v_out_hit, bb_type) %>%
    summarise(
        speed = median(launch_speed_sc, na.rm = TRUE),
        angle = median(launch_angle_sc, na.rm = TRUE)
        ) %>% 
    mutate_if(is.numeric, round, digits = 2)

clrs <- c("home_run" = "gray30", "hit" = "deeppink1", "out" = "blue3")


statcast.data.2 %>% 
    filter(!is.na(HR_v_out_hit)) %>% 
    ggplot(aes(launch_speed_sc, launch_angle_sc)) +
    geom_hline(
        aes(yintercept = angle, color = outcome),
        data = hit_mean3,
        size = .75) +
    geom_vline(
        aes(xintercept = speed, color = outcome),
        data = hit_mean3,
        size = .75) +
    geom_density_2d(aes(color = HR_v_out_hit), size = 1) +
    scale_color_manual(values = rep(clrs, 2)) +
    facet_grid(~bb_type) #+
    # ggsave("hr outcome 2d density - bb_type.png")
    # ggsave("hr outcome 2d density - bb_type.png", width = 16, height = 7)




statcast.data %>% with(., hist(launch_speed_sc, breaks = seq(0,190,.5), xlim = c(20, 130)))
statcast.data %>% with(., hist(launch_angle_sc, breaks = seq(-90,90,1), xlim = c(-90, 90)))


statcast.data %>% 
    filter(!is.na(launch_angle_sc)) %>%
    filter(str_detect(description, "hit_into_play")) %>% 
    with(., table(dist_NA = is.na(hit_distance_sc), location_NA = is.na(hit_location)))


statcast.data %>% 
    filter(!is.na(launch_angle_sc)) %>%
    filter(str_detect(description, "hit_into_play")) %>% 
    with(., table(dist_NA = is.na(hit_distance_sc), location_NA = is.na(hc_x)))


statcast.data %>% 
    filter(!is.na(launch_angle_sc)) %>%
    filter(str_detect(description, "hit_into_play")) %>% 
    with(., table(hit_location_NA = is.na(hit_location), hc_x_NA = is.na(hc_x)))


statcast.data %>% 
    filter(!is.na(launch_angle_sc)) %>%
    with(., table(hit_into_play = str_detect(description, "hit_into_play"), hc_x_NA = is.na(hc_x)))


statcast.data.2 %>% count(HR_v_out_hit, bb_type)
statcast.data.2 %>% with(., table(HR_v_out_hit, bb_type))

statcast.data %>% 
    filter(str_detect(description, "hit_into_play"))


statcast.data %>% with(., table(is.na(launch_angle_sc), is.na(launch_speed_sc)))


statcast.data %>% 
    filter(!is.na(launch_angle_sc)) %>% 
    # filter(!is.na(hit_distance_sc)) %>% 
    nrow()

statcast.data %>% filter(!is.na(launch_speed_sc)) %>% filter(launch_speed_sc %% 1 == 0) %>% nrow()
statcast.data %>% filter(!is.na(launch_speed_sc)) %>% filter(launch_speed_sc %% 1 != 0) %>% nrow()

statcast.data %>% filter(!is.na(launch_angle_sc)) %>% filter(launch_angle_sc %% 1 == 0) %>% nrow()
statcast.data %>% filter(!is.na(launch_angle_sc)) %>% filter(launch_angle_sc %% 1 != 0) %>% nrow()



statcast.data %>% 
    filter(!is.na(hc_x)) %>% 
    with(., table(angle_round = launch_angle_sc %% 1 == 0, distance_NA = is.na(hit_distance_sc)))

statcast.data %>% 
    filter(!is.na(hc_x)) %>% 
    with(., table(speed_round = launch_speed_sc %% 1 == 0, distance_NA = is.na(hit_distance_sc)))

statcast.data %>% 
    with(., table(has_hc_x = !is.na(hc_x), has_distance = !is.na(hit_distance_sc)))

statcast.data %>% 
    with(., table(has_hc_x = !is.na(hc_x), game_year))

statcast.data %>% 
    with(., table(has_distance = !is.na(hit_distance_sc), game_year))

statcast.data %>% 
    with(., table(has_angle = !is.na(launch_angle_sc), game_year))

statcast.data %>% 
    with(., table(has_speed = !is.na(launch_speed_sc), game_year))

statcast.data %>% 
    with(., table(has_speed = !is.na(launch_speed_sc), has_distance = !is.na(hit_distance_sc)))




statcast.data %>% filter(is.na(launch_speed_sc)) %>% nrow()


statcast.data.2 %>% filter(launch_angle_sc > 0 & HR_v_other == "home_run") %>% 
    ggplot(aes(x = launch_speed_sc, y = launch_angle_sc)) + 
    geom_point(alpha = .25, color = "dodgerblue2") +
    geom_smooth(method = "lm")


statcast.data.2 %>% 
    filter(launch_angle_sc >= hr_range[1] &
            launch_angle_sc <= hr_range[2]) %>%     
    ggplot(aes(x = launch_speed_sc, y = launch_angle_sc)) + 
    geom_density_2d(n = 500, contour = TRUE) +
    scale_fill_viridis(option = "inferno") +
    facet_grid(facets = ~HR_v_other)





statcast.data.2 %>% 
    filter(launch_angle_sc >= hr_range[1] &
            launch_angle_sc <= hr_range[2]) %>% 
    ggplot(aes(x = launch_speed_sc, y = launch_angle_sc)) + 
    # geom_hex(bins = 50) +
    geom_hex(binwidth = c(3,3)) +
    scale_fill_viridis(option = "inferno") +
    facet_grid(facets = ~HR_v_other)






mod.1 <- statcast.data.2 %>% with(., glm(HR_v_out ~ launch_angle_sc, family = "binomial"))

tidy(mod.1, exponentiate = TRUE) %>% mutate_if(is.numeric, round, digits = 3)

visreg(mod.1, xvar = "launch_speed_sc", scale = "response", partial = TRUE, rug = 2)


class_df.1 <-
    data.frame(observed = model.frame(mod.1)$HR_v_out,
               predicted = cut(fitted.values(mod.1), 
                               breaks = c(-Inf, 0.5, Inf), labels = c("Out","Home Run")))

pred_table.1 <- table(class_df.1)
Correct <- round(c((diag(pred_table.1)/apply(pred_table.1, 1, sum)), 
             sum(diag(pred_table.1))/(sum(pred_table.1)))*100, 1)

pred_table.1.p <- cbind(rbind(pred_table.1, c(NA,NA)), Correct)

dimnames(pred_table.1.p) <- list(observed = c("Out","Home Run", ""),
                                 predicted = c("Out","Home Run","Correct %"))

print(pred_table.1.p)


roccurve.1 <-
    roc(
        model.frame(mod.1)$HR_v_out ~ predict(mod.1, type = "response"),
        grid = TRUE,
        plot = TRUE,
        print.thres = TRUE,
        print.auc = TRUE,
        main = "mod.1"
    )




mod.2 <- statcast.data.2 %>% with(., glm(HR_v_out ~ launch_angle_sc + launch_speed_sc, family = "binomial"))

tidy(mod.2, exponentiate = TRUE) %>% mutate_if(is.numeric, round, digits = 3)

visreg(mod.2, xvar = "launch_angle_sc", scale = "response", partial = TRUE, rug = 2)
visreg(mod.2, xvar = "launch_speed_sc", scale = "response", partial = TRUE, rug = 2)

class_df.2 <-
    data.frame(observed = model.frame(mod.2)$HR_v_out,
               predicted = cut(fitted.values(mod.2), 
                               breaks = c(-Inf, 0.5, Inf), labels = c("Out","Home Run")))

pred_table.2 <- table(class_df.2)
Correct <- round(c((diag(pred_table.2)/apply(pred_table.2, 1, sum)), 
             sum(diag(pred_table.2))/(sum(pred_table.2)))*100, 1)

pred_table.2.p <- cbind(rbind(pred_table.2, c(NA,NA)), Correct)

dimnames(pred_table.2.p) <- list(observed = c("Out","Home Run", ""),
                                 predicted = c("Out","Home Run","Correct %"))

print(pred_table.2.p)



roccurve.2 <-
    roc(
        model.frame(mod.2)$HR_v_out ~ predict(mod.2, type = "response"),
        grid = TRUE,
        plot = TRUE,
        print.thres = TRUE,
        print.auc = TRUE,
        main = "mod.2"
    )





mod.3 <- statcast.data.2 %>% with(., glm(HR_v_out ~ launch_angle_sc * launch_speed_sc, family = "binomial"))

tidy(mod.3, exponentiate = TRUE) %>% mutate_if(is.numeric, round, digits = 3)

visreg(mod.3, xvar = "launch_angle_sc", scale = "response", partial = TRUE, rug = 2)
visreg(mod.3, xvar = "launch_speed_sc", scale = "response", partial = TRUE, rug = 2)
visreg(mod.3, xvar = "launch_speed_sc", by = "launch_angle_sc", scale = "response", partial = TRUE, rug = 2, layout = c(3,1))
visreg(mod.3, xvar = "launch_angle_sc", by = "launch_speed_sc", scale = "response", partial = TRUE, rug = 2, layout = c(3,1))

class_df.3 <-
    data.frame(observed = model.frame(mod.3)$HR_v_out,
               predicted = cut(fitted.values(mod.3), 
                               breaks = c(-Inf, 0.35, Inf), labels = c("Out","Home Run")))

pred_table.3 <- table(class_df.3)
Correct <- round(c((diag(pred_table.3)/apply(pred_table.3, 1, sum)), 
             sum(diag(pred_table.3))/(sum(pred_table.3)))*100, 1)

pred_table.3.p <- cbind(rbind(pred_table.3, c(NA,NA)), Correct)

dimnames(pred_table.3.p) <- list(observed = c("Out","Home Run", ""),
                                 predicted = c("Out","Home Run","Correct %"))

print(pred_table.3.p)



roccurve.3 <-
    roc(
        model.frame(mod.3)$HR_v_out ~ predict(mod.3, type = "response"),
        grid = TRUE,
        plot = TRUE,
        print.thres = TRUE,
        print.auc = TRUE,
        main = "mod.3"
    )

anova(update(mod.2, data = model.frame(mod.3)), mod.3, test = "Chisq")




mod.4 <- statcast.data.2 %>% with(., glm(HR_v_out ~ launch_angle_sc + launch_speed_sc + I(launch_angle_sc^2), family = "binomial"))

tidy(mod.4, exponentiate = TRUE) %>% mutate_if(is.numeric, round, digits = 3)

visreg(mod.4, xvar = "launch_angle_sc", scale = "response", partial = TRUE, rug = 2)
visreg(mod.4, xvar = "launch_speed_sc", scale = "response", partial = TRUE, rug = 2)
visreg(mod.4, xvar = "launch_speed_sc", by = "launch_angle_sc", scale = "response", partial = TRUE, rug = 2, layout = c(3,1))
visreg(mod.4, xvar = "launch_angle_sc", by = "launch_speed_sc", scale = "response", partial = TRUE, rug = 2, layout = c(3,1))

class_df.4 <-
    data.frame(observed = model.frame(mod.4)$HR_v_out,
               predicted = cut(fitted.values(mod.4), 
                               breaks = c(-Inf, 0.5, Inf), labels = c("Out","Home Run")))

pred_table.4 <- table(class_df.4)
Correct <- round(c((diag(pred_table.4)/apply(pred_table.4, 1, sum)), 
             sum(diag(pred_table.4))/(sum(pred_table.4)))*100, 1)

pred_table.4.p <- cbind(rbind(pred_table.4, c(NA,NA)), Correct)

dimnames(pred_table.4.p) <- list(observed = c("Out","Home Run", ""),
                                 predicted = c("Out","Home Run","Correct %"))

print(pred_table.4.p)




roccurve.4 <-
    roc(
        model.frame(mod.4)$HR_v_out ~ predict(mod.4, type = "response"),
        grid = TRUE,
        plot = TRUE,
        print.thres = TRUE,
        print.auc = TRUE,
        main = "mod.4"
    )

anova(update(mod.3, data = model.frame(mod.4)), mod.4, test = "Chisq")



mod.5 <-
    statcast.data.2 %>% with(.,
        glm(
            HR_v_out ~ 
                launch_angle_sc * 
                launch_speed_sc + 
                I(launch_angle_sc ^ 2),
            family = "binomial"
        ))

tidy(mod.5, exponentiate = TRUE) %>% mutate_if(is.numeric, round, digits = 3)

visreg(mod.5, xvar = "launch_speed_sc", by = "launch_angle_sc")
visreg(mod.5, xvar = "launch_angle_sc", by = "launch_speed_sc")

class_df.5 <-
    data.frame(observed = model.frame(mod.5)$HR_v_out,
               predicted = cut(fitted.values(mod.5), 
                               breaks = c(-Inf, 0.5, Inf), labels = c("Out","Home Run")))

pred_table.5 <- table(class_df.5)
Correct <- round(c((diag(pred_table.5)/apply(pred_table.5, 1, sum)), 
             sum(diag(pred_table.5))/(sum(pred_table.5)))*100, 1)

pred_table.5.p <- cbind(rbind(pred_table.5, c(NA,NA)), Correct)

dimnames(pred_table.5.p) <- list(observed = c("Out","Home Run", ""),
                                 predicted = c("Out","Home Run","Correct %"))

print(pred_table.5.p)



roccurve.5 <-
    roc(
        model.frame(mod.5)$HR_v_out ~ predict(mod.5, type = "response"),
        grid = TRUE,
        plot = TRUE,
        print.thres = TRUE,
        print.auc = TRUE,
        main = "mod.5"
    )

anova(update(mod.4, data = model.frame(mod.5)), mod.5, test = "Chisq")



mod.6 <-
    statcast.data.2 %>% with(.,
        glm(
            HR_v_out ~ 
                launch_angle_sc * 
                launch_speed_sc + 
                I(launch_angle_sc ^ 2) +
                release_speed,
            family = "binomial"
        ))

tidy(mod.6, exponentiate = TRUE) %>% mutate_if(is.numeric, round, digits = 3)

visreg(mod.6, xvar = "launch_angle_sc", scale = "response", partial = TRUE, rug = 2)
visreg(mod.6, xvar = "launch_speed_sc", scale = "response", partial = TRUE, rug = 2)
visreg(mod.6, xvar = "launch_speed_sc", by = "launch_angle_sc", scale = "response", partial = TRUE, rug = 2, layout = c(3,1))
visreg(mod.6, xvar = "launch_angle_sc", by = "launch_speed_sc", scale = "response", partial = TRUE, rug = 2, layout = c(3,1))

class_df.6 <-
    data.frame(observed = model.frame(mod.6)$HR_v_out,
               predicted = cut(fitted.values(mod.6), 
                               breaks = c(-Inf, 0.122, Inf), labels = c("Out","Home Run")))

pred_table.6 <- table(class_df.6)
Correct <- round(c((diag(pred_table.6)/apply(pred_table.6, 1, sum)), 
             sum(diag(pred_table.6))/(sum(pred_table.6)))*100, 1)

pred_table.6.p <- cbind(rbind(pred_table.6, c(NA,NA)), Correct)

dimnames(pred_table.6.p) <- list(observed = c("Out","Home Run", ""),
                                 predicted = c("Out","Home Run","Correct %"))

print(pred_table.6.p)


roccurve.6 <-
    roc(
        model.frame(mod.6)$HR_v_out ~ predict(mod.6, type = "response"),
        grid = TRUE,
        plot = TRUE,
        print.thres = TRUE,
        print.auc = TRUE,
        main = "mod.6"
    )

anova(update(mod.5, data = model.frame(mod.6)), mod.6, test = "Chisq")





mod.7 <-
    statcast.data.2 %>% with(.,
        glm(
            HR_v_out ~ 
                launch_angle_sc * 
                launch_speed_sc + 
                I(launch_angle_sc ^ 2) +
                pitcher_throws,
            family = "binomial"
        ))

tidy(mod.7, exponentiate = TRUE) %>% mutate_if(is.numeric, round, digits = 3)

visreg(mod.7, xvar = "launch_angle_sc", scale = "response", partial = TRUE, rug = 2)
visreg(mod.7, xvar = "launch_speed_sc", scale = "response", partial = TRUE, rug = 2)
visreg(mod.7, xvar = "launch_speed_sc", by = "launch_angle_sc", scale = "response", partial = TRUE, rug = 2, layout = c(3,1))
visreg(mod.7, xvar = "launch_angle_sc", by = "launch_speed_sc", scale = "response", partial = TRUE, rug = 2, layout = c(3,1))

class_df.7 <-
    data.frame(observed = model.frame(mod.7)$HR_v_out,
               predicted = cut(fitted.values(mod.7), 
                               breaks = c(-Inf, 0.122, Inf), labels = c("Out","Home Run")))

pred_table.7 <- table(class_df.7)
Correct <- round(c((diag(pred_table.7)/apply(pred_table.7, 1, sum)), 
             sum(diag(pred_table.7))/(sum(pred_table.7)))*100, 1)

pred_table.7.p <- cbind(rbind(pred_table.7, c(NA,NA)), Correct)

dimnames(pred_table.7.p) <- list(observed = c("Out","Home Run", ""),
                                 predicted = c("Out","Home Run","Correct %"))

print(pred_table.7.p)


roccurve.7 <-
    roc(
        model.frame(mod.7)$HR_v_out ~ predict(mod.7, type = "response"),
        grid = TRUE,
        plot = TRUE,
        print.thres = TRUE,
        print.auc = TRUE,
        main = "mod.7"
    )

anova(update(mod.6, data = model.frame(mod.7)), mod.7, test = "Chisq")



###############################



mod.1.2 <- statcast.data.2 %>% with(., glm(HR_v_hit ~ launch_angle_sc, family = "binomial"))

tidy(mod.1.2, exponentiate = TRUE) %>% mutate_if(is.numeric, round, digits = 3)

visreg(mod.1.2, xvar = "launch_speed_sc", scale = "response", partial = TRUE, rug = 2)


class_df.1 <-
    data.frame(observed = model.frame(mod.1.2)$HR_v_hit,
               predicted = cut(fitted.values(mod.1.2), 
                               breaks = c(-Inf, 0.5, Inf), labels = c("Out","Home Run")))

pred_table.1 <- table(class_df.1)
Correct <- round(c((diag(pred_table.1)/apply(pred_table.1, 1, sum)), 
             sum(diag(pred_table.1))/(sum(pred_table.1)))*100, 1)

pred_table.1.p <- cbind(rbind(pred_table.1, c(NA,NA)), Correct)

dimnames(pred_table.1.p) <- list(observed = c("Out","Home Run", ""),
                                 predicted = c("Out","Home Run","Correct %"))

print(pred_table.1.p)


roccurve.1.2 <-
    roc(
        model.frame(mod.1.2)$HR_v_hit ~ predict(mod.1.2, type = "response"),
        grid = TRUE,
        plot = TRUE,
        print.thres = TRUE,
        print.auc = TRUE,
        main = "mod.1.2"
    )




mod.2.2 <- statcast.data.2 %>% with(., glm(HR_v_hit ~ launch_angle_sc + launch_speed_sc, family = "binomial"))

tidy(mod.2.2, exponentiate = TRUE) %>% mutate_if(is.numeric, round, digits = 3)

visreg(mod.2.2, xvar = "launch_angle_sc", scale = "response", partial = TRUE, rug = 2)
visreg(mod.2.2, xvar = "launch_speed_sc", scale = "response", partial = TRUE, rug = 2)

class_df.2 <-
    data.frame(observed = model.frame(mod.2.2)$HR_v_hit,
               predicted = cut(fitted.values(mod.2.2), 
                               breaks = c(-Inf, 0.5, Inf), labels = c("Out","Home Run")))

pred_table.2 <- table(class_df.2)
Correct <- round(c((diag(pred_table.2)/apply(pred_table.2, 1, sum)), 
             sum(diag(pred_table.2))/(sum(pred_table.2)))*100, 1)

pred_table.2.p <- cbind(rbind(pred_table.2, c(NA,NA)), Correct)

dimnames(pred_table.2.p) <- list(observed = c("Out","Home Run", ""),
                                 predicted = c("Out","Home Run","Correct %"))

print(pred_table.2.p)



roccurve.2.2 <-
    roc(
        model.frame(mod.2.2)$HR_v_hit ~ predict(mod.2.2, type = "response"),
        grid = TRUE,
        plot = TRUE,
        print.thres = TRUE,
        print.auc = TRUE,
        main = "mod.2.2"
    )


anova(mod.1.2, mod.2.2, test = "Chisq")




mod.3.2 <- statcast.data.2 %>% with(., glm(HR_v_hit ~ launch_angle_sc * launch_speed_sc, family = "binomial"))

tidy(mod.3.2, exponentiate = TRUE) %>% mutate_if(is.numeric, round, digits = 3)

visreg(mod.3.2, xvar = "launch_angle_sc", scale = "response", partial = TRUE, rug = 2)
visreg(mod.3.2, xvar = "launch_speed_sc", scale = "response", partial = TRUE, rug = 2)
visreg(mod.3.2, xvar = "launch_speed_sc", by = "launch_angle_sc", scale = "response", partial = TRUE, rug = 2, layout = c(3,1))
visreg(mod.3.2, xvar = "launch_angle_sc", by = "launch_speed_sc", scale = "response", partial = TRUE, rug = 2, layout = c(3,1))

class_df.3 <-
    data.frame(observed = model.frame(mod.3.2)$HR_v_hit,
               predicted = cut(fitted.values(mod.3.2), 
                               breaks = c(-Inf, 0.35, Inf), labels = c("Out","Home Run")))

pred_table.3 <- table(class_df.3)
Correct <- round(c((diag(pred_table.3)/apply(pred_table.3, 1, sum)), 
             sum(diag(pred_table.3))/(sum(pred_table.3)))*100, 1)

pred_table.3.p <- cbind(rbind(pred_table.3, c(NA,NA)), Correct)

dimnames(pred_table.3.p) <- list(observed = c("Out","Home Run", ""),
                                 predicted = c("Out","Home Run","Correct %"))

print(pred_table.3.p)



roccurve.3.2 <-
    roc(
        model.frame(mod.3.2)$HR_v_hit ~ predict(mod.3.2, type = "response"),
        grid = TRUE,
        plot = TRUE,
        print.thres = TRUE,
        print.auc = TRUE,
        main = "mod.3.2"
    )

anova(update(mod.2.2, data = model.frame(mod.3.2)), mod.3.2, test = "Chisq")




mod.4.2 <- statcast.data.2 %>% with(., glm(HR_v_hit ~ launch_angle_sc + launch_speed_sc + I(launch_angle_sc^2), family = "binomial"))

tidy(mod.4.2, exponentiate = TRUE) %>% mutate_if(is.numeric, round, digits = 3)

visreg(mod.4.2, xvar = "launch_angle_sc", scale = "response", partial = TRUE, rug = 2)
visreg(mod.4.2, xvar = "launch_speed_sc", scale = "response", partial = TRUE, rug = 2)
visreg(mod.4.2, xvar = "launch_speed_sc", by = "launch_angle_sc", scale = "response", partial = TRUE, rug = 2, layout = c(3,1))
visreg(mod.4.2, xvar = "launch_angle_sc", by = "launch_speed_sc", scale = "response", partial = TRUE, rug = 2, layout = c(3,1))

class_df.4 <-
    data.frame(observed = model.frame(mod.4.2)$HR_v_hit,
               predicted = cut(fitted.values(mod.4.2), 
                               breaks = c(-Inf, 0.5, Inf), labels = c("Out","Home Run")))

pred_table.4 <- table(class_df.4)
Correct <- round(c((diag(pred_table.4)/apply(pred_table.4, 1, sum)), 
             sum(diag(pred_table.4))/(sum(pred_table.4)))*100, 1)

pred_table.4.p <- cbind(rbind(pred_table.4, c(NA,NA)), Correct)

dimnames(pred_table.4.p) <- list(observed = c("Out","Home Run", ""),
                                 predicted = c("Out","Home Run","Correct %"))

print(pred_table.4.p)




roccurve.4.2 <-
    roc(
        model.frame(mod.4.2)$HR_v_hit ~ predict(mod.4.2, type = "response"),
        grid = TRUE,
        plot = TRUE,
        print.thres = TRUE,
        print.auc = TRUE,
        main = "mod.4.2"
    )

anova(update(mod.3.2, data = model.frame(mod.4.2)), mod.4.2, test = "Chisq")



mod.5.2 <-
    statcast.data.2 %>% with(.,
        glm(
            HR_v_hit ~ 
                launch_angle_sc * 
                launch_speed_sc + 
                I(launch_angle_sc ^ 2),
            family = "binomial"
        ))

tidy(mod.5.2, exponentiate = TRUE) %>% mutate_if(is.numeric, round, digits = 3)

visreg(mod.5.2, xvar = "launch_speed_sc", by = "launch_angle_sc")
visreg(mod.5.2, xvar = "launch_angle_sc", by = "launch_speed_sc")

class_df.5 <-
    data.frame(observed = model.frame(mod.5.2)$HR_v_hit,
               predicted = cut(fitted.values(mod.5.2), 
                               breaks = c(-Inf, 0.5, Inf), labels = c("Out","Home Run")))

pred_table.5 <- table(class_df.5)
Correct <- round(c((diag(pred_table.5)/apply(pred_table.5, 1, sum)), 
             sum(diag(pred_table.5))/(sum(pred_table.5)))*100, 1)

pred_table.5.p <- cbind(rbind(pred_table.5, c(NA,NA)), Correct)

dimnames(pred_table.5.p) <- list(observed = c("Out","Home Run", ""),
                                 predicted = c("Out","Home Run","Correct %"))

print(pred_table.5.p)



roccurve.5.2 <-
    roc(
        model.frame(mod.5.2)$HR_v_hit ~ predict(mod.5.2, type = "response"),
        grid = TRUE,
        plot = TRUE,
        print.thres = TRUE,
        print.auc = TRUE,
        main = "mod.5.2"
    )

anova(update(mod.3.2, data = model.frame(mod.5.2)), mod.5.2, test = "Chisq")



mod.6.2 <-
    statcast.data.2 %>% with(.,
        glm(
            HR_v_hit ~ 
                launch_angle_sc * 
                launch_speed_sc + 
                I(launch_angle_sc ^ 2) +
                release_speed,
            family = "binomial"
        ))

tidy(mod.6.2, exponentiate = TRUE) %>% mutate_if(is.numeric, round, digits = 3)

visreg(mod.6.2, xvar = "launch_angle_sc", scale = "response", partial = TRUE, rug = 2)
visreg(mod.6.2, xvar = "launch_speed_sc", scale = "response", partial = TRUE, rug = 2)
visreg(mod.6.2, xvar = "launch_speed_sc", by = "launch_angle_sc", scale = "response", partial = TRUE, rug = 2, layout = c(3,1))
visreg(mod.6.2, xvar = "launch_angle_sc", by = "launch_speed_sc", scale = "response", partial = TRUE, rug = 2, layout = c(3,1))

class_df.6 <-
    data.frame(observed = model.frame(mod.6.2)$HR_v_hit,
               predicted = cut(fitted.values(mod.6.2), 
                               breaks = c(-Inf, 0.122, Inf), labels = c("Out","Home Run")))

pred_table.6 <- table(class_df.6)
Correct <- round(c((diag(pred_table.6)/apply(pred_table.6, 1, sum)), 
             sum(diag(pred_table.6))/(sum(pred_table.6)))*100, 1)

pred_table.6.p <- cbind(rbind(pred_table.6, c(NA,NA)), Correct)

dimnames(pred_table.6.p) <- list(observed = c("Out","Home Run", ""),
                                 predicted = c("Out","Home Run","Correct %"))

print(pred_table.6.p)


roccurve.6.2 <-
    roc(
        model.frame(mod.6.2)$HR_v_hit ~ predict(mod.6.2, type = "response"),
        grid = TRUE,
        plot = TRUE,
        print.thres = TRUE,
        print.auc = TRUE,
        main = "mod.6.2"
    )

anova(update(mod.5.2, data = model.frame(mod.6.2)), mod.6.2, test = "Chisq")





mod.7.2 <-
    statcast.data.2 %>% with(.,
        glm(
            HR_v_hit ~ 
                launch_angle_sc * 
                launch_speed_sc + 
                I(launch_angle_sc ^ 2) +
                pitcher_throws,
            family = "binomial"
        ))

tidy(mod.7.2, exponentiate = TRUE) %>% mutate_if(is.numeric, round, digits = 3)

visreg(mod.7.2, xvar = "launch_angle_sc", scale = "response", partial = TRUE, rug = 2)
visreg(mod.7.2, xvar = "launch_speed_sc", scale = "response", partial = TRUE, rug = 2)
visreg(mod.7.2, xvar = "launch_speed_sc", by = "launch_angle_sc", scale = "response", partial = TRUE, rug = 2, layout = c(3,1))
visreg(mod.7.2, xvar = "launch_angle_sc", by = "launch_speed_sc", scale = "response", partial = TRUE, rug = 2, layout = c(3,1))

class_df.7 <-
    data.frame(observed = model.frame(mod.7.2)$HR_v_hit,
               predicted = cut(fitted.values(mod.7.2), 
                               breaks = c(-Inf, 0.122, Inf), labels = c("Out","Home Run")))

pred_table.7 <- table(class_df.7)
Correct <- round(c((diag(pred_table.7)/apply(pred_table.7, 1, sum)), 
             sum(diag(pred_table.7))/(sum(pred_table.7)))*100, 1)

pred_table.7.p <- cbind(rbind(pred_table.7, c(NA,NA)), Correct)

dimnames(pred_table.7.p) <- list(observed = c("Out","Home Run", ""),
                                 predicted = c("Out","Home Run","Correct %"))

print(pred_table.7.p)


roccurve.7.2 <-
    roc(
        model.frame(mod.7.2)$HR_v_hit ~ predict(mod.7.2, type = "response"),
        grid = TRUE,
        plot = TRUE,
        print.thres = TRUE,
        print.auc = TRUE,
        main = "mod.7.2"
    )

anova(update(mod.6.2, data = model.frame(mod.7.2)), mod.7.2, test = "Chisq")



###############################


statcast.data.3 <-
    statcast.data %>% 
    filter(!is.na(hit_distance_sc) & !is.na(launch_speed_sc)) %>% 
    filter(bb_type != "NA" & bb_type != "") %>%
    mutate(
        launch_angle_sc = Make.Z(launch_angle_sc),
        launch_speed_sc = Make.Z(launch_speed_sc),
        release_spin_rate = Make.Z(release_spin_rate),
        effective_speed = Make.Z(effective_speed),
        bb_type = as.factor(bb_type),
        game_year = as.factor(game_year),
        game_month_by_year = floor_date(game_date, "month") %>% as.factor()
        )

contrasts(statcast.data.3$bb_type) <- "contr.Sum"
contrasts(statcast.data.3$game_year) <- "contr.sdif"
contrasts(statcast.data.3$game_month_by_year) <- "contr.sdif"


###############################
    
    
    
    
mod.1.3 <-
    statcast.data.3 %>% with(., 
        glm(babip_value ~ 
                launch_angle_sc, 
            family = "binomial"))

tidy(mod.1.3, exponentiate = TRUE) %>% mutate_if(is.numeric, round, digits = 3)

visreg(mod.1.3, xvar = "launch_angle_sc", scale = "response", partial = TRUE, rug = 2)


system.time(roccurve.1.3 <-
    roc(
        model.frame(mod.1.3)$babip_value ~ predict(mod.1.3, type = "response"),
        grid = TRUE,
        plot = TRUE,
        print.thres = TRUE,
        print.auc = TRUE,
        main = "mod.1.3"
    ))




mod.2.3 <-
    statcast.data.3 %>% with(.,
        glm(babip_value ~ 
                launch_angle_sc + 
                launch_speed_sc, 
            family = "binomial"))

tidy(mod.2.3, exponentiate = TRUE) %>% mutate_if(is.numeric, round, digits = 3)

visreg(mod.2.3, xvar = "launch_angle_sc", scale = "response", partial = TRUE, rug = 2)
visreg(mod.2.3, xvar = "launch_speed_sc", scale = "response", partial = TRUE, rug = 2)

visreg(mod.2.3, xvar = "launch_angle_sc", scale = "linear", partial = TRUE, rug = 2)
visreg(mod.2.3, xvar = "launch_speed_sc", scale = "linear", partial = TRUE, rug = 2)



system.time(roccurve.2.3 <-
    roc(
        model.frame(mod.2.3)$babip_value ~ predict(mod.2.3, type = "response"),
        grid = TRUE,
        plot = FALSE,
        print.thres = TRUE,
        print.auc = TRUE,
        main = "mod.2.3"
    ))


anova(mod.1.3, mod.2.3, test = "Chisq")




mod.3.3 <-
    statcast.data.3 %>% with(.,
        glm(babip_value ~ 
                launch_angle_sc * 
                launch_speed_sc, 
            family = "binomial"))

tidy(mod.3.3, exponentiate = TRUE) %>% mutate_if(is.numeric, round, digits = 3)

visreg(mod.3.3, xvar = "launch_angle_sc", scale = "response", partial = TRUE, rug = 2)
visreg(mod.3.3, xvar = "launch_speed_sc", scale = "response", partial = TRUE, rug = 2)
visreg(mod.3.3, xvar = "launch_speed_sc", by = "launch_angle_sc", scale = "response", partial = TRUE, rug = 2, layout = c(3,1))
visreg(mod.3.3, xvar = "launch_angle_sc", by = "launch_speed_sc", scale = "response", partial = TRUE, rug = 2, layout = c(3,1))


system.time(
    roc(
        model.frame(mod.3.3)$babip_value ~ predict(mod.3.3, type = "response"),
        grid = TRUE,
        plot = TRUE,
        print.thres = TRUE,
        print.auc = TRUE,
        main = "mod.3.3"
    )
)

anova(update(mod.2.3, data = model.frame(mod.3.3)), mod.3.3, test = "Chisq")




mod.4.3 <-
    statcast.data.3 %>% with(.,
        glm(
            babip_value ~ 
                launch_angle_sc + 
                launch_speed_sc + 
                I(launch_angle_sc ^ 2),
            family = "binomial"
        ))

tidy(mod.4.3, exponentiate = TRUE) %>% mutate_if(is.numeric, round, digits = 3)

visreg(mod.4.3, xvar = "launch_angle_sc", scale = "response", partial = TRUE, rug = 2)
visreg(mod.4.3, xvar = "launch_speed_sc", scale = "response", partial = TRUE, rug = 2)
visreg(mod.4.3, xvar = "launch_speed_sc", by = "launch_angle_sc", scale = "response", partial = TRUE, rug = 2, layout = c(3,1))
visreg(mod.4.3, xvar = "launch_angle_sc", by = "launch_speed_sc", scale = "response", partial = TRUE, rug = 2, layout = c(3,1))



roccurve.4.3 <-
    roc(
        model.frame(mod.4.3)$babip_value ~ predict(mod.4.3, type = "response"),
        grid = TRUE,
        plot = TRUE,
        print.thres = TRUE,
        print.auc = TRUE,
        main = "mod.4.3"
    )

anova(update(mod.2.3, data = model.frame(mod.4.3)), mod.4.3, test = "Chisq")



mod.5.3 <-
    statcast.data.3 %>% with(.,
        glm(
            babip_value ~ 
                launch_angle_sc * 
                launch_speed_sc + 
                I(launch_angle_sc ^ 2),
            family = "binomial"
        ))

tidy(mod.5.3, exponentiate = TRUE) %>% mutate_if(is.numeric, round, digits = 3)
tidy(mod.5.3, exponentiate = FALSE) %>% mutate_if(is.numeric, round, digits = 3)

visreg(mod.5.3, xvar = "launch_speed_sc", by = "launch_angle_sc")
visreg(mod.5.3, xvar = "launch_angle_sc", by = "launch_speed_sc")
visreg(mod.5.3, xvar = "launch_speed_sc", by = "launch_angle_sc", scale = "response", partial = TRUE, rug = 2, layout = c(3,1))
visreg(mod.5.3, xvar = "launch_angle_sc", by = "launch_speed_sc", scale = "response", partial = TRUE, rug = 2, layout = c(3,1))
visreg(mod.5.3, xvar = "launch_speed_sc", by = "launch_angle_sc", scale = "linear", partial = TRUE, rug = 2, layout = c(3,1))
visreg(mod.5.3, xvar = "launch_angle_sc", by = "launch_speed_sc", scale = "linear", partial = TRUE, rug = 2, layout = c(3,1))


roccurve.5.3 <-
    roc(
        model.frame(mod.5.3)$babip_value ~ predict(mod.5.3, type = "response"),
        grid = TRUE,
        plot = TRUE,
        print.thres = TRUE,
        print.auc = TRUE,
        main = "mod.5.3"
    )

anova(update(mod.3.3, data = model.frame(mod.5.3)), mod.5.3, test = "Chisq")
anova(update(mod.4.3, data = model.frame(mod.5.3)), mod.5.3, test = "Chisq")



mod.6.3 <-
    statcast.data.3 %>% with(.,
        glm(
            babip_value ~ 
                launch_angle_sc * 
                launch_speed_sc + 
                I(launch_angle_sc ^ 2) +
                bb_type,
            family = "binomial"
        ))

tidy(mod.6.3, exponentiate = TRUE) %>% mutate_if(is.numeric, round, digits = 3)
tidy(mod.6.3, exponentiate = FALSE) %>% mutate_if(is.numeric, round, digits = 3)

visreg(mod.6.3, xvar = "launch_angle_sc", scale = "response", partial = TRUE, rug = 2)
visreg(mod.6.3, xvar = "launch_speed_sc", scale = "response", partial = TRUE, rug = 2)
visreg(mod.6.3, xvar = "launch_speed_sc", by = "launch_angle_sc", scale = "response", partial = TRUE, rug = 2, layout = c(3,1))
visreg(mod.6.3, xvar = "launch_angle_sc", by = "launch_speed_sc", scale = "response", partial = TRUE, rug = 2, layout = c(3,1))
visreg(mod.6.3, xvar = "launch_speed_sc", by = "bb_type", scale = "response", partial = TRUE, rug = 2, layout = c(3,1))
visreg(mod.6.3, xvar = "launch_angle_sc", by = "bb_type", scale = "response", partial = TRUE, rug = 2, layout = c(3,1))



    roc(
        model.frame(mod.6.3)$babip_value ~ predict(mod.6.3, type = "response"),
        grid = TRUE,
        plot = TRUE,
        print.thres = TRUE,
        print.auc = TRUE,
        main = "mod.6.3"
    )

anova(update(mod.5.3, data = model.frame(mod.6.3)), mod.6.3, test = "Chisq")





mod.7.3 <-
    statcast.data.3 %>% with(.,
        glm(
            babip_value ~ 
                launch_angle_sc * 
                launch_speed_sc + 
                I(launch_angle_sc ^ 2) +
                release_spin_rate,
            family = "binomial"
        ))

tidy(mod.7.3, exponentiate = TRUE) %>% mutate_if(is.numeric, round, digits = 3)

visreg(mod.7.3, xvar = "launch_angle_sc", scale = "response", partial = TRUE, rug = 2)
visreg(mod.7.3, xvar = "launch_speed_sc", scale = "response", partial = TRUE, rug = 2)
visreg(mod.7.3, xvar = "launch_speed_sc", by = "launch_angle_sc", scale = "response", partial = TRUE, rug = 2, layout = c(3,1))
visreg(mod.7.3, xvar = "launch_angle_sc", by = "launch_speed_sc", scale = "response", partial = TRUE, rug = 2, layout = c(3,1))


roccurve.7.3 <-
    roc(
        model.frame(mod.7.3)$babip_value ~ predict(mod.7.3, type = "response"),
        grid = TRUE,
        plot = TRUE,
        print.thres = TRUE,
        print.auc = TRUE,
        main = "mod.7.3"
    )

anova(update(mod.5.3, data = model.frame(mod.7.3)), mod.7.3, test = "Chisq")





mod.8.3 <-
    statcast.data.3 %>% with(.,
        glm(
            babip_value ~ 
                launch_angle_sc * 
                launch_speed_sc + 
                I(launch_angle_sc ^ 2) +
                effective_speed,
            family = "binomial"
        ))

tidy(mod.8.3, exponentiate = TRUE) %>% mutate_if(is.numeric, round, digits = 3)

visreg(mod.8.3, xvar = "launch_angle_sc", scale = "response", partial = TRUE, rug = 2)
visreg(mod.8.3, xvar = "launch_speed_sc", scale = "response", partial = TRUE, rug = 2)
visreg(mod.8.3, xvar = "launch_speed_sc", by = "launch_angle_sc", scale = "response", partial = TRUE, rug = 2, layout = c(3,1))
visreg(mod.8.3, xvar = "launch_angle_sc", by = "launch_speed_sc", scale = "response", partial = TRUE, rug = 2, layout = c(3,1))


roccurve.8.3 <-
    roc(
        model.frame(mod.8.3)$babip_value ~ predict(mod.8.3, type = "response"),
        grid = TRUE,
        plot = TRUE,
        print.thres = TRUE,
        print.auc = TRUE,
        main = "mod.8.3"
    )

anova(update(mod.5.3, data = model.frame(mod.8.3)), mod.8.3, test = "Chisq")





mod.9.3 <-
    statcast.data.3 %>% with(.,
        glm(
            babip_value ~ 
                launch_angle_sc * 
                launch_speed_sc + 
                I(launch_angle_sc ^ 2) +
                bb_type +
                release_spin_rate,
            family = "binomial"
        ))

tidy(mod.9.3, exponentiate = TRUE) %>% mutate_if(is.numeric, round, digits = 3)

visreg(mod.9.3, xvar = "launch_angle_sc", scale = "response", partial = TRUE, rug = 2)
visreg(mod.9.3, xvar = "launch_speed_sc", scale = "response", partial = TRUE, rug = 2)
visreg(mod.9.3, xvar = "launch_speed_sc", by = "launch_angle_sc", scale = "response", partial = TRUE, rug = 2, layout = c(3,1))
visreg(mod.9.3, xvar = "launch_angle_sc", by = "launch_speed_sc", scale = "response", partial = TRUE, rug = 2, layout = c(3,1))


roccurve.9.3 <-
    roc(
        model.frame(mod.9.3)$babip_value ~ predict(mod.9.3, type = "response"),
        grid = TRUE,
        plot = TRUE,
        print.thres = TRUE,
        print.auc = TRUE,
        main = "mod.9.3"
    )

anova(update(mod.5.3, data = model.frame(mod.9.3)), mod.9.3, test = "Chisq")
anova(update(mod.6.3, data = model.frame(mod.9.3)), mod.9.3, test = "Chisq")
anova(update(mod.7.3, data = model.frame(mod.9.3)), mod.9.3, test = "Chisq")




mod.10.3 <-
    statcast.data.3 %>% with(.,
        glm(
            babip_value ~ 
                launch_angle_sc * 
                launch_speed_sc + 
                I(launch_angle_sc ^ 2) +
                bb_type +
                release_spin_rate +
                effective_speed,
            family = "binomial"
        ))

tidy(mod.10.3, exponentiate = TRUE) %>% mutate_if(is.numeric, round, digits = 3)

visreg(mod.10.3, xvar = "launch_angle_sc", scale = "response", partial = TRUE, rug = 2)
visreg(mod.10.3, xvar = "launch_speed_sc", scale = "response", partial = TRUE, rug = 2)
visreg(mod.10.3, xvar = "launch_speed_sc", by = "launch_angle_sc", scale = "response", partial = TRUE, rug = 2, layout = c(3,1))
visreg(mod.10.3, xvar = "launch_angle_sc", by = "launch_speed_sc", scale = "response", partial = TRUE, rug = 2, layout = c(3,1))


roccurve.10.3 <-
    roc(
        model.frame(mod.10.3)$babip_value ~ predict(mod.10.3, type = "response"),
        grid = TRUE,
        plot = TRUE,
        print.thres = TRUE,
        print.auc = TRUE,
        main = "mod.10.3"
    )

anova(update(mod.5.3, data = model.frame(mod.10.3)), mod.10.3, test = "Chisq")
anova(update(mod.6.3, data = model.frame(mod.10.3)), mod.10.3, test = "Chisq")
anova(update(mod.7.3, data = model.frame(mod.10.3)), mod.10.3, test = "Chisq")
anova(update(mod.8.3, data = model.frame(mod.10.3)), mod.10.3, test = "Chisq")
anova(update(mod.9.3, data = model.frame(mod.10.3)), mod.10.3, test = "Chisq")

AIC(update(mod.5.3, data = model.frame(mod.10.3)),
    update(mod.6.3, data = model.frame(mod.10.3)),
    update(mod.7.3, data = model.frame(mod.10.3)),
    update(mod.8.3, data = model.frame(mod.10.3)),
    update(mod.9.3, data = model.frame(mod.10.3)),
    mod.10.3)



##############################################




mod.61.3 <-
    statcast.data.3 %>%
    with(.,
        glmer(
            babip_value ~ 
                launch_angle_sc * 
                launch_speed_sc + 
                I(launch_angle_sc ^ 2) +
                (1 | bb_type),
            family = "binomial"
        ))


tidy(mod.61.3) %>% mutate_if(is.numeric, round, digits = 3)


coef(mod.61.3) %>% 
    magrittr::extract2(1) %>% 
    as.data.frame(.) %>% 
    round(digits = 2) %>% 
    t(.) %>%
    as.data.frame() %>% 
    rownames_to_column() %>% 
    as_tibble()

fixef(mod.61.3) %>% 
    round(digits = 2) %>% 
    as.data.frame() %>% 
    rownames_to_column(.) %>% 
    as_tibble()

ranef(mod.61.3) %>% 
    magrittr::extract2(1) %>% 
    round(digits = 2) %>% 
    as.data.frame() %>% 
    rownames_to_column(.) %>% 
    as_tibble()


visreg(mod.61.3, xvar = "launch_angle_sc", scale = "response", partial = TRUE, rug = 0)
visreg(mod.61.3, xvar = "launch_angle_sc", by = "launch_speed_sc", scale = "response", partial = TRUE, rug = 0)
visreg(mod.61.3, xvar = "launch_angle_sc", by = "bb_type", scale = "response", partial = TRUE, rug = 0)
visreg(mod.61.3, xvar = "launch_speed_sc", by = "bb_type", scale = "response", partial = TRUE, rug = 0)

plot(mod.61.3)

roccurve.61.3 <-
    roc(
        model.frame(mod.61.3)$babip_value ~ predict(mod.61.3, type = "response"),
        grid = TRUE,
        plot = TRUE,
        print.thres = TRUE,
        print.auc = TRUE,
        main = "mod.61.3"
    )

REsim(mod.61.3, n.sims = 500) %>% plotREsim(stat = "median", labs = TRUE)



mod.61.4 <-
    statcast.data.3 %>%
    with(.,
        glmer(
            babip_value ~ 
                launch_angle_sc * 
                launch_speed_sc + 
                I(launch_angle_sc ^ 2) +
                (launch_angle_sc | bb_type),
            family = "binomial"
        ))


tidy(mod.61.4) %>% mutate_if(is.numeric, round, digits = 3)


coef(mod.61.4) %>% 
    magrittr::extract2(1) %>% 
    as.data.frame(.) %>% 
    round(digits = 2) %>% 
    t(.) %>% 
    as.data.frame() %>% 
    rownames_to_column() %>% 
    as_tibble()

fixef(mod.61.4) %>% 
    round(digits = 2) %>% 
    as.data.frame() %>% 
    rownames_to_column(.) %>% 
    as_tibble()

ranef(mod.61.4) %>% 
    magrittr::extract2(1) %>% 
    round(digits = 2) %>% 
    as.data.frame() %>% 
    rownames_to_column(.) %>% 
    as_tibble()


visreg(mod.61.4, xvar = "launch_angle_sc", scale = "response", partial = TRUE, rug = 0)
visreg(mod.61.4, xvar = "launch_angle_sc", by = "launch_speed_sc", scale = "response", partial = TRUE, rug = 0)
visreg(mod.61.4, xvar = "launch_speed_sc", by = "launch_angle_sc", scale = "response", partial = TRUE, rug = 0)
visreg(mod.61.4, xvar = "launch_angle_sc", by = "bb_type", scale = "response", partial = TRUE, rug = 0)
visreg(mod.61.4, xvar = "launch_speed_sc", by = "bb_type", scale = "response", partial = TRUE, rug = 0)

AIC(mod.5.3, mod.6.3, mod.61.3, mod.61.4)

roccurve.61.4 <-
    roc(
        model.frame(mod.61.4)$babip_value ~ predict(mod.61.4, type = "response"),
        grid = TRUE,
        plot = TRUE,
        print.thres = TRUE,
        print.auc = TRUE,
        main = "mod.61.4"
    )

REsim(mod.61.4, n.sims = 500) %>% plotREsim(stat = "median", labs = TRUE)




mod.61.5 <-
    statcast.data.3 %>%
    with(.,
        glmer(
            babip_value ~ 
                launch_angle_sc * 
                launch_speed_sc + 
                I(launch_angle_sc ^ 2) +
                (launch_angle_sc + I(launch_angle_sc ^ 2) | bb_type),
            family = "binomial"
        ))


tidy(mod.61.5) %>% mutate_if(is.numeric, round, digits = 3)


coef(mod.61.5) %>% 
    magrittr::extract2(1) %>% 
    as.data.frame(.) %>% 
    round(digits = 2) %>% 
    t(.) %>% 
    as.data.frame() %>% 
    rownames_to_column() %>% 
    as_tibble()

fixef(mod.61.5) %>% 
    round(digits = 2) %>% 
    as.data.frame() %>% 
    rownames_to_column(.) %>% 
    as_tibble()

ranef(mod.61.5) %>% 
    magrittr::extract2(1) %>% 
    round(digits = 2) %>% 
    as.data.frame() %>% 
    rownames_to_column(.) %>% 
    as_tibble()


visreg(mod.61.5, xvar = "launch_angle_sc", scale = "response", partial = TRUE, rug = 0)
visreg(mod.61.5, xvar = "launch_angle_sc", by = "launch_speed_sc", scale = "response", partial = TRUE, rug = 0)
visreg(mod.61.5, xvar = "launch_speed_sc", by = "launch_angle_sc", scale = "response", partial = TRUE, rug = 0)
visreg(mod.61.5, xvar = "launch_angle_sc", by = "bb_type", scale = "response", partial = TRUE, rug = 0)
visreg(mod.61.5, xvar = "launch_speed_sc", by = "bb_type", scale = "response", partial = TRUE, rug = 0)

AIC(mod.5.3, mod.6.3, mod.61.3, mod.61.4, mod.61.5)

roccurve.61.5 <-
    roc(
        model.frame(mod.61.5)$babip_value ~ predict(mod.61.5, type = "response"),
        grid = TRUE,
        plot = TRUE,
        print.thres = TRUE,
        print.auc = TRUE,
        main = "mod.61.5"
    )

REsim(mod.61.5, n.sims = 500) %>% plotREsim(stat = "median", labs = TRUE)


############################################


mod.62.1 <- 
    statcast.data.3 %>%
    with(.,
        glmer(
            babip_value ~ 
                launch_angle_sc * 
                launch_speed_sc + 
                I(launch_angle_sc ^ 2) +
                (launch_angle_sc * launch_speed_sc + I(launch_angle_sc ^ 2) | game_year),
            family = "binomial"
        ))


tidy(mod.62.1) %>% mutate_if(is.numeric, round, digits = 2)


coef(mod.62.1) %>% 
    magrittr::extract2(1) %>% 
    as.data.frame(.) %>% 
    round(digits = 2) %>% 
    t(.) %>%
    as.data.frame() %>% 
    rownames_to_column() %>% 
    as_tibble()

fixef(mod.62.1) %>% 
    round(digits = 2) %>% 
    as.data.frame() %>% 
    rownames_to_column(.) %>% 
    as_tibble()

ranef(mod.62.1) %>% 
    magrittr::extract2(1) %>% 
    round(digits = 2) %>% 
    as.data.frame() %>% 
    rownames_to_column(.) %>% 
    as_tibble()


visreg(mod.62.1, xvar = "launch_angle_sc", scale = "response", partial = TRUE, rug = 0)
visreg(mod.62.1, xvar = "launch_angle_sc", by = "launch_speed_sc", scale = "response", partial = TRUE, rug = 0)
visreg(mod.62.1, xvar = "launch_angle_sc", by = "bb_type", scale = "response", partial = TRUE, rug = 0)
visreg(mod.62.1, xvar = "launch_speed_sc", by = "bb_type", scale = "response", partial = TRUE, rug = 0)


REsim(mod.62.1, n.sims = 500) %>% plotREsim(stat = "median", labs = TRUE)


roccurve.62.1 <-
    roc(
        model.frame(mod.62.1)$babip_value ~ predict(mod.62.1, type = "response"),
        grid = TRUE,
        plot = TRUE,
        print.thres = TRUE,
        print.auc = TRUE,
        main = "mod.62.1"
    )



############################################


mod.62.2 <- 
    statcast.data.3 %>%
    with(.,
        glmer(
            babip_value ~ 
                launch_angle_sc * 
                launch_speed_sc + 
                I(launch_angle_sc ^ 2) +
                (1 | game_month_by_year),
            family = "binomial"
        ))


tidy(mod.62.2) %>% mutate_if(is.numeric, round, digits = 2)


coef(mod.62.2) %>% 
    magrittr::extract2(1) %>% 
    as.data.frame(.) %>% 
    round(digits = 2) %>% 
    # t(.) %>%
    as.data.frame() %>% 
    rownames_to_column() %>% 
    as_tibble()

fixef(mod.62.2) %>% 
    round(digits = 2) %>% 
    as.data.frame() %>% 
    rownames_to_column(.) %>% 
    as_tibble()

ranef(mod.62.2) %>% 
    magrittr::extract2(1) %>% 
    round(digits = 2) %>% 
    as.data.frame() %>% 
    rownames_to_column(.) %>% 
    as_tibble()


visreg(mod.62.2, xvar = "launch_angle_sc", scale = "response", partial = TRUE, rug = 0)
visreg(mod.62.2, xvar = "launch_angle_sc", by = "launch_speed_sc", scale = "response", partial = TRUE, rug = 0)
visreg(mod.62.2, xvar = "launch_angle_sc", by = "game_month_by_year", scale = "response", partial = TRUE, rug = 0)
visreg(mod.62.2, xvar = "launch_speed_sc", by = "game_month_by_year", scale = "response", partial = TRUE, rug = 0)


REsim(mod.62.2, n.sims = 500) %>% plotREsim(stat = "median", labs = TRUE)


roc(
    model.frame(mod.62.2)$babip_value ~ predict(mod.62.2, type = "response"),
    grid = TRUE,
    plot = TRUE,
    print.thres = TRUE,
    print.auc = TRUE,
    main = "mod.62.2"
)



#############################

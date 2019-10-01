###########################################################################################-
###########################################################################################-
##
## Batted ball distance for every speed x angle combination ----
##
###########################################################################################-
###########################################################################################-

#=========================================================================================#
# Setting up ----
#=========================================================================================#

#-----------------------------------------------------------------------------------------#
# Loading libraries
#-----------------------------------------------------------------------------------------#

library(tidyverse)
library(dbplyr)
library(DBI)
library(tibbletime)
library(glue)
library(RSQLite)
library(lubridate)
library(splines)
library(tictoc)
library(broom)
library(broom.mixed)
library(lme4)

#-----------------------------------------------------------------------------------------#
# Loading custom functions
#-----------------------------------------------------------------------------------------#

source("code/functions/se_funs.R")

#-----------------------------------------------------------------------------------------#
# Connecting to database
#-----------------------------------------------------------------------------------------#

statcast_db <- dbConnect(SQLite(), "data/statcast_db_rebuilt.sqlite3")

#-----------------------------------------------------------------------------------------#
# Pulling data from database
#-----------------------------------------------------------------------------------------#

statcast_data <- 
    tbl(statcast_db, "statcast_data") %>% 
    filter(!is.na(events) & game_type == "R") %>% 
    select(
        game_date, 
        game_year,
        events,
        type,
        bb_type,
        batter,
        launch_speed,
        launch_angle,
        hit_distance_sc
    ) %>% 
    collect()

dbDisconnect(statcast_db)

#=========================================================================================#
# Hit distance ----
#=========================================================================================#

#-----------------------------------------------------------------------------------------#
# Subsetting data
#-----------------------------------------------------------------------------------------#

hits_data <- 
    statcast_data %>%
    filter(type == "X") %>%
    drop_na(hit_distance_sc, launch_speed, launch_angle) %>% 
    filter(hit_distance_sc >= 0) %>%
    mutate(
        launch_speed = plyr::round_any(launch_speed, accuracy = 1, f = floor),
    ) %>% 
    select(hit_distance = hit_distance_sc, launch_speed, launch_angle, game_year) %>% 
    mutate(
        launch_speed_10 = plyr::round_any(launch_speed, accuracy = 10, f = floor),
        launch_angle_10 = plyr::round_any(launch_angle, accuracy = 10, f = floor)
    )

rm(statcast_data)
gc()

#=========================================================================================#
# Hit distance ----
#=========================================================================================#

speed_angle_distance <- 
    hits_data %>% 
    group_by(launch_speed, launch_angle) %>% 
    summarise(
        distance_mean = mean(hit_distance),
        distance_se = se(hit_distance),
        distance_median = median(hit_distance),
        hit_count = n()
    ) %>% 
    ungroup() %>% 
    mutate() %>% 
    filter(hit_count > 1) %>% 
    mutate(
        launch_speed_10 = plyr::round_any(launch_speed, accuracy = 10, f = floor),
        launch_angle_10 = plyr::round_any(launch_angle, accuracy = 10, f = floor)
    ) %>% 
    group_by(launch_speed_10) %>% 
    group_modify(~ mutate(.x, hit_count_nrm = hit_count/max(hit_count))) %>% 
    ungroup()


#=========================================================================================#
# Plot ----
#=========================================================================================#

angle_x_distance_plot <- 
    speed_angle_distance %>% 
    filter(launch_angle_10 > -40, launch_speed_10 > 10) %>% 
    ggplot(aes(launch_angle, distance_mean)) +
    geom_point(aes(color = hit_count_nrm), shape = 16, size = .325) +
    theme_dark() +
    scale_color_viridis_c(option = "D", name = "Normalized\nCount*", breaks = seq(0, 1, .25), limits = c(0, 1)) +
    scale_x_continuous(breaks = seq(-30, 90, 30)) +
    coord_fixed(ratio = .25, xlim = c(-40, 100), ylim = c(0, 500), expand = FALSE) +
    geom_vline(xintercept = 30, color = "white", size = .5, alpha = .75) +
    facet_grid(cols = vars(Speed = launch_speed_10), labeller = label_both) +
    labs(
        title = "Effect of launch angle and exit speed on batted ball distance", 
        subtitle = "Grouped into 10 mph exit speed bins",
        caption = 
            glue(
                "Note: Each point represents a launch angle x exit speed combination, rounded down to the nearest integer.\n",
                "*Normalized Count is the ratio of batted balls at that point, versus the maximum count within that exit ",
                "speed bin")
    ) +
    xlab("Launch angle") +
    ylab("Batted ball distance") +
    theme(panel.grid.minor = element_blank())


ggsave(
    plot = angle_x_distance_plot,
    "plots/angle_x_distance_plot.png",
    width = 11,
    height = 2.25,
    dpi = 250, 
    scale = 1.25
)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# #
# #                             ---- THIS IS THE END! ----
# #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

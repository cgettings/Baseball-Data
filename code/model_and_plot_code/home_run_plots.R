###########################################-
###########################################-
##
## Home run trends ----
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
# Pulling data from database ----
#--------------------------------#

hr_data <- 
    tbl(statcast_db, "statcast_data_updated") %>% 
    filter(!is.na(events) & game_type == "R") %>% 
    select(game_date, 
           game_year,
           at_bat_number,
           # pitch_number,
           events,
           type,
           player_name,
           batter_id_sc,
           launch_speed_sc,
           launch_angle_sc,
           estimated_woba_using_speedangle) %>% 
    collect()

most_recent_day <-
    hr_data %>%
    pull(game_date) %>% 
    sort(decreasing = TRUE) %>% 
    extract2(1) %>% 
    as_date()

#--------------------------------#
# Calculating home run rate ----
#--------------------------------#

hr_data_2 <- 
    hr_data %>%
    mutate(game_date = as_date(game_date)) %>% 
    arrange(game_date) %>% 
    as_tbl_time(index = game_date) %>% 
    mutate(partition_index = partition_index(game_date, "weekly")) %>% 
    mutate(
        HR_v_other_pa = case_when(
            events == "home_run" ~ 1L, 
            TRUE ~ 0L),
        HR_v_other_bb = case_when(
            events == "home_run" ~ 1L, 
            events != "home_run" & type == "X" ~ 0L,
            TRUE ~ NA_integer_)
    )

hr_rate <- 
    hr_data_2 %>% 
    group_by(partition_index) %>% 
    summarise(
        weekly_hr_per_pa = mean(HR_v_other_pa, na.rm = TRUE),
        weekly_hr_per_bb = mean(HR_v_other_bb, na.rm = TRUE),
        weekly_launch_speed = mean(launch_speed_sc, na.rm = TRUE),
        weekly_launch_angle = mean(launch_angle_sc, na.rm = TRUE)
    ) %>% 
    left_join(
        .,
        hr_data_2 %>% 
            select(game_date, partition_index) %>% 
            distinct(partition_index, .keep_all = TRUE)
    )


hr_rate_yearly <- 
    hr_data_2 %>% 
    group_by(game_year) %>% 
    summarise(
        yearly_hr_per_pa    = mean(HR_v_other_pa, na.rm = TRUE),
        yearly_hr_per_pa_se = se(HR_v_other_pa, na.rm = TRUE),
        yearly_hr_per_bb    = mean(HR_v_other_bb, na.rm = TRUE),
        yearly_hr_per_bb_se = se(HR_v_other_bb, na.rm = TRUE)
    )


#================================#
# Plotting home run rate ----
#================================#

#--------------------------------#
# Per plate appearance ----
#--------------------------------#

plot_1 <- 
    hr_rate %>% 
    ggplot(aes(x = game_date, y = weekly_hr_per_pa)) + 
    geom_smooth(
        method = "loess",
        span = 0.25,
        size = 0.75,
        color = "dodgerblue3",
        fill = "dodgerblue2",
        alpha = .125
    ) +
    geom_point(shape = 16, color = "gray40") +
    geom_rect(
        data = hr_rate_yearly,
        aes(
            xmin = as_date(paste0(game_year, "01-01")),
            xmax = as_date(paste0(game_year, "12-31")),
            ymin = yearly_hr_per_pa - yearly_hr_per_pa_se,
            ymax = yearly_hr_per_pa + yearly_hr_per_pa_se
        ),
        fill = "red",
        alpha = .175,
        inherit.aes = FALSE
    ) +
    geom_segment(
        data = hr_rate_yearly,
        aes(
            x = as_date(paste0(game_year, "01-01")),
            xend = as_date(paste0(game_year, "12-31")),
            y = yearly_hr_per_pa,
            yend = yearly_hr_per_pa
        ),
        color = "red2",
        alpha = .875,
        size = 1,
        inherit.aes = FALSE
    ) +
    labs(
        title = "Weekly home runs per batted ball",
        subtitle = glue("As of {format(most_recent_day, '%m/%d/%Y')}"),
        x = "Date",
        y = "HR Rate",
        caption = "Note: red lines are yearly average; shaded areas are \u00B1 1 S.E."
    ) +
    scale_x_date(date_breaks = "year", date_minor_breaks = "month", date_labels = "%Y") +
    scale_y_continuous(breaks = seq(0.01, 0.06, 0.01)) +
    theme_minimal() +
    coord_cartesian(ylim = c(.01, .06)) +
    theme(
        axis.text.x = element_text(angle = 0),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(color = "black", size = .25, fill = NA))


ggsave(
    plot = plot_1,
    glue("./plots/home_run_rate_pa_2008_2018_1.png"),
    width = 14,
    height = 8,
    dpi = 250
)

ggsave(
    plot = plot_1,
    glue("./plots/home_run_rate_pa_2008_2018_{most_recent_day}.png"),
    width = 14,
    height = 8,
    dpi = 250
)


#--------------------------------#
# Per batted ball ----
#--------------------------------#

plot_2 <- 
    hr_rate %>% 
    ggplot(aes(x = game_date, y = weekly_hr_per_bb)) + 
    geom_smooth(
        method = "loess",
        span = 0.25,
        size = 0.75,
        color = "dodgerblue3",
        fill = "dodgerblue2",
        alpha = .125
    ) +
    geom_point(shape = 16, color = "gray40") +
    geom_rect(
        data = hr_rate_yearly,
        aes(
            xmin = as_date(paste0(game_year, "01-01")),
            xmax = as_date(paste0(game_year, "12-31")),
            ymin = yearly_hr_per_bb - yearly_hr_per_bb_se,
            ymax = yearly_hr_per_bb + yearly_hr_per_bb_se
        ),
        fill = "red",
        alpha = .175,
        inherit.aes = FALSE
    ) +
    geom_segment(
        data = hr_rate_yearly,
        aes(
            x = as_date(paste0(game_year, "01-01")),
            xend = as_date(paste0(game_year, "12-31")),
            y = yearly_hr_per_bb,
            yend = yearly_hr_per_bb
        ),
        color = "red2",
        alpha = .875,
        size = 1,
        inherit.aes = FALSE
    ) +
    labs(
        title = "Weekly home runs per batted ball",
        subtitle = glue("As of {format(most_recent_day, '%m/%d/%Y')}"),
        x = "Date",
        y = "HR Rate",
        caption = "Note: red lines are yearly average; shaded areas are \u00B1 1 S.E."
    ) +
    scale_x_date(date_breaks = "year", date_minor_breaks = "month", date_labels = "%Y") +
    scale_y_continuous(breaks = seq(0.01, 0.06, 0.01)) +
    theme_minimal() +
    coord_cartesian(ylim = c(.01, .06)) +
    theme(
        axis.text.x = element_text(angle = 0),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(color = "black", size = .25, fill = NA))

ggsave(
    plot = plot_2,
    glue("./plots/home_run_rate_bb_2008_2018_1.png"),
    width = 14,
    height = 8,
    dpi = 250
)

ggsave(
    plot = plot_2,
    glue("./plots/home_run_rate_bb_2008_2018_{most_recent_day}.png"),
    width = 14,
    height = 8,
    dpi = 250
)


##################################
##################################

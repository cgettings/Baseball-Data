###########################################-
###########################################-
##
## Barrels for hot hand - PLOT ----
##
###########################################-
###########################################-

#=========================#
# Setting up ----
#=========================#

#-------------------------#
# Loading libraries ----
#-------------------------#

library(tidyverse)
library(lubridate)
library(stringdist)
library(magrittr)
library(dbplyr)
library(DBI)
library(glue)

source("./code/functions/se_funs.R")

#--------------------------------#
# Connecting to database ----
#--------------------------------#

statcast_db <- dbConnect(RSQLite::SQLite(), "./data/statcast_db.sqlite3")

#--------------------------------#
# Pulling data from database ----
#--------------------------------#

barrel_data <-
    tbl(statcast_db, "statcast_data_updated") %>%
    filter(!is.na(events) & game_type == "R") %>% 
    select(game_date, 
           game_year,
           at_bat_number,
           events,
           type,
           player_name,
           batter_id_sc,
           estimated_woba_using_speedangle) %>% 
    collect() %>% 
    mutate(
        game_date = as_date(game_date),
        barrel = case_when(
            estimated_woba_using_speedangle > .950 ~ 1L, 
            NA ~ NA_integer_, 
            TRUE ~ 0L))


#--------------------------------#
# Cleaning data ----
#--------------------------------#

most_recent_day <-
    barrel_data %>%
    pull(game_date) %>% 
    sort(decreasing = TRUE) %>% 
    extract2(1) %>% 
    as_date()


barrel_count <- 
    barrel_data %>% 
    filter(game_year == 2018) %>% 
    group_by(batter_id_sc) %>% 
    count(barrel) %>% 
    arrange(-barrel, -n) %>% 
    left_join(
        .,
        barrel_data %>% filter(game_year == 2018) %>% select(player_name, batter_id_sc) %>% distinct()
    )


barrel_count_players <- 
    barrel_count %>% 
    filter(barrel == 1) %>% 
    select(batter_id_sc) %>% 
    ungroup() %>% 
    distinct()


barrel_data_2_1 <- 
    barrel_data %>% 
    filter(game_year == 2018) %>% 
    inner_join(., barrel_count_players) %>% 
    mutate(player_name = as_factor(player_name)) %>% 
    replace_na(list(estimated_woba_using_speedangle = 0)) %>% 
    mutate(
        player_name = fct_reorder(
            .f = player_name, 
            .x = estimated_woba_using_speedangle, 
            .fun = mean,
            .desc = TRUE,
            na.rm = TRUE)
    )


barrel_data_3_1 <- 
    barrel_data_2_1 %>% 
    group_by(player_name, game_date) %>% 
    summarise(xwoba_mean_daily = mean(estimated_woba_using_speedangle)) %>% 
    ungroup()


barrel_data_4_1 <- 
    barrel_data_2_1 %>% 
    group_by(player_name) %>% 
    summarise(
        xwoba_mean_player = mean(estimated_woba_using_speedangle, na.rm = TRUE),
        n = n()
    ) %>%
    ungroup() %>% 
    filter(n >= 150)


barrel_data_5_1 <- 
    barrel_data_4_1 %>% 
    arrange(-xwoba_mean_player) %>% 
    filter(row_number() %in% c(1:20))


barrel_data_2_1_d <- barrel_data_2_1 %>% filter(player_name %in% unique(barrel_data_5_1$player_name))
barrel_data_3_1_d <- barrel_data_3_1 %>% filter(player_name %in% unique(barrel_data_5_1$player_name))
barrel_data_4_1_d <- barrel_data_4_1 %>% filter(player_name %in% unique(barrel_data_5_1$player_name))

overall_xwoba_df_1 <- barrel_data_2_1_d %>% summarise(overall_xwoba = mean(estimated_woba_using_speedangle))


######################

## Mean estimated wOBA over time, for all at-bats, including walks and sacrifices


plot1 <- 
    barrel_data_3_1_d %>% 
    ggplot(aes(x = game_date, y = xwoba_mean_daily)) + 
    
    # geom_hline(
    #     yintercept = 0.0,
    #     color = "gray20",
    #     size = 0.375,
    #     linetype = 1
    # ) +
    geom_hline(
        yintercept = 0.950, 
        color = "black",
        linetype = 2,
        size = 0.5
    ) +
    
    geom_point(
        data = barrel_data_2_1_d,
        aes(x = game_date, y = estimated_woba_using_speedangle),
        shape = 1,
        size = 1,
        color = "gray50",
        alpha = .7
    ) +
    
    geom_line(
        size = 0.375, 
        color = "gray50"
    ) +
    
    geom_point(
        size = 1.25,
        shape = 16,
        color = "gray30"
    ) +
    
    geom_smooth(
        method = "loess",
        span = 0.5,
        size = 0.75,
        alpha = 0.125,
        color = "dodgerblue3",
        fill = "dodgerblue"
    ) +
    
    geom_hline(
        data = barrel_data_4_1_d,
        aes(yintercept = xwoba_mean_player),
        color = "red",
        size = 0.65,
        linetype = 1
    ) +
    
    geom_hline(
        data = overall_xwoba_df_1,
        aes(yintercept = overall_xwoba),
        color = "black",
        size = 0.65,
        linetype = 1
    ) +
    
    facet_wrap(~player_name, ncol = 5) +
    labs(
        title = glue("Mean daily expected weighted on-base average (xwOBA), ",
                     "based on each plate appearance's xwOBA (top-20 / min. 150 PAs)"),
        subtitle = glue("As of {format(most_recent_day, '%m/%d/%Y')}"),
        x = "Date",
        y = "xwOBA",
        caption = 'Note: dashed line indicates an xwOBA of 0.950, i.e. a "barrel"; black line is MLB season average xwOBA; red line is player\'s season average xwOBA'
    ) +
    coord_cartesian(ylim = c(0, 2), expand = TRUE) +
    theme_minimal() +
    theme(
        axis.text.x = element_text(angle = 0)
    )

ggsave(
    plot = plot1,
    "./plots/woba_daily_mean_top20_2018_1.png",
    width = 14,
    height = 8,
    dpi = 250,
    scale = 1.1
)

ggsave(
    plot = plot1,
    glue("./plots/woba_daily_mean_top20_2018_{most_recent_day}.png"),
    width = 14,
    height = 8,
    dpi = 250,
    scale = 1.1
)


#########################################################################################
#########################################################################################


barrel_count_players <- 
    barrel_count %>% 
    filter(barrel == 1) %>% 
    select(batter_id_sc) %>% 
    ungroup() %>% 
    distinct()

barrel_data_2_2 <- 
    barrel_data %>% 
    filter(game_year == 2018) %>% 
    inner_join(., barrel_count_players) %>% 
    mutate(player_name = as_factor(player_name)) %>% 
    replace_na(list(estimated_woba_using_speedangle = 0)) %>% 
    mutate(
        player_name = fct_reorder(
            .f = player_name, 
            .x = estimated_woba_using_speedangle, 
            .fun = mean,
            .desc = TRUE,
            na.rm = TRUE)
    )


barrel_data_3_2 <- 
    barrel_data_2_2 %>% 
    group_by(player_name, game_date) %>% 
    summarise(xwoba_mean_daily = mean(estimated_woba_using_speedangle)) %>% 
    ungroup()


barrel_data_4_2 <- 
    barrel_data_2_2 %>% 
    group_by(player_name) %>% 
    summarise(
        xwoba_mean_player = mean(estimated_woba_using_speedangle, na.rm = TRUE),
        n = n()
        ) %>%
    ungroup() %>% 
    filter(n >= 100)


barrel_data_5_2 <- 
    barrel_data_4_2 %>% 
    arrange(-xwoba_mean_player) %>% 
    filter(row_number() %in% c(1:5, 51:55, 101:105, 151:155))


barrel_data_2_2_d <- barrel_data_2_2 %>% filter(player_name %in% unique(barrel_data_5_2$player_name))
barrel_data_3_2_d <- barrel_data_3_2 %>% filter(player_name %in% unique(barrel_data_5_2$player_name))
barrel_data_4_2_d <- barrel_data_4_2 %>% filter(player_name %in% unique(barrel_data_5_2$player_name))


overall_xwoba_df_2 <- barrel_data_2_2 %>% summarise(overall_xwoba = mean(estimated_woba_using_speedangle))


######################

## Mean estimated wOBA over time, for all at-bats, including walks and sacrifices


plot2 <- 
    barrel_data_3_2_d %>% 
    ggplot(aes(x = game_date, y = xwoba_mean_daily)) + 
    
    # geom_hline(
    #     yintercept = 0.0,
    #     color = "gray20",
    #     size = 0.375,
    #     linetype = 1
    # ) +
    geom_hline(
        yintercept = 0.950, 
        color = "black",
        linetype = 2,
        size = 0.5
    ) +
    
    geom_point(
        data = barrel_data_2_2_d,
        aes(x = game_date, y = estimated_woba_using_speedangle),
        shape = 1,
        size = 1,
        color = "gray50",
        alpha = .7
    ) +
    
    geom_line(
        size = 0.375, 
        color = "gray50"
    ) +
    
    geom_point(
        size = 1.25,
        shape = 16,
        color = "gray30"
    ) +
    
    geom_smooth(
        method = "loess",
        span = 0.5,
        size = 0.75,
        alpha = 0.125,
        color = "dodgerblue3",
        fill = "dodgerblue"
    ) +
    
    geom_hline(
        data = barrel_data_4_2_d,
        aes(yintercept = xwoba_mean_player),
        color = "red",
        size = 0.65,
        linetype = 1
    ) +
    
    geom_hline(
        data = overall_xwoba_df_2,
        aes(yintercept = overall_xwoba),
        color = "black",
        size = 0.65,
        linetype = 1
    ) +
    
    facet_wrap(~player_name, ncol = 5) +
    labs(
        title = glue("Mean daily expected weighted on-base average (xwOBA), ",
                     "based on each plate appearance's xwOBA (ranks 1-5, 51-55, 101-105, 151-155 / min. 150 PAs)"),
        subtitle = glue("As of {format(most_recent_day, '%m/%d/%Y')}"),
        x = "Date",
        y = "xwOBA",
        caption = 'Note: dashed line indicates an xwOBA of 0.950, i.e. a "barrel"; black line is MLB season average xwOBA; red line is player\'s season average xwOBA'
    ) +
    coord_cartesian(ylim = c(0, 2), expand = TRUE) +
    theme_minimal() +
    theme(
        axis.text.x = element_text(angle = 0)
    )

ggsave(
    plot = plot2,
    "./plots/woba_daily_mean_1_5,51_55,101_105_2018_1.png",
    width = 14,
    height = 8,
    dpi = 250,
    scale = 1.1
)

ggsave(
    plot = plot2,
    glue("./plots/woba_daily_mean_1_5,51_55,101_105_2018_{most_recent_day}.png"),
    width = 14,
    height = 8,
    dpi = 250,
    scale = 1.1
)

#########################################################################################
#########################################################################################

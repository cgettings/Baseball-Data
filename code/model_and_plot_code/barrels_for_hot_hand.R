###########################################-
###########################################-
##
## Barrels for hot hand ----
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
library(tidyquant)
library(tibbletime)
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
           # pitch_number,
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
    distinct() %>% 
    slice(1:20)

barrel_data_2 <- 
    barrel_data %>% 
    filter(game_year == 2018) %>% 
    inner_join(., barrel_count_players) %>% 
    replace_na(list(estimated_woba_using_speedangle = 0)) %>% 
    group_by(player_name, game_date)


barrel_data_3 <- 
    barrel_data_2 %>% 
    summarise(woba_mean = mean(estimated_woba_using_speedangle))



######################

## Mean estimated wOBA over time, for all at-bats, including walks and sacrifices


plot1 <- 
    barrel_data_3 %>% 
    ggplot(aes(x = game_date, y = woba_mean)) + 
    geom_hline(yintercept = 0.950, color = "red") +
    geom_hline(yintercept = 0.0, color = "gray20", size = 0.375, linetype = 1) +
    geom_point(
        data = barrel_data_2,
        aes(x = game_date, y = estimated_woba_using_speedangle),
        shape = 16,
        size = 1,
        color = "gray50",
        alpha = .7
    ) +
    geom_smooth(method = "loess", span = 0.5, size = 0.6, alpha = .25, color = "dodgerblue4", fill = "dodgerblue") +
    geom_line(size = 0.375) +
    geom_point(
        size = 1.5,
        shape = 1,
        stroke = 0.5,
        color = "gray20"
    ) +
    facet_wrap(~player_name, ncol = 5) +
    labs(
        title = "Mean daily expected weighted on-base average (xwOBA), based on each plate appearance's xwOBA",
        caption = glue("As of {format(most_recent_day, '%m/%d/%Y')}"),
        x = "Date",
        y = "xwOBA",
        caption = 'Note: red line indicates an xwOBA of 0.950, i.e. a "barrel"'
    ) +
    coord_cartesian(ylim = c(0, 2), expand = TRUE) +
    theme_minimal() +
    theme(
        axis.text.x = element_text(angle = 0)
        # panel.grid = element_blank()
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

########################################


barrel_data <- read_rds("./data/barrel_data.rds")


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


#################################


barrel_count_players <- 
    barrel_count %>% 
    filter(barrel == 1) %>% 
    select(batter_id_sc) %>% 
    ungroup() %>% 
    distinct() %>% 
    slice(1:12)

barrel_data_2 <- 
    barrel_data %>% 
    filter(game_year == 2018) %>% 
    inner_join(., barrel_count_players) %>% 
    replace_na(list(estimated_woba_using_speedangle = 0)) %>% 
    group_by(player_name, game_date)


barrel_data_nested <- 
    barrel_data_2 %>% 
    select(player_name, game_date, estimated_woba_using_speedangle) %>%
    arrange(player_name, game_date) %>%
    group_by(player_name, game_date) %>%
    tidyr::nest() %>% 
    group_by(player_name)


unnested_mean <- function(x) unlist(x) %>% mean(., na.rm = TRUE)
unnested_se   <- function(x) unlist(x) %>% se(., na.rm = TRUE)

mean_roll_1 <- rollify(unnested_mean, window = 1)
mean_roll_3 <- rollify(unnested_mean, window = 3)
mean_roll_6 <- rollify(unnested_mean, window = 6)
se_roll_1  <- rollify(unnested_se, window = 1)
se_roll_3  <- rollify(unnested_se, window = 3)
se_roll_6  <- rollify(unnested_se, window = 6)

barrel_data_rolling_3_6 <- 
    barrel_data_nested %>% 
    mutate(
        woba_1_day_rolling_mean = mean_roll_1(data),
        woba_3_day_rolling_mean = mean_roll_3(data),
        woba_6_day_rolling_mean = mean_roll_6(data),
        woba_1_day_rolling_se   = se_roll_1(data),
        woba_3_day_rolling_se   = se_roll_3(data),
        woba_6_day_rolling_se   = se_roll_6(data)
    )

barrel_data_daily <- 
    barrel_data_2 %>% 
    summarise(woba_mean = mean(estimated_woba_using_speedangle))


#################################


plot2 <- 
    barrel_data_rolling_3_6 %>% 
    ggplot() + 
    
    # geom_hline(yintercept = 0.950, color = "red", linetype = 1) +
    # geom_hline(yintercept = 0.0, color = "black", size = 0.375, linetype = 1) +
    
    ## Daily average ##
    
    # geom_point(
    #     data = barrel_data_daily,
    #     aes(x = game_date, y = woba_mean),
    #     size = 0.5,
    #     shape = 16,
    #     color = "black"
    # ) +
    
    ## Daily average ##

    geom_point(
        aes(x = game_date, y = woba_1_day_rolling_mean),
        shape = 16,
        size = 1,
        color = "gray20"
    ) +
    
    
    ## 3-day moving average ##
    
    geom_ribbon(
        aes(
            x = game_date,
            ymin = woba_3_day_rolling_mean - woba_3_day_rolling_se,
            ymax = woba_3_day_rolling_mean + woba_3_day_rolling_se
        ),
        fill = "cyan3",
        # color = "cyan3",
        alpha = 0.125
    ) +
    
    geom_line(
        aes(x = game_date, y = woba_3_day_rolling_mean),
        size = 0.625,
        color = "cyan4"
    ) +
    
    
    ## 6-day moving average ##
    
    geom_ribbon(
        aes(
            x = game_date,
            ymin = woba_6_day_rolling_mean - woba_6_day_rolling_se,
            ymax = woba_6_day_rolling_mean + woba_6_day_rolling_se
        ),
        fill = "orangered",
        # color = "orangered",
        alpha = 0.125
    ) +
    
    geom_line(
        aes(x = game_date, y = woba_6_day_rolling_mean),
        size = 0.75,
        color = "orangered"
    ) +
    
    
    facet_wrap(~player_name, ncol = 4) +
    labs(
        title = "Barreling toward the hot hand",
        subtitle = "3-day (cyan) and 6-day (orange) moving average for expected weighted on-base average (xwOBA), based on each plate appearance's xwOBA",
        x = "Date",
        y = "xwOBA",
        caption = 'Note: xwOBA of 0.950 = a "barrel"'
    ) +
    
    scale_x_date(date_breaks = "week", date_minor_breaks = "day", date_labels = "%b-%e") +
    
    theme_minimal() +
    # theme_bw() +
    theme(
        axis.text.x = element_text(angle = 0),
        panel.grid.major = element_line(size = .375, color = "gray70"),
        panel.grid.minor = element_line(size = .25, color = "gray90"),
        panel.border = element_rect(color = "black", size = .25, fill = NA)
        )

ggsave(plot = plot2, "./plots/woba_3_6_day_mean_top12_2017_1.png", width = 14, height = 8, dpi = 250)
ggsave(plot = plot2, "./plots/woba_3_6_day_mean_top12_2017_1.pdf", width = 14, height = 8, dpi = 250)


#################################


unnested_mean <- function(x) unlist(x) %>% mean(., na.rm = TRUE)
unnested_se   <- function(x) unlist(x) %>% se(.,   na.rm = TRUE)


mean_roll_1  <- rollify(unnested_mean, window = 1)
mean_roll_2  <- rollify(unnested_mean, window = 2)
mean_roll_3  <- rollify(unnested_mean, window = 3)
mean_roll_4  <- rollify(unnested_mean, window = 4)
mean_roll_5  <- rollify(unnested_mean, window = 5)
mean_roll_6  <- rollify(unnested_mean, window = 6)
mean_roll_7  <- rollify(unnested_mean, window = 7)
mean_roll_8  <- rollify(unnested_mean, window = 8)
mean_roll_9  <- rollify(unnested_mean, window = 9)
mean_roll_10 <- rollify(unnested_mean, window = 10)
mean_roll_11 <- rollify(unnested_mean, window = 11)
mean_roll_12 <- rollify(unnested_mean, window = 12)
mean_roll_13 <- rollify(unnested_mean, window = 13)
mean_roll_14 <- rollify(unnested_mean, window = 14)
mean_roll_15 <- rollify(unnested_mean, window = 15)
mean_roll_16 <- rollify(unnested_mean, window = 16)
mean_roll_17 <- rollify(unnested_mean, window = 17)
mean_roll_18 <- rollify(unnested_mean, window = 18)
mean_roll_19 <- rollify(unnested_mean, window = 19)
mean_roll_20 <- rollify(unnested_mean, window = 20)
mean_roll_21 <- rollify(unnested_mean, window = 21)
mean_roll_22 <- rollify(unnested_mean, window = 22)
mean_roll_23 <- rollify(unnested_mean, window = 23)
mean_roll_24 <- rollify(unnested_mean, window = 24)
mean_roll_25 <- rollify(unnested_mean, window = 25)
mean_roll_26 <- rollify(unnested_mean, window = 26)
mean_roll_27 <- rollify(unnested_mean, window = 27)
mean_roll_28 <- rollify(unnested_mean, window = 28)


se_roll_1  <- rollify(unnested_se, window = 1)
se_roll_2  <- rollify(unnested_se, window = 2)
se_roll_3  <- rollify(unnested_se, window = 3)
se_roll_4  <- rollify(unnested_se, window = 4)
se_roll_5  <- rollify(unnested_se, window = 5)
se_roll_6  <- rollify(unnested_se, window = 6)
se_roll_7  <- rollify(unnested_se, window = 7)
se_roll_8  <- rollify(unnested_se, window = 8)
se_roll_9  <- rollify(unnested_se, window = 9)
se_roll_10 <- rollify(unnested_se, window = 10)
se_roll_11 <- rollify(unnested_se, window = 11)
se_roll_12 <- rollify(unnested_se, window = 12)
se_roll_13 <- rollify(unnested_se, window = 13)
se_roll_14 <- rollify(unnested_se, window = 14)
se_roll_15 <- rollify(unnested_se, window = 15)
se_roll_16 <- rollify(unnested_se, window = 16)
se_roll_17 <- rollify(unnested_se, window = 17)
se_roll_18 <- rollify(unnested_se, window = 18)
se_roll_19 <- rollify(unnested_se, window = 19)
se_roll_20 <- rollify(unnested_se, window = 20)
se_roll_21 <- rollify(unnested_se, window = 21)
se_roll_22 <- rollify(unnested_se, window = 22)
se_roll_23 <- rollify(unnested_se, window = 23)
se_roll_24 <- rollify(unnested_se, window = 24)
se_roll_25 <- rollify(unnested_se, window = 25)
se_roll_26 <- rollify(unnested_se, window = 26)
se_roll_27 <- rollify(unnested_se, window = 27)
se_roll_28 <- rollify(unnested_se, window = 28)


barrel_data_rolling_28 <- 
    barrel_data_nested %>% 
    mutate(
        woba_mean_1  = mean_roll_1(data),
        woba_mean_2  = mean_roll_2(data),
        woba_mean_3  = mean_roll_3(data),
        woba_mean_4  = mean_roll_4(data),
        woba_mean_5  = mean_roll_5(data),
        woba_mean_6  = mean_roll_6(data),
        woba_mean_7  = mean_roll_7(data),
        woba_mean_8  = mean_roll_8(data),
        woba_mean_9  = mean_roll_9(data),
        woba_mean_10 = mean_roll_10(data),
        woba_mean_11 = mean_roll_11(data),
        woba_mean_12 = mean_roll_12(data),
        woba_mean_13 = mean_roll_13(data),
        woba_mean_14 = mean_roll_14(data),
        woba_mean_15 = mean_roll_15(data),
        woba_mean_16 = mean_roll_16(data),
        woba_mean_17 = mean_roll_17(data),
        woba_mean_18 = mean_roll_18(data),
        woba_mean_19 = mean_roll_19(data),
        woba_mean_20 = mean_roll_20(data),
        woba_mean_21 = mean_roll_21(data),
        woba_mean_22 = mean_roll_22(data),
        woba_mean_23 = mean_roll_23(data),
        woba_mean_24 = mean_roll_24(data),
        woba_mean_25 = mean_roll_25(data),
        woba_mean_26 = mean_roll_26(data),
        woba_mean_27 = mean_roll_27(data),
        woba_mean_28 = mean_roll_28(data),
        woba_se_1  = se_roll_1(data),
        woba_se_2  = se_roll_2(data),
        woba_se_3  = se_roll_3(data),
        woba_se_4  = se_roll_4(data),
        woba_se_5  = se_roll_5(data),
        woba_se_6  = se_roll_6(data),
        woba_se_7  = se_roll_7(data),
        woba_se_8  = se_roll_8(data),
        woba_se_9  = se_roll_9(data),
        woba_se_10 = se_roll_10(data),
        woba_se_11 = se_roll_11(data),
        woba_se_12 = se_roll_12(data),
        woba_se_13 = se_roll_13(data),
        woba_se_14 = se_roll_14(data),
        woba_se_15 = se_roll_15(data),
        woba_se_16 = se_roll_16(data),
        woba_se_17 = se_roll_17(data),
        woba_se_18 = se_roll_18(data),
        woba_se_19 = se_roll_19(data),
        woba_se_20 = se_roll_20(data),
        woba_se_21 = se_roll_21(data),
        woba_se_22 = se_roll_22(data),
        woba_se_23 = se_roll_23(data),
        woba_se_24 = se_roll_24(data),
        woba_se_25 = se_roll_25(data),
        woba_se_26 = se_roll_26(data),
        woba_se_27 = se_roll_27(data),
        woba_se_28 = se_roll_28(data)
    )


#################################

barrel_data_rolling_28_mean_long <- 
    barrel_data_rolling_28 %>% 
    select(player_name, game_date, contains("woba_mean")) %>% 
    tidyr::gather(key = woba_mean_window, value = woba_mean, woba_mean_1:woba_mean_28)


barrel_data_rolling_28_se_long <- 
    barrel_data_rolling_28 %>% 
    select(player_name, game_date, contains("woba_se")) %>% 
    tidyr::gather(key = woba_se_window, value = woba_se, woba_se_1:woba_se_28)


barrel_data_rolling_28_long <- 
    bind_cols(barrel_data_rolling_28_mean_long,
              barrel_data_rolling_28_se_long) %>% 
    select(-player_name1, -game_date1)

barrel_data_rolling_28_long$woba_mean_window <-
    ordered(
        barrel_data_rolling_28_long$woba_mean_window,
        levels = unique(barrel_data_rolling_28_long$woba_mean_window)
    )


player_names <- barrel_data_2 %>% pull(player_name) %>% unique()


#################################

# num <- 1

for (num in 1:length(player_names)) {
    
    # plot3 <-
    
    barrel_data_rolling_28_long %>% filter(player_name == player_names[num]) %>%
        
        ggplot(aes(x = game_date,
                   y = woba_mean)) +
        
        geom_hline(yintercept = 0.950,
                   color = "gray50",
                   linetype = 2) +
        geom_hline(
            yintercept = 0.0,
            color = "black",
            size = 0.375,
            linetype = 1
        ) +
        
        
        ## Daily average ##
        
        geom_line(
            data = barrel_data_daily %>% filter(player_name == player_names[num]),
            aes(x = game_date,
                y = woba_mean),
            size = 0.375,
            color = "gray50",
            inherit.aes = FALSE
        ) +
        
        
        ## Moving averages ##
        
        geom_ribbon(
            aes(
                x = game_date,
                ymin = woba_mean - woba_se,
                ymax = woba_mean + woba_se
            ),
            fill = "cyan3",
            alpha = 0.75
        ) +
        
        geom_line(size = 0.625,
                  color = "orangered") +
        
        
        ## Options ##
        
        facet_wrap( ~ woba_mean_window, ncol = 4) +
        
        labs(
            title = paste("Barreling toward the hot hand:", player_names[num]),
            subtitle = glue(
                "Moving average for expected weighted on-base average (xwOBA), ",
                "based on each plate appearance's xwOBA"
            ),
            caption = 'Note: dashed line indicates an xwOBA of 0.950, i.e. a "barrel"',
            x = "Date",
            y = "xwOBA"
        ) +
        
        scale_x_date(date_breaks = "month", date_labels = "%b") +
        theme_minimal() +
        theme(
            axis.text.x = element_text(angle = 0),
            panel.grid.minor = element_blank(),
            legend.position = "bottom"
        ) +
        
        
        ### Saving plot ###
        
        ggsave(
            # plot = plot3,
            glue("./plots/{player_names[num]} - woba_28_day_mean_2017_1.png"),
            width = 14,
            height = 14,
            dpi = 250
        )
    
}


#################################

barrel_data_tbl_time <- 
    as_tbl_time(barrel_data_2 %>% ungroup(), game_date) %>% 
    group_by(player_name, game_date) %>% 
    arrange(player_name, game_date)


WMA()


#################################

x.Date <- as.Date(paste(2004, rep(1:4, 4:1), sample(1:28, 10), sep = "-"))
x <- zoo(rnorm(12), x.Date)

rollmean(x, 3)


barrel_data_tbl_time %>% ungroup(game_date) %>% select(game_date, estimated_woba_using_speedangle)


barrel_data_tbl_time %$% partition_index(index = game_date, period = "daily")


get_index_col(barrel_data_tbl_time)


barrel_data_tbl_time$test <- 
    barrel_data_tbl_time %$% 
    zoo(x = estimated_woba_using_speedangle, order.by = game_date)


test2 <- barrel_data_zoo %$% WMA(test, n = 3)

a <- mean(c(1, 2))
b <- mean(c(2, 3, 2))
c <- mean(c(3, 4))
d <- mean(c(3, 4, 5, 6))

mean(c(a, b, c, d))

mean(c(1, 2, 2, 3, 2, 3, 4, 3, 4, 5, 6))

mean(c(1, 2, 2, 3, 2, 3, 4, 3, 4, 5, 6))

mean(c(1, 2, 2, 3, 2, 3, 4, 3, 4, 5, 6))


function(x) unlist(x) %>% mean(., na.rm = TRUE)

xw <- na.omit(cbind(x, wts))
ma <- runSum(xw[, 1] * xw[, 2], n)/runSum(xw[, 2], n)



.Fortran("wma", ia = as.double(c(3, 4, 5, 6)), lia = as.integer(NROW(4)), 
        wts = as.double(wts), n = as.integer(n), oa = as.double(x), 
        loa = as.integer(NROW(x)), PACKAGE = "TTR", DUP = TRUE)$oa


barrel_data_4 <- 
    barrel_data_tbl_time %>% 
    summarise(woba_sum = sum(estimated_woba_using_speedangle), woba_pa = n())



barrel_data_4 %$% zoo(x = woba_sum, order.by = game_date)


barrel_data_tbl_time %>% filter(player_name == "Carlos Santana" & game_date %in% as_date(c("2018-03-29", "2018-03-30", "2018-03-31"))) %>% select(game_date, estimated_woba_using_speedangle)



barrel_data_ma <- 
    barrel_data_tbl_time %>% 
    summarise(
        woba_sum = sum(estimated_woba_using_speedangle), 
        woba_pa = n()
    ) %>% 
    mutate(
        woba_sma_sum_3 = SMA(zoo(x = woba_sum, order.by = game_date), n = 3),
        woba_sma_sum_6 = SMA(zoo(x = woba_sum, order.by = game_date), n = 6),
        woba_wma_sum_3 = WMA(zoo(x = woba_sum, order.by = game_date), n = 3),
        woba_wma_sum_6 = WMA(zoo(x = woba_sum, order.by = game_date), n = 6),
        woba_pa_sma_3  = SMA(zoo(x = woba_pa,  order.by = game_date), n = 3),
        woba_pa_sma_6  = SMA(zoo(x = woba_pa,  order.by = game_date), n = 6),
        woba_pa_wma_3  = WMA(zoo(x = woba_pa,  order.by = game_date), n = 3),
        woba_pa_wma_6  = WMA(zoo(x = woba_pa,  order.by = game_date), n = 6),
        woba_sma_3     = woba_sma_sum_3/woba_pa_sma_3,
        woba_sma_6     = woba_sma_sum_6/woba_pa_sma_6,
        woba_wma_3     = woba_wma_sum_3/woba_pa_wma_3,
        woba_wma_6     = woba_wma_sum_6/woba_pa_wma_6
    ) %>% 
    select(
        -woba_sma_sum_3,
        -woba_wma_sum_3,
        -woba_pa_sma_3,
        -woba_pa_wma_3,
        -woba_sma_sum_6,
        -woba_wma_sum_6,
        -woba_pa_sma_6,
        -woba_pa_wma_6
    )


########################################################


plot4 <- 
    barrel_data_ma %>% 
    ggplot() + 
    
    # geom_hline(yintercept = 0.950, color = "red", linetype = 1) +
    # geom_hline(yintercept = 0.0, color = "black", size = 0.375, linetype = 1) +
    

    ## Daily average ##
    
    geom_point(
        aes(x = game_date, y = woba_sum/woba_pa),
        shape = 16,
        size = 1,
        color = "gray20"
    ) +
    
    
    ## 3-day moving average ##
    
    geom_line(
        aes(x = game_date, y = woba_sma_3),
        size = 0.5,
        color = "cyan3"
    ) +
    
    geom_line(
        aes(x = game_date, y = woba_wma_3),
        size = 0.5,
        color = "cyan4",
        linetype = 2
    ) +
    
    
    ## 6-day moving average ##
    
    geom_line(
        aes(x = game_date, y = woba_sma_6),
        size = 0.5,
        color = "orangered"
    ) +
    
    geom_line(
        aes(x = game_date, y = woba_wma_6),
        size = 0.5,
        color = "orangered",
        linetype = 2
    ) +
    
    
    ## Options ##
    
    scale_x_date(date_breaks = "week", date_minor_breaks = "day", date_labels = "%b-%e") +
    
    facet_wrap(~player_name, ncol = 4) +
    
    labs(
        title = "Barreling toward the hot hand",
        subtitle = glue("3-day (cyan) and 6-day (orange) moving average for",
                        " expected weighted on-base average (xwOBA), based on each",
                        " plate appearance's xwOBA"),
        x = "Date",
        y = "xwOBA"
        # caption = 'Note: xwOBA of 0.950 = a "barrel"'
        ) +
    
    theme_minimal() +
    
    theme(
        axis.text.x = element_text(angle = 0),
        panel.grid.major = element_line(size = .375, color = "gray70"),
        panel.grid.minor = element_line(size = .25, color = "gray90"),
        panel.border = element_rect(color = "black", size = .25, fill = NA))


### Saving plot ###

ggsave(
    plot = plot4,
    "./plots/woba_3_6_day_weighted_mean_top12_2018_1.png",
    width = 14,
    height = 8,
    dpi = 250
)

ggsave(
    plot = plot4,
    "./plots/woba_3_6_day_weighted_mean_top12_2018_1.pdf",
    width = 14,
    height = 8,
    dpi = 250
)


#################################


barrel_count <- 
    barrel_data %>% 
    filter(game_year == 2017) %>% 
    group_by(batter_id_sc) %>% 
    count(barrel) %>% 
    arrange(-barrel, -n) %>% 
    left_join(
        .,
        barrel_data %>% 
            filter(game_year == 2017) %>% 
            select(player_name, batter_id_sc) %>% 
            distinct())

barrel_count_players <- 
    barrel_count %>% 
    filter(barrel == 1) %>% 
    select(batter_id_sc) %>% 
    ungroup() %>% 
    distinct() %>% 
    slice(1:12)

barrel_data_2 <- 
    barrel_data %>% 
    filter(game_year == 2017) %>% 
    inner_join(., barrel_count_players) %>% 
    replace_na(list(estimated_woba_using_speedangle = 0)) %>% 
    group_by(player_name, game_date)


#################################


barrel_data_ma <- 
    barrel_data_2 %>% 
    summarise(
        woba_sum = sum(estimated_woba_using_speedangle), 
        woba_pa = n()
    ) %>% 
    mutate(
        woba_mean      = woba_sum/woba_pa,
        woba_sma_sum_3 = SMA(zoo(x = woba_sum, order.by = game_date), n = 3),
        woba_sma_sum_6 = SMA(zoo(x = woba_sum, order.by = game_date), n = 6),
        woba_wma_sum_3 = WMA(zoo(x = woba_sum, order.by = game_date), n = 3),
        woba_wma_sum_6 = WMA(zoo(x = woba_sum, order.by = game_date), n = 6),
        woba_pa_sma_3  = SMA(zoo(x = woba_pa,  order.by = game_date), n = 3),
        woba_pa_sma_6  = SMA(zoo(x = woba_pa,  order.by = game_date), n = 6),
        woba_pa_wma_3  = WMA(zoo(x = woba_pa,  order.by = game_date), n = 3),
        woba_pa_wma_6  = WMA(zoo(x = woba_pa,  order.by = game_date), n = 6),
        woba_sma_3     = woba_sma_sum_3/woba_pa_sma_3,
        woba_sma_6     = woba_sma_sum_6/woba_pa_sma_6,
        woba_wma_3     = woba_wma_sum_3/woba_pa_wma_3,
        woba_wma_6     = woba_wma_sum_6/woba_pa_wma_6
    ) %>% 
    select(
        -woba_sma_sum_3,
        -woba_wma_sum_3,
        -woba_pa_sma_3,
        -woba_pa_wma_3,
        -woba_sma_sum_6,
        -woba_wma_sum_6,
        -woba_pa_sma_6,
        -woba_pa_wma_6
    )


########################################################


plot4 <- 
    barrel_data_ma %>% 
    ggplot() + 
    
    # geom_hline(yintercept = 0.950, color = "red", linetype = 1) +
    # geom_hline(yintercept = 0.0, color = "black", size = 0.375, linetype = 1) +
    

    ## Daily average ##
    
    geom_point(
        aes(x = game_date, y = woba_mean),
        shape = 16,
        size = 1,
        color = "gray20"
    ) +
    
    
    ## 3-day moving average ##
    
    # geom_line(
    #     aes(x = game_date, y = woba_sma_3),
    #     size = 0.375,
    #     color = "cyan3"
    # ) +
    # 
    # geom_line(
    #     aes(x = game_date, y = woba_wma_3),
    #     size = 0.375,
    #     color = "blue"
    # ) +
    
    
    ## 6-day moving average ##
    
    geom_line(
        aes(x = game_date, y = woba_sma_6),
        size = 0.375,
        color = "orange"
    ) +

    geom_line(
        aes(x = game_date, y = woba_wma_6),
        size = 0.375,
        color = "red"
    ) +
    
    
    ## Options ##
    
    # scale_x_date(date_breaks = "week", date_minor_breaks = "day", date_labels = "%b-%e") +
    scale_x_date(date_breaks = "month", date_minor_breaks = "week", date_labels = "%b") +
    
    facet_wrap(~player_name, ncol = 4) +
    
    labs(
        title = "Barreling toward the hot hand",
        subtitle = glue("6-day (orange) moving average for",
                        " expected weighted on-base average (xwOBA), based on each",
                        " plate appearance's xwOBA"),
        x = "Date",
        y = "xwOBA"
        # caption = 'Note: xwOBA of 0.950 = a "barrel"'
        ) +
    
    theme_minimal() +
    
    theme(
        axis.text.x = element_text(angle = 0),
        panel.grid.major = element_line(size = .375, color = "gray70"),
        panel.grid.minor = element_line(size = .25, color = "gray90"),
        panel.border = element_rect(color = "black", size = .25, fill = NA))


### Saving plot ###

ggsave(
    plot = plot4,
    "./plots/woba_6_day_weighted_mean_top12_2017_1.png",
    width = 14,
    height = 8,
    dpi = 250
)

ggsave(
    plot = plot4,
    "./plots/woba_6_day_weighted_mean_top12_2017_1.pdf",
    width = 14,
    height = 8,
    dpi = 250
)


#################################

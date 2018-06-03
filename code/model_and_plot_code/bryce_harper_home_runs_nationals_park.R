######################################################-
######################################################-
##
## Bryce Harper home runs @ Nationals Park - PLOT ----
##
######################################################-
######################################################-

#=========================#
# Setting up ----
#=========================#

#-------------------------#
# Loading libraries ----
#-------------------------#

library(tidyverse)
library(lubridate)
library(magrittr)
library(dbplyr)
library(DBI)
library(glue)

#-----------------------------------#
# Loading Nationals Park trace ----
#-----------------------------------#

nationals.park.df <- read_csv("./data/nationals.park.trace.df.csv")

#--------------------------------#
# Connecting to database ----
#--------------------------------#

statcast_db <- dbConnect(RSQLite::SQLite(), "./data/statcast_db.sqlite3")

#--------------------------------#
# Pulling data from database ----
#--------------------------------#

harper_data <-
    tbl(statcast_db, "statcast_data_updated") %>%
    filter(
        batter_team == "WSH" & 
            ballpark == "Nationals Park" & 
            events == "home_run"
    ) %>%
    filter(!is.na(events) & game_type == "R") %>% 
    select(game_date, 
           game_year,
           bb_type,
           events,
           player_name,
           batter_id_sc,
           hit_distance_sc,
           launch_speed_sc,
           horiz_angle,
           horiz_angle_new_deg
           ) %>% 
    collect() %>% 
    mutate(
        horiz_angle_3 = case_when(
            horiz_angle_new_deg >= 0  & horiz_angle_new_deg <  30 ~ "Right",
            horiz_angle_new_deg >= 30 & horiz_angle_new_deg <= 60 ~ "Center",
            horiz_angle_new_deg >  60 & horiz_angle_new_deg <= 90 ~ "Left"
            )
        ) %>%
    filter(str_detect(player_name, "Harper"))


most_recent_day <-
    tbl(statcast_db, "statcast_data_updated") %>%
    select(game_date, obs_date_time_loc) %>% 
    arrange(desc(obs_date_time_loc)) %>% 
    head(100) %>% 
    pull(game_date) %>% 
    extract2(1) %>% 
    as_date()


#=========================#
# Plotting ----
#=========================#


diamond.plot.field.BH <-
    harper_data %>% 
    ggplot(
        aes(
            x = 0 + hit_distance_sc * cos(horiz_angle), 
            y = 0 + hit_distance_sc * sin(horiz_angle))) +
    
    coord_fixed(xlim = c(0, 500), ylim = c(0, 500), expand = TRUE) +
    
    labs(
        title = "Bryce Harper Home Runs @ Nationals Park",
        subtitle = glue("As of {format(most_recent_day, '%m/%d/%Y')}")
    ) +
    
    
    ## Dividing the fields ##
    
    ### Left ###
    
    annotate(
        x = c(
            0,
            (0 + 550 * cos((60:90 * pi) / 180)),
            0),
        y = c(
            0,
            (0 + 550 * sin((60:90 * pi) / 180)),
            0),
        geom = "polygon",
        fill = "white",
        alpha = 1) +
    
    ### Center ###
    
    annotate(
        x = c(
            0,
            (0 + 550 * cos((30:60 * pi) / 180)),
            0), 
        y = c(
            0,
            (0 + 550 * sin((30:60 * pi) / 180)),
            0), 
        geom = "polygon",
        fill = "ivory2",
        alpha = 1) +
    
    
    ### Right ###
    
    annotate(
        x = c(
            0,
            (0 + 550 * cos((0:30 * pi) / 180)),
            0),
        y = c(
            0,
            (0 + 550 * sin((0:30 * pi) / 180)),
            0),
        geom = "polygon",
        fill = "white",
        alpha = 1) +
    
    
    ## Grass ##
    
    annotate(
        x = nationals.park.df$x,
        y = nationals.park.df$y,
        geom = "polygon",
        fill = "#9ACB9A",
        alpha = 1) +
    
    
    ## Infield dirt ##
    
    annotate(
        x = c(
            (59 * cos((45 * pi) / 180)) + 95 * cos((-26:116 * pi) / 180),
            0,
            90,
            90),
        y = c(
            (59 * cos((45 * pi) / 180)) + 95 * sin((-26:116 * pi) / 180),
            90,
            90,
            0),
        geom = "polygon",
        fill = "#DBAF84",
        alpha = 1) +
    
    ## Infield grass ##
    
    annotate(
        x = c(90, 90, 0, 0), 
        y = c(0, 90, 90, 0),
        geom = "polygon", 
        fill = "#9ACB9A",
        alpha = 1) + 
    
    ## Pitcher's mound dirt ##
    
    annotate(
        x = (59 * cos((45 * pi) / 180)) + (10 * cos((seq(0, 360, .1) * pi) / 180)),
        y = (59 * sin((45 * pi) / 180)) + (10 * sin((seq(0, 360, .1) * pi) / 180)),
        geom = "polygon",
        fill = "#DBAF84",
        alpha = 1) +
    
    ## Pitcher's mound cut ##
    
    annotate(
        x = (59 * cos((45 * pi) / 180)) + (10 * cos((seq(0, 360, .1) * pi) / 180)),
        y = (59 * sin((45 * pi) / 180)) + (10 * sin((seq(0, 360, .1) * pi) / 180)),
        geom = "path",
        size = .25,
        color = "gray20"
    ) +
    
    ## Pitcher's mound rubber ##
    
    annotate(
        geom = "text",
        label = "-",
        x = (58 * cos((45 * pi) / 180)),
        y = (58 * sin((45 * pi) / 180)), 
        color = "white",
        size = 8,
        angle = 135
    ) +
    
    ## Wall ##
    
    annotate(
        geom = "path",
        x = nationals.park.df$x,
        y = nationals.park.df$y,
        color = "black",
        size = 1
    ) +
    
    ## Grass cut ##
    
    annotate(
        geom = "path",
        x = (59 * cos((45 * pi) / 180)) + 95 * cos((-26:116 * pi) / 180),
        y = (59 * sin((45 * pi) / 180)) + 95 * sin((-26:116 * pi) / 180),
        color = "gray20",
        size = .5, 
        lineend = "round"
    ) +
    
    ## Basepaths ##
    
    annotate(
        geom = "path",
        x = c(
            home1  = 0,
            first  = 90,
            second = 90,
            third  = 0,
            home2  = 0),
        y = c(
            home1  = 0,
            first  = 0,
            second = 90,
            third  = 90,
            home2  = 0), 
        color = "gray20",
        size = .5
    ) +
    
    ## Extended foul lines ##
    
    annotate(
        geom = "segment", 
        x = c(0, 0), 
        y = c(0, 0),
        xend = c(550, 0),
        yend = c(0, 550),
        color = "black",
        size = .5
    ) +
    
    ## Foul line up to foul poles ##
    
    annotate(
        geom = "segment",
        x = 0,
        y = 0,
        xend = c(nationals.park.df$y[2], 0),
        yend = c(0, nationals.park.df$x[10]),
        color = "black",
        size = .75
    ) +
    
    ## Bases ##
    
    annotate(
        geom = "point",
        x = c(
            first  = 90,
            second = 90,
            third  = 0),
        y = c(
            first  = 0,
            second = 90,
            third  = 90), 
        shape = 22,
        color = "gray20",
        fill = "white",
        size = 4
    ) +
    
    annotate(
        geom = "point",
        x = c(home = -2),
        y = c(home = -2), 
        shape = 23,
        color = "gray20",
        fill = "white",
        size = 4
    ) +
    
    ## Range rings ##
    
    annotate(
        geom = "path",
        x = 0 + 300 * cos((0:90 * pi) / 180),
        y = 0 + 300 * sin((0:90 * pi) / 180),
        color = "gray50",
        size = .25,
        linetype = "dashed"
    ) +
    annotate(
        geom = "path",
        x = 0 + 350 * cos((0:90 * pi) / 180),
        y = 0 + 350 * sin((0:90 * pi) / 180),
        color = "gray50",
        size = .25,
        linetype = "dashed"
    ) +
    annotate(
        geom = "path",
        x = 0 + 400 * cos((0:90 * pi) / 180),
        y = 0 + 400 * sin((0:90 * pi) / 180),
        color = "gray50",
        size = .25,
        linetype = "dashed"
    ) +
    annotate(
        geom = "path",
        x = 0 + 450 * cos((0:90 * pi) / 180),
        y = 0 + 450 * sin((0:90 * pi) / 180),
        color = "gray50",
        size = .25,
        linetype = "dashed"
    ) +
    annotate(
        geom = "path",
        x = 0 + 500 * cos((0:90 * pi) / 180),
        y = 0 + 500 * sin((0:90 * pi) / 180),
        color = "gray50",
        size = .25,
        linetype = "dashed"
    ) +
    annotate(
        geom = "path",
        x = 0 + 550 * cos((0:90 * pi) / 180),
        y = 0 + 550 * sin((0:90 * pi) / 180),
        color = "gray50",
        size = .25,
        linetype = "dashed"
    ) +
    
    ## Text ##
    
    annotate(
        geom = "text",
        label   = 300,
        x = -10 + c(300, 300) * cos((c(6, 84) * pi) / 180),
        y = -10 + c(300, 300) * sin((c(6, 84) * pi) / 180)
    ) +
    annotate(
        geom = "text",
        label   = 350,
        x = -10 + c(350, 350) * cos((c(6, 84) * pi) / 180),
        y = -10 + c(350, 350) * sin((c(6, 84) * pi) / 180)
    ) +
    annotate(
        geom = "text",
        label   = 400,
        x = -10 + c(400, 400) * cos((c(6, 84) * pi) / 180),
        y = -10 + c(400, 400) * sin((c(6, 84) * pi) / 180)
    ) +
    annotate(
        geom = "text",
        label   = 450,
        x = -10 + c(450, 450) * cos((c(6, 84) * pi) / 180),
        y = -10 + c(450, 450) * sin((c(6, 84) * pi) / 180)
    ) +
    annotate(
        geom = "text",
        label   = 500,
        x = -10 + c(500, 500) * cos((c(6, 84) * pi) / 180),
        y = -10 + c(500, 500) * sin((c(6, 84) * pi) / 180)
    ) +
    annotate(
        geom = "text",
        label   = 550,
        x = -10 + c(550, 550) * cos((c(6, 84) * pi) / 180),
        y = -10 + c(550, 550) * sin((c(6, 84) * pi) / 180)
    ) +
    annotate(
        geom = "point",
        x = c(nationals.park.df$y[2], 0),
        y = c(0, nationals.park.df$x[10]),
        shape = 21,
        size = 2,
        color = "black",
        fill = "white"
    ) +
    
    geom_spoke(
        data = harper_data,
        aes(angle = horiz_angle,
            radius = -hit_distance_sc),
        color = "gray30",
        size = .375,
        alpha = .475) +
    
    geom_point(
        data = harper_data,
        aes(fill = launch_speed_sc),
        shape = 21,
        color = "black",
        size = 3.5,
        alpha = .75
    ) +
    
    ## Field percentages ##
    
    annotate(
        geom = "text",
        label = harper_data %>% 
            with(., prop.table(table(horiz_angle_3)) * 100) %>%
            round(digits = 1) %>%
            paste(., "%", sep = "") %>%
            c(), 
        x = c(525, 475, 475) * cos((c(45, 75, 15) * pi) / 180),
        y = c(525, 475, 475) * sin((c(45, 75, 15) * pi) / 180),
        size = 6,
        fontface = "bold"
    ) +
    
    scale_fill_gradientn(
        colors = c("gray80", "green2", "yellow", "#FF0000", "#FF4DA7"), 
        name = "Launch\nSpeed", 
        limits = c(85, 125),
        breaks = c(85, 90, 95, 100, 105, 110, 115, 120, 125)
    ) + 
    
    guides(
        fill = guide_colorbar(
            ticks = TRUE,
            label = TRUE,
            order = 2,
            title.position = "top",
            label.position = "left"),
        color = guide_colorbar(
            ticks = TRUE,
            label = TRUE,
            order = 1,
            title.position = "top",
            label.position = "left")
    ) +
    
theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_rect(color = "black", linetype = "solid", size = .5, fill = NA),
    strip.background = element_blank(),
    strip.text = element_text(size = 9),
    legend.key.size = unit(.75, "cm"),
    legend.key.height = unit(1.675, "cm"),
    legend.position = "left",
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 10),
    panel.background = element_rect(fill = "ivory4"), 
    panel.grid = element_blank(),
    legend.title.align = 1
)


## Saving plots ####

ggsave(
    plot = diamond.plot.field.BH,
    "./plots/Bryce_Harper_Nationals_Park_Home_Runs_By_Field.png",
    width = 10,
    height = 9,
    dpi = 250,
    scale = 1
)

ggsave(
    plot = diamond.plot.field.BH,
    glue("./plots/Bryce_Harper_Nationals_Park_Home_Runs_By_Field_{most_recent_day}.png"),
    width = 10,
    height = 9,
    dpi = 250,
    scale = 1
)

################################################################################
################################################################################

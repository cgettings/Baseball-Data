###########################################################################################-
###########################################################################################-
##
## Aaron Judge home runs - PLOT ----
##
###########################################################################################-
###########################################################################################-

#=========================================================================================#
# Setting up ----
#=========================================================================================#

#-----------------------------------------------------------------------------------------#
# Loading libraries
#-----------------------------------------------------------------------------------------#

library(magrittr)
library(tidyverse)
library(lubridate)
library(dbplyr)
library(DBI)
library(RSQLite)
library(glue)

#-----------------------------------------------------------------------------------------#
# Sourcing Yankee Stadium trace
#-----------------------------------------------------------------------------------------#

# yankee_stadium_trace

source("code/model_and_plot_code/yankee_stadium_trace.R")

#-----------------------------------------------------------------------------------------#
# Connecting to database
#-----------------------------------------------------------------------------------------#

statcast_db <- dbConnect(SQLite(), "data/statcast_db_rebuilt.sqlite3")

#-----------------------------------------------------------------------------------------#
# Pulling data from database ----
#-----------------------------------------------------------------------------------------#

aaron_judge_hr <-
    statcast_db %>% 
    tbl("statcast_data") %>%
    filter(player_name == "Aaron Judge" & events == "home_run") %>%
    select(game_date, 
           game_year,
           ballpark,
           bb_type,
           events,
           player_name,
           hit_distance_sc,
           launch_speed,
           horiz_angle,
           horiz_angle_3
    ) %>% 
    collect() %>% 
    mutate(
        game_date = as_date(game_date),
        yankee_stadium = if_else(ballpark == "Yankee Stadium", "Yankee Stadium", "Other", "Other")
    )

most_recent_day <-
    tbl(statcast_db, "statcast_data") %>%
    select(game_date) %>%
    arrange(desc(game_date)) %>%
    head(1) %>%
    collect() %>%
    pull(game_date) %>% 
    as_date()

#=========================================================================================#
# Plotting ----
#=========================================================================================#

#-----------------------------------------------------------------------------------------#
# Yankee Stadium ----
#-----------------------------------------------------------------------------------------#

diamond_plot_field_yankee <-
    aaron_judge_hr %>% 
    filter(yankee_stadium == "Yankee Stadium") %>% 
    ggplot(
        aes(
            x = 0 + hit_distance_sc * cos(horiz_angle), 
            y = 0 + hit_distance_sc * sin(horiz_angle))) +
    
    labs(
        title = "Aaron Judge Home Runs: Yankee Stadium",
        subtitle = glue(
            "{nrow(aaron_judge_hr %>% filter(yankee_stadium == 'Yankee Stadium'))} home runs",
            " ({aaron_judge_hr %>% filter(yankee_stadium == 'Yankee Stadium') %>% drop_na(hit_distance_sc, horiz_angle) %>% nrow()} plotted)")
    ) +
    
    coord_fixed(xlim = c(0, 500), ylim = c(0, 500), expand = TRUE) +
    
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
        x = c(0, yankee_stadium_trace$x),
        y = c(yankee_stadium_trace$y, 0),
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
        x = yankee_stadium_trace$x,
        y = yankee_stadium_trace$y,
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
        xend = c(yankee_stadium_trace$y[2], 0),
        yend = c(0, yankee_stadium_trace$x[length(yankee_stadium_trace$x) - 1]),
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
        x = c(first(yankee_stadium_trace$x), 0),
        y = c(0, last(yankee_stadium_trace$y)),
        shape = 21,
        size = 2,
        color = "black",
        fill = "white"
    ) +
    
    geom_spoke(
        data = aaron_judge_hr %>% filter(yankee_stadium == "Yankee Stadium"),
        aes(angle = horiz_angle,
            radius = -hit_distance_sc),
        color = "gray30",
        size = .375,
        alpha = .475) +
    
    geom_point(
        data = aaron_judge_hr %>% filter(yankee_stadium == "Yankee Stadium"),
        aes(fill = launch_speed),
        shape = 21,
        color = "black",
        size = 3.5,
        alpha = 1
    ) +
    
    ## Field percentages ##
    
    annotate(
        geom = "text",
        label = aaron_judge_hr %>% 
            filter(yankee_stadium == "Yankee Stadium") %>% 
            with(., prop.table(table(horiz_angle_3)) * 100) %>%
            round(digits = 1) %>%
            paste(., "%", sep = "") %>%
            c(), 
        x = c(525, 475, 475) * cos((c(45, 75, 15) * pi) / 180),
        y = c(525, 475, 475) * sin((c(45, 75, 15) * pi) / 180),
        size = 6,
        fontface = "bold"
    ) +
    
    scale_fill_viridis_c(
        name = "Launch\nSpeed",
        option = "C",
        limits = c(95, 122),
        breaks = c(95, 100, 105, 110, 115, 120, 125)
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
    plot = diamond_plot_field_yankee,
    "plots/Aaron_Judge_Home_Runs_Yankee_Stadium_By_Field.png",
    width = 10,
    height = 10,
    dpi = 250,
    scale = 1
)

ggsave(
    plot = diamond_plot_field_yankee,
    glue("plots/Aaron_Judge_Home_Runs_Yankee_Stadium_By_Field_{most_recent_day}.png"),
    width = 10,
    height = 10,
    dpi = 250,
    scale = 1
)


#-----------------------------------------------------------------------------------------#
# Other Stadium ----
#-----------------------------------------------------------------------------------------#

diamond_plot_field_other <-
    aaron_judge_hr %>% 
    filter(yankee_stadium == "Other") %>% 
    ggplot(
        aes(
            x = 0 + hit_distance_sc * cos(horiz_angle), 
            y = 0 + hit_distance_sc * sin(horiz_angle))) +
    
    labs(
        title = "Aaron Judge Home Runs: Other Stadium",
        subtitle = glue(
            "{nrow(aaron_judge_hr %>% filter(yankee_stadium == 'Other'))} home runs",
            " ({aaron_judge_hr %>% filter(yankee_stadium == 'Other') %>% drop_na(hit_distance_sc, horiz_angle) %>% nrow()} plotted)")
    ) +
    
    coord_fixed(xlim = c(0, 500), ylim = c(0, 500), expand = TRUE) +
    
    
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
        x = c(0, yankee_stadium_trace$x),
        y = c(yankee_stadium_trace$y, 0),
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
        x = yankee_stadium_trace$x,
        y = yankee_stadium_trace$y,
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
        xend = c(yankee_stadium_trace$y[2], 0),
        yend = c(0, yankee_stadium_trace$x[length(yankee_stadium_trace$x) - 1]),
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
        x = c(first(yankee_stadium_trace$x), 0),
        y = c(0, last(yankee_stadium_trace$y)),
        shape = 21,
        size = 2,
        color = "black",
        fill = "white"
    ) +
    
    geom_spoke(
        data = aaron_judge_hr %>% filter(yankee_stadium == "Other"),
        aes(angle = horiz_angle,
            radius = -hit_distance_sc),
        color = "gray30",
        size = .375,
        alpha = .475) +
    
    geom_point(
        data = aaron_judge_hr %>% filter(yankee_stadium == "Other"),
        aes(fill = launch_speed),
        shape = 21,
        color = "black",
        size = 3.5,
        alpha = 1
    ) +
    
    ## Field percentages ##
    
    annotate(
        geom = "text",
        label = aaron_judge_hr %>% 
            filter(yankee_stadium == "Other") %>% 
            with(., prop.table(table(horiz_angle_3)) * 100) %>%
            round(digits = 1) %>%
            paste(., "%", sep = "") %>%
            c(), 
        x = c(525, 475, 475) * cos((c(45, 75, 15) * pi) / 180),
        y = c(525, 475, 475) * sin((c(45, 75, 15) * pi) / 180),
        size = 6,
        fontface = "bold"
    ) +
    
    scale_fill_viridis_c(
        name = "Launch\nSpeed",
        option = "C",
        limits = c(95, 122),
        breaks = c(95, 100, 105, 110, 115, 120, 125)
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
    plot = diamond_plot_field_other,
    "plots/Aaron_Judge_Home_Runs_Other_Stadium_By_Field.png",
    width = 10,
    height = 10,
    dpi = 250,
    scale = 1
)

ggsave(
    plot = diamond_plot_field_other,
    glue("plots/Aaron_Judge_Home_Runs_Other_Stadium_By_Field_{most_recent_day}.png"),
    width = 10,
    height = 10,
    dpi = 250,
    scale = 1
)


#-----------------------------------------------------------------------------------------#
# Combining ----
#-----------------------------------------------------------------------------------------#

diamond_plot_field <- cowplot::plot_grid(diamond_plot_field_yankee, diamond_plot_field_other)

## Saving plots ####

ggsave(
    plot = diamond_plot_field,
    "plots/Aaron_Judge_Home_Runs_By_Field.png",
    width = 20,
    height = 10,
    dpi = 250,
    scale = 0.9
)

ggsave(
    plot = diamond_plot_field,
    glue("plots/Aaron_Judge_Home_Runs_By_Field_{most_recent_day}.png"),
    width = 20,
    height = 10,
    dpi = 250,
    scale = 0.9
)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# #
# #                             ---- THIS IS THE END! ----
# #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

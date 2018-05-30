###########################################-
###########################################-
##
## updating Statcast data ----
##
###########################################-
###########################################-

## This code will build and update an SQLite database containing Statcast data,
##  downloaded from https://baseballsavant.mlb.com/statcast_search. I use it mostly for
##  updating an existing database, and for this purpose you can just source this file and 
##  let it do its thing. If you want to build one from scratch, then it's best to use this 
##  file interactively. There are blocks that print progress info to the console, because I find
##  this helpful.
##
## The code also sources in a custom workhorse function that does the dirty work of
##  constructing a URL using the parameters you specify in the function call. Bill Petti's
##  {baseballr} package has functions (e.g., `scrape_statcast_savant_batter`) that do this
##  also (and probably better, but at least in a more sophicticated way), in addition to
##  some other neat scraping functions.

#=========================#
# Setting up ----
#=========================#

#-------------------------#
# Loading libraries ----
#-------------------------#

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(stringdist))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(dbplyr))
suppressPackageStartupMessages(library(DBI))
suppressPackageStartupMessages(library(pryr))
suppressPackageStartupMessages(library(useful))

#--------------------------------#
# Loading functions ----
#--------------------------------#

source("./code/functions/get_savant_pitches_data.R")

get_savant_pitches_data_quietly <- quietly(get_savant_pitches_data)

#--------------------------------#
# Connecting to database ----
#--------------------------------#

statcast_db <- dbConnect(RSQLite::SQLite(), "./data/statcast_db.sqlite3")

#-----------------------------------------#
# Setting search parameters ----
#-----------------------------------------#

## set date range if manually constructing the database for the first time ##

# from    <- today()
# back_to <- from - 1 


# ---- Finding most recent date in database ----

most_recent_day <-
    tbl(statcast_db, "statcast_data_updated") %>%
    select(game_date) %>%
    arrange(desc(game_date)) %>%
    # arrange(game_date) %>%
    head(1) %>%
    collect() %>%
    pull(game_date)

## Substituting 2018 opening day ##

# most_recent_day <- as_date("2018-03-29")

## setting the start date ##

start_date <- today(tzone = "US/Eastern")

## setting the time span ##

num_days <- (start_date - as_date(most_recent_day)) %>% as.numeric()

## sequence of single days ##

every.1.days <- seq(1, num_days, 1)

## single-day spans ##

back_to <- sort(start_date - every.1.days)

## kludge for getting data for just a single day - gets around data download limit ##

from <- back_to + 0

## printing date range ##

data_frame(from, back_to)

#=============================#
# Downloading data ####
#=============================#

## need to break up in "home" and "away" batches, to avoid getting a "too much data" error ##

#------------------------#
# Home ----
#------------------------#

statcast_update_home_all <- data.frame()

for(each_day in 1:length(back_to)) {
    
    statcast_update_home <-
        
        get_savant_pitches_data_quietly(
            back_to = back_to[each_day],
            from = from[each_day],
            home_road = "Home") %>% 
        extract2("result") %>% 
        mutate_all(funs("as.character"))
    
    
        statcast_update_home_all <- 
            
            bind_rows(statcast_update_home_all,
                      statcast_update_home) %>% 
            as_tibble()
        
        
        ## Pretty printing info to console
        
        cat("\n===============================\n")
        cat("Home")
        cat("\n-------------------------------\n")
        cat("Loop", each_day)
        cat("\n-------------------------------\n")
        cat("|-- ")
        cat(back_to[each_day] %>% as.character())
        cat(" --|")
        cat("\n-------------------------------\n")
        cat(nrow(statcast_update_home), "rows added\n")
        cat("Size: ")
        object_size(statcast_update_home) %>% print()
        cat("-------------------------------\n")
        cat(nrow(statcast_update_home_all), "rows added in total\n")
        cat("Size: ")
        object_size(statcast_update_home_all) %>% print()
        cat("===============================\n")
    
gc()

}


#------------------------#
# Road ----
#------------------------#

statcast_update_road_all <- data.frame()

for(each_day in 1:length(back_to)) {
    
    statcast_update_road <-
        
        get_savant_pitches_data_quietly(
            back_to = back_to[each_day],
            from = from[each_day],
            home_road = "Road") %>% 
        extract2("result") %>% 
        mutate_all(funs("as.character"))
    
    
        statcast_update_road_all <- 
            
            bind_rows(statcast_update_road_all,
                      statcast_update_road) %>% 
            as_tibble()
    
        
        ## Pretty printing info to console
        
        cat("\n===============================\n")
        cat("Road")
        cat("\n-------------------------------\n")
        cat("Loop", each_day)
        cat("\n-------------------------------\n")
        cat("|-- ")
        cat(back_to[each_day] %>% as.character())
        cat(" --|")
        cat("\n-------------------------------\n")
        cat(nrow(statcast_update_road), "rows added\n")
        cat("Size: ")
        object_size(statcast_update_road) %>% print()
        cat("-------------------------------\n")
        cat(nrow(statcast_update_road_all), "rows added in total\n")
        cat("Size: ")
        object_size(statcast_update_road_all) %>% print()
        cat("===============================\n")
    
gc()

}


#-----------------------------#
# Combining home and away ----
#-----------------------------#

## Combining and dropping variables not in database ##

drop_vars <- 
    c(
        # "pitch_name",
        # "home_score",
        # "away_score",
        # "bat_score",
        # "fld_score",
        # "post_away_score",
        # "post_home_score",
        # "post_bat_score",
        # "post_fld_score"
    )


statcast_update_comb <- 
    
    bind_rows(
        statcast_update_home_all,
        statcast_update_road_all) %>% 
    select_if(!names(.) %in% drop_vars) %>% 
    as_tibble()


rm(statcast_update_home_all)
rm(statcast_update_road_all)
gc()


## filtering out overlapping data from the most recent day ##

most_recent_data <-
    tbl(statcast_db, "statcast_data_updated") %>%
    filter(game_date == most_recent_day) %>%
    mutate_all(funs("as.character")) %>% 
    collect()


## getting distinct records ##

statcast_update_comb_distinct <-
    anti_join(
        statcast_update_comb,
        most_recent_data,
        by = c(
            "game_date",
            "game_pk",
            "at_bat_number",
            "pitch_number",
            "batter" = "batter_id_sc",
            "pitcher" = "pitcher_id_sc"
        )
    )


## stopping execution if there are no rows to update ##

if (nrow(statcast_update_comb_distinct) == 0) {
    message("No rows to update; stopping here by calling `.rs.restartR()`.")
    .rs.restartR()
}


rm(statcast_update_comb)
gc()


## writing to temporary table in database ##

dbWriteTable(
    statcast_db,
    "statcast_update_comb_distinct",
    statcast_update_comb_distinct,
    overwrite = TRUE,
    append = FALSE
)


#------------------------#
# Cleaning ----
#------------------------#


statcast_update_comb_distinct_clean <-
    
    statcast_update_comb_distinct %>%
    
    select(
        # -error,
        -contains("_deprecated"), # dropping deprecated columns
        -pos2_person_id) %>%      # dropping strangely empty column
    
    rename(pos2_person_id = pos2_person_id_1) %>% 
    
    mutate(
        pitch_type      = case_when(pitch_type      == "null" ~ NA_character_, TRUE ~ pitch_type),
        release_speed   = case_when(release_speed   == "null" ~ NA_character_, TRUE ~ release_speed),
        release_pos_x   = case_when(release_pos_x   == "null" ~ NA_character_, TRUE ~ release_pos_x),
        release_pos_z   = case_when(release_pos_z   == "null" ~ NA_character_, TRUE ~ release_pos_z),
        batter          = case_when(batter          == "null" ~ NA_character_, TRUE ~ batter),
        pitcher         = case_when(pitcher         == "null" ~ NA_character_, TRUE ~ pitcher),
        events          = case_when(events          == "null" ~ NA_character_, TRUE ~ events),
        zone            = case_when(zone            == "null" ~ NA_character_, TRUE ~ zone),
        des             = case_when(des             == "null" ~ NA_character_, TRUE ~ des),
        balls           = case_when(balls           == "null" ~ NA_character_, TRUE ~ balls),
        strikes         = case_when(strikes         == "null" ~ NA_character_, TRUE ~ strikes),
        game_year       = case_when(game_year       == "null" ~ NA_character_, TRUE ~ game_year),
        pfx_x           = case_when(pfx_x           == "null" ~ NA_character_, TRUE ~ pfx_x),
        pfx_z           = case_when(pfx_z           == "null" ~ NA_character_, TRUE ~ pfx_z),
        plate_x         = case_when(plate_x         == "null" ~ NA_character_, TRUE ~ plate_x),
        plate_z         = case_when(plate_z         == "null" ~ NA_character_, TRUE ~ plate_z),
        on_3b           = case_when(on_3b           == "null" ~ NA_character_, TRUE ~ on_3b),
        on_2b           = case_when(on_2b           == "null" ~ NA_character_, TRUE ~ on_2b),
        on_1b           = case_when(on_1b           == "null" ~ NA_character_, TRUE ~ on_1b),
        outs_when_up    = case_when(outs_when_up    == "null" ~ NA_character_, TRUE ~ outs_when_up),
        inning          = case_when(inning          == "null" ~ NA_character_, TRUE ~ inning),
        hc_x            = case_when(hc_x            == "null" ~ NA_character_, TRUE ~ hc_x),
        hc_y            = case_when(hc_y            == "null" ~ NA_character_, TRUE ~ hc_y),
        umpire          = case_when(umpire          == "null" ~ NA_character_, TRUE ~ umpire),
        sv_id           = case_when(sv_id           == "null" ~ NA_character_, TRUE ~ sv_id),
        vx0             = case_when(vx0             == "null" ~ NA_character_, TRUE ~ vx0),
        vy0             = case_when(vy0             == "null" ~ NA_character_, TRUE ~ vy0),
        vz0             = case_when(vz0             == "null" ~ NA_character_, TRUE ~ vz0),
        ax              = case_when(ax              == "null" ~ NA_character_, TRUE ~ ax),
        ay              = case_when(ay              == "null" ~ NA_character_, TRUE ~ ay),
        az              = case_when(az              == "null" ~ NA_character_, TRUE ~ az),
        sz_top          = case_when(sz_top          == "null" ~ NA_character_, TRUE ~ sz_top),
        sz_bot          = case_when(sz_bot          == "null" ~ NA_character_, TRUE ~ sz_bot),
        hit_distance_sc = case_when(hit_distance_sc == "null" ~ NA_character_, TRUE ~ hit_distance_sc),
        bb_type         = case_when(bb_type         == "null" ~ NA_character_, TRUE ~ bb_type),
        hit_location    = case_when(hit_location    == "null" ~ NA_character_, TRUE ~ hit_location),
        launch_speed    = case_when(launch_speed    == "null" ~ NA_character_, TRUE ~ launch_speed),
        launch_angle    = case_when(launch_angle    == "null" ~ NA_character_, TRUE ~ launch_angle),
        effective_speed = case_when(effective_speed == "null" ~ NA_character_, TRUE ~ effective_speed),
        
        release_spin_rate = case_when(release_spin_rate == "null" ~ NA_character_ , 
                                                            TRUE  ~ release_spin_rate),
        
        release_extension = case_when(
            release_extension == "null" ~ NA_character_ , 
                                  TRUE  ~ release_extension),
        
        game_pk        = case_when(game_pk        == "null" ~ NA_character_, TRUE ~ game_pk),
        pos1_person_id = case_when(pos1_person_id == "null" ~ NA_character_, TRUE ~ pos1_person_id),
        pos2_person_id = case_when(pos2_person_id == "null" ~ NA_character_, TRUE ~ pos2_person_id),
        pos3_person_id = case_when(pos3_person_id == "null" ~ NA_character_, TRUE ~ pos3_person_id),
        pos4_person_id = case_when(pos4_person_id == "null" ~ NA_character_, TRUE ~ pos4_person_id),
        pos5_person_id = case_when(pos5_person_id == "null" ~ NA_character_, TRUE ~ pos5_person_id),
        pos6_person_id = case_when(pos6_person_id == "null" ~ NA_character_, TRUE ~ pos6_person_id),
        pos7_person_id = case_when(pos7_person_id == "null" ~ NA_character_, TRUE ~ pos7_person_id),
        pos8_person_id = case_when(pos8_person_id == "null" ~ NA_character_, TRUE ~ pos8_person_id),
        pos9_person_id = case_when(pos9_person_id == "null" ~ NA_character_, TRUE ~ pos9_person_id),
        release_pos_y  = case_when(release_pos_y  == "null" ~ NA_character_, TRUE ~ release_pos_y),
        
        estimated_ba_using_speedangle = case_when(
            estimated_ba_using_speedangle == "null" ~ NA_character_,
                                              TRUE  ~ estimated_ba_using_speedangle
        ),
        
        estimated_woba_using_speedangle = case_when(
            estimated_woba_using_speedangle == "null" ~ NA_character_,
                                                TRUE  ~ estimated_woba_using_speedangle
        ),
        
        woba_value  = case_when(woba_value  == "null" ~ NA_character_, TRUE ~ woba_value),
        woba_denom  = case_when(woba_denom  == "null" ~ NA_character_, TRUE ~ woba_denom),
        babip_value = case_when(babip_value == "null" ~ NA_character_, TRUE ~ babip_value),
        iso_value   = case_when(iso_value   == "null" ~ NA_character_, TRUE ~ iso_value),
        
        launch_speed_angle = case_when(launch_speed_angle == "null" ~ NA_character_ ,
                                                              TRUE  ~ launch_speed_angle), 
        
        at_bat_number = case_when(at_bat_number == "null" ~ NA_character_, TRUE ~ at_bat_number),
        pitch_number  = case_when(pitch_number  == "null" ~ NA_character_, TRUE ~ pitch_number),
        
        inning_topbot = case_when(inning_topbot == "Bot"  ~ "bot",
                                  inning_topbot == "Top"  ~ "top")
    ) %>% 
                
    mutate(
        release_speed                   = as.double(release_speed),
        release_pos_x                   = as.double(release_pos_x),
        release_pos_z                   = as.double(release_pos_z),
        batter                          = as.integer(batter),
        pitcher                         = as.integer(pitcher),
        zone                            = as.integer(zone),
        hit_location                    = as.integer(hit_location),
        balls                           = as.integer(balls),
        strikes                         = as.integer(strikes),
        game_year                       = as.integer(game_year),
        pfx_x                           = as.double(pfx_x),
        pfx_z                           = as.double(pfx_z),
        plate_x                         = as.double(plate_x),
        plate_z                         = as.double(plate_z),
        on_3b                           = as.integer(on_3b),
        on_2b                           = as.integer(on_2b),
        on_1b                           = as.integer(on_1b),
        outs_when_up                    = as.integer(outs_when_up),
        inning                          = as.integer(inning),
        hc_x                            = as.double(hc_x),
        hc_y                            = as.double(hc_y),
        umpire                          = as.integer(umpire),
        vx0                             = as.double(vx0),
        vy0                             = as.double(vy0),
        vz0                             = as.double(vz0),
        ax                              = as.double(ax),
        ay                              = as.double(ay),
        az                              = as.double(az),
        sz_top                          = as.double(sz_top),
        sz_bot                          = as.double(sz_bot),
        hit_distance_sc                 = as.integer(hit_distance_sc),
        launch_speed                    = as.double(launch_speed),
        launch_angle                    = as.double(launch_angle),
        effective_speed                 = as.double(effective_speed),
        release_spin_rate               = as.integer(release_spin_rate),
        release_extension               = as.double(release_extension),
        game_pk                         = as.integer(game_pk),
        pos1_person_id                  = as.integer(pos1_person_id),
        pos2_person_id                  = as.integer(pos2_person_id),
        pos3_person_id                  = as.integer(pos3_person_id),
        pos4_person_id                  = as.integer(pos4_person_id),
        pos5_person_id                  = as.integer(pos5_person_id),
        pos6_person_id                  = as.integer(pos6_person_id),
        pos7_person_id                  = as.integer(pos7_person_id),
        pos8_person_id                  = as.integer(pos8_person_id),
        pos9_person_id                  = as.integer(pos9_person_id),
        release_pos_y                   = as.double(release_pos_y),
        estimated_ba_using_speedangle   = as.double(estimated_ba_using_speedangle),
        estimated_woba_using_speedangle = as.double(estimated_woba_using_speedangle),
        woba_value                      = as.double(woba_value),
        woba_denom                      = as.integer(woba_denom),
        babip_value                     = as.integer(babip_value),
        iso_value                       = as.integer(iso_value),
        launch_speed_angle              = as.integer(launch_speed_angle),
        at_bat_number                   = as.integer(at_bat_number),
        pitch_number                    = as.integer(pitch_number)
        
    ) %>% 

    rename(
        launch_speed_sc = launch_speed,
        launch_angle_sc = launch_angle
        ) %>% 
    
    mutate(
        sv_id = case_when(sv_id == "null" ~ NA_character_ ,
                          sv_id == ""     ~ NA_character_ ,
                          TRUE            ~ sv_id)
        ) %>% 
    
    mutate(
        home_team = case_when(home_team == "FLA" ~ "MIA",
                              TRUE               ~ home_team)
        ) %>% 

    mutate(
        away_team = case_when(away_team == "FLA" ~ "MIA",
                              TRUE               ~ away_team)
        ) %>% 
        
    mutate(
        ballpark = case_when(
            home_team == "ATL" & as.integer(game_year) <  2017L ~ "Turner Field",
            home_team == "ATL" & as.integer(game_year) >= 2017L ~ "SunTrust Park",
            home_team == "ARI"                                  ~ "Chase Field",
            home_team == "BAL"                                  ~ "Camden Yards",
            home_team == "BOS"                                  ~ "Fenway Park",
            home_team == "CIN"                                  ~ "Great American Ball Park",
            home_team == "COL"                                  ~ "Coors Field",
            home_team == "CWS"                                  ~ "U.S. Cellular Field",
            home_team == "DET"                                  ~ "Comerica Park",
            home_team == "CHC"                                  ~ "Wrigley Field",
            home_team == "CLE"                                  ~ "Progressive Field",
            home_team == "HOU"                                  ~ "Minute Maid Park",
            home_team == "KC"                                   ~ "Kauffman Stadium",
            home_team == "LAA"                                  ~ "Angel Stadium",
            home_team == "LAD"                                  ~ "Dodger Stadium",
            home_team == "MIA" & as.integer(game_year) <  2012L ~ "Land Shark Stadium",
            home_team == "MIA" & as.integer(game_year) >= 2012L ~ "Marlins Park",
            home_team == "MIL"                                  ~ "Miller Park",
            home_team == "MIN" & as.integer(game_year) <  2010L ~ "Metrodome",
            home_team == "MIN" & as.integer(game_year) >= 2010L ~ "Target Field",
            home_team == "NYM" & as.integer(game_year) <  2009L ~ "Shea Stadium",
            home_team == "NYM" & as.integer(game_year) >= 2009L ~ "Citi Field",
            home_team == "NYY" & as.integer(game_year) <  2009L ~ "Old Yankee Stadium",
            home_team == "NYY" & as.integer(game_year) >= 2009L ~ "Yankee Stadium",
            home_team == "OAK"                                  ~ "O.co Coliseum",
            home_team == "PIT"                                  ~ "PNC Park",
            home_team == "PHI"                                  ~ "Citizens Bank Park",
            home_team == "STL"                                  ~ "Busch Stadium",
            home_team == "SEA"                                  ~ "Safeco Field",
            home_team == "SF"                                   ~ "AT&T Park",
            home_team == "SD"                                   ~ "PETCO Park",
            home_team == "TB"                                   ~ "Tropicana Field",
            home_team == "TEX"                                  ~ "Globe Life Park",
            home_team == "TOR"                                  ~ "Rogers Centre",
            home_team == "WSH"                                  ~ "Nationals Park"
            
        )
    ) %>% 

    mutate(
        pitch_type = case_when(
            pitch_type == "CH" ~ "Changeup",
            pitch_type == "CU" ~ "Curveball",
            pitch_type == "EP" ~ "Eephus",
            pitch_type == "FA" ~ "Fastball",
            pitch_type == "FC" ~ "Cutter",
            pitch_type == "FF" ~ "Four-seam Fastball",
            pitch_type == "FS" ~ "Splitter",
            pitch_type == "FT" ~ "Two-seam Fastball",
            pitch_type == "FO" ~ "Forkball",
            pitch_type == "IN" ~ "Intent ball",
            pitch_type == "KC" ~ "Knuckle ball Curve",
            pitch_type == "KN" ~ "Knuckle ball",
            pitch_type == "PO" ~ "Pitch Out",
            pitch_type == "SC" ~ "Screwball",
            pitch_type == "SI" ~ "Sinker",
            pitch_type == "SL" ~ "Slider"
        )
    ) %>% 
    
    rename(pitcher_id_sc = pitcher,
           batter_id_sc = batter) %>% 

    mutate(batter_team = case_when(
            inning_topbot == "bot" ~ home_team,
            inning_topbot == "top" ~ away_team
        )
    ) %>%

    mutate(pitcher_team = case_when(
            inning_topbot == "bot" ~ away_team,
            inning_topbot == "top" ~ home_team
        )
    ) %>%

    
    ## trigonometric operations
    
    mutate(
        hc_x.0 = hc_x - 125.42,
        hc_y.0 = 198.27 - hc_y,
        hc_x.new.1 = hc_x.0 * cos(-.75) - hc_y.0 * sin(-.75),
        hc_y.new.1 = hc_x.0 * sin(-.75) + hc_y.0 * cos(-.75),
        horiz_angle = cart2pol(x = hc_x.new.1, y = hc_y.new.1, degrees = FALSE) %>% pull(theta),
        hit_radius = cart2pol(x = hc_x.new.1, y = hc_y.new.1, degrees = FALSE) %>% pull(r),
        horiz_angle_new_deg = (horiz_angle * 180) / pi,
        horiz_angle_c = (horiz_angle_new_deg - 45) * -1
    ) %>% 
    
    # mutate(
    #     hc_x.0 = hc_x - 125,
    #     hc_y.0 = 200 - hc_y,
    #     hc_x.new.1 = hc_x.0 * cos(-.75) - hc_y.0 * sin(-.75),
    #     hc_y.new.1 = hc_x.0 * sin(-.75) + hc_y.0 * cos(-.75),
    #     horiz_angle = atan((hc_y.new.1) / (hc_x.new.1)),
    #     hit_radius = sqrt((hc_x.new.1)^2 + ((hc_y.new.1))^2)) %>%
    # 
    # mutate(
    #     horiz_angle_new = case_when(
    # 
    #         (sign(hc_x.new.1) == 1 | sign(hc_x.new.1) == 0 | is.na(hc_x.new.1)) &
    #         (sign(hc_y.new.1) == 1 | sign(hc_y.new.1) == 0 | is.na(hc_y.new.1)) ~ horiz_angle + 0,
    # 
    #         (sign(hc_x.new.1) == -1) ~ horiz_angle + pi,
    # 
    #         (sign(hc_x.new.1) == 1) & (sign(hc_y.new.1) == -1) ~ horiz_angle + 2*pi, 
    #         
    #         NA ~ NaN)) %>%
    # 
    # mutate(
    #     horiz_angle_new_deg = (horiz_angle_new * 180) / pi,
    #     hc_x.new            = hit_radius * cos(horiz_angle_new),
    #     hc_y.new            = hit_radius * sin(horiz_angle_new)) %>%
    # 
    # mutate(horiz_angle_c = (horiz_angle_new_deg - 45) * -1) %>%

    
    ## categorizing hits into angle groupings
    
    mutate(
        horiz_angle_2 = case_when(
                horiz_angle_c <= 0 & horiz_angle_c >= -45 ~ "Left",
                horiz_angle_c  > 0 & horiz_angle_c <=  45 ~ "Right") %>%
            factor(.,
                   levels = c("Left",
                              "Right"),
                   ordered = TRUE), 

        horiz_angle_3 = case_when(
                horiz_angle_c < -15                           ~ "Left",
                (horiz_angle_c >= -15) & (horiz_angle_c < 15) ~ "Center",
                horiz_angle_c >= 15                           ~ "Right") %>%
            factor(.,
                   levels = c("Left",
                              "Center",
                              "Right"),
                   ordered = TRUE), 

        horiz_angle_4 = case_when(
                horiz_angle_c  < -22.5                            ~ "Left",
                (horiz_angle_c >= -22.5) & (horiz_angle_c < 0)    ~ "Left Center",
                (horiz_angle_c >= 0)     & (horiz_angle_c < 22.5) ~ "Right Center",
                horiz_angle_c  >= 22.5                            ~ "Right") %>%
            factor(.,
                   levels = c("Left",
                              "Left Center",
                              "Right Center",
                              "Right"),
                   ordered = TRUE), 

        horiz_angle_5 = case_when(
                horiz_angle_c   < -27                          ~ "Left",
                (horiz_angle_c >= -27) & (horiz_angle_c  < -9) ~ "Left Center",
                (horiz_angle_c >= -9)  & (horiz_angle_c  <  9) ~ "Center",
                (horiz_angle_c >=  9)  & (horiz_angle_c  < 27) ~ "Right Center",
                horiz_angle_c  >=  27                          ~ "Right") %>%
            factor(.,
                   levels = c("Left",
                              "Left Center",
                              "Center",
                              "Right Center",
                              "Right"),
                   ordered = TRUE), 

        horiz_angle_6 = case_when(
                horiz_angle_c  <  -30                          ~ "Left",
                (horiz_angle_c >= -30) & (horiz_angle_c < -15) ~ "Left Center",
                (horiz_angle_c >= -15) & (horiz_angle_c < 0)   ~ "Center Left",
                (horiz_angle_c >=  0)  & (horiz_angle_c < 15)  ~ "Center Right",
                (horiz_angle_c >=  15) & (horiz_angle_c < 30)  ~ "Right Center",
                horiz_angle_c  >=  30                          ~ "Right") %>%
            factor(.,
                   levels = c("Left",
                              "Left Center",
                              "Center Left",
                              "Center Right",
                              "Right Center",
                              "Right"),
                   ordered = TRUE)
        ) %>% 
    

    
    ## times are coded in local time. you can't set more than one time zone in a single date 
    ##  column. so to sync with the other time zones, I'm pretending that they are all UTC, then 
    ##  forcing them to their proper time zones, then converting to UTC for real
    
    mutate(
        obs_date_time_loc =
            make_datetime(
                year = str_c("20", substr(sv_id, 1, 2)) %>% as.integer(),
                month = substr(sv_id, 3, 4) %>% as.integer(),
                day = substr(sv_id, 5, 6) %>% as.integer(),
                hour = substr(sv_id, 8, 9) %>% as.integer(),
                min = substr(sv_id, 10, 11) %>% as.integer(),
                sec = substr(sv_id, 12, 13) %>% as.integer(),
                tz = "UTC"
            )
    ) %>% 

    mutate(
        obs_date_time_utc =
            case_when(
                ballpark %in% c("Busch Stadium",
                                "Globe Life Park",
                                "Target Field",
                                "Kauffman Stadium",
                                "Metrodome",
                                "Miller Park",
                                "Minute Maid Park",
                                "Target Field",
                                "U.S. Cellular Field",
                                "Wrigley Field") ~ force_tz(obs_date_time_loc,
                                                            tzone = "US/Central") %>%
                    with_tz(., tzone = "UTC"), 
                
                ballpark %in% c("Camden Yards",
                                "Citi Field",
                                "Citizens Bank Park",
                                "Comerica Park",
                                "Fenway Park",
                                "Great American Ball Park",
                                "Land Shark Stadium",
                                "Marlins Park",
                                "Nationals Park",
                                "Old Yankee Stadium",
                                "PNC Park",
                                "Progressive Field",
                                "Rogers Centre",
                                "Shea Stadium",
                                "SunTrust Park",
                                "Tropicana Field",
                                "Turner Field",
                                "Yankee Stadium") ~ force_tz(obs_date_time_loc, 
                                                             tzone = "US/Eastern") %>%
                    with_tz(., tzone = "UTC"), 
                
                ballpark == "Coors Field" ~ force_tz(obs_date_time_loc, 
                                                     tzone = "US/Mountain") %>% 
                    with_tz(., tzone = "UTC"),
                
                ballpark == "Chase Field" ~ force_tz(obs_date_time_loc, 
                                                     tzone = "America/Phoenix") %>% 
                    with_tz(., tzone = "UTC"),
                
                ballpark %in% c("Dodger Stadium",
                                "AT&T Park",
                                "PETCO Park",
                                "O.co Coliseum",
                                "Angel Stadium",
                                "Safeco Field") ~ force_tz(obs_date_time_loc, 
                                                           tzone = "US/Pacific") %>% 
                    with_tz(., tzone = "UTC")
                
            )) %>% 
    
    mutate(
        obs_date_time_loc_chr = as.character(obs_date_time_loc),
        obs_date_time_utc_chr = as.character(obs_date_time_utc)) %>%
    
    mutate(
        loc_date_round = round_date(obs_date_time_loc, unit = "hour"),
        utc_date_round = round_date(obs_date_time_utc, unit = "hour")) %>%
    
    mutate(
        loc_date_round_chr = as.character(loc_date_round),
        utc_date_round_chr = as.character(utc_date_round))


# rm(statcast_update_comb_distinct)
gc()


#------------------------#
# Finishing up ----
#------------------------#

## querying the number of rows ##

updated_rows <-
    statcast_update_comb_distinct_clean %>% 
    select(game_date) %>% 
    nrow()


## pretty printing ##

cat("---------\n")
cat("Overall:\n")
cat(updated_rows, "rows added\n")
statcast_update_comb_distinct_clean %>% object_size() %>% print()
cat("---------\n")


## writing to temporary table in database ##

dbWriteTable(
    statcast_db,
    "statcast_update_comb_distinct_clean",
    statcast_update_comb_distinct_clean,
    overwrite = TRUE
)


## appending to main table in database ##

dbWriteTable(
    statcast_db,
    "statcast_data_updated",
    statcast_update_comb_distinct_clean,
    append = TRUE, 
    overwrite = FALSE, 
    temporary = FALSE)


# rm(statcast_update_comb_distinct_clean)
gc()


# tbl(statcast_db, "statcast_data_updated") %>% glimpse()
# tbl(statcast_db, "statcast_data_updated") %>% select(game_date) %>% collect() %>% nrow()

#=========================#
# creating indexes ----
#=========================#

## If building database from scratch, create indexes ##

# db_create_index(statcast_db, "statcast_data_updated", "pitch_type")
# db_create_index(statcast_db, "statcast_data_updated", "game_date")
# db_create_index(statcast_db, "statcast_data_updated", "game_year")
# db_create_index(statcast_db, "statcast_data_updated", "game_pk")
# db_create_index(statcast_db, "statcast_data_updated", "at_bat_number")
# db_create_index(statcast_db, "statcast_data_updated", "pitch_number")
# db_create_index(statcast_db, "statcast_data_updated", "events")
# db_create_index(statcast_db, "statcast_data_updated", "type")
# db_create_index(statcast_db, "statcast_data_updated", "player_name")
# db_create_index(statcast_db, "statcast_data_updated", "batter_id_sc")
# db_create_index(statcast_db, "statcast_data_updated", "pitcher_id_sc")
# db_create_index(statcast_db, "statcast_data_updated", "batter_team")
# db_create_index(statcast_db, "statcast_data_updated", "pitcher_team")
# db_create_index(statcast_db, "statcast_data_updated", "description")
# db_create_index(statcast_db, "statcast_data_updated", "hit_location")
# db_create_index(statcast_db, "statcast_data_updated", "game_type")
# db_create_index(statcast_db, "statcast_data_updated", "home_team")
# db_create_index(statcast_db, "statcast_data_updated", "away_team")
# db_create_index(statcast_db, "statcast_data_updated", "ballpark")
# db_create_index(statcast_db, "statcast_data_updated", "bb_type")
# db_create_index(statcast_db, "statcast_data_updated", "balls")
# db_create_index(statcast_db, "statcast_data_updated", "strikes")
# db_create_index(statcast_db, "statcast_data_updated", "outs_when_up")
# db_create_index(statcast_db, "statcast_data_updated", "inning")
# db_create_index(statcast_db, "statcast_data_updated", "stand")
# db_create_index(statcast_db, "statcast_data_updated", "p_throws")
# db_create_index(statcast_db, "statcast_data_updated", "utc_date_round_chr")
# db_create_index(statcast_db, "statcast_data_updated", "obs_date_utc_chr")
# 
# dbGetQuery(statcast_db, "SELECT * FROM sqlite_master WHERE type = 'index'")


dbDisconnect(statcast_db)


############################################################################################
############################################################################################

# dbExecute(statcast_db, "ALTER TABLE statcast_data_updated ADD if_fielding_alignment")
# dbExecute(statcast_db, "ALTER TABLE statcast_data_updated ADD of_fielding_alignment")
# dbExecute(statcast_db, "ALTER TABLE statcast_data_updated ADD post_away_score")
# dbExecute(statcast_db, "ALTER TABLE statcast_data_updated ADD post_home_score")
# dbExecute(statcast_db, "ALTER TABLE statcast_data_updated ADD post_bat_score")
# dbExecute(statcast_db, "ALTER TABLE statcast_data_updated ADD post_fld_score")
# dbExecute(statcast_db, "ALTER TABLE statcast_data_updated ADD pitch_name")
# dbExecute(statcast_db, "ALTER TABLE statcast_data_updated ADD home_score")
# dbExecute(statcast_db, "ALTER TABLE statcast_data_updated ADD away_score")
# dbExecute(statcast_db, "ALTER TABLE statcast_data_updated ADD bat_score")
# dbExecute(statcast_db, "ALTER TABLE statcast_data_updated ADD fld_score")
# dbExecute(statcast_db, "ALTER TABLE statcast_data_updated ADD error")

# dbExecute(statcast_db, paste0("DELETE FROM statcast_data_updated WHERE game_date >= '2018-03-29'"))


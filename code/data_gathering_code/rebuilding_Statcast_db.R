###########################################################################################-
###########################################################################################-
##
## Building Statcast database ----
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
library(lubridate)
library(stringdist)
library(magrittr)
library(dbplyr)
library(DBI)
library(RSQLite)
library(pryr)
library(useful)
library(tictoc)
library(glue)
library(openxlsx)

#-----------------------------------------------------------------------------------------#
# Loading custom functions
#-----------------------------------------------------------------------------------------#

source("code/functions/get_savant_pitches_data.R")

rate <- rate_backoff(pause_base = 1, pause_cap = 512, pause_min = 1, max_times = Inf, jitter = FALSE)

insistently_get_savant_pitches_data <- 
    insistently(
        get_savant_pitches_data, 
        rate = rate, 
        quiet = FALSE
    )

game_date <- NA_character_
get_game_date <- function(...) identity(game_date)

insistently_get_savant_pitches_data_possibly <-
    possibly(
        insistently_get_savant_pitches_data,
        otherwise = tibble(game_date = get_game_date()))

#-----------------------------------------------------------------------------------------#
# Connecting to database
#-----------------------------------------------------------------------------------------#

statcast_db <- dbConnect(SQLite(), "data/statcast_db_rebuilt.sqlite3")

#-----------------------------------------------------------------------------------------#
# Setting search parameters
#-----------------------------------------------------------------------------------------#

## Setting start_date to March 1, 2008 ##

start_date <- as_date("2008-03-01")

## Setting end_date to today ##

end_date <- today(tzone = "US/Eastern")

## setting the time span ##

num_days <- (end_date - start_date) %>% as.numeric()

## sequence of single days ##

every.1.days <- seq(1, num_days, 1)

## single-day spans ##

single_days_all <- sort(end_date - every.1.days, decreasing = FALSE)

single_days <- single_days_all[which(month(single_days_all) %in% c(3:11))]

#=========================================================================================#
# Downloading data ----
#=========================================================================================#

## need to break up in "home" and "away" batches, to avoid getting a "too much data" error ##

now() %>% format("%r")
tic()

for(each_day in 1:length(single_days)) {
    
    
    game_date <- single_days[each_day]
    
    
    ## Home ##
    
    statcast_update_home <-
        insistently_get_savant_pitches_data_possibly(
            start_date = game_date,
            end_date = game_date,
            home_road = "Home")
    
    
    ## Road ##
    
    statcast_update_road <-
        insistently_get_savant_pitches_data_possibly(
            start_date = game_date,
            end_date = game_date,
            home_road = "Road")
    
    ## Combining home and away ##

    statcast_update_comb <-
        bind_rows(
            statcast_update_home,
            statcast_update_road
        ) %>%
        as_tibble()
    
    ## Writing to database ##
    
    dbWriteTable(
        statcast_db,
        "statcast_update_comb",
        statcast_update_comb,
        overwrite = FALSE,
        append = TRUE
    )
    
    
    ## Pretty printing info to console
    
    if (nrow(statcast_update_home) > 0 | nrow(statcast_update_road) > 0) {
        
        cat("\n===============================\n")
        cat("Loop", each_day)
        cat("|-- ")
        cat(game_date %>% as.character())
        cat(" --|")
        cat("\n-------------------------------\n")
        cat("Home")
        cat("\n- - - - - - - - - - - - - - - -\n")
        cat(nrow(statcast_update_home), "rows added\n")
        cat("Size: ")
        object_size(statcast_update_home) %>% print()
        cat("-------------------------------\n")
        cat("Road")
        cat("\n- - - - - - - - - - - - - - - -\n")
        cat(nrow(statcast_update_road), "rows added\n")
        cat("Size: ")
        object_size(statcast_update_road) %>% print()
        cat("===============================\n")
        
    }
    
    gc(verbose = FALSE)
    
    Sys.sleep(1)

}


now() %>% format("%r")
toc()


#=========================================================================================#
# Cleaning ----
#=========================================================================================#

statcast_db       <- dbConnect(SQLite(), "data/statcast_db_rebuilt.sqlite3")
statcast_db_chunk <- dbConnect(SQLite(), "data/statcast_db_rebuilt_chunk.sqlite3")

#-----------------------------------------------------------------------------------------#
# Setting up query
#-----------------------------------------------------------------------------------------#

statcast_res <- 
    dbSendQuery(
        statcast_db,
        "SELECT * FROM statcast_update_comb"
    )

#-----------------------------------------------------------------------------------------#
# Getting data in chunks
#-----------------------------------------------------------------------------------------#

while (!dbHasCompleted(statcast_res)) {
    
    statcast_update_comb_distinct_clean <-
        
        dbFetch(statcast_res, n = 5e5) %>% 
        as_tibble() %>% 
        
        select(
            -contains("error"),
            -contains("_deprecated"), # dropping deprecated columns
            -fielder_2
        ) %>%     
        rename(fielder_2 = fielder_2_1) %>% 
        mutate_all(list(~case_when(. == "null" ~ NA_character_, TRUE ~ .))) %>% 
        type_convert() %>% 
        
        mutate(
            game_date = as_date(game_date),
            year      = year(game_date),
            month     = month(game_date),
            day       = day(game_date),
            wday      = wday(game_date)
        ) %>% 
        
        mutate(
            obs_date_time_loc =
                make_datetime(
                    year  = str_c("20", substr(sv_id, 1, 2)) %>% as.integer(),
                    month = substr(sv_id, 3, 4) %>% as.integer(),
                    day   = substr(sv_id, 5, 6) %>% as.integer(),
                    hour  = substr(sv_id, 8, 9) %>% as.integer(),
                    min   = substr(sv_id, 10, 11) %>% as.integer(),
                    sec   = substr(sv_id, 12, 13) %>% as.integer(),
                    tz    = "UTC"
                )
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
            obs_date_time_utc =
                case_when(
                    
                    ballpark %in% 
                        c("Busch Stadium",
                          "Globe Life Park",
                          "Target Field",
                          "Kauffman Stadium",
                          "Metrodome",
                          "Miller Park",
                          "Minute Maid Park",
                          "Target Field",
                          "U.S. Cellular Field",
                          "Wrigley Field") ~
                        force_tz(obs_date_time_loc, tzone = "US/Central") %>%
                        with_tz(., tzone = "UTC"), 
                    
                    ballpark %in% 
                        c("Camden Yards",
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
                          "Yankee Stadium") ~
                        force_tz(obs_date_time_loc, tzone = "US/Eastern") %>%
                        with_tz(., tzone = "UTC"), 
                    
                    ballpark == "Coors Field" ~ 
                        force_tz(obs_date_time_loc, tzone = "US/Mountain") %>% 
                        with_tz(., tzone = "UTC"),
                    
                    ballpark == "Chase Field" ~ 
                        force_tz(obs_date_time_loc, tzone = "America/Phoenix") %>% 
                        with_tz(., tzone = "UTC"),
                    
                    ballpark %in% 
                        c("Dodger Stadium",
                          "AT&T Park",
                          "PETCO Park",
                          "O.co Coliseum",
                          "Angel Stadium",
                          "Safeco Field") ~ 
                        force_tz(obs_date_time_loc, tzone = "US/Pacific") %>% 
                        with_tz(., tzone = "UTC")
                    
                )) %>% 
        
        mutate(
            obs_date_time_loc_chr = as.character(obs_date_time_loc),
            obs_date_time_utc_chr = as.character(obs_date_time_utc)
        ) %>%
        mutate(
            loc_date_round = round_date(obs_date_time_loc, unit = "hour"),
            utc_date_round = round_date(obs_date_time_utc, unit = "hour")
        ) %>%
        mutate(
            loc_date_round_chr = as.character(loc_date_round),
            utc_date_round_chr = as.character(utc_date_round)
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
        
        mutate(batter_team = case_when(
            inning_topbot == "bot" ~ home_team,
            inning_topbot == "top" ~ away_team
        )) %>% 
        
        mutate(pitcher_team = case_when(
            inning_topbot == "bot" ~ away_team,
            inning_topbot == "top" ~ home_team
        )) %>% 
        
        
        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
        # trigonometric operations
        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
        
        mutate(
            hc_x.0 = hc_x - 125.42,
            hc_y.0 = 198.27 - hc_y,
            hc_x.new.1 = hc_x.0 * cos(-.75) - hc_y.0 * sin(-.75),
            hc_y.new.1 = hc_x.0 * sin(-.75) + hc_y.0 * cos(-.75),
            horiz_angle = cart2pol(x = hc_x.new.1, y = hc_y.new.1, degrees = FALSE) %>% pull(theta),
            hit_radius = cart2pol(x = hc_x.new.1, y = hc_y.new.1, degrees = FALSE) %>% pull(r),
            horiz_angle_deg = (horiz_angle * 180) / pi,
            horiz_angle_c = (horiz_angle_deg - 45) * -1
        ) %>% 
        
    
        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
        # categorizing hits into angle groupings
        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
        
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
        )
        
    
    gc()
    
    
    #-----------------------------------------------------------------------------------------#
    # Finishing up
    #-----------------------------------------------------------------------------------------#
    
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
    # querying the number of rows
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

    updated_rows <-
        statcast_update_comb_distinct_clean %>% 
        select(game_date) %>% 
        nrow()
    
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
    # pretty printing
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
    
    cat("---------\n")
    cat("Overall:\n")
    cat(updated_rows, "rows added\n")
    statcast_update_comb_distinct_clean %>% object_size() %>% print()
    cat("---------\n")
    
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
    # appending to main table in database
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
    
    dbWriteTable(
        statcast_db_chunk,
        "statcast_data",
        statcast_update_comb_distinct_clean,
        append = TRUE, 
        overwrite = FALSE, 
        temporary = FALSE)
    
    
}

dbClearResult(statcast_res)
dbDisconnect(statcast_db)
dbDisconnect(statcast_db_chunk)

rm(statcast_update_comb_distinct_clean)

gc()

#-----------------------------------------------------------------------------------------#
# Putting into real database ----
#-----------------------------------------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Connecting to databases
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

statcast_db       <- dbConnect(SQLite(), "data/statcast_db_rebuilt.sqlite3")
statcast_db_chunk <- dbConnect(SQLite(), "data/statcast_db_rebuilt_chunk.sqlite3")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Putting into real databsse
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

statcast_res <- 
    dbSendQuery(
        statcast_db_chunk, 
        "SELECT * FROM statcast_data"
    )

while (!dbHasCompleted(statcast_res)) {
    
    statcast_chunk <- dbFetch(statcast_res, n = 5e5)
    
    dbWriteTable(
        statcast_db,
        "statcast_data",
        value = statcast_chunk,
        append = TRUE,
        temporary = FALSE)
    
    gc()
    
}

dbClearResult(statcast_res)

rm(statcast_chunk)

gc()

dbDisconnect(statcast_db)
dbDisconnect(statcast_db_chunk)

#=========================================================================================#
# creating indexes ----
#=========================================================================================#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Connecting to databases
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

statcast_db <- dbConnect(SQLite(), "data/statcast_db_rebuilt.sqlite3")

## If building database from scratch, create indexes ##

db_create_index(statcast_db, "statcast_data", "pitch_type")
db_create_index(statcast_db, "statcast_data", "game_date")
db_create_index(statcast_db, "statcast_data", "game_year")
db_create_index(statcast_db, "statcast_data", "game_pk")
db_create_index(statcast_db, "statcast_data", "at_bat_number")
db_create_index(statcast_db, "statcast_data", "pitch_number")
db_create_index(statcast_db, "statcast_data", "events")
db_create_index(statcast_db, "statcast_data", "type")
db_create_index(statcast_db, "statcast_data", "player_name")
db_create_index(statcast_db, "statcast_data", "batter")
db_create_index(statcast_db, "statcast_data", "pitcher")
db_create_index(statcast_db, "statcast_data", "batter_team")
db_create_index(statcast_db, "statcast_data", "pitcher_team")
db_create_index(statcast_db, "statcast_data", "description")
db_create_index(statcast_db, "statcast_data", "hit_location")
db_create_index(statcast_db, "statcast_data", "game_type")
db_create_index(statcast_db, "statcast_data", "home_team")
db_create_index(statcast_db, "statcast_data", "away_team")
db_create_index(statcast_db, "statcast_data", "ballpark")
db_create_index(statcast_db, "statcast_data", "bb_type")
db_create_index(statcast_db, "statcast_data", "balls")
db_create_index(statcast_db, "statcast_data", "strikes")
db_create_index(statcast_db, "statcast_data", "outs_when_up")
db_create_index(statcast_db, "statcast_data", "inning")
db_create_index(statcast_db, "statcast_data", "stand")
db_create_index(statcast_db, "statcast_data", "p_throws")
db_create_index(statcast_db, "statcast_data", "utc_date_round_chr")

dbGetQuery(statcast_db, "SELECT * FROM sqlite_master WHERE type = 'index'")

dbExecute(statcast_db, "VACUUM")

dbDisconnect(statcast_db)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# #
# #                             ---- THIS IS THE END! ----
# #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

###########################################
###########################################
##
## Auto downloading hit tracker data
##
###########################################
###########################################

#=========================#
#### Loading packages ####
#=========================#

library(QuantPsyc)
library(readr)
library(rvest)
library(tidyr)
library(stringr)
library(pitchRx)
library(lubridate)
library(stringdist)
library(dplyr)
library(tibble)


#=====================#
#### Loading Data ####
#=====================#

setwd("~/BASP/R analyses/Baseball Data/Data Files")

# hittracker_all <- read_rds("hittracker_all_distinct.RDS")
hittracker_all <- read_rds("hittracker_updated_distinct.RDS")

cat("-----\n")

paste("Searching results from: ", today() - 1, ", back to: ", max(hittracker_all$game_date), sep = "") %>% noquote() %>% print()

cat("-----\n")


#=========================#
#### Downloading Data ####
#=========================#

ht_session <- 
    html_session(
        "http://www.hittrackeronline.com/index.php",
        httr::set_cookies(season = 2017, perpage = 10000)
    )

hittracker_update_raw <-
    ht_session %>% 
    html_nodes("table") %>% 
    magrittr::extract2(17) %>%
    html_table(header = FALSE) %>% 
    slice(3:nrow(.)) %>% 
    magrittr::set_colnames(.[1,]) %>% 
    slice(2:(nrow(.)-2)) %>%
    as_data_frame() %>% 
    transmute(
        game_date = parse_date(Date, format = "%D"),
        batter_name = parse_character(Hitter),
        batter_team = parse_character(HitterTeam),
        pitcher_name = parse_character(Pitcher),
        pitcher_team = parse_character(PitcherTeam),
        inning = parse_number(INN),
        ballpark = parse_character(Ballpark),
        type_luck = parse_character(`Type/Luck`),
        hit_distance_ht = parse_number(TrueDist.),
        launch_speed_ht = parse_number(SpeedOffBat),
        launch_angle_ht = parse_number(Elev.Angle),
        horiz_angle = parse_number(Horiz.Angle),
        apex = parse_number(Apex),
        num_parks = parse_number(`#Parks`),
        game_year = game_date %>% year(),
        game_type = "Regular Season"
    )
    
# hittracker_update_raw %>% glimpse()
# hittracker_update_raw %>% slice(1) %>% glimpse()
# hittracker_update_raw %>% slice(nrow(.)) %>% glimpse()


#=====================#
#### Cleaning Data ####
#=====================#

hittracker_update <-
    
    hittracker_update_raw %>% 


## Modifying names
    

    mutate(batter_name_ch = as.character(batter_name)) %>%

    mutate(
        batter_name_first = str_split(
            batter_name_ch,
            pattern = ", ", 
            n = 2, 
            simplify = TRUE) %>% unlist() %>% .[,2],
        batter_name_last = str_split(
            batter_name_ch, 
            pattern = ", ", 
            n = 2, 
            simplify = TRUE) %>% unlist() %>% .[,1]
        ) %>% 

    select(-batter_name_ch) %>%
    
    mutate(batter_name_last = str_replace_all(batter_name_last, " Jr.", "")) %>%
    mutate(batter_name_last = str_replace_all(batter_name_last, "\\ ", "")) %>%
    mutate(batter_name_last = str_replace_all(batter_name_last, "\\.", "")) %>%
    mutate(batter_name_last = str_to_lower(string = batter_name_last)) %>% 
    mutate(batter_name_first = str_replace_all(batter_name_first, "\\ ", "")) %>%
    mutate(batter_name_first = str_replace_all(batter_name_first, "\\.", "")) %>%
    mutate(batter_name_first = str_to_lower(string = batter_name_first)) %>% 
    mutate(batter_name_c = str_c(batter_name_last, batter_name_first, sep = ",")) %>% 
    mutate(batter_name_c = str_to_lower(string = batter_name_c)) %>% 
    
    
    mutate(pitcher_name_ch = as.character(pitcher_name)) %>%
    mutate(
        pitcher_name_first = str_split(
            pitcher_name_ch,
            pattern = ", ", 
            n = 2, 
            simplify = TRUE) %>% unlist() %>% .[,2],
        pitcher_name_last = str_split(
            pitcher_name_ch, 
            pattern = ", ", 
            n = 2, 
            simplify = TRUE) %>% unlist() %>% .[,1]
        ) %>% 
    
    select(-pitcher_name_ch) %>%

    mutate(pitcher_name_last = str_replace_all(pitcher_name_last, " Jr.", "")) %>%
    mutate(pitcher_name_last = str_replace_all(pitcher_name_last, "\\ ", "")) %>%
    mutate(pitcher_name_last = str_replace_all(pitcher_name_last, "\\.", "")) %>%
    mutate(pitcher_name_last = str_to_lower(string = pitcher_name_last)) %>% 
    mutate(pitcher_name_first = str_replace_all(pitcher_name_first, "\\ ", "")) %>%
    mutate(pitcher_name_first = str_replace_all(pitcher_name_first, "\\.", "")) %>%
    mutate(pitcher_name_first = str_to_lower(string = pitcher_name_first)) %>% 
    mutate(pitcher_name_c = str_c(pitcher_name_last, pitcher_name_first, sep = ",")) %>% 
    mutate(pitcher_name_c = str_to_lower(string = pitcher_name_c)) %>%   
    
    
## Accounting for ballpark name changes
    
    
    mutate(
        ballpark = with(., 
            case_when(
                str_detect(ballpark, "Globe") ~ "Globe Life Park",
                str_detect(ballpark, "Ameriquest") ~ "Globe Life Park",
                str_detect(ballpark, "Rangers") ~ "Globe Life Park",
                
                str_detect(ballpark, "Land Shark") ~ "Land Shark Stadium",
                str_detect(ballpark, "Sun Life") ~ "Land Shark Stadium",
                str_detect(ballpark, "Dolphin") ~ "Land Shark Stadium",
                
                str_detect(ballpark, "Progressive") ~ "Progressive Field",
                str_detect(ballpark, "Jacobs") ~ "Progressive Field",
                
                str_detect(ballpark, "O.co Coliseu") ~ "O.co Coliseum",
                str_detect(ballpark, "Oakland-Alam...") ~ "O.co Coliseum",
                str_detect(ballpark, "McAfee") ~ "O.co Coliseum",
                
                str_detect(ballpark, "O.co Coliseu") ~ "O.co Coliseum",
                str_detect(ballpark, "Oakland-Alam...") ~ "O.co Coliseum",
                str_detect(ballpark, "McAfee") ~ "O.co Coliseum",
                
                TRUE ~ identity(ballpark) %>% as.character()))) %>% 
    
## Accounting for ballpark name truncations
    
    mutate(
        ballpark = with(., 
            case_when(
                str_detect(ballpark, "Angel Stadiu...") ~ "Angel Stadium",
                str_detect(ballpark, "Busch Stadiu...") ~ "Busch Stadium",
                str_detect(ballpark, "Citizens Ban...") ~ "Citizens Bank Park",
                str_detect(ballpark, "Comerica Par...") ~ "Comerica Park",
                str_detect(ballpark, "Dodger Stadi...") ~ "Dodger Stadium",
                str_detect(ballpark, "Great Americ...") ~ "Great American Ball Park",
                str_detect(ballpark, "Kauffman Sta...") ~ "Kauffman Stadium",
                str_detect(ballpark, "Minute Maid...") ~  "Minute Maid Park",
                str_detect(ballpark, "Nationals Pa...") ~ "Nationals Park",
                str_detect(ballpark, "Old Yankee S...") ~ "Old Yankee Stadium",
                str_detect(ballpark, "Oriole Park ...") ~ "Camden Yards",
                str_detect(ballpark, "Rogers Centr...") ~ "Rogers Centre",
                str_detect(ballpark, "Tropicana Fi...") ~ "Tropicana Field",
                str_detect(ballpark, "U.S. Cellula...") ~ "U.S. Cellular Field",
                str_detect(ballpark, "Wrigley Fiel...") ~ "Wrigley Field",
                str_detect(ballpark, "Yankee Stadi...") ~ "Yankee Stadium",
                str_detect(ballpark, "Hiram Bithor...") ~ "Hiram Bithorn Stadium",
                str_detect(ballpark, "Sydney Crick...") ~ "Sydney Cricket Ground",
                str_detect(ballpark, "SunTrust Par...") ~ "SunTrust Park",
                
                TRUE ~ identity(ballpark) %>% as.character()))) %>% 
    
## Accounting for incorrect team name
    
    mutate(
        pitcher_team = with(., 
            case_when(
                str_detect(pitcher_team, "CHW") ~ "CWS",
                str_detect(pitcher_team, "FLA") ~ "MIA",
                
                TRUE ~ identity(pitcher_team) %>% as.character())),
        batter_team = with(., 
            case_when(
                str_detect(batter_team, "CHW") ~ "CWS",
                str_detect(batter_team, "FLA") ~ "MIA",
                
                TRUE ~ identity(batter_team) %>% as.character()))) %>% 
    
## Recoding horizonal angle (-45 left field line, 0 straightaway center, +45 right field line)
    
    mutate(horiz_angle_c = (horiz_angle - 90) * -1) %>% 
    mutate(horiz_angle = horiz_angle - 45) %>% 
    mutate(horiz_angle_rad = (horiz_angle * pi) / 180) %>% 

    mutate(
        horiz_angle_2 = with(., 
            case_when(
                horiz_angle_c < 0 ~ "Left",
                horiz_angle_c >= 0 ~ "Right") %>%
                factor(., levels = c("Left", "Right"), 
                    ordered = TRUE)), 
        
        horiz_angle_3 = with(., 
            case_when(
                horiz_angle_c < -15 ~ "Left",
                (horiz_angle_c >= -15) & (horiz_angle_c < 15) ~ "Center",
                horiz_angle_c >= 15 ~ "Right") %>% 
                factor(., levels = c("Left", "Center", "Right"), 
                    ordered = TRUE)), 
        
        horiz_angle_4 = with(., 
            case_when(
                horiz_angle_c < -22.5 ~ "Left",
                (horiz_angle_c >= -22.5) & (horiz_angle_c < 0) ~ "Left Center",
                (horiz_angle_c >= 0) & (horiz_angle_c < 22.5) ~ "Right Center",
                horiz_angle_c >= 22.5 ~ "Right") %>% 
                factor(., levels = c("Left", "Left Center", "Right Center", "Right"), 
                    ordered = TRUE)), 
        
        horiz_angle_5 = with(., 
            case_when(
                horiz_angle_c < -27 ~ "Left",
                (horiz_angle_c >= -27) & (horiz_angle_c < -9) ~ "Left Center",
                (horiz_angle_c >= -9) & (horiz_angle_c < 9) ~ "Center",
                (horiz_angle_c >= 9) & (horiz_angle_c < 27) ~ "Right Center",
                horiz_angle_c >= 27 ~ "Right") %>% 
                factor(., levels = c("Left", "Left Center", "Center", "Right Center", "Right"), 
                    ordered = TRUE)),
            
        horiz_angle_6 = with(., 
            case_when(
                horiz_angle_c < -30 ~ "Left",
                (horiz_angle_c >= -30) & (horiz_angle_c < -15) ~ "Left Center",
                (horiz_angle_c >= -15) & (horiz_angle_c < 0) ~ "Center Left",
                (horiz_angle_c >= 0) & (horiz_angle_c < 15) ~ "Center Right",
                (horiz_angle_c >= 15) & (horiz_angle_c < 30) ~ "Right Center",
                horiz_angle_c >= 30 ~ "Right") %>% 
                factor(., levels = c("Left", "Left Center", "Center Left", "Center Right", "Right Center", "Right"), 
                    ordered = TRUE))
        ) %>% 
    
## Converting character variables to factors

    mutate_if(is.character, as.factor) %>% 

    mutate(inning = as.factor(inning)) %>% 
    
    mutate(game_date = as_date(game_date)) %>% 
    
## Cleaning up
    
    # filter(launch_speed_ht != 0) %>% 
    filter(ballpark != "Fort Bragg" | is.na(ballpark)) %>% 
    filter(ballpark != "Hiram Bithorn Stadium" | is.na(ballpark)) %>%
    filter(ballpark != "Sydney Cricket Ground" | is.na(ballpark)) %>%
    
    filter(type_luck != "No homeruns found." | is.na(type_luck)) %>%
    
    droplevels()


contrasts(hittracker_update$ballpark) <- "contr.sum"
contrasts(hittracker_update$batter_name) <- "contr.sum"
contrasts(hittracker_update$batter_team) <- "contr.sum"
contrasts(hittracker_update$pitcher_name) <- "contr.sum"
contrasts(hittracker_update$pitcher_team) <- "contr.sum"
contrasts(hittracker_update$inning) <- "contr.sum"
contrasts(hittracker_update$type_luck) <- "contr.sum"


#=======================#
#### Combining Data ####
#=======================#

hittracker_update_new <-
    setdiff(
        hittracker_update,
        hittracker_all)

anti_join(
    hittracker_update,
    hittracker_all,
    by = c(
        "game_date",
        "batter_name",
        "pitcher_name",
        "inning",
        "hit_distance_ht",
        "launch_speed_ht",
        "launch_angle_ht",
        "horiz_angle",
        "apex"
    )
)

######################


hittracker_update_new %>%
    summarise(
        max_date = max(game_date),
        min_date = min(game_date)) %>% 
    with(.,
        paste("Found results from: ", max_date, ", back to: ", min_date, sep = "") %>% 
            noquote() %>% 
            print()
    )

cat("-----\n")

paste(nrow(hittracker_update_new), "rows added") %>% noquote() %>% print()

cat("-----\n")

paste(gdata::object.size(hittracker_update_new) %>% format(humanReadable = TRUE), "added") %>% noquote() %>% print()

cat("-----\n")

######################


hittracker_updated <-
    bind_rows(
        hittracker_all,
        hittracker_update_new)

hittracker_updated_distinct <- hittracker_updated %>% distinct()
hittracker_updated_distinct <- hittracker_updated_distinct %>% arrange(desc(game_date))


#=====================#
#### Saving Data ####
#=====================#

write_rds(hittracker_updated_distinct, "hittracker_updated_distinct.RDS")
write_csv(hittracker_updated_distinct, "hittracker_updated_distinct.csv")


#=====================#
#### Cleaning up ####
#=====================#

rm(list = ls(envir = globalenv()), envir = globalenv())
# lapply(
#     paste("package:", names(sessionInfo()$otherPkgs), sep = ""),
#     detach,
#     character.only = TRUE,
#     unload = TRUE,
#     force = TRUE
# )
gc()

#=====================#
#### End ####
#=====================#




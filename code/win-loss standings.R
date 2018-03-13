
#########################

library(pitchRx)
library(baseballr)
library(Lahman)
library(readr)
library(dplyr)
library(tibble)
library(lubridate)
library(rvest)
library(httr)
library(DBI)
library(stringr)
library(magrittr)

setwd("~/BASP/R analyses/Baseball Data/Data Files")

#######################

years <- 2016:1900

standings <- data.frame()

for (i in 1:length(years)) {
    
    cat("----------------------------------\n")
    now() %>% print()
    print(paste("Loop:", i))
    print(paste("Year:", years[i]))
    cat("----------------------------------\n")
    
    standings <-
        read_html(
            paste0(
                "https://widgets.sports-reference.com/wg.fcgi?css=1&site=br",
                "&url=%2Fleagues%2FMLB%2F",
                years[i],
                "-standings.shtml&div=div_expanded_standings_overall"
            )
        ) %>%
        html_nodes("body") %>%
        html_node("table") %>%
        html_table(header = TRUE) %>%
        extract2(1) %>%
        rename(
            rank = Rk,
            team = Tm,
            league = Lg,
            games = G,
            win = W,
            loss = L,
            win_percent = `W-L%`,
            home_record = Home,
            road_record = Road
        ) %>%
        mutate(
            home_wins = str_split(home_record,
                "-",
                simplify = TRUE) %>%
                extract(, 1) %>% as.integer(),
            home_losses = str_split(home_record,
                "-",
                simplify = TRUE) %>%
                extract(, 2) %>% as.integer(),
            road_wins = str_split(road_record,
                "-",
                simplify = TRUE) %>%
                extract(, 1) %>% as.integer(),
            road_losses = str_split(road_record,
                "-",
                simplify = TRUE) %>%
                extract(, 2) %>% as.integer()
        ) %>%
        mutate(
            win_percent_home = home_wins / (home_wins + home_losses),
            win_percent_road = road_wins / (road_wins + road_losses),
            HvR_percent =      win_percent_home - win_percent_road
            ) %>%         
        `colnames<-`(enc2native(names(.))) %>%
        add_column(year = years[i], .before = 1) %>% 
        filter(team != "Avg") %>% 
        filter(league != "FL") %>% 
        mutate(
            year_date = ymd(year, truncated = 2),
            SOSx10    = SOS * 10,
            SRSx10    = SRS * 10
            ) %>%
        bind_rows(standings, .) %>% 
        as_tibble()
    
}


write_rds(standings, "baseball_standings.RDS")



##########################################################################


data("Teams")

years_teams <- 
    Teams %>% 
    select(
        year = yearID,
        league = lgID,
        team = teamIDBR) %>% 
    filter(year >= 1900 & league %in% c("NL", "AL")) %>% 
    as_tibble()


##################################


team_results <- data.frame()


for (i in 1:nrow(years_teams)) {
    
    cat("----------------------------------\n")
    now() %>% print()
    print(paste("Loop:", i))
    print(paste("Year:", years_teams$year[i]))
    print(paste("Team:", years_teams$team[i]))
    cat("----------------------------------\n")
    
    tryCatch({
        
        team_results <-
            
            team_results_bref(
                years_teams$team[i],
                years_teams$year[i]) %>%
            
            as_tibble() %>% 
            
            bind_rows(team_results, .)
        
    },
        finally = next)
    
}

team_results_3 <-
    team_results %>% 
    mutate(
        win_loss = str_trunc(
            Result,
            width = 1,
            side = "right",
            ellipsis = ""),
        game_date_tmp = case_when(
            str_detect(Date, "\\(") ~ str_sub(Date, 0, -4),
            TRUE ~ identity(Date)
            ),
        game_date_tmp = str_trim(game_date_tmp),
        game_date_tmp_2 = str_c(game_date_tmp, " ", Year),
        game_date = parse_date_time(game_date_tmp_2, orders = "abdY")
        ) %>% 
    select(-game_date_tmp, -game_date_tmp_2) %>% 
    select(
        game = Gm,
        game_date,
        game_year = Year,
        team = Tm,
        home_v_road = H_A,
        opponent = Opp,
        win_loss,
        R,
        RA,
        end_inn = Inn,
        record = Record,
        rank = Rank,
        GB,
        win = Win,
        loss = Loss,
        save = Save,
        game_time = Time,
        day_night = `D/N`,
        attendance = Attendance,
        streak = Streak
        ) %>% 
    mutate(
        game = as.integer(game),
        attendance = as.integer(attendance),
        streak = as.integer(streak),
        rank = as.integer(rank),
        R = as.integer(R),
        RA = as.integer(RA),
        )

write_rds(team_results_3, "team_results.RDS")


##########################################################################

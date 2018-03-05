################################################################################

library(jsonlite)
library(lubridate)
library(dplyr)
library(tibble)
library(readr)
library(purrr)
library(DBI)
library(tidyr)
library(magrittr)
library(glue)
library(rvest)
library(stringr)
library(gepaf)
library(dbplyr)
library(V8)
library(httr)

setwd("~/BASP/R analyses/Baseball Data/Data Files")

################################################################################

base_url <- "https://www.fangraphs.com/leaders.aspx"

full_url_chr <-
    paste0(
        "{base_url}?",
        "pos={pos}&",
        "stats={stats}&",
        "lg={lg}&",
        "qual={qual}&",
        "type={type}&",
        "season={season}&",
        "month={month}&",
        "season1={season1}&",
        "ind={ind}&",
        "team={team}&",
        "rost={rost}&",
        "age={age}&",
        "filter={filter}&",
        "players={players}&",
        "page=1_10000&",
        "sort=2,a"
    )

################################################################################

batting_std <-
    data_frame(
        pos     = "all",
        stats   = "bat",
        lg      = "all",
        qual    = 0,
        type    = 0,
        season  = 2017:2006,
        month   = 0,
        season1 = 2017:2006,
        ind     = 0,
        team    = 0,
        rost    = 0,
        age     = 0,
        filter  = "",
        players = 0
    )

batting_std_table <- data.frame()

for (yr in 1:length(2017:2006)) {
    
    # print(batting_std$season[yr])
    
    batting_std_table <-
        read_html(glue_data(batting_std %>% slice(yr), full_url_chr)) %>%
        html_nodes("table.rgMasterTable") %>%
        html_table() %>%
        extract2(1) %>%
        slice(c(-1, -3)) %>%
        set_colnames(
            slice(., 1) %>%
                str_replace_all(., "#", "num") %>%
                str_replace_all(., "%", "_prct") %>%
                str_replace_all(., regex("\\+"), "_plus") %>%
                str_replace_all(., "-", "_minus") %>%
                str_replace_all(., "/", "_per_")
            
        ) %>%
        set_colnames(., make.names(names = names(.), unique = TRUE)) %>%
        slice(-1) %>%
        add_column(season = batting_std$season[yr], .before = 1) %>% 
        # mutate(G = as.integer(G)) %>%
        # arrange(desc(G)) %>% 
        arrange(Name) %>% 
        bind_rows(batting_std_table, .) %>%
        as_tibble()
    
}

write_csv(batting_std_table, "batting_std_table_2006-2017.csv")
write_rds(batting_std_table, "batting_std_table_2006-2017.rds")

################################################################################

batting_adv <-
    data_frame(
        pos     = "all",
        stats   = "bat",
        lg      = "all",
        qual    = 0,
        type    = 1,
        season  = 2017:2006,
        month   = 0,
        season1 = 2017:2006,
        ind     = 0,
        team    = 0,
        rost    = 0,
        age     = 0,
        filter  = "",
        players = 0
    )


batting_adv_table <- data.frame()

for (yr in 1:length(2017:2006)) {
    
    print(batting_adv$season[yr])

    batting_adv_table <-
        read_html(glue_data(batting_adv %>% slice(yr), full_url_chr)) %>%
        html_nodes("table.rgMasterTable") %>%
        html_table() %>%
        extract2(1) %>%
        slice(c(-1, -3)) %>%
        set_colnames(
            slice(., 1) %>%
                str_replace_all(., "#", "num") %>%
                str_replace_all(., "%", "_prct") %>%
                str_replace_all(., regex("\\+"), "_plus") %>%
                str_replace_all(., "-", "_minus_") %>%
                str_replace_all(., "/", "_per_")
            
        ) %>%
        set_colnames(., make.names(names = names(.), unique = TRUE)) %>%
        slice(-1) %>%
        add_column(season = batting_adv$season[yr], .before = 1) %>% 
        # mutate(PA = as.integer(PA)) %>%
        # arrange(desc(PA)) %>% 
        arrange(Name) %>% 
        bind_rows(batting_adv_table, .) %>%
        as_tibble()
    
}

write_csv(batting_adv_table, "batting_adv_table_2006-2017.csv")
write_rds(batting_adv_table, "batting_adv_table_2006-2017.rds")

################################################################################

pitching_std <-
    data_frame(
        pos     = "all",
        stats   = "pit",
        lg      = "all",
        qual    = 0,
        type    = 0,
        season  = 2017:2006,
        month   = 0,
        season1 = 2017:2006,
        ind     = 0,
        team    = 0,
        rost    = 0,
        age     = 0,
        filter  = "",
        players = 0
    )


pitching_std_table <- data.frame()

for (yr in 1:length(2017:2006)) {
    
    print(pitching_std$season[yr])
    
    pitching_std_table <- 
        read_html(glue_data(pitching_std %>% slice(yr), full_url_chr)) %>%
        html_nodes("table.rgMasterTable") %>%
        html_table() %>%
        extract2(1) %>%
        slice(c(-1, -3)) %>%
        set_colnames(
            slice(., 1) %>%
                str_replace_all(., "#", "num") %>%
                str_replace_all(., "%", "_prct") %>%
                str_replace_all(., regex("\\+"), "_plus") %>%
                str_replace_all(., "-", "_minus_") %>%
                str_replace_all(., "/", "_per_")
            
        ) %>%
        set_colnames(., make.names(names = names(.), unique = TRUE)) %>%
        slice(-1) %>%
        add_column(season = pitching_std$season[yr], .before = 1) %>%
        # mutate(IP = as.integer(IP)) %>%
        # arrange(desc(IP)) %>%         
        arrange(Name) %>% 
        bind_rows(pitching_std_table, .) %>%
        as_tibble()
    
}

write_csv(pitching_std_table, "pitching_std_table_2006-2017.csv")
write_rds(pitching_std_table, "pitching_std_table_2006-2017.rds")

################################################################################

pitching_adv <-
    data_frame(
        pos     = "all",
        stats   = "pit",
        lg      = "all",
        qual    = 0,
        type    = 1,
        season  = 2017:2006,
        month   = 0,
        season1 = 2017:2006,
        ind     = 0,
        team    = 0,
        rost    = 0,
        age     = 0,
        filter  = "",
        players = 0
    )

pitching_adv_table <- data.frame()

for (yr in 1:length(2017:2006)) {
    
    print(pitching_adv$season[yr])
    
    pitching_adv_table <- 
        read_html(glue_data(pitching_adv %>% slice(yr), full_url_chr)) %>%
        html_nodes("table.rgMasterTable") %>%
        html_table() %>%
        extract2(1) %>%
        slice(c(-1, -3)) %>%
        set_colnames(
            slice(., 1) %>%
                str_replace_all(., "#", "num") %>%
                str_replace_all(., "%", "_prct") %>%
                str_replace_all(., regex("\\+"), "_plus") %>%
                str_replace_all(., "-", "_minus_") %>%
                str_replace_all(., "/", "_per_")
            
        ) %>%
        set_colnames(., make.names(names = names(.), unique = TRUE)) %>%
        slice(-1) %>%
        add_column(season = pitching_adv$season[yr], .before = 1) %>% 
        arrange(Name) %>% 
        bind_rows(pitching_adv_table, .) %>%
        as_tibble()
    
}

write_csv(pitching_adv_table, "pitching_adv_table_2006-2017.csv")
write_rds(pitching_adv_table, "pitching_adv_table_2006-2017.rds")

################################################################################

# from 2016 season  [season1=2016]
# to 2017 season    [season=2017]

# AL    [lg=al]
# NL    [lg=nl]
# Both  [lg=all]

# 100 page results [page=1_100]

# Leaderboaards         [leaders.aspx]
# Splits Leaderboards   [leaderssplits.aspx]

## Player Stats [qual=y]
## Team Stats   [qual=0 | team=0,ts]
## League Stats [qual=0 | team=0,ss]

### Batting     [stats=bat]
### Pitching    [stats=pit]
### Fielding    [stats=fld]

#### All    [pos=all]
#### P      [pos=p]
#### C      [pos=c]
#### 1B     [pos=1b]
#### 2B     [pos=2b]
#### SS     [pos=ss]
#### 3B     [pos=3b]
#### RF     [pos=rf]
#### CF     [pos=cf]
#### LF     [pos=lf]
#### OF     [pos=of]
#### DH     [pos=dh]
#### NP     [pos=np]

##### Dashboard             [type=8]
##### Standard              [type=0]
##### Advanced              [type=1]
##### Batted Ball           [type=2]
##### Win Probability       [type=3]
##### Pitch Type            [type=4]
##### Pitch Value           [type=7]
##### Plate Discipline      [type=5]
##### Value                 [type=6]
##### Pitch Info:           :::::::::
#####    Pitch Type         [type=16]
#####    Velocity           [type=17]
#####    H-Movement         [type=18]
#####    V-Movement         [type=19]
#####    Pitch Type Value   [type=20]
#####    Pitch Value/100    [type=21]
#####    Plate Discipline   [type=22]


"https://www.fangraphs.com/depthcharts.aspx?position=Standings"
"https://www.fangraphs.com/depthcharts.aspx?position=BaseRuns"
"https://www.fangraphs.com/depthcharts.aspx?position=Team"

"https://www.fangraphs.com/depthcharts.aspx?position=ALL&teamid=25"

"https://www.fangraphs.com/boxscore.aspx?date=2017-11-01&team=Dodgers&dh=0"

"https://www.fangraphs.com/plays.aspx?date=2017-11-01&team=Dodgers&dh=0&season=2017"
"https://www.fangraphs.com/wins.aspx?date=2017-11-01&team=Dodgers&dh=0&season=2017"

"https://www.fangraphs.com/projections.aspx?pos=all&stats=bat&type=zips&team=0&lg=all&players=0"
"https://www.fangraphs.com/projections.aspx?pos=all&stats=bat&type=steamer"




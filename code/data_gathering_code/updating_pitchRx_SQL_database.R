###########################################-
###########################################-
##
## updating pitchRx data ----
##
###########################################-
###########################################-

#=========================#
# Setting up ----
#=========================#

#-------------------------#
# Loading libraries ----
#-------------------------#

library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(dbplyr)
library(stringr)
library(lubridate)
library(stringdist)
library(stringr)
library(tibble)
library(magrittr)
library(DBI)
library(pryr)
library(pitchRx)

#-------------------------#
# Loading libraries ----
#-------------------------#

source("./code/functions/get_game.xml.R")
source("./code/functions/gid2date.R")

#--------------------------------#
# Connecting to database ----
#--------------------------------#

pitchRx_db      <- dbConnect(RSQLite::SQLite(), "./data/pitchRx_db.sqlite3")
pitchRx_db_temp <- dbConnect(RSQLite::SQLite(), "./data/pitchRx_db_temp.sqlite3")

#-----------------------------------#
# Dropping old temp tables ----
#-----------------------------------#

if (db_has_table(pitchRx_db_temp, "action"))
    db_drop_table(pitchRx_db_temp, "action")

if (db_has_table(pitchRx_db_temp, "atbat"))
    db_drop_table(pitchRx_db_temp, "atbat")

if (db_has_table(pitchRx_db_temp, "coach"))
    db_drop_table(pitchRx_db_temp, "coach")

if (db_has_table(pitchRx_db_temp, "game"))
    db_drop_table(pitchRx_db_temp, "game")

if (db_has_table(pitchRx_db_temp, "hip"))
    db_drop_table(pitchRx_db_temp, "hip")

if (db_has_table(pitchRx_db_temp, "media"))
    db_drop_table(pitchRx_db_temp, "media")

if (db_has_table(pitchRx_db_temp, "pitch"))
    db_drop_table(pitchRx_db_temp, "pitch")

if (db_has_table(pitchRx_db_temp, "player"))
    db_drop_table(pitchRx_db_temp, "player")

if (db_has_table(pitchRx_db_temp, "po"))
    db_drop_table(pitchRx_db_temp, "po")

if (db_has_table(pitchRx_db_temp, "runner"))
    db_drop_table(pitchRx_db_temp, "runner")

if (db_has_table(pitchRx_db_temp, "umpire"))
    db_drop_table(pitchRx_db_temp, "umpire")

#--------------------------------#
# Getting gids ----
#--------------------------------#

most_recent_day <-
    tbl(pitchRx_db, "game") %>%
    select(game_date) %>% 
    collect() %>% 
    pull(game_date) %>% 
    max(na.rm = TRUE) %>% 
    as_date() - 1


# most_recent_day <-
#     tbl(pitchRx_db, "atbat") %>%
#     arrange(desc(game_date)) %>%
#     head(1) %>% 
#     select(game_date) %>%
#     collect() %>% 
#     extract2(1)


## Substituting 2018 opening day ##

url <- makeUrls(start = most_recent_day, end = as_date("2017-10-11"))


# url <- makeUrls(most_recent_day, today())
gameday_link <- str_trunc(url, 30, "left", "")
game_date <- gid2date(gameday_link)
all_gids <- data_frame(game_date, gameday_link, url)




most_recent_data <-
    tbl(pitchRx_db, "game") %>%
    filter(game_date == most_recent_day) %>%
    select(gameday_link) %>%
    collect()


# most_recent_data <-
#     tbl(pitchRx_db, "game") %>%
#     filter(game_date >= most_recent_day) %>%
#     select(gameday_link) %>%
#     collect()


gids_distinct <-
    anti_join(
        all_gids,
        most_recent_data,
        by = "gameday_link")


# gids_distinct <- most_recent_data


if (nrow(gids_distinct) == 0) {
    message("No games to update; stopping here by calling `.rs.restartR()`.")
    .rs.restartR()
}


#=============================#
# Downloading data ----
#=============================#

#------------------------------------------#
# Initializing temporary database ----
#------------------------------------------#

pitchRx_db_temp <- dbConnect(RSQLite::SQLite(), "pitchRx_db_temp.sqlite3")

#----------------------------------------------------------#
# Downloading gameday data with `pitchRx::scrape` ----
#----------------------------------------------------------#

scrape(
    game.ids = gids_distinct$gameday_link,
    connect = pitchRx_db_temp,
    async = TRUE,
    suffix = c(
        "inning/inning_all.xml",
        "inning/inning_hit.xml",
        "players.xml",
        "miniscoreboard.xml"
    )
)


#-----------------------------------------------#
# Creating new columns in scraped data ----
#-----------------------------------------------#

# ---- atbat ----

## adding new columns ##

dbExecute(pitchRx_db_temp, "ALTER TABLE atbat ADD at_bat_number")
dbExecute(pitchRx_db_temp, "ALTER TABLE atbat ADD game_date")
dbExecute(pitchRx_db_temp, "ALTER TABLE atbat ADD at_bat_event")
dbExecute(pitchRx_db_temp, "ALTER TABLE atbat ADD game_year")

dbExecute(pitchRx_db_temp, "UPDATE atbat SET at_bat_number = num")
dbExecute(pitchRx_db_temp, "UPDATE atbat SET at_bat_event = event")
dbExecute(pitchRx_db_temp, "UPDATE atbat SET game_date = replace(substr(gameday_link, 5, 10), '_', '-')")
dbExecute(pitchRx_db_temp, "UPDATE atbat SET game_year = substr(game_date, 1, 4)")


# ---- pitch ----

## adding new columns ##

dbExecute(pitchRx_db_temp, "ALTER TABLE pitch ADD pitch_number")
dbExecute(pitchRx_db_temp, "ALTER TABLE pitch ADD at_bat_number")
dbExecute(pitchRx_db_temp, "ALTER TABLE pitch ADD pitch_event")
dbExecute(pitchRx_db_temp, "ALTER TABLE pitch ADD game_date")
dbExecute(pitchRx_db_temp, "ALTER TABLE pitch ADD game_year")

dbExecute(pitchRx_db_temp, "UPDATE pitch SET pitch_number = id")
dbExecute(pitchRx_db_temp, "UPDATE pitch SET at_bat_number = num")
dbExecute(pitchRx_db_temp, "UPDATE pitch SET pitch_event = des")
dbExecute(pitchRx_db_temp, "UPDATE pitch SET game_date = replace(substr(gameday_link, 5, 10), '_', '-')")
dbExecute(pitchRx_db_temp, "UPDATE pitch SET game_year = substr(game_date, 1, 4)")


# ---- player ----

## adding new columns ##

dbExecute(pitchRx_db_temp, "ALTER TABLE player ADD at_bat_number")
dbExecute(pitchRx_db_temp, "ALTER TABLE player ADD game_date")
dbExecute(pitchRx_db_temp, "ALTER TABLE player ADD game_year")

dbExecute(pitchRx_db_temp, "UPDATE player SET at_bat_number = num")
dbExecute(pitchRx_db_temp, "UPDATE player SET game_date = replace(substr(gameday_link, 5, 10), '_', '-')")
dbExecute(pitchRx_db_temp, "UPDATE player SET game_year = substr(game_date, 1, 4)")


# ---- action ----

## adding new columns ##

dbExecute(pitchRx_db_temp, "ALTER TABLE action ADD at_bat_number")
dbExecute(pitchRx_db_temp, "ALTER TABLE action ADD pitch_number")
dbExecute(pitchRx_db_temp, "ALTER TABLE action ADD action_event")
dbExecute(pitchRx_db_temp, "ALTER TABLE action ADD game_date")
dbExecute(pitchRx_db_temp, "ALTER TABLE action ADD game_year")

dbExecute(pitchRx_db_temp, "UPDATE action SET at_bat_number = num")
dbExecute(pitchRx_db_temp, "UPDATE action SET pitch_number = pitch")
dbExecute(pitchRx_db_temp, "UPDATE action SET action_event = event")
dbExecute(pitchRx_db_temp, "UPDATE action SET game_date = replace(substr(gameday_link, 5, 10), '_', '-')")
dbExecute(pitchRx_db_temp, "UPDATE action SET game_year = substr(game_date, 1, 4)")


# ---- game ----

## adding new columns ##

dbExecute(pitchRx_db_temp, "ALTER TABLE game ADD game_date")
dbExecute(pitchRx_db_temp, "ALTER TABLE game ADD game_year")

dbExecute(pitchRx_db_temp, "UPDATE game SET gameday_link = substr('gid_' || gameday_link, 1, 40)")
dbExecute(pitchRx_db_temp, "UPDATE game SET game_date = replace(substr(gameday_link, 5, 10), '_', '-')")
dbExecute(pitchRx_db_temp, "UPDATE game SET game_year = substr(game_date, 1, 4)")


# ---- hip ----

## adding new columns ##

dbExecute(pitchRx_db_temp, "ALTER TABLE hip ADD hip_event")
dbExecute(pitchRx_db_temp, "ALTER TABLE hip ADD game_date")
dbExecute(pitchRx_db_temp, "ALTER TABLE hip ADD game_year")

dbExecute(pitchRx_db_temp, "UPDATE hip SET hip_event = des")
dbExecute(pitchRx_db_temp, "UPDATE hip SET game_date = replace(substr(gameday_link, 5, 10), '_', '-')")
dbExecute(pitchRx_db_temp, "UPDATE hip SET game_year = substr(game_date, 1, 4)")


# ---- runner ----

## adding new columns ##

dbExecute(pitchRx_db_temp, "ALTER TABLE runner ADD at_bat_number")
dbExecute(pitchRx_db_temp, "ALTER TABLE runner ADD runner_event")
dbExecute(pitchRx_db_temp, "ALTER TABLE runner ADD game_date")
dbExecute(pitchRx_db_temp, "ALTER TABLE runner ADD game_year")

dbExecute(pitchRx_db_temp, "UPDATE runner SET at_bat_number = num")
dbExecute(pitchRx_db_temp, "UPDATE runner SET runner_event = event")
dbExecute(pitchRx_db_temp, "UPDATE runner SET game_date = replace(substr(gameday_link, 5, 10), '_', '-')")
dbExecute(pitchRx_db_temp, "UPDATE runner SET game_year = substr(game_date, 1, 4)")


# ---- po ----

## adding new columns ##

dbExecute(pitchRx_db_temp, "ALTER TABLE po ADD at_bat_number")
dbExecute(pitchRx_db_temp, "ALTER TABLE po ADD po_event")
dbExecute(pitchRx_db_temp, "ALTER TABLE po ADD game_date")
dbExecute(pitchRx_db_temp, "ALTER TABLE po ADD game_year")

dbExecute(pitchRx_db_temp, "UPDATE po SET at_bat_number = num")
dbExecute(pitchRx_db_temp, "UPDATE po SET po_event = des")
dbExecute(pitchRx_db_temp, "UPDATE po SET game_date = replace(substr(gameday_link, 5, 10), '_', '-')")
dbExecute(pitchRx_db_temp, "UPDATE po SET game_year = substr(game_date, 1, 4)")


# ---- coach ----

## adding new columns ##

dbExecute(pitchRx_db_temp, "ALTER TABLE coach ADD game_date")
dbExecute(pitchRx_db_temp, "ALTER TABLE coach ADD game_year")

dbExecute(pitchRx_db_temp, "UPDATE coach SET game_date = replace(substr(gameday_link, 5, 10), '_', '-')")
dbExecute(pitchRx_db_temp, "UPDATE coach SET game_year = substr(game_date, 1, 4)")


# ---- umpire ----

## adding new columns ##

dbExecute(pitchRx_db_temp, "ALTER TABLE umpire ADD game_date")
dbExecute(pitchRx_db_temp, "ALTER TABLE umpire ADD game_year")

dbExecute(pitchRx_db_temp, "UPDATE umpire SET game_date = replace(substr(gameday_link, 5, 10), '_', '-')")
dbExecute(pitchRx_db_temp, "UPDATE umpire SET game_year = substr(game_date, 1, 4)")


#======================================#
# Copying data to main database ----
#======================================#

#----------------#
# atbat ----
#----------------#

atbat <- tbl(pitchRx_db_temp, "atbat") %>% collect()

atbat_2 <-
    atbat %>%
    mutate(
        obs_date_time_east =
            make_datetime(
                year = substr(start_tfs_zulu, 1, 4) %>% as.integer(),
                month = substr(start_tfs_zulu, 6, 7) %>% as.integer(),
                day = substr(start_tfs_zulu, 9, 10) %>% as.integer(),
                hour = substr(start_tfs_zulu, 12, 13) %>% as.integer(),
                min = substr(start_tfs_zulu, 15, 16) %>% as.integer(),
                sec = substr(start_tfs_zulu, 18, 19) %>% as.integer(),
                tz = "UTC"
            )
    ) %>%
    mutate(obs_date_time_east = force_tz(obs_date_time_east, tzone = "US/Eastern")) %>%
    mutate(obs_date_time_utc = with_tz(obs_date_time_east, tzone = "UTC")) %>%
    mutate(
        obs_date_time_east_chr = as.character(obs_date_time_east),
        obs_date_time_utc_chr = as.character(obs_date_time_utc)
    ) %>%
    mutate(
        east_date_round = round_date(obs_date_time_east, unit = "hour"),
        utc_date_round = round_date(obs_date_time_utc, unit = "hour")
    ) %>%
    mutate(
        east_date_round_chr = as.character(east_date_round),
        utc_date_round_chr = as.character(utc_date_round)
    )

cat("-------------------------------\n")
cat(nrow(atbat_2), "rows added to `atbat`\n")
cat("Size: ")
object_size(atbat_2) %>% print()


dbWriteTable(
    pitchRx_db,
    "atbat",
    value = atbat_2,
    append = TRUE,
    temporary = FALSE
)

rm(atbat)
rm(atbat_2)
gc()


#----------------#
# pitch ----
#----------------#

pitch <- tbl(pitchRx_db_temp, "pitch") %>% collect()

pitch_2 <-
    pitch %>%
    mutate(
        obs_date_time_east =
            make_datetime(
                year = substr(tfs_zulu, 1, 4) %>% as.integer(),
                month = substr(tfs_zulu, 6, 7) %>% as.integer(),
                day = substr(tfs_zulu, 9, 10) %>% as.integer(),
                hour = substr(tfs_zulu, 12, 13) %>% as.integer(),
                min = substr(tfs_zulu, 15, 16) %>% as.integer(),
                sec = substr(tfs_zulu, 18, 19) %>% as.integer(),
                tz = "UTC"
            )
    ) %>%
    mutate(obs_date_time_east = force_tz(obs_date_time_east, tzone = "US/Eastern")) %>%
    mutate(obs_date_time_utc = with_tz(obs_date_time_east, tzone = "UTC")) %>%
    mutate(
        obs_date_time_east_chr = as.character(obs_date_time_east),
        obs_date_time_utc_chr = as.character(obs_date_time_utc)
    ) %>%
    mutate(
        east_date_round = round_date(obs_date_time_east, unit = "hour"),
        utc_date_round = round_date(obs_date_time_utc, unit = "hour")
    ) %>%
    mutate(
        east_date_round_chr = as.character(east_date_round),
        utc_date_round_chr = as.character(utc_date_round)
    )

cat("-------------------------------\n")
cat(nrow(pitch_2), "rows added to `pitch`\n")
cat("Size: ")
object_size(pitch_2) %>% print()

dbWriteTable(
    pitchRx_db,
    "pitch",
    value = pitch_2,
    append = TRUE,
    temporary = FALSE
)

rm(pitch)
rm(pitch_2)
gc()


#----------------#
# action ----
#----------------#

action <- tbl(pitchRx_db_temp, "action") %>% collect()

action_2 <- 
    action %>%
    mutate(
        obs_date_time_east =
            make_datetime(
                year = substr(tfs_zulu, 1, 4) %>% as.integer(),
                month = substr(tfs_zulu, 6, 7) %>% as.integer(),
                day = substr(tfs_zulu, 9, 10) %>% as.integer(),
                hour = substr(tfs_zulu, 12, 13) %>% as.integer(),
                min = substr(tfs_zulu, 15, 16) %>% as.integer(),
                sec = substr(tfs_zulu, 18, 19) %>% as.integer(),
                tz = "UTC"
            )
    ) %>%
    mutate(obs_date_time_east = force_tz(obs_date_time_east, tzone = "US/Eastern")) %>%
    mutate(obs_date_time_utc = with_tz(obs_date_time_east, tzone = "UTC")) %>%
    mutate(
        obs_date_time_east_chr = as.character(obs_date_time_east),
        obs_date_time_utc_chr = as.character(obs_date_time_utc)
    ) %>%
    mutate(
        east_date_round = round_date(obs_date_time_east, unit = "hour"),
        utc_date_round = round_date(obs_date_time_utc, unit = "hour")
    ) %>%
    mutate(
        east_date_round_chr = as.character(east_date_round),
        utc_date_round_chr = as.character(utc_date_round)
    ) #%>% 
    # mutate(at_bat_number = as.double(at_bat_number))
    
    
cat("-------------------------------\n")
cat(nrow(action_2), "rows added to `action`\n")
cat("Size: ")
object_size(action_2) %>% print()

dbWriteTable(
    pitchRx_db,
    "action",
    value = action_2,
    append = TRUE,
    temporary = FALSE
)

rm(action)
rm(action_2)
gc()


#----------------#
# game ----
#----------------#

game <- tbl(pitchRx_db_temp, "game") %>% collect()

cat("-------------------------------\n")
cat(nrow(game), "rows added to `game`\n")
cat("Size: ")
object_size(game) %>% print()

dbWriteTable(
    pitchRx_db,
    "game",
    value = game,
    append = TRUE,
    temporary = FALSE
)

rm(game)
gc()

#----------------#
# player ----
#----------------#

player <- tbl(pitchRx_db_temp, "player") %>% collect()

cat("-------------------------------\n")
cat(nrow(player), "rows added to `player`\n")
cat("Size: ")
object_size(player) %>% print()

dbWriteTable(
    pitchRx_db,
    "player",
    value = player,
    append = TRUE,
    temporary = FALSE
)

rm(player)
gc()


#----------------#
# hip ----
#----------------#

hip <- tbl(pitchRx_db_temp, "hip") %>% collect()

cat("-------------------------------\n")
cat(nrow(hip), "rows added to `hip`\n")
cat("Size: ")
object_size(hip) %>% print()

dbWriteTable(
    pitchRx_db,
    "hip",
    value = hip,
    append = TRUE,
    temporary = FALSE
)

rm(hip)
gc()


#----------------#
# runner ----
#----------------#

runner <- tbl(pitchRx_db_temp, "runner") %>% collect()

cat("-------------------------------\n")
cat(nrow(runner), "rows added to `runner`\n")
cat("Size: ")
object_size(runner) %>% print()

dbWriteTable(
    pitchRx_db,
    "runner",
    value = runner,
    append = TRUE,
    temporary = FALSE
)

rm(runner)
gc()


#----------------#
# po ----
#----------------#

po <- tbl(pitchRx_db_temp, "po") %>% collect()

cat("-------------------------------\n")
cat(nrow(po), "rows added to `po`\n")
cat("Size: ")
object_size(po) %>% print()

dbWriteTable(
    pitchRx_db,
    "po",
    value = po,
    append = TRUE,
    temporary = FALSE
)

rm(po)
gc()


#----------------#
# coach ----
#----------------#

coach <- tbl(pitchRx_db_temp, "coach") %>% collect()

cat("-------------------------------\n")
cat(nrow(coach), "rows added to `coach`\n")
cat("Size: ")
object_size(coach) %>% print()

dbWriteTable(
    pitchRx_db,
    "coach",
    value = coach,
    append = TRUE,
    temporary = FALSE
)

rm(coach)
gc()


#----------------#
# umpire ----
#----------------#

umpire <- tbl(pitchRx_db_temp, "umpire") %>% collect()

cat("-------------------------------\n")
cat(nrow(umpire), "rows added to `umpire`\n")
cat("Size: ")
object_size(umpire) %>% print()

dbWriteTable(
    pitchRx_db,
    "umpire",
    value = umpire,
    append = TRUE,
    temporary = FALSE
)

rm(umpire)
gc()

########################

cat("-------------------------------\n")

########################

dbDisconnect(pitchRx_db_temp)
dbDisconnect(pitchRx_db)

########################

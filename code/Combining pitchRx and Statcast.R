###########################################
###########################################
##
## Combining pitchRx and Statcast
##
###########################################
###########################################

#=========================#
#### Loading packages ####
#=========================#

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


#==========================#
#### Loading functions ####
#==========================#

source("~/BASP/R analyses/gid2date.R")

#=====================#
#### Loading Data ####
#=====================#

setwd("~/BASP/R analyses/Baseball Data/Data Files")


#=============================#
#### Initilizing Database ####
#=============================#

statcast_db <- dbConnect(RSQLite::SQLite(), "statcast_db.sqlite3")
pitchRx_db <- dbConnect(RSQLite::SQLite(), "pitchRx_db.sqlite3")

dbListTables(statcast_db)

tbl(statcast_db, "sc_all_comb_clean_distinct") %>% glimpse()


dbListTables(pitchRx_db)

tbl(pitchRx_db, "atbat") %>% glimpse()
tbl(pitchRx_db, "pitch") %>% glimpse()
tbl(pitchRx_db, "action") %>% glimpse()
tbl(pitchRx_db, "runner") %>% glimpse()
tbl(pitchRx_db, "hip") %>% glimpse()
tbl(pitchRx_db, "po") %>% glimpse()
tbl(pitchRx_db, "player") %>% glimpse()
tbl(pitchRx_db, "coach") %>% glimpse()
tbl(pitchRx_db, "umpire") %>% glimpse()


#=============================#
#### Adding game_date etc. ####
#=============================#

#----------------#
# ---- atbat ----
#----------------#

tbl(pitchRx_db, "atbat") %>% glimpse()

dbExecute(pitchRx_db, "ALTER TABLE atbat ADD game_date")
dbExecute(pitchRx_db, "UPDATE atbat SET game_date = replace(substr(gameday_link, 5, 10), '_', '-')")

tbl(pitchRx_db, "atbat") %>% glimpse()


doubleheaders_1 <-
    tbl(pitchRx_db, "atbat") %>% 
    mutate(
        game_year = as.numeric(substr(game_date, 1, 4)),
        doubleheader = case_when(substr(gameday_link, 30, 30) == "2" ~ "TRUE", TRUE ~ "FALSE"))

compute(doubleheaders_1, name = "atbat_temp", temporary = FALSE, overwrite = TRUE)

# db_drop_table(pitchRx_db, "atbat")

dbExecute(pitchRx_db, 'ALTER TABLE atbat_temp RENAME TO atbat')

tbl(pitchRx_db, "atbat") %>% glimpse()

db_create_index(pitchRx_db, "atbat", "at_bat_number")


#----------------#
# ---- pitch ----
#----------------#

tbl(pitchRx_db, "pitch") %>% glimpse()

dbExecute(pitchRx_db, "ALTER TABLE pitch ADD game_date")
dbExecute(pitchRx_db, "UPDATE pitch SET game_date = replace(substr(gameday_link, 5, 10), '_', '-')")

tbl(pitchRx_db, "pitch") %>% glimpse()


doubleheaders_1 <-
    tbl(pitchRx_db, "pitch") %>% 
    mutate(
        game_year = as.numeric(substr(game_date, 1, 4)),
        doubleheader = case_when(substr(gameday_link, 30, 30) == "2" ~ "TRUE", TRUE ~ "FALSE"))

compute(doubleheaders_1, name = "pitch_temp", temporary = FALSE, overwrite = TRUE)
tbl(pitchRx_db, "pitch_temp") %>% glimpse()

# db_drop_table(pitchRx_db, "pitch")

dbExecute(pitchRx_db, 'ALTER TABLE pitch_temp RENAME TO pitch')

tbl(pitchRx_db, "pitch") %>% glimpse()

db_create_index(pitchRx_db, "pitch", "pitch_number")
db_create_index(pitchRx_db, "pitch", "at_bat_number")


#----------------#
# ---- action ----
#----------------#

tbl(pitchRx_db, "action") %>% glimpse()

dbExecute(pitchRx_db, "ALTER TABLE action ADD game_date")
dbExecute(pitchRx_db, "UPDATE action SET game_date = replace(substr(gameday_link, 5, 10), '_', '-')")

tbl(pitchRx_db, "action") %>% glimpse()


doubleheaders_1 <-
    tbl(pitchRx_db, "action") %>% 
    mutate(
        game_year = as.numeric(substr(game_date, 1, 4)),
        doubleheader = case_when(substr(gameday_link, 30, 30) == "2" ~ "TRUE", TRUE ~ "FALSE"))

compute(doubleheaders_1, name = "action_temp", temporary = FALSE, overwrite = TRUE)
tbl(pitchRx_db, "action_temp") %>% glimpse()

# db_drop_table(pitchRx_db, "action")

dbExecute(pitchRx_db, 'ALTER TABLE action_temp RENAME TO action')

tbl(pitchRx_db, "action") %>% glimpse()

db_create_index(pitchRx_db, "pitch", "pitch_number")
db_create_index(pitchRx_db, "pitch", "at_bat_number")

#----------------#
# ---- runner ----
#----------------#

tbl(pitchRx_db, "runner") %>% glimpse()

dbExecute(pitchRx_db, "ALTER TABLE runner ADD game_date")
dbExecute(pitchRx_db, "UPDATE runner SET game_date = replace(substr(gameday_link, 5, 10), '_', '-')")

tbl(pitchRx_db, "runner") %>% glimpse()


doubleheaders_1 <-
    tbl(pitchRx_db, "runner") %>% 
    mutate(
        game_year = as.numeric(substr(game_date, 1, 4)),
        doubleheader = case_when(substr(gameday_link, 30, 30) == "2" ~ "TRUE", TRUE ~ "FALSE"))

compute(doubleheaders_1, name = "runner_temp", temporary = FALSE, overwrite = TRUE)
tbl(pitchRx_db, "runner_temp") %>% glimpse()

# db_drop_table(pitchRx_db, "runner")

dbExecute(pitchRx_db, 'ALTER TABLE runner_temp RENAME TO runner')

tbl(pitchRx_db, "runner") %>% glimpse()



#----------------#
# ---- hip ----
#----------------#

tbl(pitchRx_db, "hip") %>% glimpse()

dbExecute(pitchRx_db, "ALTER TABLE hip ADD game_date")
dbExecute(pitchRx_db, "UPDATE hip SET game_date = replace(substr(gameday_link, 5, 10), '_', '-')")

tbl(pitchRx_db, "hip") %>% glimpse()


doubleheaders_1 <-
    tbl(pitchRx_db, "hip") %>% 
    mutate(
        game_year = as.numeric(substr(game_date, 1, 4)),
        doubleheader = case_when(substr(gameday_link, 30, 30) == "2" ~ "TRUE", TRUE ~ "FALSE"))

compute(doubleheaders_1, name = "hip_temp", temporary = FALSE, overwrite = TRUE)
tbl(pitchRx_db, "hip_temp") %>% glimpse()

# db_drop_table(pitchRx_db, "hip")

dbExecute(pitchRx_db, 'ALTER TABLE hip_temp RENAME TO hip')

tbl(pitchRx_db, "hip") %>% glimpse()



#----------------#
# ---- po ----
#----------------#

tbl(pitchRx_db, "po") %>% glimpse()

dbExecute(pitchRx_db, "ALTER TABLE po ADD game_date")
dbExecute(pitchRx_db, "UPDATE po SET game_date = replace(substr(gameday_link, 5, 10), '_', '-')")

tbl(pitchRx_db, "po") %>% glimpse()


doubleheaders_1 <-
    tbl(pitchRx_db, "po") %>% 
    mutate(
        game_year = as.numeric(substr(game_date, 1, 4)),
        doubleheader = case_when(substr(gameday_link, 30, 30) == "2" ~ "TRUE", TRUE ~ "FALSE"))

compute(doubleheaders_1, name = "po_temp", temporary = FALSE, overwrite = TRUE)
tbl(pitchRx_db, "po_temp") %>% glimpse()

# db_drop_table(pitchRx_db, "po")

dbExecute(pitchRx_db, 'ALTER TABLE po_temp RENAME TO po')

tbl(pitchRx_db, "po") %>% glimpse()



#----------------#
# ---- player ----
#----------------#

tbl(pitchRx_db, "player") %>% glimpse()

dbExecute(pitchRx_db, "ALTER TABLE player ADD game_date")
dbExecute(pitchRx_db, "UPDATE player SET game_date = replace(substr(gameday_link, 5, 10), '_', '-')")

tbl(pitchRx_db, "player") %>% glimpse()


doubleheaders_1 <-
    tbl(pitchRx_db, "player") %>% 
    mutate(
        game_year = as.numeric(substr(game_date, 1, 4)),
        doubleheader = case_when(substr(gameday_link, 30, 30) == "2" ~ "TRUE", TRUE ~ "FALSE"))

compute(doubleheaders_1, name = "player_temp", temporary = FALSE, overwrite = TRUE)
tbl(pitchRx_db, "player_temp") %>% glimpse()

# db_drop_table(pitchRx_db, "player")

dbExecute(pitchRx_db, 'ALTER TABLE player_temp RENAME TO player')

tbl(pitchRx_db, "player") %>% glimpse()



#----------------#
# ---- coach ----
#----------------#

tbl(pitchRx_db, "coach") %>% glimpse()

dbExecute(pitchRx_db, "ALTER TABLE coach ADD game_date")
dbExecute(pitchRx_db, "UPDATE coach SET game_date = replace(substr(gameday_link, 5, 10), '_', '-')")

tbl(pitchRx_db, "coach") %>% glimpse()


doubleheaders_1 <-
    tbl(pitchRx_db, "coach") %>% 
    mutate(
        game_year = as.numeric(substr(game_date, 1, 4)),
        doubleheader = case_when(substr(gameday_link, 30, 30) == "2" ~ "TRUE", TRUE ~ "FALSE"))

compute(doubleheaders_1, name = "coach_temp", temporary = FALSE, overwrite = TRUE)
tbl(pitchRx_db, "coach_temp") %>% glimpse()

# db_drop_table(pitchRx_db, "coach")

dbExecute(pitchRx_db, 'ALTER TABLE coach_temp RENAME TO coach')

tbl(pitchRx_db, "coach") %>% glimpse()



#----------------#
# ---- umpire ----
#----------------#

tbl(pitchRx_db, "umpire") %>% glimpse()

dbExecute(pitchRx_db, "ALTER TABLE umpire ADD game_date")
dbExecute(pitchRx_db, "UPDATE umpire SET game_date = replace(substr(gameday_link, 5, 10), '_', '-')")

tbl(pitchRx_db, "umpire") %>% glimpse()


doubleheaders_1 <-
    tbl(pitchRx_db, "umpire") %>% 
    mutate(
        game_year = as.numeric(substr(game_date, 1, 4)),
        doubleheader = case_when(substr(gameday_link, 30, 30) == "2" ~ "TRUE", TRUE ~ "FALSE"))

compute(doubleheaders_1, name = "umpire_temp", temporary = FALSE, overwrite = TRUE)
tbl(pitchRx_db, "umpire_temp") %>% glimpse()

# db_drop_table(pitchRx_db, "umpire")

dbExecute(pitchRx_db, 'ALTER TABLE umpire_temp RENAME TO umpire')

tbl(pitchRx_db, "umpire") %>% glimpse()



#=============================#
#### Initilizing Database ####
#=============================#

hip_tbl_head <- tbl(pitchRx_db, "hip") %>% head(15) %>% collect()

dbWriteTable(pitchRx_db, "hip_tbl_head", hip_tbl_head, overwrite = TRUE)

dbListTables(pitchRx_db)


tbl(pitchRx_db, "hip_tbl_head") %>% glimpse()



#=============================#
#### Initilizing Database ####
#=============================#



doubleheaders_1 <-
    tbl(pitchRx_db, "atbat") %>% 
    mutate(
        game_year = as.numeric(substr(game_date, 1, 4)),
        doubleheader = case_when(substr(gameday_link, 30, 30) == "2" ~ "TRUE", TRUE ~ "FALSE"))

compute(doubleheaders_1, name = "atbat_temp", temporary = FALSE, overwrite = TRUE)

# db_drop_table(pitchRx_db, "atbat")

dbExecute(pitchRx_db, 'ALTER TABLE atbat_temp RENAME TO atbat')







x1 <- tbl(pitchRx_db, "atbat") %>% select(gameday_link)

x1 %>% collect() %>% nrow()





dbExecute(pitchRx_db, "ALTER TABLE atbat ADD at_bat_event")

dbExecute(pitchRx_db, "ALTER TABLE pitch ADD pitch_number")

dbExecute(pitchRx_db, "ALTER TABLE pitch ADD pitch_event")

dbExecute(pitchRx_db, "ALTER TABLE action ADD at_bat_number")

dbExecute(pitchRx_db, "ALTER TABLE action ADD pitch_number")

dbExecute(pitchRx_db, "ALTER TABLE action ADD action_event")

dbExecute(pitchRx_db, "ALTER TABLE player ADD at_bat_number")

dbExecute(pitchRx_db, "ALTER TABLE hip ADD hip_event")

dbExecute(pitchRx_db, "ALTER TABLE runner ADD at_bat_number")

dbExecute(pitchRx_db, "ALTER TABLE runner ADD runner_event")

dbExecute(pitchRx_db, "ALTER TABLE po ADD at_bat_number")

dbExecute(pitchRx_db, "ALTER TABLE po ADD po_event")




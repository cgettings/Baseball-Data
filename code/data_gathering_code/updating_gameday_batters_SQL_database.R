###########################################
###########################################
##
## Updating gameday batters SQL database
##
###########################################
###########################################

#=========================#
#### Loading packages ####
#=========================#

library(readr)
library(dplyr)
library(tidyr)
library(dbplyr)
library(stringr)
library(lubridate)
library(stringr)
library(tibble)
library(magrittr)
library(DBI)
library(pryr)
library(pitchRx)
library(rvest)
library(xml2)

#==========================#
#### Loading functions ####
#==========================#

source("./code/functions/get_batters.xml_do.R")
source("./code/functions/gid2date.R")

#=====================#
#### Loading Data ####
#=====================#

#--------------------------------#
# Connecting to database ----
#--------------------------------#

batter_db <- dbConnect(RSQLite::SQLite(), "./data/batter_db.sqlite3")

#--------------------------------#
# ---- Getting gids ----
#--------------------------------#


most_recent_day <-
    tbl(batter_db, "batters") %>%
    select(game_date) %>%
    collect() %>%
    pull(game_date) %>% 
    max() %>% 
    as_date() - 1


most_recent_gameday_links <-
    tbl(batter_db, "batters") %>%
    filter(game_date == most_recent_day) %>%
    select(gameday_link = gid) %>%
    distinct() %>% 
    collect()


url <- makeUrls(most_recent_day, today())
gameday_links <- str_trunc(url, 30, "left", "")
game_date <- gid2date(gameday_links)
all_gids <- data_frame(game_date, gameday_link = gameday_links, url)


gids_distinct <-
    anti_join(
        all_gids,
        most_recent_gameday_links,
        by = "gameday_link") %>% 
    arrange(game_date)


#------------------------------------------#
# ---- Downloading batter information ----
#------------------------------------------#


# ---- Downloading ----

template <- read_rds("./data/template.RDS")
all_batter_info <- template


for (i in 1:15) {
    
    
    cat("\nLoop ", i, " <", gids_distinct$gameday_link[i], ">", "\n", sep = "")
    
    i_batter_info   <- get_batters.xml_do(gid = gids_distinct$gameday_link[i])
    i_batter_info_2 <- 
        bind_rows(template, i_batter_info) %>%
        mutate(game_date = gid2date(gids_distinct$gameday_link[i]) %>% as.character()) %>%
        mutate(game_year = game_date %>% year() %>% as.integer())
    
    all_batter_info <- bind_rows(all_batter_info, i_batter_info_2)
    
    # if (nrow(i_batter_info) != 0)
        
        dbWriteTable(
            batter_db,
            "batters",
            value = i_batter_info_2,
            append = TRUE,
            temporary = FALSE
        )
    
    gc()
}

cat("\n===============================\n")
cat(nrow(all_batter_info), "rows added in total\n")
cat("Size: ")
object_size(all_batter_info) %>% print()
cat("===============================\n")
    

gc()

dbDisconnect(batter_db)

################################################################################
################################################################################
################################################################################
################################################################################

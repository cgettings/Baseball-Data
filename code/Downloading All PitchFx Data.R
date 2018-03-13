###########################################
###########################################
##
## Downloading all pitchRx data
##
###########################################
###########################################

#=========================#
#### Loading packages ####
#=========================#

library(readr)
library(dplyr)
library(dbplyr)
library(DBI)
library(tidyr)
library(stringr)
library(lubridate)
library(stringdist)
library(stringr)
library(tibble)
library(magrittr)
library(pryr)
library(purrr)
library(iterators)
library(pitchRx)

source("~/BASP/R analyses/Baseball Data/gid2date.R")


setwd("~/BASP/R analyses/Baseball Data/Data Files")

#===============================#
#### Automatic Update ####
#===============================#

pitchRx_db <- dbConnect(RSQLite::SQLite(), "pitchRx_db.sqlite3")

pitchRx::update_db(
    connect = pitchRx_db,
    suffix = c(
        "inning/inning_all.xml",
        "inning/inning_hit.xml",
        "players.xml",
        "miniscoreboard.xml"
    )
)



#===============================#
#### Multiple period update ####
#===============================#


from <- c(
    today()
    # ymd("2016-11-02"),
    # ymd("2015-11-01"),
    # ymd("2014-10-29"),
    # ymd("2013-10-30"),
    # ymd("2012-10-28"),
    # ymd("2011-10-28"),
    # ymd("2010-11-01"),
    # ymd("2009-11-04"),
    # ymd("2008-10-29")
    )

back_to <- c(
    ymd("2017-04-02")
    # ymd("2016-04-03"),
    # ymd("2015-04-05"),
    # ymd("2014-03-22"),
    # ymd("2013-03-31"),
    # ymd("2012-03-28"),
    # ymd("2011-03-31"),
    # ymd("2010-04-04"),
    # ymd("2009-04-05"),
    # ymd("2008-03-25")
    )


# data.frame(from, back_to)

cat("-----\n")

paste("Searching results from: ", max(from), ", back to: ", min(back_to), sep = "") %>% noquote() %>% print()

cat("-----\n")


#===============================#
#### Downloading ####
#===============================#

con <- dbConnect(RSQLite::SQLite(), "my_db2.sqlite3")
dbWriteTable(con, "mtcars", mtcars, append = TRUE)
dbReadTable(con, "mtcars")

dbListTables(con)
dbListFields(con, "mtcars")

mtcars2 <- tbl(con, "mtcars")

src_sqlite("my_db3.sqlite3", create = TRUE)


#===============================#
#### Downloading ####
#===============================#

# pitchRx_data_raw <- list(atbat = data.frame(), pitch = data.frame())
pitchRx_data_raw_atbat <- data.frame()
pitchRx_data_raw_pitch <- data.frame()
pitchRx_data_joined    <- data.frame()

for (i in 1:length(back_to)) {

    start_time       <- now()
    start_rows_atbat <- pitchRx_data_raw_atbat %>% nrow()
    start_rows_pitch <- pitchRx_data_raw_pitch %>% nrow()
    start_size_atbat <- object_size(pitchRx_data_raw_atbat)
    start_size_pitch <- object_size(pitchRx_data_raw_pitch)

    pitchRx_data_raw <-
        pitchRx::scrape(
            start = back_to[i],
            end = from[i],
            suffix = "inning/inning_all.xml",
            async = TRUE
        ) %>%
        extract(c("atbat", "pitch"))
    
    pitchRx_data_raw_atbat <- 
        pitchRx_data_raw %>%
        extract2("atbat") %>% 
        mutate(game_date = ymd(date)) %>%
        mutate(game_datetime = ymd_hms(start_tfs_zulu)) %>%
        mutate(game_datetime_east = force_tz(game_datetime, tzone = "US/Eastern")) %>%
        mutate(game_datetime_utc = with_tz(game_datetime_east, tzone = "UTC")) %>% 
        bind_rows(pitchRx_data_raw_atbat, .) %>% 
        as_tibble()
    
    pitchRx_data_raw_pitch <- 
        pitchRx_data_raw %>%
        extract2("pitch") %>% 
        mutate(game_date = str_replace_all(gameday_link, "\\_", "-")) %>%
        with(., str_split_fixed(
            string = game_date,
            pattern = "\\-",
            n = 5
        )) %>%
        as_data_frame() %>%
        transmute(game_date = str_c(V2, V3, V4, sep = "-") %>% as_date()) %>%
        bind_cols(., pitchRx_inning_all %>% extract2("pitch")) %>%
        mutate(game_datetime = ymd_hms(tfs_zulu)) %>%
        mutate(game_datetime_east = force_tz(game_datetime, tzone = "US/Eastern")) %>%
        mutate(game_datetime_utc = with_tz(game_datetime_east, tzone = "UTC")) %>%
        bind_rows(pitchRx_data_raw_pitch, .) %>% 
        as_tibble()
    
    pitchRx_data_joined <-
        right_join(
            pitchRx_data_raw$atbat, 
            pitchRx_data_raw$pitch, 
            by = "play_guid") %>% 
        bind_rows(pitchRx_data_joined, .) %>% 
        as_tibble()
    
    
    # write_rds(
    #     pitchRx_data_raw_atbat,
    #     paste("pitchRx_data_raw_atbat - ", from[1], " to ", back_to[i], ".RDS", sep = "")
    # )
    # 
    # write_rds(
    #     pitchRx_data_raw_pitch,
    #     paste("pitchRx_data_raw_pitch - ", from[1], " to ", back_to[i], ".RDS", sep = "")
    # )
    

        
    cat("\n")
    cat("-----\n")

    cat(format(from[i], "%F"), " -> ", format(back_to[i], "%F"), sep = "")

    cat("\n")
    end_time <- now()
    diff_time <- difftime(end_time, start_time, units = "secs") %>% round(1)
    cat(format(end_time, "%X"), " (", diff_time, " seconds)", sep = "")

    end_rows_atbat <- pitchRx_data_raw_atbat %>% nrow()
    end_rows_pitch <- pitchRx_data_raw_pitch %>% nrow()
    
    cat("\n")
    cat("'atbat': ", end_rows_atbat, " rows (", end_rows_atbat - start_rows_atbat, " added)", sep = "")
    cat("\n")
    cat("'pitch': ", end_rows_pitch, " rows (", end_rows_pitch - start_rows_pitch, " added)", sep = "")

    cat("\n")
    end_size_atbat <- object_size(pitchRx_data_raw_atbat)
    end_size_pitch <- object_size(pitchRx_data_raw_pitch)
    
    cat(round(end_size_atbat, 1), " KB (", round(end_size_atbat - start_size_atbat, 1), " KB added)", sep = "")
    cat("\n")
    cat(round(end_size_pitch, 1), " KB (", round(end_size_pitch - start_size_pitch, 1), " KB added)", sep = "")

    cat("\n")
    cat("-----\n")

    gc(verbose = FALSE)

}


# write_rds(pitchRx_data_raw_atbat, "pitchRx_data_raw_atbat 6.26.2017.RDS", compress = "gz")
# write_rds(pitchRx_data_raw_pitch, "pitchRx_data_raw_pitch 6.26.2017.RDS", compress = "gz")
# write_rds(pitchRx_data_joined,    "pitchRx_data_joined 6.26.2017.RDS", compress = "gz")

# statcast_update_raw_distinct <- statcast_update_raw %>% distinct()
# write_rds(statcast_update_raw_distinct, "statcast_update_raw_distinct 6.26.2017.RDS", compress = "gz")

#### END ####



con <- dbConnect(RSQLite::SQLite(), "my_db2.sqlite3")
dbWriteTable(con, "mtcars", mtcars, append = TRUE)
dbReadTable(con, "mtcars")

dbListTables(con)
dbListFields(con, "mtcars")

mtcars2 <- tbl(con, "mtcars")

src_sqlite("my_db3.sqlite3", create = TRUE)


#===============================#
#### Multiple period update ####
#===============================#


from <- c(
    # today(),
    # ymd("2016-11-02"),
    # ymd("2016-07-15"),
    # ymd("2015-11-01"),
    # ymd("2015-07-15"),
    # ymd("2014-10-29"),
    # ymd("2014-07-15"),
    ymd("2013-10-30"),
    ymd("2013-07-15"),
    ymd("2012-10-28"),
    ymd("2012-07-15"),
    ymd("2011-10-28"),
    ymd("2011-07-15"),
    ymd("2010-11-01"),
    ymd("2010-07-15"),
    ymd("2009-11-04"),
    ymd("2009-07-15"),
    ymd("2008-10-29"),
    ymd("2008-07-15")
)

back_to <- c(
    # ymd("2017-04-02"),
    # ymd("2016-07-16"),
    # ymd("2016-04-03"),
    # ymd("2015-07-16"),
    # ymd("2015-04-05"),
    # ymd("2014-07-16"),
    # ymd("2014-03-22"),
    ymd("2013-07-16"),
    ymd("2013-03-31"),
    ymd("2012-07-16"),
    ymd("2012-03-28"),
    ymd("2011-07-16"),
    ymd("2011-03-31"),
    ymd("2010-07-16"),
    ymd("2010-04-04"),
    ymd("2009-07-16"),
    ymd("2009-04-05"),
    ymd("2008-07-16"),
    ymd("2008-03-25")
    )

# every.10.days <- seq(10, 220, 10)

# back_to <-
#     c(
        # lubridate::today() - every.10.days[1:9]
        # ymd("2016-11-04") -  every.10.days
        # ymd("2015-11-04") -  every.10.days
        # ymd("2015-11-05") -  every.10.days[22:23]
        # ymd("2014-10-31") -  every.10.days
        # ymd("2013-11-01") -  every.10.days
        # ymd("2012-11-01") -  every.10.days
        # ymd("2011-11-01") -  every.10.days,
        # ymd("2010-11-03") -  every.10.days,
        # ymd("2009-11-05") -  every.10.days,
        # ymd("2008-10-31") -  every.10.days
    # )

# from <-   back_to + 9

data.frame(from, back_to)
from - back_to


# from    <- c("2016-11-02", "2016-10-31")
# back_to <- c("2016-11-01", "2016-10-30")

#===============================#
#### Downloading ####
#===============================#



# pitchRx_data_raw <- list()


#########################################

pitchRx_db <- dbConnect(RSQLite::SQLite(), "gameday_2.sqlite3")

i <- 4

# for(i in 1:length(back_to)) {
    
    # pitchRx_data_raw <-
        
        pitchRx::scrape(
            # start = "2014-03-22",
            # end = "2015-04-13",
            start = back_to[i],
            end = from[i],
            suffix = "inning/inning_all.xml",
            async = TRUE,
            connect = pitchRx_db,
            append = TRUE
        ) #%>%
        # extract(c("atbat", "pitch")) %>% 
        # mutate()
    
    
    # dbWriteTable(pitchRx_db,
    #     "atbat",
    #     pitchRx_data_raw$atbat   %>% select(one_of(cols_atbat)),
    #     append = TRUE)
    
    # dbWriteTable(pitchRx_db,
    #     "action",
    #     pitchRx_data_raw$action  %>% select(one_of(cols_action)),
    #     append = TRUE)
    
    # dbWriteTable(pitchRx_db,
    #     "pitch",
    #     pitchRx_data_raw$pitch   %>% select(one_of(cols_pitch)),
    #     append = TRUE)
    
    # dbWriteTable(pitchRx_db,
    #     "po",
    #     pitchRx_data_raw$po      %>% select(one_of(cols_po)),
    #     append = TRUE)
    # 
    # dbWriteTable(pitchRx_db,
    #     "runner",
    #     pitchRx_data_raw$runner  %>% select(one_of(cols_runner)),
    #     append = TRUE)
    
    # gc()
    .rs.restartR()
    
    # pitchRx_db <- dbConnect(RSQLite::SQLite(), "gameday_2.sqlite3")

# }

#########################################

dbDisconnect(pitchRx_db)

#########################################


cols_atbat  <- tbl(pitchRx_db, "atbat")  %>% filter(row_number() == 1) %>% colnames()
# cols_action <- tbl(pitchRx_db, "action") %>% filter(row_number() == 1) %>% colnames()
cols_pitch  <- tbl(pitchRx_db, "pitch")  %>% filter(row_number() == 1) %>% colnames()
# cols_po     <- tbl(pitchRx_db, "po")     %>% filter(row_number() == 1) %>% colnames()
# cols_runner <- tbl(pitchRx_db, "runner") %>% filter(row_number() == 1) %>% colnames()


# dbWriteTable(pitchRx_db, "atbat",  pitchRx_data_raw$atbat   %>% select(one_of(cols_atbat)),  append = TRUE)
# dbWriteTable(pitchRx_db, "action", pitchRx_data_raw$action  %>% select(one_of(cols_action)), append = TRUE)
# dbWriteTable(pitchRx_db, "pitch",  pitchRx_data_raw$pitch   %>% select(one_of(cols_pitch)),  append = TRUE)
# dbWriteTable(pitchRx_db, "po",     pitchRx_data_raw$po      %>% select(one_of(cols_po)),     append = TRUE)
# dbWriteTable(pitchRx_db, "runner", pitchRx_data_raw$runner  %>% select(one_of(cols_runner)), append = TRUE)


tbl(pitchRx_db, "atbat")  %>% select(date) %>% collect() %>% mutate(date = ymd(date)) %>% summarize(max(date, na.rm = TRUE), min(date, na.rm = TRUE))

tbl(pitchRx_db, "action") %>% select(tfs_zulu) %>% collect() %>% mutate(tfs_zulu = ymd_hms(tfs_zulu)) %>% summarize(max(tfs_zulu, na.rm = TRUE), min(tfs_zulu, na.rm = TRUE))

tbl(pitchRx_db, "pitch")  %>% select(tfs_zulu) %>% collect() %>% mutate(tfs_zulu = ymd_hms(tfs_zulu)) %>% summarize(max(tfs_zulu, na.rm = TRUE), min(tfs_zulu, na.rm = TRUE))

tbl(pitchRx_db, "po")     %>% select(date) %>% collect() %>% mutate(date = ymd(date)) %>% summarize(max(date, na.rm = TRUE), min(date, na.rm = TRUE))

tbl(pitchRx_db, "runner") %>% select(start) %>% collect() %>% mutate(start = ymd(start)) %>% summarize(max(start, na.rm = TRUE), min(start, na.rm = TRUE))




tbl(pitchRx_db, "atbat") %>% select(date) %>% collect() %>% mutate(date = ymd(date)) %>% count(date) %>% arrange(n)

tbl(pitchRx_db, "action") %>% select(tfs_zulu) %>% collect() %>% mutate(tfs_zulu = ymd_hms(tfs_zulu)) %>% summarize(max(tfs_zulu, na.rm = TRUE), min(tfs_zulu, na.rm = TRUE))

tbl(pitchRx_db, "pitch")  %>% select(tfs_zulu) %>% collect() %>% mutate(tfs_zulu = ymd_hms(tfs_zulu)) %>% summarize(max(tfs_zulu, na.rm = TRUE), min(tfs_zulu, na.rm = TRUE))

tbl(pitchRx_db, "po")     %>% select(date) %>% collect() %>% mutate(date = ymd(date)) %>% summarize(max(date, na.rm = TRUE), min(date, na.rm = TRUE))

tbl(pitchRx_db, "runner") %>% select(start) %>% collect() %>% mutate(start = ymd(start)) %>% summarize(max(start, na.rm = TRUE), min(start, na.rm = TRUE))


#########################################


game.ids <- pitchRx::gids %>% rev()


game.ids.1 <-
    game.ids[!game.ids %in%
            c(
                str_subset(game.ids, "03_1"),
                str_subset(game.ids, "03_0"),
                str_subset(game.ids, "02_2"),
                str_subset(game.ids, "02_1"),
                str_subset(game.ids, "02_0")
            )]


start_gids <- seq(1, 21600, 200)
end_gids   <- start_gids + 199

game.ids.cut <- list()

for(i in 1:length(start_gids)) {
    game.ids.cut[[i]] <- game.ids.1[start_gids[i]:end_gids[i]]
}



game.ids.cut[[108]] <- game.ids.cut[[108]][!is.na(game.ids.cut[[108]])]


#########################################


pitchRx_db <- dbConnect(RSQLite::SQLite(), "gameday_3.sqlite3")

i <- 1

xxxx <- 
pitchRx::scrape(
    game.ids = game.ids.cut[[i]],
    suffix = "inning/inning_hit.xml",
    async = TRUE
    # connect = pitchRx_db,
    # append = TRUE
)

gc()
print(i)
.rs.restartR()

#########################################


#########################################

pitchRx_db <- dbConnect(RSQLite::SQLite(), "gameday_3.sqlite3")

for(i in 78:length(game.ids.cut)) {
    
    pitchRx::scrape(
        # game.ids = (game.ids.cut[[i]]),
        game.ids = game.ids.cut[[i]],
        suffix = "inning/inning_hit.xml",
        async = TRUE,
        connect = pitchRx_db,
        append = TRUE
    )
    
    # gc()
    print(i)
    
}

gc()

#########################################



scrape_gameids_lapply <-
    
    function(game_ids = X, suffix = "inning/inning_all.xml", connection = NULL) {
        
        if (is.null(connection)) {
            connection <- dbConnect(RSQLite::SQLite(), ":memory:")
            
        } else {
            if (!dbIsValid(connection)) {
                connection <- dbConnect(RSQLite::SQLite(), connection@dbname)
            }
            pitchRx::scrape(
                game.ids = game_ids,
                suffix = suffix,
                async = TRUE,
                connect = connection,
                append = TRUE
            )
            
            gc(verbose = FALSE)
            return(connection)
        }
    }


 
con <- dbConnect(RSQLite::SQLite(), "my_db.sqlite3")

tab <-
    lapply(
        X = game.ids.cut[1:10],
        FUN = scrape_gameids_lapply,
        connection = con)




.rs.restartR()

#====#===#===#===#===#===#===#===#
# ==== Filtering distinct ====
#====#===#===#===#===#===#===#===#

pitchRx_db <- dbConnect(RSQLite::SQLite(), "gameday_3.sqlite3")

db_tables <- db_list_tables(pitchRx_db) %>% print()


#======================#
# ---- `atbat` ----
#======================#

#------------------#
# ---- On Disk ----
#------------------#

atbat <- tbl(pitchRx_db, "atbat")
atbat %>% glimpse()

atbat_tab <- 
    tbl(pitchRx_db, "atbat") %>%
    select(gameday_link, num) %>%
    count(gameday_link, num) %>%
    arrange(-n)

atbat_tab_df <- collect(atbat_tab) %>% print()


dbWriteTable(pitchRx_db, "atbat_tab_df", atbat_tab_df %>% filter(n > 1), overwrite = TRUE)

atbat_3 <- inner_join(tbl(pitchRx_db, "atbat"), tbl(pitchRx_db, "atbat_tab_df"))
atbat_4 <- atbat_3 %>% collect()

atbat_5 <- atbat %>% distinct(gameday_link, num, .keep_all = FALSE)
atbat_6 <- semi_join(atbat, atbat_5)

compute(atbat_6, name = "atbat_distinct", temporary = FALSE)


#--------------------#
# ---- In Memory ----
#--------------------#

atbat_df <- tbl(pitchRx_db, "atbat") %>% collect()
atbat_df_2 <- atbat_df %>% count(gameday_link, num) %>% arrange(-n)

atbat_df_3 <- atbat_df %>% distinct(gameday_link, num, play_guid, .keep_all = TRUE)

atbat_df_4 <-
    atbat_df_3 %>%
    mutate(pitch_type = na_if(pitch_type, "")) %>% 
    count(gameday_link, num, play_guid) %>%
    # count(play_guid) %>%
    right_join(., atbat_df_3) %>%
    filter(!is.na(pitch_type) & n < 2) %>%
    ungroup()


atbat_df_5 <- left_join(atbat_df, atbat_df_4, by = c("gameday_link", "num"))

atbat_df_5 %>% 
    filter(n == 11) %>% 
    select(event, 
        player, 
        num, 
        gameday_link, 
        event_num, 
        play_guid, 
        n) %>%
    print(n = 100)

dbWriteTable(pitchRx_db, "atbat_distinct", atbat_df_3)

#--------------------#
# ---- Renaming ----
#--------------------#

tbl(pitchRx_db, "atbat")          %>% collect() %>% nrow()
tbl(pitchRx_db, "atbat_distinct") %>% collect() %>% nrow()

db_drop_table(pitchRx_db, "atbat")
db_drop_table(pitchRx_db, "atbat_tab_df")

dbExecute(pitchRx_db, 'ALTER TABLE atbat_distinct RENAME TO atbat')

db_tables <- db_list_tables(pitchRx_db) %>% print()


#======================#
# ---- `pitch` ----
#======================#

#------------------#
# ---- On Disk ----
#------------------#

pitch <- tbl(pitchRx_db, "pitch")
pitch %>% glimpse()

pitch_tab <- 
    tbl(pitchRx_db, "pitch") %>%
    select(gameday_link, num, id) %>%
    count(gameday_link, num, id) %>%
    arrange(-n)

pitch_tab_df <- collect(pitch_tab) %>% print()


pitch_3 <- pitch %>% distinct()
compute(pitch_3, name = "pitch_distinct", temporary = FALSE)

#--------------------#
# ---- In Memory ----
#--------------------#

pitch_df <- tbl(pitchRx_db, "pitch") %>% collect()

pitch_df_2 <- 
    pitch_df %>% 
    count(gameday_link, num, id) %>% 
    arrange(-n)

pitch_df_3 <- pitch_df %>% distinct()

pitch_df_4 <-
    pitch_df_3 %>%
    mutate(pitch_type = na_if(pitch_type, "")) %>% 
    count(gameday_link, num, id) %>%
    # count(play_guid) %>%
    right_join(., statcast_update_raw_distinct) %>%
    filter(!is.na(pitch_type) & n < 2) %>%
    ungroup()


pitch_df_5 <- left_join(pitch_df, pitch_df_4, by = c("gameday_link", "num"))

pitch_df_5 %>% 
    filter(n == 11) %>% 
    select(event, 
        player, 
        num, 
        gameday_link, 
        event_num, 
        play_guid, 
        n) %>%
    print(n = 100)

dbWriteTable(pitchRx_db, "pitch_distinct", pitch_df_5)

#--------------------#
# ---- Renaming ----
#--------------------#

tbl(pitchRx_db, "pitch")          %>% collect() %>% nrow()
tbl(pitchRx_db, "pitch_distinct") %>% collect() %>% nrow()

db_drop_table(pitchRx_db, "pitch")
db_drop_table(pitchRx_db, "action_tab_df")

dbExecute(pitchRx_db, 'ALTER TABLE pitch_distinct RENAME TO pitch')

db_tables <- db_list_tables(pitchRx_db) %>% print()

#--------------------#
# ---- Etc. ----
#--------------------#



#======================#
# ---- `action` ----
#======================#

#------------------#
# ---- On Disk ----
#------------------#

action <- tbl(pitchRx_db, "action") %>% show_query()
action %>% glimpse()

action_tab <- 
    action %>%
    select(gameday_link, num, pitch, player, tfs) %>%
    count(gameday_link, num, pitch, player, tfs) %>%
    arrange(-n)

action_tab <- 
    action %>%
    select(gameday_link, pitch, num) %>%
    count(gameday_link, pitch, num) %>%
    arrange(gameday_link, pitch)

action_tab_df <- collect(action_tab) %>% print()


dbWriteTable(pitchRx_db, "action_tab_df", tab_df %>% filter(n > 1), overwrite = TRUE)

action_4 <- inner_join(tbl(pitchRx_db, "action"), tbl(pitchRx_db, "action_tab_df"))
action_5 <- action_4 %>% collect()


action_3 <- action %>% distinct()


compute(action_3, name = "action_distinct", temporary = FALSE)

#--------------------#
# ---- In Memory ----
#--------------------#

action_df <- tbl(pitchRx_db, "action") %>% collect()
action_df_2 <- action_df %>% count(gameday_link, num) %>% arrange(-n)

action_df_3 <- action_df %>% distinct()

action_df_4 <-
    action_df_3 %>%
    mutate(pitch_type = na_if(pitch_type, "")) %>% 
    count(gameday_link, num, pitch, tfs) %>%
    # count(play_guid) %>%
    right_join(., statcast_update_raw_distinct) %>%
    filter(!is.na(pitch_type) & n < 2) %>%
    ungroup()


action_df_5 <- left_join(action_df, action_df_4, by = c("gameday_link", "num"))

action_df_5 %>% 
    filter(n == 11) %>% 
    select(event, 
        player, 
        num, 
        gameday_link, 
        event_num, 
        play_guid, 
        n) %>%
    print(n = 100)

dbWriteTable(pitchRx_db, "action_distinct", action_df_5)


#--------------------#
# ---- Renaming ----
#--------------------#

tbl(pitchRx_db, "action")          %>% collect() %>% nrow()
tbl(pitchRx_db, "action_distinct") %>% collect() %>% nrow()

db_drop_table(pitchRx_db, "action") %>% compute()

dbExecute(pitchRx_db, 'ALTER TABLE action_distinct RENAME TO action')

db_tables <- db_list_tables(pitchRx_db) %>% print()


#======================#
# ---- `player` ----
#======================#

#------------------#
# ---- On Disk ----
#------------------#

player <- tbl(pitchRx_db, "player")
player %>% glimpse()


tab <- 
    tbl(pitchRx_db, "player") %>%
    select(gameday_link, num) %>%
    count(gameday_link, num) %>%
    arrange(-n)

tab_df <- collect(tab) %>% print()


dbWriteTable(pitchRx_db, "player_tab_df", tab_df %>% filter(n > 1), overwrite = TRUE)

player_4 <- inner_join(tbl(pitchRx_db, "player"), tbl(pitchRx_db, "player_tab_df"))
player_5 <- player_4 %>% collect()


player_3 <- player %>% distinct()


compute(player_3, name = "player_distinct", temporary = FALSE)

#--------------------#
# ---- In Memory ----
#--------------------#

player_df <- tbl(pitchRx_db, "player") %>% collect()
player_df %>% glimpse()

player_df_2 <- player_df %>% count(gameday_link, num, id) %>% arrange(-n)

player_df_3 <- player_df %>% distinct()

player_df_4 <-
    player_df_3 %>%
    mutate(pitch_type = na_if(pitch_type, "")) %>% 
    count(gameday_link, num, play_guid) %>%
    # count(play_guid) %>%
    right_join(., statcast_update_raw_distinct) %>%
    filter(!is.na(pitch_type) & n < 2) %>%
    ungroup()


player_df_5 <- left_join(player_df, player_df_4, by = c("gameday_link", "num"))

player_df_5 %>% 
    filter(n == 11) %>% 
    select(event, 
        player, 
        num, 
        gameday_link, 
        event_num, 
        play_guid, 
        n) %>%
    print(n = 100)

dbWriteTable(pitchRx_db, "player_distinct", player_df_5)

#--------------------#
# ---- Renaming ----
#--------------------#

tbl(pitchRx_db, "player")          %>% collect() %>% nrow()
tbl(pitchRx_db, "player_distinct") %>% collect() %>% nrow()

db_drop_table(pitchRx_db, "player")

dbExecute(pitchRx_db, 'ALTER TABLE player_distinct RENAME TO player')

db_tables <- db_list_tables(pitchRx_db) %>% print()



#======================#
# ---- `player` ----
#======================#

#------------------#
# ---- On Disk ----
#------------------#

hip <- tbl(pitchRx_db, "hip")
hip %>% glimpse()


tab <- 
    tbl(pitchRx_db, "hip") %>%
    select(gameday_link, num) %>%
    count(gameday_link, num) %>%
    arrange(-n)

tab_df <- collect(tab) %>% print()


dbWriteTable(pitchRx_db, "hip_tab_df", tab_df %>% filter(n > 1), overwrite = TRUE)

hip_4 <- inner_join(tbl(pitchRx_db, "hip"), tbl(pitchRx_db, "hip_tab_df"))
hip_5 <- hip_4 %>% collect()


hip_3 <- hip %>% distinct()


compute(hip_3, name = "hip_distinct", temporary = FALSE)

#--------------------#
# ---- In Memory ----
#--------------------#

hip_df <- tbl(pitchRx_db, "hip") %>% collect()
hip_df %>% glimpse()

hip_df_2 <- hip_df %>% count(gameday_link, num) %>% arrange(-n)

hip_df_3 <- hip_df %>% distinct()

hip_df_4 <-
    hip_df_3 %>%
    mutate(pitch_type = na_if(pitch_type, "")) %>% 
    count(gameday_link, num, play_guid) %>%
    # count(play_guid) %>%
    right_join(., statcast_update_raw_distinct) %>%
    filter(!is.na(pitch_type) & n < 2) %>%
    ungroup()


hip_df_5 <- left_join(hip_df, hip_df_4, by = c("gameday_link", "num"))

hip_df_5 %>% 
    filter(n == 11) %>% 
    select(event, 
        hip, 
        num, 
        gameday_link, 
        event_num, 
        play_guid, 
        n) %>%
    print(n = 100)

dbWriteTable(pitchRx_db, "hip_distinct", hip_df_5)

#--------------------#
# ---- Renaming ----
#--------------------#

tbl(pitchRx_db, "hip")          %>% collect() %>% nrow()
tbl(pitchRx_db, "hip_distinct") %>% collect() %>% nrow()

db_drop_table(pitchRx_db, "hip")

dbExecute(pitchRx_db, 'ALTER TABLE hip_distinct RENAME TO hip')

db_tables <- db_list_tables(pitchRx_db) %>% print()



#======================#
# ---- `runner` ----
#======================#

#------------------#
# ---- On Disk ----
#------------------#

runner <- tbl(pitchRx_db, "runner")
runner %>% glimpse()


tab <- 
    tbl(pitchRx_db, "runner") %>%
    select(gameday_link, num) %>%
    count(gameday_link, num) %>%
    arrange(-n)

tab_df <- collect(tab) %>% print()


dbWriteTable(pitchRx_db, "runner_tab_df", tab_df %>% filter(n > 1), overwrite = TRUE)

runner_4 <- inner_join(tbl(pitchRx_db, "runner"), tbl(pitchRx_db, "runner_tab_df"))
runner_5 <- runner_4 %>% collect()


runner_3 <- runner %>% distinct()


compute(runner_3, name = "runner_distinct", temporary = FALSE)

#--------------------#
# ---- In Memory ----
#--------------------#

runner_df <- tbl(pitchRx_db, "runner") %>% collect()
runner_df %>% glimpse()

runner_df_2 <- runner_df %>% count(gameday_link, num, id) %>% arrange(-n)

runner_df_3 <- runner_df %>% distinct()

runner_df_4 <-
    runner_df_3 %>%
    mutate(pitch_type = na_if(pitch_type, "")) %>% 
    count(gameday_link, num, play_guid) %>%
    # count(play_guid) %>%
    right_join(., statcast_update_raw_distinct) %>%
    filter(!is.na(pitch_type) & n < 2) %>%
    ungroup()


runner_df_5 <- left_join(runner_df, runner_df_4, by = c("gameday_link", "num"))

runner_df_5 %>% 
    filter(n == 11) %>% 
    select(event, 
        runner, 
        num, 
        gameday_link, 
        event_num, 
        play_guid, 
        n) %>%
    print(n = 100)

dbWriteTable(pitchRx_db, "runner_distinct", runner_df_5)

#--------------------#
# ---- Renaming ----
#--------------------#

tbl(pitchRx_db, "runner")          %>% collect() %>% nrow()
tbl(pitchRx_db, "runner_distinct") %>% collect() %>% nrow()

db_drop_table(pitchRx_db, "runner")

dbExecute(pitchRx_db, 'ALTER TABLE runner_distinct RENAME TO runner')

db_tables <- db_list_tables(pitchRx_db) %>% print()



#======================#
# ---- `po` ----
#======================#

#------------------#
# ---- On Disk ----
#------------------#

po <- tbl(pitchRx_db, "po")
po %>% glimpse()


tab <- 
    tbl(pitchRx_db, "po") %>%
    select(gameday_link, num) %>%
    count(gameday_link, num) %>%
    arrange(-n)

tab_df <- collect(tab) %>% print()


dbWriteTable(pitchRx_db, "po_tab_df", tab_df %>% filter(n > 1), overwrite = TRUE)

po_4 <- inner_join(tbl(pitchRx_db, "po"), tbl(pitchRx_db, "po_tab_df"))
po_5 <- po_4 %>% collect()


po_3 <- po %>% distinct()


compute(po_3, name = "po_distinct", temporary = FALSE)

#--------------------#
# ---- In Memory ----
#--------------------#

po_df <- tbl(pitchRx_db, "po") %>% collect()
po_df_2 <- po_df %>% count(gameday_link, num) %>% arrange(-n)

po_df_3 <- po_df %>% distinct()

po_df_4 <-
    po_df_3 %>%
    mutate(pitch_type = na_if(pitch_type, "")) %>% 
    count(gameday_link, num, play_guid) %>%
    # count(play_guid) %>%
    right_join(., statcast_update_raw_distinct) %>%
    filter(!is.na(pitch_type) & n < 2) %>%
    ungroup()


po_df_5 <- left_join(po_df, po_df_4, by = c("gameday_link", "num"))

po_df_5 %>% 
    filter(n == 11) %>% 
    select(event, 
        po, 
        num, 
        gameday_link, 
        event_num, 
        play_guid, 
        n) %>%
    print(n = 100)

dbWriteTable(pitchRx_db, "po_distinct", po_df_5)

#--------------------#
# ---- Renaming ----
#--------------------#

tbl(pitchRx_db, "po")          %>% collect() %>% nrow()
tbl(pitchRx_db, "po_distinct") %>% collect() %>% nrow()

db_drop_table(pitchRx_db, "po")

dbExecute(pitchRx_db, 'ALTER TABLE po_distinct RENAME TO po')

db_tables <- db_list_tables(pitchRx_db) %>% print()


#======================#
# ---- `coach` ----
#======================#

#------------------#
# ---- On Disk ----
#------------------#

coach <- tbl(pitchRx_db, "coach")
coach %>% glimpse()


tab <- 
    tbl(pitchRx_db, "coach") %>%
    select(gameday_link, num) %>%
    count(gameday_link, num) %>%
    arrange(-n)

tab_df <- collect(tab) %>% print()


dbWriteTable(pitchRx_db, "coach_tab_df", tab_df %>% filter(n > 1), overwrite = TRUE)

coach_4 <- inner_join(tbl(pitchRx_db, "coach"), tbl(pitchRx_db, "coach_tab_df"))
coach_5 <- coach_4 %>% collect()


coach_3 <- coach %>% distinct()


compute(coach_3, name = "coach_distinct", temporary = FALSE)

#--------------------#
# ---- In Memory ----
#--------------------#

coach_df <- tbl(pitchRx_db, "coach") %>% collect()
coach_df %>% glimpse()

coach_df_2 <- coach_df %>% count(gameday_link, id) %>% arrange(-n)

semi_join(coach_df, coach_df_2 %>% filter(n > 1))

coach_df_3 <- coach_df %>% distinct()

coach_df_4 <-
    coach_df_3 %>%
    mutate(pitch_type = na_if(pitch_type, "")) %>% 
    count(gameday_link, num, play_guid) %>%
    # count(play_guid) %>%
    right_join(., statcast_update_raw_distinct) %>%
    filter(!is.na(pitch_type) & n < 2) %>%
    ungroup()


coach_df_5 <- left_join(coach_df, coach_df_4, by = c("gameday_link", "num"))

coach_df_5 %>% 
    filter(n == 11) %>% 
    select(event, 
        coach, 
        num, 
        gameday_link, 
        event_num, 
        play_guid, 
        n) %>%
    print(n = 100)

dbWriteTable(pitchRx_db, "coach_distinct", coach_df_5)

#--------------------#
# ---- Renaming ----
#--------------------#

tbl(pitchRx_db, "coach")          %>% collect() %>% nrow()
tbl(pitchRx_db, "coach_distinct") %>% collect() %>% nrow()

db_drop_table(pitchRx_db, "coach")

dbExecute(pitchRx_db, 'ALTER TABLE coach_distinct RENAME TO coach')

db_tables <- db_list_tables(pitchRx_db) %>% print()



#======================#
# ---- `umpire` ----
#======================#

#------------------#
# ---- On Disk ----
#------------------#

umpire <- tbl(pitchRx_db, "umpire")
umpire %>% glimpse()


tab <- 
    tbl(pitchRx_db, "umpire") %>%
    select(gameday_link, num) %>%
    count(gameday_link, num) %>%
    arrange(-n)

tab_df <- collect(tab) %>% print()


dbWriteTable(pitchRx_db, "umpire_tab_df", tab_df %>% filter(n > 1), overwrite = TRUE)

umpire_4 <- inner_join(tbl(pitchRx_db, "umpire"), tbl(pitchRx_db, "umpire_tab_df"))
umpire_5 <- umpire_4 %>% collect()


umpire_3 <- umpire %>% distinct()


compute(umpire_3, name = "umpire_distinct", temporary = FALSE)

#--------------------#
# ---- In Memory ----
#--------------------#

umpire_df <- tbl(pitchRx_db, "umpire") %>% collect()
umpire_df %>% glimpse()

umpire_df_2 <- umpire_df %>% count(gameday_link, num) %>% arrange(-n)

umpire_df_3 <- umpire_df %>% distinct()

umpire_df_4 <-
    umpire_df_3 %>%
    mutate(pitch_type = na_if(pitch_type, "")) %>% 
    count(gameday_link, num, play_guid) %>%
    # count(play_guid) %>%
    right_join(., statcast_update_raw_distinct) %>%
    filter(!is.na(pitch_type) & n < 2) %>%
    ungroup()


umpire_df_5 <- left_join(umpire_df, umpire_df_4, by = c("gameday_link", "num"))

umpire_df_5 %>% 
    filter(n == 11) %>% 
    select(event, 
        umpire, 
        num, 
        gameday_link, 
        event_num, 
        play_guid, 
        n) %>%
    print(n = 100)

dbWriteTable(pitchRx_db, "umpire_distinct", umpire_df_5)

#--------------------#
# ---- Renaming ----
#--------------------#

tbl(pitchRx_db, "umpire")          %>% collect() %>% nrow()
tbl(pitchRx_db, "umpire_distinct") %>% collect() %>% nrow()

# db_drop_table(pitchRx_db, "umpire")

dbExecute(pitchRx_db, 'ALTER TABLE umpire_distinct RENAME TO umpire')

db_tables <- db_list_tables(pitchRx_db) %>% print()


################################################################################
################################################################################
################################################################################
################################################################################


#=========================#
#### Modifying dates ####
#=========================#

setwd("~/BASP/R analyses/Baseball Data/Data Files")

#--------------------#
# ---- atbat ----
#--------------------#

pitchRx_db <- dbConnect(RSQLite::SQLite(), "pitchRx_db.sqlite3")
pitchRx_db_chunk <- dbConnect(RSQLite::SQLite(), "pitchRx_db_chunk.sqlite3")


#- - - - - - - - - - - - - - - - - - #
#### Putting into temp database ####
#- - - - - - - - - - - - - - - - - - #


i_inf <- icount()
res <- dbSendQuery(pitchRx_db, "SELECT * FROM atbat")

while (!dbHasCompleted(res)) {
    
    atbat_chunk <- dbFetch(res, n = 50000)
    
    atbat_chunk_2 <-
        atbat_chunk %>%
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
    
    
    cat("Chunk", nextElem(i_inf))
    cat("\n-------------------------------\n")
    cat("|== ")
    cat(min(atbat_chunk_2$game_date),
        "-",
        max(atbat_chunk_2$game_date))
    cat(" ==|")
    cat("\n-------------------------------\n")    
    
    
    dbWriteTable(
        pitchRx_db_chunk,
        "atbat_temp",
        value = atbat_chunk_2,
        append = TRUE,
        temporary = FALSE)
}


dbClearResult(res)

db_list_tables(pitchRx_db_chunk)

tbl(pitchRx_db_chunk, "atbat_temp") %>% select(game_date) %>% collect() %>% nrow()
tbl(pitchRx_db_chunk, "atbat_temp") %>% glimpse()

dbDisconnect(pitchRx_db)
dbDisconnect(pitchRx_db_chunk)


#- - - - - - - - - - - - - - - - - - #
#### Putting into real database ####
#- - - - - - - - - - - - - - - - - - #


pitchRx_db <- dbConnect(RSQLite::SQLite(), "pitchRx_db.sqlite3")
pitchRx_db_chunk <- dbConnect(RSQLite::SQLite(), "pitchRx_db_chunk.sqlite3")


#####


i_inf <- icount()
res <- dbSendQuery(pitchRx_db_chunk, "SELECT * FROM atbat_temp")

while (!dbHasCompleted(res)) {
    
    atbat_chunk <- dbFetch(res, n = 50000)
    
    cat("Chunk", nextElem(i_inf))
    cat("\n-------------------------------\n")
    cat("|== ")
    cat(min(atbat_chunk$game_date),
        "-",
        max(atbat_chunk$game_date))
    cat(" ==|")
    cat("\n-------------------------------\n")    
    
    dbWriteTable(
        pitchRx_db,
        "atbat_temp",
        value = atbat_chunk,
        append = TRUE,
        temporary = FALSE)
}

dbClearResult(res)

db_list_tables(pitchRx_db)

tbl(pitchRx_db_chunk, "atbat_temp") %>% select(game_date) %>% collect() %>% nrow()

tbl(pitchRx_db, "atbat_temp") %>% select(game_date) %>% collect() %>% nrow()
tbl(pitchRx_db, "atbat_temp") %>% glimpse()


#- - - - - - - - - - - - - - - - - - #
#### Dropping and renaming table ####
#- - - - - - - - - - - - - - - - - - #


# db_drop_table(pitchRx_db, "atbat")
# dbExecute(pitchRx_db, 'ALTER TABLE atbat_temp RENAME TO atbat')

dbDisconnect(pitchRx_db)
dbDisconnect(pitchRx_db_chunk)


#--------------------#
# ---- pitch ----
#--------------------#


pitchRx_db <- dbConnect(RSQLite::SQLite(), "pitchRx_db.sqlite3")
pitchRx_db_chunk <- dbConnect(RSQLite::SQLite(), "pitchRx_db_chunk.sqlite3")


#- - - - - - - - - - - - - - - - - - #
#### Putting into temp database ####
#- - - - - - - - - - - - - - - - - - #


i_inf <- icount()
res <- dbSendQuery(pitchRx_db, "SELECT * FROM pitch")

while (!dbHasCompleted(res)) {
    
    pitch_chunk <- dbFetch(res, n = 50000)
    
    pitch_chunk_2 <-
        pitch_chunk %>%
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
    
    cat("Chunk", nextElem(i_inf))
    cat("\n-------------------------------\n")
    cat("|== ")
    cat(min(pitch_chunk_2$game_date),
        "-",
        max(pitch_chunk_2$game_date))
    cat(" ==|")
    cat("\n-------------------------------\n")    
    
    
    dbWriteTable(
        pitchRx_db_chunk,
        "pitch_temp",
        value = pitch_chunk_2,
        append = TRUE,
        temporary = FALSE)
}


dbClearResult(res)

db_list_tables(pitchRx_db_chunk)

tbl(pitchRx_db_chunk, "pitch_temp") %>% select(game_date) %>% collect() %>% nrow()
tbl(pitchRx_db_chunk, "pitch_temp") %>% glimpse()

dbDisconnect(pitchRx_db)
dbDisconnect(pitchRx_db_chunk)


#- - - - - - - - - - - - - - - - - - #
#### Putting into real database ####
#- - - - - - - - - - - - - - - - - - #


pitchRx_db <- dbConnect(RSQLite::SQLite(), "pitchRx_db.sqlite3")
pitchRx_db_chunk <- dbConnect(RSQLite::SQLite(), "pitchRx_db_chunk.sqlite3")


#####


i_inf <- icount()
res <- dbSendQuery(pitchRx_db_chunk, "SELECT * FROM pitch_temp")

while (!dbHasCompleted(res)) {
    
    pitch_chunk <- dbFetch(res, n = 50000)
    
    cat("Chunk", nextElem(i_inf))
    cat("\n-------------------------------\n")
    cat("|== ")
    cat(min(pitch_chunk$game_date),
        "-",
        max(pitch_chunk$game_date))
    cat(" ==|")
    cat("\n-------------------------------\n")  
    
    dbWriteTable(
        pitchRx_db,
        "pitch_temp",
        value = pitch_chunk,
        append = TRUE,
        temporary = FALSE)
}

dbClearResult(res)

db_list_tables(pitchRx_db)

tbl(pitchRx_db_chunk, "pitch_temp") %>% select(game_date) %>% collect() %>% nrow()

tbl(pitchRx_db, "pitch_temp") %>% select(game_date) %>% collect() %>% nrow()
tbl(pitchRx_db, "pitch_temp") %>% glimpse()


#- - - - - - - - - - - - - - - - - - #
#### Dropping and renaming table ####
#- - - - - - - - - - - - - - - - - - #


db_drop_table(pitchRx_db, "pitch")
dbExecute(pitchRx_db, 'ALTER TABLE pitch_temp RENAME TO pitch')

dbDisconnect(pitchRx_db)
dbDisconnect(pitchRx_db_chunk)


#--------------------#
# ---- action ----
#--------------------#

pitchRx_db <- dbConnect(RSQLite::SQLite(), "pitchRx_db.sqlite3")
pitchRx_db_chunk <- dbConnect(RSQLite::SQLite(), "pitchRx_db_chunk.sqlite3")


#- - - - - - - - - - - - - - - - - - #
#### Putting into temp database ####
#- - - - - - - - - - - - - - - - - - #


i_inf <- icount()
res <- dbSendQuery(pitchRx_db, "SELECT * FROM action")

while (!dbHasCompleted(res)) {
    
    action_chunk <- dbFetch(res, n = 50000)
    
    action_chunk_2 <-
        action_chunk %>%
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
    
    cat("Chunk", nextElem(i_inf))
    cat("\n-------------------------------\n")
    cat("|== ")
    cat(min(action_chunk_2$game_date),
        "-",
        max(action_chunk_2$game_date))
    cat(" ==|")
    cat("\n-------------------------------\n")
    
    
    dbWriteTable(
        pitchRx_db_chunk,
        "action_temp",
        value = action_chunk_2,
        append = TRUE,
        temporary = FALSE)
}

dbClearResult(res)

db_list_tables(pitchRx_db_chunk)

tbl(pitchRx_db_chunk, "action_temp") %>% select(game_date) %>% collect() %>% nrow()
tbl(pitchRx_db_chunk, "action_temp") %>% glimpse()

dbDisconnect(pitchRx_db)
dbDisconnect(pitchRx_db_chunk)


#- - - - - - - - - - - - - - - - - - #
#### Putting into real database ####
#- - - - - - - - - - - - - - - - - - #


pitchRx_db <- dbConnect(RSQLite::SQLite(), "pitchRx_db.sqlite3")
pitchRx_db_chunk <- dbConnect(RSQLite::SQLite(), "pitchRx_db_chunk.sqlite3")


#####


i_inf <- icount()
res <- dbSendQuery(pitchRx_db_chunk, "SELECT * FROM action_temp")

while (!dbHasCompleted(res)) {
    
    action_chunk <- dbFetch(res, n = 50000)
    
    cat("Chunk", nextElem(i_inf))
    cat("\n-------------------------------\n")
    cat("|== ")
    cat(min(action_chunk$game_date),
        "-",
        max(action_chunk$game_date))
    cat(" ==|")
    cat("\n-------------------------------\n")
    
    dbWriteTable(
        pitchRx_db,
        "action_temp",
        value = action_chunk,
        append = TRUE,
        temporary = FALSE)
}

dbClearResult(res)

db_list_tables(pitchRx_db)

tbl(pitchRx_db_chunk, "action_temp") %>% select(game_date) %>% collect() %>% nrow()

tbl(pitchRx_db, "action_temp") %>% select(game_date) %>% collect() %>% nrow()
tbl(pitchRx_db, "action_temp") %>% glimpse()


#- - - - - - - - - - - - - - - - - - #
#### Dropping and renaming table ####
#- - - - - - - - - - - - - - - - - - #


# db_drop_table(pitchRx_db, "action")
# dbExecute(pitchRx_db, 'ALTER TABLE action_temp RENAME TO action')

dbDisconnect(pitchRx_db)
dbDisconnect(pitchRx_db_chunk)


#########################################
#########################################
#########################################
#########################################


# dbListFields(pitchRx_db, "action") %>% as.data.frame()
# dbListFields(pitchRx_db, "atbat")  %>% as.data.frame()
# dbListFields(pitchRx_db, "coach")  %>% as.data.frame()
# dbListFields(pitchRx_db, "hip")    %>% as.data.frame()
# dbListFields(pitchRx_db, "pitch")  %>% as.data.frame()
# dbListFields(pitchRx_db, "player") %>% as.data.frame()
# dbListFields(pitchRx_db, "po")     %>% as.data.frame()
# dbListFields(pitchRx_db, "runner") %>% as.data.frame()
# dbListFields(pitchRx_db, "umpire") %>% as.data.frame()



action_distinct <- tbl(pitchRx_db, "action") %>% distinct()
atbat_distinct  <- tbl(pitchRx_db, "atbat")  %>% distinct()
coach_distinct  <- tbl(pitchRx_db, "coach")  %>% distinct()
hip_distinct    <- tbl(pitchRx_db, "hip")    %>% distinct()
pitch_distinct  <- tbl(pitchRx_db, "pitch")  %>% distinct()
player_distinct <- tbl(pitchRx_db, "player") %>% distinct()
po_distinct     <- tbl(pitchRx_db, "po")     %>% distinct()
runner_distinct <- tbl(pitchRx_db, "runner") %>% distinct()
umpire_distinct <- tbl(pitchRx_db, "umpire") %>% distinct()



compute(action_distinct, name = "action_distinct", temporary = FALSE)
compute(atbat_distinct,  name = "atbat_distinct",  temporary = FALSE)
compute(coach_distinct,  name = "coach_distinct",  temporary = FALSE)
compute(hip_distinct,    name = "hip_distinct",    temporary = FALSE)
compute(pitch_distinct,  name = "pitch_distinct",  temporary = FALSE)
compute(player_distinct, name = "player_distinct", temporary = FALSE)
compute(po_distinct,     name = "po_distinct",     temporary = FALSE)
compute(runner_distinct, name = "runner_distinct", temporary = FALSE)
compute(umpire_distinct, name = "umpire_distinct", temporary = FALSE)


tbl(pitchRx_db, "action")          %>% collect() %>% nrow()
tbl(pitchRx_db, "action_distinct") %>% collect() %>% nrow()
tbl(pitchRx_db, "atbat")           %>% collect() %>% nrow()
tbl(pitchRx_db, "atbat_distinct")  %>% collect() %>% nrow()
tbl(pitchRx_db, "coach")           %>% collect() %>% nrow()
tbl(pitchRx_db, "coach_distinct")  %>% collect() %>% nrow()
tbl(pitchRx_db, "hip")             %>% collect() %>% nrow()
tbl(pitchRx_db, "hip_distinct")    %>% collect() %>% nrow()
tbl(pitchRx_db, "pitch")           %>% collect() %>% nrow()
tbl(pitchRx_db, "pitch_distinct")  %>% collect() %>% nrow()
tbl(pitchRx_db, "player")          %>% collect() %>% nrow()
tbl(pitchRx_db, "player_distinct") %>% collect() %>% nrow()
tbl(pitchRx_db, "po")              %>% collect() %>% nrow()
tbl(pitchRx_db, "po_distinct")     %>% collect() %>% nrow()
tbl(pitchRx_db, "runner")          %>% collect() %>% nrow()
tbl(pitchRx_db, "runner_distinct") %>% collect() %>% nrow()
tbl(pitchRx_db, "umpire")          %>% collect() %>% nrow()
tbl(pitchRx_db, "umpire_distinct") %>% collect() %>% nrow()

# 
# db_drop_table(pitchRx_db, "action")
# db_drop_table(pitchRx_db, "atbat")
# db_drop_table(pitchRx_db, "coach")
# db_drop_table(pitchRx_db, "hip")
# db_drop_table(pitchRx_db, "pitch")
# db_drop_table(pitchRx_db, "player")
# db_drop_table(pitchRx_db, "po")
# db_drop_table(pitchRx_db, "runner")
# db_drop_table(pitchRx_db, "umpire")

db_list_tables(pitchRx_db)


dbExecute(pitchRx_db, 'ALTER TABLE action_distinct RENAME TO action')
dbExecute(pitchRx_db, 'ALTER TABLE atbat_distinct RENAME TO atbat')
dbExecute(pitchRx_db, 'ALTER TABLE coach_distinct RENAME TO coach')
dbExecute(pitchRx_db, 'ALTER TABLE hip_distinct RENAME TO hip')
dbExecute(pitchRx_db, 'ALTER TABLE pitch_distinct RENAME TO pitch')
dbExecute(pitchRx_db, 'ALTER TABLE player_distinct RENAME TO player')
dbExecute(pitchRx_db, 'ALTER TABLE po_distinct RENAME TO po')
dbExecute(pitchRx_db, 'ALTER TABLE runner_distinct RENAME TO runner')
dbExecute(pitchRx_db, 'ALTER TABLE umpire_distinct RENAME TO umpire')

db_list_tables(pitchRx_db)


tbl(pitchRx_db, "sqlite_stat1")
tbl(pitchRx_db, "sqlite_stat4")



#-----------------------------------------------#
# ---- Creating new columns in scraped data ----
#-----------------------------------------------#

pitchRx_db <- dbConnect(RSQLite::SQLite(), "pitchRx_db.sqlite3")

# ---- atbat ----

dbExecute(pitchRx_db, "ALTER TABLE atbat ADD at_bat_number")
dbExecute(pitchRx_db, "ALTER TABLE atbat ADD game_date")
dbExecute(pitchRx_db, "ALTER TABLE atbat ADD at_bat_event")
dbExecute(pitchRx_db, "ALTER TABLE atbat ADD game_year")

dbExecute(pitchRx_db, "UPDATE atbat SET at_bat_number = num")
dbExecute(pitchRx_db, "UPDATE atbat SET at_bat_event = event")
# dbExecute(pitchRx_db, "UPDATE atbat SET game_date = replace(substr(gameday_link, 5, 10), '_', '-')")
dbExecute(pitchRx_db, "UPDATE atbat SET game_year = substr(game_date, 1, 4)")


# ---- pitch ----

dbExecute(pitchRx_db, "ALTER TABLE pitch ADD pitch_number")
dbExecute(pitchRx_db, "ALTER TABLE pitch ADD at_bat_number")
dbExecute(pitchRx_db, "ALTER TABLE pitch ADD pitch_event")
dbExecute(pitchRx_db, "ALTER TABLE pitch ADD game_date")
dbExecute(pitchRx_db, "ALTER TABLE pitch ADD game_year")

dbExecute(pitchRx_db, "UPDATE pitch SET pitch_number = id")
dbExecute(pitchRx_db, "UPDATE pitch SET at_bat_number = num")
dbExecute(pitchRx_db, "UPDATE pitch SET pitch_event = des")
# dbExecute(pitchRx_db, "UPDATE pitch SET game_date = replace(substr(gameday_link, 5, 10), '_', '-')")
dbExecute(pitchRx_db, "UPDATE pitch SET game_year = substr(game_date, 1, 4)")


# ---- action ----

dbExecute(pitchRx_db, "ALTER TABLE action ADD at_bat_number")
dbExecute(pitchRx_db, "ALTER TABLE action ADD pitch_number")
dbExecute(pitchRx_db, "ALTER TABLE action ADD action_event")
dbExecute(pitchRx_db, "ALTER TABLE action ADD game_date")
dbExecute(pitchRx_db, "ALTER TABLE action ADD game_year")

dbExecute(pitchRx_db, "UPDATE action SET at_bat_number = num")
dbExecute(pitchRx_db, "UPDATE action SET pitch_number = pitch")
dbExecute(pitchRx_db, "UPDATE action SET action_event = event")
# dbExecute(pitchRx_db, "UPDATE action SET game_date = replace(substr(gameday_link, 5, 10), '_', '-')")
dbExecute(pitchRx_db, "UPDATE action SET game_year = substr(game_date, 1, 4)")


# ---- player ----

dbExecute(pitchRx_db, "ALTER TABLE player ADD at_bat_number")
dbExecute(pitchRx_db, "ALTER TABLE player ADD game_date")
dbExecute(pitchRx_db, "ALTER TABLE player ADD game_year")

dbExecute(pitchRx_db, "UPDATE player SET at_bat_number = num")
# dbExecute(pitchRx_db, "UPDATE player SET game_date = replace(substr(gameday_link, 5, 10), '_', '-')")
dbExecute(pitchRx_db, "UPDATE player SET game_year = substr(game_date, 1, 4)")


# ---- game ----

dbExecute(pitchRx_db, "ALTER TABLE game ADD game_date")
dbExecute(pitchRx_db, "ALTER TABLE game ADD game_year")

dbExecute(pitchRx_db, "UPDATE game SET gameday_link = substr('gid_' || gameday_link, 1, 40)")
dbExecute(pitchRx_db, "UPDATE game SET game_date = replace(substr(gameday_link, 1, 10), '_', '-')")
dbExecute(pitchRx_db, "UPDATE game SET game_year = substr(game_date, 1, 4)")


# ---- hip ----

dbExecute(pitchRx_db, "ALTER TABLE hip ADD hip_event")
dbExecute(pitchRx_db, "ALTER TABLE hip ADD game_date")
dbExecute(pitchRx_db, "ALTER TABLE hip ADD game_year")

dbExecute(pitchRx_db, "UPDATE hip SET hip_event = des")
# dbExecute(pitchRx_db, "UPDATE hip SET game_date = replace(substr(gameday_link, 5, 10), '_', '-')")
dbExecute(pitchRx_db, "UPDATE hip SET game_year = substr(game_date, 1, 4)")


# ---- runner ----

dbExecute(pitchRx_db, "ALTER TABLE runner ADD at_bat_number")
dbExecute(pitchRx_db, "ALTER TABLE runner ADD runner_event")
dbExecute(pitchRx_db, "ALTER TABLE runner ADD game_date")
dbExecute(pitchRx_db, "ALTER TABLE runner ADD game_year")

dbExecute(pitchRx_db, "UPDATE runner SET at_bat_number = num")
dbExecute(pitchRx_db, "UPDATE runner SET runner_event = event")
# dbExecute(pitchRx_db, "UPDATE runner SET game_date = replace(substr(gameday_link, 5, 10), '_', '-')")
dbExecute(pitchRx_db, "UPDATE runner SET game_year = substr(game_date, 1, 4)")


# ---- po ----

dbExecute(pitchRx_db, "ALTER TABLE po ADD at_bat_number")
dbExecute(pitchRx_db, "ALTER TABLE po ADD po_event")
dbExecute(pitchRx_db, "ALTER TABLE po ADD game_date")
dbExecute(pitchRx_db, "ALTER TABLE po ADD game_year")

dbExecute(pitchRx_db, "UPDATE po SET at_bat_number = num")
dbExecute(pitchRx_db, "UPDATE po SET po_event = des")
# dbExecute(pitchRx_db, "UPDATE po SET game_date = replace(substr(gameday_link, 5, 10), '_', '-')")
dbExecute(pitchRx_db, "UPDATE po SET game_year = substr(game_date, 1, 4)")


# ---- coach ----

dbExecute(pitchRx_db, "ALTER TABLE coach ADD game_date")
dbExecute(pitchRx_db, "ALTER TABLE coach ADD game_year")

dbExecute(pitchRx_db, "UPDATE coach SET game_date = replace(substr(gameday_link, 5, 10), '_', '-')")
dbExecute(pitchRx_db, "UPDATE coach SET game_year = substr(game_date, 1, 4)")


# ---- umpire ----

dbExecute(pitchRx_db, "ALTER TABLE umpire ADD game_date")
dbExecute(pitchRx_db, "ALTER TABLE umpire ADD game_year")

dbExecute(pitchRx_db, "UPDATE umpire SET game_date = replace(substr(gameday_link, 5, 10), '_', '-')")
dbExecute(pitchRx_db, "UPDATE umpire SET game_year = substr(game_date, 1, 4)")


#########################################

# tbl(pitchRx_db, "atbat") %>% glimpse()

db_create_index(pitchRx_db, "atbat", "gameday_link")
db_create_index(pitchRx_db, "atbat", "at_bat_number")
db_create_index(pitchRx_db, "atbat", "pitcher")
db_create_index(pitchRx_db, "atbat", "batter")
db_create_index(pitchRx_db, "atbat", "game_date")
db_create_index(pitchRx_db, "atbat", "game_year")
db_create_index(pitchRx_db, "atbat", "play_guid")
db_create_index(pitchRx_db, "atbat", "at_bat_event")
db_create_index(pitchRx_db, "atbat", "utc_date_round_chr")
db_create_index(pitchRx_db, "atbat", "obs_date_utc_chr")


# tbl(pitchRx_db, "pitch") %>% glimpse()

db_create_index(pitchRx_db, "pitch", "gameday_link")
db_create_index(pitchRx_db, "pitch", "game_date")
db_create_index(pitchRx_db, "pitch", "game_year")
db_create_index(pitchRx_db, "pitch", "at_bat_number")
db_create_index(pitchRx_db, "pitch", "pitch_number")
db_create_index(pitchRx_db, "pitch", "pitch_event")
db_create_index(pitchRx_db, "pitch", "play_guid")
db_create_index(pitchRx_db, "pitch", "utc_date_round_chr")
db_create_index(pitchRx_db, "pitch", "obs_date_utc_chr")

# tbl(pitchRx_db, "action") %>% glimpse()

db_create_index(pitchRx_db, "action", "play_guid")
db_create_index(pitchRx_db, "action", "gameday_link")
db_create_index(pitchRx_db, "action", "game_date")
db_create_index(pitchRx_db, "action", "game_year")
db_create_index(pitchRx_db, "action", "at_bat_number")
db_create_index(pitchRx_db, "action", "pitch_number")
db_create_index(pitchRx_db, "action", "action_event")


# tbl(pitchRx_db, "player") %>% glimpse()

db_create_index(pitchRx_db, "player", "id")
db_create_index(pitchRx_db, "player", "gameday_link")
db_create_index(pitchRx_db, "player", "game_date")
db_create_index(pitchRx_db, "player", "game_year")
db_create_index(pitchRx_db, "player", "team_abbrev")
db_create_index(pitchRx_db, "player", "at_bat_number")


# tbl(pitchRx_db, "game") %>% glimpse()

db_create_index(pitchRx_db, "game", "gameday_link")
db_create_index(pitchRx_db, "game", "game_date")
db_create_index(pitchRx_db, "game", "game_year")
# db_create_index(pitchRx_db, "game", "home_name_abbrev")
db_create_index(pitchRx_db, "game", "game_pk")
db_create_index(pitchRx_db, "game", "home_time_zone")
db_create_index(pitchRx_db, "game", "game_type")

db_create_index(pitchRx_db, "game", "game_pk")
db_create_index(pitchRx_db, "game", "game_pk")
db_create_index(pitchRx_db, "game", "game_pk")
db_create_index(pitchRx_db, "game", "game_pk")


# tbl(pitchRx_db, "hip") %>% glimpse()

db_create_index(pitchRx_db, "hip", "gameday_link")
db_create_index(pitchRx_db, "hip", "game_date")
db_create_index(pitchRx_db, "hip", "game_year")
db_create_index(pitchRx_db, "hip", "hip_event")
db_create_index(pitchRx_db, "hip", "batter")
db_create_index(pitchRx_db, "hip", "pitcher")


# tbl(pitchRx_db, "runner") %>% glimpse()

db_create_index(pitchRx_db, "runner", "gameday_link")
db_create_index(pitchRx_db, "runner", "game_date")
db_create_index(pitchRx_db, "runner", "game_year")
db_create_index(pitchRx_db, "runner", "runner_event")
db_create_index(pitchRx_db, "runner", "id")
db_create_index(pitchRx_db, "runner", "at_bat_number")


# tbl(pitchRx_db, "po") %>% glimpse()

db_create_index(pitchRx_db, "po", "gameday_link")
db_create_index(pitchRx_db, "po", "game_date")
db_create_index(pitchRx_db, "po", "game_year")
db_create_index(pitchRx_db, "po", "po_event")
db_create_index(pitchRx_db, "po", "at_bat_number")


# tbl(pitchRx_db, "coach") %>% glimpse()

db_create_index(pitchRx_db, "coach", "gameday_link")
db_create_index(pitchRx_db, "coach", "game_date")
db_create_index(pitchRx_db, "coach", "game_year")
db_create_index(pitchRx_db, "coach", "name_abbrev")
db_create_index(pitchRx_db, "coach", "id")
db_create_index(pitchRx_db, "coach", "position")


# tbl(pitchRx_db, "umpire") %>% glimpse()

db_create_index(pitchRx_db, "umpire", "gameday_link")
db_create_index(pitchRx_db, "umpire", "game_date")
db_create_index(pitchRx_db, "umpire", "game_year")
db_create_index(pitchRx_db, "umpire", "id")
db_create_index(pitchRx_db, "umpire", "position")

#########################################


# dbExecute(pitchRx_db, "DROP INDEX po_des_idx")

# dbExecute(pitchRx_db, "DROP INDEX action_idx")
# dbExecute(pitchRx_db, "DROP INDEX play_idx")
# dbExecute(pitchRx_db, "DROP INDEX atbat_idx")
# dbExecute(pitchRx_db, "DROP INDEX pitcher_id_idx")
# dbExecute(pitchRx_db, "DROP INDEX batter_id_idx")
# dbExecute(pitchRx_db, "DROP INDEX date_idx")
# dbExecute(pitchRx_db, "DROP INDEX play_idx")
# dbExecute(pitchRx_db, "DROP INDEX coach_idx")
# dbExecute(pitchRx_db, "DROP INDEX coach_id_idx")
# dbExecute(pitchRx_db, "DROP INDEX position_idx")
# dbExecute(pitchRx_db, "DROP INDEX team_idx")
# dbExecute(pitchRx_db, "DROP INDEX hip_idx")
# dbExecute(pitchRx_db, "DROP INDEX pitcher_id_idx")
# dbExecute(pitchRx_db, "DROP INDEX batter_id_idx")
# dbExecute(pitchRx_db, "DROP INDEX des_idx")
# dbExecute(pitchRx_db, "DROP INDEX pitch_idx")
# dbExecute(pitchRx_db, "DROP INDEX play_idx")
# dbExecute(pitchRx_db, "DROP INDEX player_idx")
# dbExecute(pitchRx_db, "DROP INDEX player_id_idx")
# dbExecute(pitchRx_db, "DROP INDEX team_idx")
# dbExecute(pitchRx_db, "DROP INDEX po_idx")
# dbExecute(pitchRx_db, "DROP INDEX runner_idx")
# dbExecute(pitchRx_db, "DROP INDEX runner_id_idx")
# dbExecute(pitchRx_db, "DROP INDEX umpire_idx")
# dbExecute(pitchRx_db, "DROP INDEX ump_id_idx")
# dbExecute(pitchRx_db, "DROP INDEX action_gameday_link_num")
# dbExecute(pitchRx_db, "DROP INDEX atbat_gameday_link_num")
# dbExecute(pitchRx_db, "DROP INDEX coach_gameday_link_num")


tbl(pitchRx_db, "sqlite_stat1") %>% explain()
tbl(pitchRx_db, "sqlite_stat4") %>% explain()

dbGetQuery(pitchRx_db, "SELECT * FROM sqlite_master WHERE type = 'index'")


dbDisconnect(pitchRx_db)

.rs.restartR()

#########################################



# game_urls <- makeUrls("2008-02-26", today())
# game_urls <- makeUrls(today()-7, today())

gameday_link <- str_trunc(game_urls, 30, "left", "")

game_dates <- gid2date(gameday_link)

all_gids <- data_frame(game_dates, gameday_link, game_urls)

write_rds(game_gids, "all_gids.RDS")
write_rds(all_game_xml, "played_game_gids.RDS")

###################

all_game_xml <- data.frame()
errors <- data.frame()

for (i in 1:nrow(all_gids)) {
    
    cat("Loop", i, "\n")
    
    tryCatch(
        all_game_xml <- 
            
            get_game.xml(
                gid_df = all_gids, 
                i = i) %>% 
            bind_rows(all_game_xml),
        
        error = function(e) {
            errors <<- data.frame(
                errors = paste0(
                    "ERROR gid: ", 
                    all_gids$game_gids[i],
                    " [row: ", i, "]")
            ) %>% bind_rows(errors)}, 
        finally = next
    )
    
}

write_rds(all_game_xml, "all_game_xml_1.RDS")

#####################

all_game_xml2 <- 
    all_game_xml %>% 
    rename(
        gameday_link = game_gids,
        game_date = game_dates,
        url = game_urls,
        game_type = type,
        home_team = abbrev_home,
        away_team = abbrev_away,
        ballpark = name,
        game_type = type
        ) %>% 
    mutate(
        game_date = as.character(game_date),
        game_year = game_date %>% year() %>% as.integer(),
        game_pk = as.integer(game_pk),
        id_home = as.integer(id_home),
        w_home = as.integer(w_home),
        l_home = as.integer(l_home),
        division_id_home = as.integer(division_id_home),
        league_id_home = as.integer(league_id_home),
        division_id_away = as.integer(division_id_away),
        league_id_away = as.integer(league_id_away),
        id_away = as.integer(id_away),
        w_away = as.integer(w_away),
        l_away = as.integer(l_away),
        id = as.integer(id)
        )

write_rds(all_game_xml2, "all_game_xml_1.RDS")

#####################


all_game_xml2 %>% count(abbrev_home) %>% print(n = Inf)

db_list_tables(pitchRx_db)

dbWriteTable(pitchRx_db, "game", all_game_xml2, overwrite = TRUE)


db_create_index(pitchRx_db, "game", "game_date")
db_create_index(pitchRx_db, "game", "gameday_link")
db_create_index(pitchRx_db, "game", "game_pk")
db_create_index(pitchRx_db, "game", "game_type")
db_create_index(pitchRx_db, "game", "game_year")

dbGetQuery(pitchRx_db, "SELECT * FROM sqlite_master WHERE type = 'index'")


#####################


pitchRx_db <- dbConnect(RSQLite::SQLite(), "pitchRx_db.sqlite3")

# dbGetQuery(statcast_db, "SELECT * FROM sqlite_master WHERE type = 'index'")

most_recent_day <-
    tbl(pitchRx_db, "game") %>%
    select(game_date) %>%
    arrange(desc(game_date)) %>%
    head(1) %>% 
    collect() %>% 
    extract2(1)


url <- makeUrls(most_recent_day, today())
gameday_link <- str_trunc(url, 30, "left", "")
game_date <- gid2date(gameday_link)
all_gids <- data_frame(game_date, gameday_link, url)


most_recent_data <-
    tbl(pitchRx_db, "game") %>%
    filter(game_date == most_recent_day) %>%
    select(gameday_link) %>% 
    collect()


gids_distinct <-
    anti_join(
        all_gids,
        most_recent_data,
        by = c("gameday_link"))


if (nrow(gids_distinct) == 0) {
    message("No games to update; stopping here by calling `.rs.restartR()`.")
    .rs.restartR()
}


###################


all_game_xml <- data.frame()
errors <- data.frame()

for (i in 1:nrow(gids_distinct)) {
    
    cat("Loop", i, "\n")
    
    tryCatch(
        all_game_xml <- 
            
            get_game.xml(
                gid_df = gids_distinct, 
                i = i) %>% 
            bind_rows(all_game_xml),
        
        error = function(e) {
            errors <<- data.frame(
                errors = paste0(
                    "ERROR gid: ", 
                    gids_distinct$game_gids[i],
                    " [row: ", i, "]")
            ) %>% bind_rows(errors)}, 
        finally = next
    )
    
}


###################

game_year = as.numeric(substr(game_date, 1, 4))


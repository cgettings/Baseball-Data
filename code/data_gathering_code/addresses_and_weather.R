###########################################
###########################################
##
## Addresses
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
library(pryr)
library(pitchRx)
library(rvest)
library(iterators)

setwd("D:/Files/BASP/R analyses/Weather")

weather_db <- dbConnect(RSQLite::SQLite(), "weather_db.sqlite3")


#=====================================#
#### Checking database ####
#=====================================#

db_list_tables(weather_db)

#=====================================#
#### Loading stadium address data ####
#=====================================#

# addresses <-
#     read_csv("addresses.csv") %>%
#     mutate(
#         lat_trunc = trunc(lat),
#         lng_trunc = trunc(lng),
#         lat_round = round(lat),
#         lng_round = round(lng)
#     )
# 
# dbWriteTable(
#     weather_db,
#     "addresses",
#     value = addresses,
#     overwrite = TRUE,
#     # append = TRUE,
#     temporary = FALSE
# )


#---------------------------------#
#---- Creating indexes ----
#---------------------------------#

db_create_index(weather_db, "addresses", "home_team")
db_create_index(weather_db, "addresses", "game_type")
db_create_index(weather_db, "addresses", "ballpark")

#---------------------------------#
#---- Reading in data ----
#---------------------------------#


addresses <- tbl(weather_db, "addresses") %>% collect()


#========================================#
#### Downloaded weather files ####
#========================================#


weather_files <- list.files("Hourly Files/")

weather_files.2 <-
    weather_files[str_detect(weather_files, "QCLCD")] %>% 
    data_frame(weather_files = .)

weather_files.3 <-
    lapply(
        weather_files.2 %>% extract2(1), 
        unzip, 
        list = TRUE) %>%
    bind_rows() %>%
    as_data_frame()


#========================================#
#### Adding stations to addresses ####
#========================================#


station_names <-
    weather_files.3 %>% 
    filter(str_detect(Name, "station")) %>% 
    select(station_names = Name)


###---###---###---###---###---###---###---###


for (i in 1:nrow(station_names)) {
    
    
    ### Unzipping station files ###
    
    unzip(
        zipfile = weather_files.2$weather_files[i],
        files = c(station_names$station_names[i]),
        exdir = "Hourly Files"
    )
    
    
    ## cleaning the data ##
    
    station_unzip <- 
        
        read_delim(
            paste0(
                "Hourly Files/", 
                station_names$station_names[i]), 
            delim = "|", col_types = cols(WBAN = col_integer())) %>%
        `attr<-`("problems", NULL) %>% 
    
        transmute(
            obs_year = station_names$station_names[i] %>% 
                str_sub(1, 4) %>% 
                as.integer(), 
            obs_month = station_names$station_names[i] %>% 
                str_sub(5, 6) %>% 
                as.integer(), 
            wban = as.integer(WBAN),
            wmo = WMO,
            call_sign = CallSign,
            name = Name,
            state = State,
            location = Location,
            altitude = StationHeight,
            lat = Latitude,
            lng = Longitude,
            time_zone = TimeZone
        ) %>%
        
        mutate(
            lat_trunc = trunc(lat),
            lng_trunc = trunc(lng),
            lat_round = round(lat),
            lng_round = round(lng)
        )

    
    ### Matching stadiums to stations ###
    
    ## truncating lat, long ##
    
    address_station_trunc <-
        
        left_join(
            addresses,
            station_unzip,
            by = c("lat_trunc", "lng_trunc")) %>%
        filter(obs_year >= start_date & obs_year <= end_date) %>%
        mutate(
            distance = geosphere::distGeo(
                p1 = as.matrix(data.frame(lng.x, lat.x)), 
                p2 = as.matrix(data.frame(lng.y, lat.y)))) %>%
        filter(!is.na(distance)) %>%
        group_by(ballpark) %>%
        arrange(ballpark, distance) %>% 
        ungroup()
    
    
    ## rounding lat, long ##
    
    address_station_round <-
        
        left_join(
            addresses,
            station_unzip,
            by = c("lat_round", "lng_round")) %>%
        filter(obs_year >= start_date & obs_year <= end_date) %>%
        mutate(
            distance = geosphere::distGeo(
                p1 = as.matrix(data.frame(lng.x, lat.x)), 
                p2 = as.matrix(data.frame(lng.y, lat.y)))) %>%
        filter(!is.na(distance)) %>%
        group_by(ballpark) %>%
        arrange(ballpark, distance) %>% 
        ungroup()
    
    
    ## binding two methods together ##
    
    address_station <-
        
        bind_rows(
            trunc = address_station_trunc,
            round = address_station_round,
            .id = "id") %>%
        filter(!is.na(distance)) %>%
        group_by(ballpark) %>%
        arrange(game_type, obs_year, obs_month, ballpark, distance) %>%
        distinct(game_type, obs_year, obs_month, ballpark, distance, .keep_all = TRUE) %>% 
        ungroup()
    
    
    ## writing to database ##
    
    dbWriteTable(
        weather_db,
        "address_station",
        value = address_station,
        append = TRUE,
        temporary = FALSE
    )
    
    
    cat("===============================\n")
    cat("Loop", i, "\n")
    cat(nrow(address_station), "rows added")
    cat("\n-------------------------------\n")
    cat("|-- ")
    cat(address_station$obs_year[i], "-", address_station$obs_month[i], sep = "")
    cat(" --|")
    cat("\n-------------------------------\n")  
    
    # write_csv(
    #     address_station,
    #     paste(
    #         "Hourly Files/address_station_",
    #         station_names$station_names[i],
    #         ".csv",
    #         sep = ""
    #     )
    # )
    
    
    ## removing the unzipped files ##
    
    file.remove(paste("Hourly Files/", station_names$station_names[i], sep = ""))
    
    gc()
    
}


#---------------------------------#
#---- Creating indexes ----
#---------------------------------#

db_create_index(weather_db, "address_station", "obs_year")
db_create_index(weather_db, "address_station", "obs_month")
db_create_index(weather_db, "address_station", "ballpark")
db_create_index(weather_db, "address_station", "home_team")
db_create_index(weather_db, "address_station", "game_type")
db_create_index(weather_db, "address_station", "wban")

#========================================#
#### Unzipping hourly weather files ####
#========================================#

weather_files <- list.files()

weather_files.2 <-
    weather_files[str_detect(weather_files, "QCLCD")] %>% 
    data_frame(weather_files = .)

weather_files.3 <-
    lapply(
        weather_files.2 %>% extract2(1), 
        unzip, 
        list = TRUE) %>%
    bind_rows() %>%
    as_data_frame()

#---------------------------------#
#---- Initializing databases ----
#---------------------------------#

weather_db <- dbConnect(RSQLite::SQLite(), "weather_db.sqlite3")

pitchRx_db <-
    dbConnect(
        RSQLite::SQLite(),
        "~/BASP/R analyses/Baseball Data/Data Files/pitchRx_db.sqlite3")

#---------------------------------#
#---- Loading addresses ----
#---------------------------------#


address_station <- 
    tbl(weather_db, "address_station") %>% 
    select(
        -id,
        -lat_trunc,
        -lng_trunc,
        -lat_round,
        -lng_round,
        -lat_round.x,
        -lng_round.x,
        -lat_round.y, 
        -lng_round.y,
        -lat_trunc.x,
        -lng_trunc.x,
        -lat_trunc.y,
        -lng_trunc.y
        ) %>% 
    collect()


#---------------------------------#
#---- Assigning stadiums ----
#---------------------------------#


games <-
    tbl(pitchRx_db, "game") %>%
    select(
        game_date,
        game_type,
        gameday_link,
        game_pk,
        venue,
        home_team = home_name_abbrev) %>% 
    collect()  


#---------------------------------#
#---- Hourly names ----
#---------------------------------#

### Finding hourly file names in names extracted from zip files ###


hourly_names_df <-
    weather_files.3 %>%
    filter(str_detect(Name, "hourly")) %>%
    rename(hourly_names = Name) %>% 
    select(hourly_names)


#---------------------------------#
#---- Chunking ----
#---------------------------------#


for (i in 1:nrow(hourly_names_df)) {
# for (i in 21:20) {
    
    
    ## cleaning the data ##
    
    tryCatch({
        
    address_station_temp <-
        address_station %>%
        
        filter(
            obs_year == hourly_names_df$hourly_names[i] %>%
                str_sub(1, 4) %>%
                as.integer(),
            obs_month == hourly_names_df$hourly_names[i] %>%
                str_sub(5, 6) %>%
                as.integer()
        )

    
    ## unzipping the files ##
    
    
    unzip(
        zipfile = weather_files.2$weather_files[i],
        files = c(hourly_names_df$hourly_names[i]),
        exdir = "Hourly Files"
    )
    
    
    ### Hourly weather files ###
    
    
    hourly_unzip_0 <-

        read_csv(
            paste0(
                "Hourly Files/", 
                hourly_names_df$hourly_names[i]), 
            col_types = cols(WBAN = col_integer())) %>%
        `attr<-`("problems", NULL)
        
    
    ### Extracting variables ###
    
    
    hourly_unzip_1 <- 
        hourly_unzip_0 %>% 
        
        transmute(
            wban = as.integer(WBAN),
            # wmo = as.character(WMO),
            obs_year = hourly_names_df$hourly_names[i] %>%
                str_sub(1, 4) %>%
                as.integer(),
            obs_month = hourly_names_df$hourly_names[i] %>%
                str_sub(5, 6) %>%
                as.integer(),
            obs_date = ymd(as.character(Date)),
            obs_date_chr = as.character(obs_date),
            obs_time_chr = as.character(Time),
            wet_temp = as.character(WetBulbCelsius),
            dry_temp = as.character(DryBulbCelsius),
            humidity = as.character(RelativeHumidity),
            wind_speed = as.character(WindSpeed),
            wind_direction = as.character(WindDirection),
            pressure = as.character(StationPressure)
        ) %>%
        
        left_join(
            address_station_temp, .,
            by = c(
                "wban",
                "obs_year",
                "obs_month")) %>% 
        
        # filter(!is.na(obs_date)) %>%

        mutate(
            obs_date_time_loc =
                make_datetime(
                    year = substr(obs_date_chr, 1, 4),
                    month = substr(obs_date_chr, 6, 7),
                    day = substr(obs_date_chr, 9, 10),
                    hour = substr(obs_time_chr, 1, 2),
                    min = substr(obs_time_chr, 3, 4),
                    sec = 0,
                    tz = "UTC"
                )
        ) %>%
        
        mutate(obs_date_time_utc = obs_date_time_loc - hours(as.numeric(time_zone))) %>% 
        
        mutate(
            obs_date_time_loc_chr = as.character(obs_date_time_loc),
            obs_date_time_utc_chr = as.character(obs_date_time_utc)
            ) %>% 
        
        mutate(
            obs_date_time_loc_round = round_date(obs_date_time_loc, "hour"),
            obs_date_time_utc_round = round_date(obs_date_time_utc, "hour")
            ) %>% 
        
        mutate(
            obs_date_time_loc_round_chr = as.character(obs_date_time_loc_round),
            obs_date_time_utc_round_chr = as.character(obs_date_time_utc_round)
            ) %>% 
        
        mutate(
            obs_date_loc = substr(obs_date_time_loc_chr, 1, 10) %>% ymd(),
            obs_date_utc = substr(obs_date_time_utc_chr, 1, 10) %>% ymd()
            ) %>% 
        
        mutate(
            obs_date_loc_chr = as.character(obs_date_loc),
            obs_date_utc_chr = as.character(obs_date_utc)
            ) %>% 
        
        inner_join(
            ., 
            games, 
            by = c(
                "game_type",
                "obs_date_loc_chr" = "game_date", 
                "home_team")) %>% 
        
        add_column(row_n = 1:nrow(.)) %>%
        group_by(row_n) %>%
        mutate(na_vars = sum(
            !is.na(dry_temp),
            !is.na(humidity),
            !is.na(wind_speed),
            !is.na(pressure)
        )) %>% 
        
        group_by(ballpark, obs_date_time_utc_round_chr) %>%
        arrange(-na_vars, -distance) %>%
        slice(1) %>% 
        ungroup()
    
    rm(hourly_unzip_0)
    gc()
    
    
    cat("===============================\n")
    cat("Loop", i)
    cat("\n-------------------------------\n")
    cat("|-- ")
    cat(
        min(hourly_unzip_1$obs_date_chr, na.rm = TRUE), 
        " - ", 
        max(hourly_unzip_1$obs_date_chr, na.rm = TRUE), 
        sep = "")
    cat(" --|")
    cat("\n-------------------------------\n")
    cat(nrow(hourly_unzip_1), "rows added\n")
    object_size(hourly_unzip_1) %>% print()
    cat("-------------------------------\n")

    
    dbWriteTable(
        weather_db,
        "hourly_with_address",
        value = hourly_unzip_1,
        append = TRUE,
        temporary = FALSE
    )
    
    rm(hourly_unzip_1)
    gc()
    
    
    # write_csv(
    #     hourly_unzip,
    #     paste(
    #         "Hourly Files/hourly_unzip_",
    #         hourly_names$hourly_names[i],
    #         ".csv",
    #         sep = ""
    #     )
    # )

    
    file.remove(
        paste0(
            "Hourly Files/",
            str_subset(
                weather_files.3$Name, 
                hourly_names_df$hourly_names[i])
        ))
    
    gc()
    
    },
    
    finally = next
    
    )
    
}

dbDisconnect(weather_db)

#=========================#
#### Creating indexes ####
#=========================#


weather_db <- dbConnect(RSQLite::SQLite(), "weather_db.sqlite3")

db_list_tables(weather_db)

tbl(weather_db, "hourly_with_address") %>% glimpse()


################################

# db_drop_table(weather_db, "address_station")
# db_drop_table(weather_db, "address_station_2")
# db_drop_table(weather_db, "address_station_3")
# 
# db_drop_table(weather_db, "hourly_with_address")
# db_drop_table(weather_db, "hourly_with_address_3")
# db_drop_table(weather_db, "hourly_with_address_4")

dbExecute(weather_db, "ALTER TABLE hourly_with_address_5 RENAME TO hourly_with_address")
dbExecute(weather_db, "ALTER TABLE address_station_2 RENAME TO address_station")

################################


db_create_index(weather_db, "hourly_with_address", "home_team")
db_create_index(weather_db, "hourly_with_address", "ballpark")
db_create_index(weather_db, "hourly_with_address", "game_type")
db_create_index(weather_db, "hourly_with_address", "wban")
db_create_index(weather_db, "hourly_with_address", "obs_year")
db_create_index(weather_db, "hourly_with_address", "obs_month")
db_create_index(weather_db, "hourly_with_address", "obs_date_loc_chr")
db_create_index(weather_db, "hourly_with_address", "obs_date_utc_chr")
db_create_index(weather_db, "hourly_with_address", "obs_date_time_utc_round_chr")

dbGetQuery(weather_db, "SELECT * FROM sqlite_master WHERE type = 'index'")

# dbDisconnect(weather_db)


#############################################
#############################################

weather_db <- dbConnect(RSQLite::SQLite(), "weather_db.sqlite3")

tbl(weather_db, "hourly_with_address") %>% pull(wban) %>% length()


#############################################
#############################################
#############################################
#############################################



#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
#
#### Matching ####
#
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#


#=========================#
#### atbat ####
#=========================#

#--------------------------------#
#---- Initializing databases ----
#--------------------------------#

pitchRx_db <-
    dbConnect(
        RSQLite::SQLite(),
        "~/BASP/R analyses/Baseball Data/Data Files/pitchRx_db.sqlite3")

statcast_db <-
    dbConnect(
        RSQLite::SQLite(),
        "~/BASP/R analyses/Baseball Data/Data Files/statcast_db.sqlite3")

weather_db <- 
    dbConnect(RSQLite::SQLite(), "weather_db.sqlite3")

baseball_weather_db <-
    dbConnect(RSQLite::SQLite(), "baseball_weather_db.sqlite3")


# ---- Examining data -----

#### pitchRx ####

db_list_tables(pitchRx_db)

tbl(pitchRx_db, "atbat") %>% glimpse()
tbl(pitchRx_db, "pitch") %>% glimpse()
tbl(pitchRx_db, "game") %>% glimpse()


#### statcast ####

db_list_tables(statcast_db)

tbl(statcast_db, "statcast_data_updated") %>% glimpse()


#### weather ####

db_list_tables(weather_db)

tbl(weather_db, "hourly_with_address") %>% glimpse()


#--------------------------------#
#---- Combining by chunking ----
#--------------------------------#


unique_dates <-
    tbl(pitchRx_db, "atbat") %>% 
    pull(game_date) %>% 
    unique() %>% 
    sort(decreasing = TRUE)


for (each_day in 1:length(unique_dates)) {
    
    ###---###---###---###---###---###---###---###---###---###---###---###---###
    
    atbat_res <-
        dbSendQuery(
            pitchRx_db,
            paste0(
                "SELECT * FROM atbat WHERE game_date = '",
                unique_dates[each_day],
                "'"
            )
        )
    
    atbat_chunk <- 
        dbFetch(atbat_res) %>% 
        select(
            -atbat_des_es,
            -event_es,
            -event2_es,
            -event3_es,
            -event4_es,
            -next_) %>%
        as_tibble()

    dbClearResult(atbat_res)
    
    ###---###---###---###---###---###---###---###---###---###---###---###---###
    
    game_res <-
        dbSendQuery(
            pitchRx_db,
            paste0(
                "SELECT * FROM game WHERE game_date = '",
                unique_dates[each_day],
                "'"
            )
        )
    
    game_chunk <- 
        dbFetch(game_res) %>% 
        select(
            -if_necessary,
            -home_audio_link,
            -away_audio_link,
            -home_preview_link,
            -away_preview_link,
            -postseason_tv_link) %>%
        as_tibble()
    
    dbClearResult(game_res)
    
    ###---###---###---###---###---###---###---###---###---###---###---###---###
    
    weather_res <-
        dbSendQuery(
            weather_db,
            paste0(
                "SELECT * FROM hourly_with_address WHERE obs_date_utc_chr IN ('",
                unique_dates[each_day],
                "', '",
                ymd(unique_dates[each_day]) + 1,
                "')"
            )
        )
    
    weather_chunk <- dbFetch(weather_res) %>% as_tibble()
    
    dbClearResult(weather_res)
    
    ###---###---###---###---###---###---###---###---###---###---###---###---###
    
    atbat_game <-
        left_join(
            atbat_chunk,
            game_chunk %>% 
                select(
                    gameday_link,
                    game_pk,
                    game_type,
                    home_time_zone,
                    double_header_sw,
                    home_team = home_name_abbrev,
                    venue),
            by = "gameday_link"
        )
    
    ###---###---###---###---###---###---###---###---###---###---###---###---###
    
    atbat_weather <-
        left_join(
            atbat_game,
            weather_chunk,
            by = c(
                "home_team", 
                "game_type",
                "utc_date_round_chr" = "obs_date_time_utc_round_chr")
        )
    
    rm(atbat_chunk, game_chunk, weather_chunk, atbat_game)
    gc()

    
    ###---###---###---###---###---###---###---###---###---###---###---###---###
    
    cat("===============================\n")
    cat("Loop", each_day)
    cat("\n-------------------------------\n")
    cat("|-- ")
    cat(min(atbat_weather$game_date),
        "-",
        max(atbat_weather$game_date))
    cat(" --|")
    cat("\n-------------------------------\n")
    cat(nrow(atbat_weather), "rows added\n")
    cat("Size: ")
    object_size(atbat_weather) %>% print()
    cat("-------------------------------\n")    
    
    ###---###---###---###---###---###---###---###---###---###---###---###---###
    
    dbWriteTable(
        baseball_weather_db,
        "atbat_weather_2",
        value = atbat_weather,
        append = TRUE,
        temporary = FALSE
    )
    
}


#=========================#
#### pitches ####
#=========================#

#--------------------------------#
#---- Initializing databases ----
#--------------------------------#

pitchRx_db <-
    dbConnect(
        RSQLite::SQLite(),
        "~/BASP/R analyses/Baseball Data/Data Files/pitchRx_db.sqlite3")

statcast_db <-
    dbConnect(
        RSQLite::SQLite(),
        "~/BASP/R analyses/Baseball Data/Data Files/statcast_db.sqlite3")

weather_db <- 
    dbConnect(RSQLite::SQLite(), "weather_db.sqlite3")

baseball_weather_db <-
    dbConnect(RSQLite::SQLite(), "baseball_weather_db.sqlite3")


# ---- Examining data -----

#### pitchRx ####

db_list_tables(pitchRx_db)

tbl(pitchRx_db, "atbat") %>% glimpse()
tbl(pitchRx_db, "pitch") %>% glimpse()
tbl(pitchRx_db, "game") %>% glimpse()


#### statcast ####

db_list_tables(statcast_db)

tbl(statcast_db, "statcast_data_updated") %>% glimpse()


#### weather ####

db_list_tables(weather_db)

tbl(weather_db, "hourly_with_address") %>% glimpse()


#### atbat weather ####

db_list_tables(baseball_weather_db)

tbl(baseball_weather_db, "atbat_weather") %>% glimpse()


#--------------------------------#
#---- Combining by chunking ----
#--------------------------------#


unique_dates <-
    tbl(pitchRx_db, "pitch") %>% 
    pull(game_date) %>% 
    unique() %>% 
    sort(decreasing = TRUE)


for (each_day in 1:length(unique_dates)) {
    
    ###---###---###---###---###---###---###---###---###---###---###---###---###
    
    pitch_res <-
        dbSendQuery(
            pitchRx_db,
            paste0(
                "SELECT * FROM pitch WHERE game_date = '",
                unique_dates[each_day],
                "'"
            )
        )
    
    pitch_chunk <- 
        dbFetch(pitch_res) %>% 
        select(
            -des_es,
            -cc,
            -mt,
            -next_) %>%
        as_tibble()

    dbClearResult(pitch_res)
    
    ###---###---###---###---###---###---###---###---###---###---###---###---###
    
    game_res <-
        dbSendQuery(
            pitchRx_db,
            paste0(
                "SELECT * FROM game WHERE game_date = '",
                unique_dates[each_day],
                "'"
            )
        )
    
    game_chunk <- 
        dbFetch(game_res) %>% 
        select(
            -if_necessary,
            -home_audio_link,
            -away_audio_link,
            -home_preview_link,
            -away_preview_link,
            -postseason_tv_link) %>%
        as_tibble()
    
    dbClearResult(game_res)
    
    ###---###---###---###---###---###---###---###---###---###---###---###---###
    
    weather_res <-
        dbSendQuery(
            weather_db,
            paste0(
                "SELECT * FROM hourly_with_address WHERE obs_date_utc_chr IN ('",
                unique_dates[each_day],
                "', '",
                ymd(unique_dates[each_day]) + 1,
                "')"
            )
        )
    
    weather_chunk <- dbFetch(weather_res) %>% as_tibble()
    
    dbClearResult(weather_res)
    
    ###---###---###---###---###---###---###---###---###---###---###---###---###
    
    pitch_game <-
        left_join(
            pitch_chunk,
            game_chunk %>% 
                select(
                    gameday_link,
                    game_pk,
                    game_type,
                    home_time_zone,
                    double_header_sw,
                    home_team = home_name_abbrev,
                    venue),
            by = "gameday_link"
        )
    
    ###---###---###---###---###---###---###---###---###---###---###---###---###
    
    pitch_weather <-
        left_join(
            pitch_game,
            weather_chunk,
            by = c(
                "home_team", 
                "game_type",
                "utc_date_round_chr" = "obs_date_time_utc_round_chr")
        )

    
    rm(pitch_chunk, game_chunk, weather_chunk, pitch_game)
    gc()
    
    
    ###---###---###---###---###---###---###---###---###---###---###---###---###
    
    cat("===============================\n")
    cat("Loop", each_day)
    cat("\n-------------------------------\n")
    cat("|-- ")
    cat(min(pitch_weather$game_date),
        "-",
        max(pitch_weather$game_date))
    cat(" --|")
    cat("\n-------------------------------\n")
    cat(nrow(pitch_weather), "rows added\n")
    cat("Size: ")
    object_size(pitch_weather) %>% print()
    cat("-------------------------------\n")    
    
    ###---###---###---###---###---###---###---###---###---###---###---###---###
    
    dbWriteTable(
        baseball_weather_db,
        "pitch_weather_2",
        value = pitch_weather,
        append = TRUE,
        temporary = FALSE
    )
    
}


#=========================#
#### statcast ####
#=========================#

#--------------------------------#
#---- Initializing databases ----
#--------------------------------#

pitchRx_db <-
    dbConnect(
        RSQLite::SQLite(),
        "~/BASP/R analyses/Baseball Data/Data Files/pitchRx_db.sqlite3")

statcast_db <-
    dbConnect(
        RSQLite::SQLite(),
        "~/BASP/R analyses/Baseball Data/Data Files/statcast_db.sqlite3")

weather_db <- 
    dbConnect(RSQLite::SQLite(), "weather_db.sqlite3")

baseball_weather_db <-
    dbConnect(RSQLite::SQLite(), "baseball_weather_db.sqlite3")


# ---- Examining data -----

#### pitchRx ####

db_list_tables(pitchRx_db)

tbl(pitchRx_db, "atbat") %>% glimpse()
tbl(pitchRx_db, "pitch") %>% glimpse()
tbl(pitchRx_db, "game") %>% glimpse()
tbl(pitchRx_db, "hip") %>% glimpse()


#### statcast ####

db_list_tables(statcast_db)

tbl(statcast_db, "statcast_data_updated") %>% glimpse()


#### weather ####

db_list_tables(weather_db)

tbl(weather_db, "hourly_with_address") %>% glimpse()


#### atbat weather ####

db_list_tables(baseball_weather_db)

tbl(baseball_weather_db, "atbat_weather_2") %>% glimpse()
tbl(baseball_weather_db, "pitch_weather_2") %>% glimpse()


#--------------------------------#
#---- Combining by chunking ----
#--------------------------------#


unique_dates <- 
    tbl(statcast_db, "statcast_data_updated") %>% 
    pull(game_date) %>% 
    unique() %>% 
    sort(decreasing = TRUE)


error_ls <- list()


for (each_day in 1:length(unique_dates)) {

    
    ###---###---###---###---###---###---###---###---###---###---###---###---###
    
    
    statcast_res <-
        dbSendQuery(
            statcast_db,
            paste0(
                "SELECT * FROM statcast_data_updated WHERE game_date = '",
                unique_dates[each_day],
                "'"
            )
        )
    
    statcast_chunk <- 
        dbFetch(statcast_res) %>% 
        as_tibble() %>% 
        group_by(at_bat_number) %>%
        arrange(game_pk, at_bat_number, pitch_number) %>% 
        mutate(pitch_number_ab = 1:length(game_pk)) %>% 
        ungroup()

    dbClearResult(statcast_res)
    
    
    ###---###---###---###---###---###---###---###---###---###---###---###---###


    pitch_res <-
        dbSendQuery(
            pitchRx_db,
            paste0(
                "SELECT * FROM pitch WHERE game_date = '",
                unique_dates[each_day],
                "'"
            )
        )
    
    pitch_chunk <- 
        dbFetch(pitch_res) %>% 
        select(
            at_bat_number,
            pitch_number,
            sv_id,
            utc_date_round_chr,
            east_date_round_chr,
            gameday_link,
            nasty,
            play_guid,
            tfs_zulu) %>%
        as_tibble()
    
    
    dbClearResult(pitch_res)
    
    
    ###---###---###---###---###---###---###---###---###---###---###---###---###
    
    
    atbat_res <-
        dbSendQuery(
            pitchRx_db,
            paste0(
                "SELECT * FROM atbat WHERE game_date = '",
                unique_dates[each_day],
                "'"
            )
        )
    
    atbat_chunk <- 
        dbFetch(atbat_res) %>% 
        select(
            batter,
            pitcher,
            batter_name,
            at_bat_number,
            gameday_link) %>%
        as_tibble()

    dbClearResult(atbat_res)    
    
    
    ###---###---###---###---###---###---###---###---###---###---###---###---###
    
    
    pitch_atbat <-
        left_join(
            pitch_chunk,
            atbat_chunk,
            by = c(
                "gameday_link", 
                "at_bat_number"))
    
    
    ###---###---###---###---###---###---###---###---###---###---###---###---###
    
    
    game_res <-
        dbSendQuery(
            pitchRx_db,
            paste0(
                "SELECT * FROM game WHERE game_date = '",
                unique_dates[each_day],
                "'"
            )
        )
    
    game_chunk <- 
        dbFetch(game_res) %>% 
        select(
            -if_necessary,
            -home_audio_link,
            -away_audio_link,
            -home_preview_link,
            -away_preview_link,
            -postseason_tv_link) %>%
        as_tibble()
    
    dbClearResult(game_res)
    
    
    ###---###---###---###---###---###---###---###---###---###---###---###---###
    
    
    pitch_game <-
        left_join(
            pitch_atbat,
            game_chunk %>% mutate(game_pk = as.integer(game_pk)) %>% 
                select(
                    gameday_link,
                    game_pk,
                    game_type,
                    home_time_zone,
                    double_header_sw,
                    home_team = home_name_abbrev,
                    venue),
            by = "gameday_link"
        ) %>% 
        group_by(at_bat_number) %>%
        arrange(game_pk, pitch_number) %>% 
        mutate(pitch_number_ab = 1:length(game_pk)) %>% 
        ungroup()
    
    
    ###---###---###---###---###---###---###---###---###---###---###---###---###
    
    
    statcast_game <- 
        # left_join(
        full_join(
            statcast_chunk,
            pitch_game,
            # pitch_atbat,
            by = c(
                "game_pk", 
                # "home_team", 
                "at_bat_number", 
                "pitch_number_ab")
        )
    
    equal_rows <- nrow(statcast_chunk) == nrow(pitch_game)
    
    
    ###---###---###---###---###---###---###---###---###---###---###---###---###
    
    
    weather_res <-
        dbSendQuery(
            weather_db,
            paste0(
                "SELECT * FROM hourly_with_address WHERE obs_date_utc_chr IN ('",
                unique_dates[each_day],
                "', '",
                ymd(unique_dates[each_day]) + 1,
                "')"
            )
        )
    
    weather_chunk <- dbFetch(weather_res) %>% as_tibble()
    
    dbClearResult(weather_res)
    
    
    ###---###---###---###---###---###---###---###---###---###---###---###---###
    
    
    statcast_weather <-
        left_join(
            statcast_game,
            weather_chunk,
            by = c(
                "home_team.x" = "home_team", 
                "game_type.x" = "game_type",
                "utc_date_round_chr.x" = "obs_date_time_utc_round_chr")
        )

    
    rm(pitch_chunk, game_chunk, weather_chunk, pitch_game)
    gc()
    
    
    ###---###---###---###---###---###---###---###---###---###---###---###---###
    
    
    cat("\n===============================\n")
    cat("Loop", each_day)
    cat("\n-------------------------------\n")
    cat("|-- ")
    cat(min(statcast_weather$game_date),
        "-",
        max(statcast_weather$game_date))
    cat(" --|")
    cat("\n-------------------------------\n")
    cat("nrow:Statcast = Pitch? ")
    cat("||")
    cat(equal_rows)
    cat("||")
    cat("\n-------------------------------\n")
    cat(nrow(statcast_weather), "rows added\n")
    cat("Size: ")
    object_size(statcast_weather) %>% print()
    cat("===============================\n")
    
    
    ###---###---###---###---###---###---###---###---###---###---###---###---###
    
    
    error_ls[[each_day]] <- 
        bind_cols(
            loop = each_day, 
            date = as.character(unique_dates[each_day]), 
            equal_rows = equal_rows)
    
    
    ###---###---###---###---###---###---###---###---###---###---###---###---###
    
    
    dbWriteTable(
        baseball_weather_db,
        "statcast_weather_2",
        value = statcast_weather,
        append = TRUE,
        temporary = FALSE
    )
    
    gc()
    
}




#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
#
#### Creating indexes ####
#
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

#--------------------------------#
#---- Initializing databases ----
#--------------------------------#

pitchRx_db <-
    dbConnect(
        RSQLite::SQLite(),
        "~/BASP/R analyses/Baseball Data/Data Files/pitchRx_db.sqlite3")

statcast_db <-
    dbConnect(
        RSQLite::SQLite(),
        "~/BASP/R analyses/Baseball Data/Data Files/statcast_db.sqlite3")

weather_db <- 
    dbConnect(RSQLite::SQLite(), "weather_db.sqlite3")

baseball_weather_db <-
    dbConnect(RSQLite::SQLite(), "baseball_weather_db.sqlite3")

#--------------------------------#
# ---- Examining data ----
#--------------------------------#

db_list_tables(baseball_weather_db)

tbl(baseball_weather_db, "atbat_weather_2") %>% glimpse()
tbl(baseball_weather_db, "pitch_weather_2") %>% glimpse()
tbl(baseball_weather_db, "statcast_weather_2") %>% glimpse()

tbl(baseball_weather_db, "atbat_weather_2") %>% pull(wban) %>% length()
tbl(baseball_weather_db, "pitch_weather_2") %>% select(gameday_link.x) %>% collect() %>% nrow()
tbl(baseball_weather_db, "statcast_weather_2") %>% select(gameday_link.x) %>% collect() %>% nrow()


################################################


# db_drop_table(baseball_weather_db, "atbat_weather")
# db_drop_table(baseball_weather_db, "pitch_weather")
# db_drop_table(baseball_weather_db, "statcast_weather")

dbExecute(baseball_weather_db, "ALTER TABLE atbat_weather RENAME TO atbat_weather_drop")
dbExecute(baseball_weather_db, "ALTER TABLE pitch_weather RENAME TO pitch_weather_drop")
dbExecute(baseball_weather_db, "ALTER TABLE statcast_weather RENAME TO statcast_weather_drop")

dbExecute(baseball_weather_db, "ALTER TABLE atbat_weather_2 RENAME TO atbat_weather")
dbExecute(baseball_weather_db, "ALTER TABLE pitch_weather_2 RENAME TO pitch_weather")
dbExecute(baseball_weather_db, "ALTER TABLE statcast_weather_2 RENAME TO statcast_weather")


#=========================#
#### atbat ####
#=========================#

# db_create_index(baseball_weather_db, "atbat_weather_2", "home_team")
# db_create_index(baseball_weather_db, "atbat_weather_2", "ballpark")
# db_create_index(baseball_weather_db, "atbat_weather_2", "venue.x")
# db_create_index(baseball_weather_db, "atbat_weather_2", "game_type")
# # db_create_index(baseball_weather_db, "atbat_weather", "wban")
# # db_create_index(baseball_weather_db, "atbat_weather", "obs_year")
# # db_create_index(baseball_weather_db, "atbat_weather", "obs_month")
# db_create_index(baseball_weather_db, "atbat_weather_2", "pitcher")
# db_create_index(baseball_weather_db, "atbat_weather_2", "batter")
# db_create_index(baseball_weather_db, "atbat_weather_2", "gameday_link.x")
# db_create_index(baseball_weather_db, "atbat_weather_2", "event")
# db_create_index(baseball_weather_db, "atbat_weather_2", "stand")
# db_create_index(baseball_weather_db, "atbat_weather_2", "p_throws")
# # db_create_index(baseball_weather_db, "atbat_weather", "at_bat_number")
# db_create_index(baseball_weather_db, "atbat_weather_2", "game_year")
# db_create_index(baseball_weather_db, "atbat_weather_2", "game_date")
# # db_create_index(baseball_weather_db, "atbat_weather", "game_pk.x")

gc()

#=========================#
#### pitch ####
#=========================#

# db_create_index(baseball_weather_db, "pitch_weather_2", "home_team")
# db_create_index(baseball_weather_db, "pitch_weather_2", "ballpark")
# db_create_index(baseball_weather_db, "pitch_weather_2", "venue.x")
# db_create_index(baseball_weather_db, "pitch_weather_2", "game_type")
# # db_create_index(baseball_weather_db, "pitch_weather", "wban")
# # db_create_index(baseball_weather_db, "pitch_weather", "obs_year")
# # db_create_index(baseball_weather_db, "pitch_weather", "obs_month")
# 
# db_create_index(baseball_weather_db, "pitch_weather_2", "pitcher")
# db_create_index(baseball_weather_db, "pitch_weather_2", "batter")
# db_create_index(baseball_weather_db, "pitch_weather_2", "batter_team")
# db_create_index(baseball_weather_db, "pitch_weather_2", "pitcher_team")
# 
# db_create_index(baseball_weather_db, "pitch_weather_2", "gameday_link.x")
# db_create_index(baseball_weather_db, "pitch_weather_2", "pitch_event")
# 
# db_create_index(baseball_weather_db, "pitch_weather_2", "stand")
# 
# # db_create_index(baseball_weather_db, "pitch_weather", "at_bat_number")
# # db_create_index(baseball_weather_db, "pitch_weather", "pitch_number")
# db_create_index(baseball_weather_db, "pitch_weather_2", "game_year")
# db_create_index(baseball_weather_db, "pitch_weather_2", "game_date")
# # db_create_index(baseball_weather_db, "pitch_weather", "game_pk.x")
# # db_create_index(baseball_weather_db, "pitch_weather", "play_guid")

gc()

#=========================#
#### statcast ####
#=========================#

db_create_index(baseball_weather_db, "statcast_weather_2", "home_team.x")
db_create_index(baseball_weather_db, "statcast_weather_2", "ballpark.x")
db_create_index(baseball_weather_db, "statcast_weather_2", "game_type.x")
# db_create_index(baseball_weather_db, "statcast_weather", "wban")
# db_create_index(baseball_weather_db, "statcast_weather", "obs_year")
# db_create_index(baseball_weather_db, "statcast_weather", "obs_month")
db_create_index(baseball_weather_db, "statcast_weather_2", "pitcher_id_sc")
db_create_index(baseball_weather_db, "statcast_weather_2", "batter_id_sc")
db_create_index(baseball_weather_db, "statcast_weather_2", "batter_team")
db_create_index(baseball_weather_db, "statcast_weather_2", "pitcher_team")
db_create_index(baseball_weather_db, "statcast_weather_2", "gameday_link.x")
db_create_index(baseball_weather_db, "statcast_weather_2", "events")
db_create_index(baseball_weather_db, "statcast_weather_2", "stand")
db_create_index(baseball_weather_db, "statcast_weather_2", "p_throws")
# db_create_index(baseball_weather_db, "statcast_weather", "at_bat_number")
# db_create_index(baseball_weather_db, "statcast_weather", "pitch_number_ab")
db_create_index(baseball_weather_db, "statcast_weather_2", "game_year")
db_create_index(baseball_weather_db, "statcast_weather_2", "game_date")
# db_create_index(baseball_weather_db, "statcast_weather", "game_pk.x")
db_create_index(baseball_weather_db, "statcast_weather_2", "description")
# db_create_index(baseball_weather_db, "statcast_weather", "play_guid")
db_create_index(baseball_weather_db, "statcast_weather_2", "pitch_type")
db_create_index(baseball_weather_db, "statcast_weather_2", "bb_type")
db_create_index(baseball_weather_db, "statcast_weather", "type")
# db_create_index(baseball_weather_db, "statcast_weather", "balls")
# db_create_index(baseball_weather_db, "statcast_weather", "strikes")
# db_create_index(baseball_weather_db, "statcast_weather", "outs_when_up")
# db_create_index(baseball_weather_db, "statcast_weather", "inning")
# db_create_index(baseball_weather_db, "statcast_weather", "inning_topbot")

gc()

#############################################
#############################################

dbGetQuery(baseball_weather_db, "SELECT * FROM sqlite_master WHERE type = 'index'")
dbDisconnect(baseball_weather_db)

#############################################
#############################################


tbl(baseball_weather_db, "atbat_weather_2") %>% pull(gameday_link.x) %>% length()
tbl(baseball_weather_db, "pitch_weather_2") %>% pull(gameday_link.x) %>% length()
tbl(baseball_weather_db, "statcast_weather_2") %>% pull(gameday_link.x) %>% length()



#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
#
# ### Filtering ####
#
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

baseball_weather_db <-
    dbConnect(RSQLite::SQLite(), "baseball_weather_db.sqlite3")

#=========================#
#### atbat ####
#=========================#

atbat_gameday_link <-
    tbl(baseball_weather_db, "atbat_weather") %>% 
    pull(gameday_link.x) %>% 
    unique() %>% 
    sort(decreasing = TRUE)


for (i in 1:length(atbat_gameday_link)) {
    
    ###---###---###---###---###---###---###---###---###---###---###---###---###
    
    atbat_weather_res <-
        dbSendQuery(
            baseball_weather_db,
            paste0(
                "SELECT * FROM atbat_weather WHERE atbat_gameday_link = '",
                atbat_gameday_link[i],
                "'"
            )
        )
    
    atbat_weather_chunk <- 
        dbFetch(atbat_weather_res) %>% 
        mutate(
            na_vars = case_when(
                !is.na(dry_temp) & !is.na(humidity) & !is.na(wind_speed) ~ 3,
                (!is.na(dry_temp) & !is.na(humidity)) | 
                    (!is.na(dry_temp) & !is.na(wind_speed)) | 
                    (!is.na(wind_speed) & !is.na(humidity)) ~ 2,
                !is.na(dry_temp) | !is.na(humidity) | !is.na(wind_speed) ~ 1
                ))
                
        filter_at(vars(dry_temp, humidity, wind_speed), any_vars(!is.na(.)))

    
    
    dbClearResult(atbat_res)
    
    ###---###---###---###---###---###---###---###---###---###---###---###---###
    
}

xx <-
    tbl(baseball_weather_db, "atbat_weather") %>%
    filter(obs_date_loc_chr == "2017-07-06") %>% 
    collect() %>%
    mutate(na_vars = case_when(
        !is.na(dry_temp) & !is.na(humidity) & !is.na(wind_speed) ~ 3,
        (!is.na(dry_temp) & !is.na(humidity)) |
            (!is.na(dry_temp) & !is.na(wind_speed)) |
            (!is.na(wind_speed) & !is.na(humidity)) ~ 2,
        !is.na(dry_temp) | !is.na(humidity) | !is.na(wind_speed) ~ 1,
        is.na(dry_temp) & is.na(humidity) & is.na(wind_speed) ~ 0
        
    )) %>% 
    group_by(play_guid) %>% 
    arrange(-na_vars, -distance) %>% 
    slice(1)


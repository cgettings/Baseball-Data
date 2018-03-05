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
library(xml2)

setwd("~/BASP/R analyses/Baseball Data/Data Files/Weather Data")


#========================================#
#### Downloaded weather files ####
#========================================#


#---------------------------------#
#---- Initializing databases ----
#---------------------------------#

weather_db <- dbConnect(RSQLite::SQLite(), "weather_db.sqlite3")

hourly_with_address <- tbl(weather_db, "hourly_with_address")

most_recent_date <- 
    hourly_with_address %>% 
    select(obs_date_chr, obs_date, obs_month) %>% 
    arrange(desc(obs_date)) %>%
    collect() %>% 
    slice(1) %>% 
    mutate(obs_date_chr = ymd(obs_date_chr))

 

#========================================#
#### Downloaded weather files ####
#========================================#


### Finding most recent weather file ###

root_url <- "https://www.ncdc.noaa.gov/orders/qclcd/"

filename_zip <-
    read_html(root_url) %>% 
    html_node("table") %>%
    html_table() %>% 
    select(
        filename = Name,
        modified = `Last modified`,
        size = Size) %>% 
    as_tibble() %>% 
    filter(str_detect(filename, "QCLCD")) %>% 
    mutate(modified = dmy_hm(modified)) %>% 
    arrange(desc(modified)) %>% 
    slice(1) %>% 
    pull(filename)

### Downloading weather zip file ###

# filename_zip <- "QCLCD201707.zip"

download.file(
    url = paste0(root_url, filename_zip),
    destfile = filename_zip)

unzipped_filenames <-
    unzip(
        zipfile = filename_zip,
        list = TRUE) %>%
    pull(Name)

hourly_name  <- str_subset(unzipped_filenames, "hourly")
station_name <- str_subset(unzipped_filenames, "station")


#========================================#
#### Adding stations to addresses ####
#========================================#

#---------------------------------#
#---- Initializing databases ----
#---------------------------------#

weather_db <- dbConnect(RSQLite::SQLite(), "weather_db.sqlite3")

#---------------------------------#
#---- Extracting addresses ----
#---------------------------------#

addresses <- tbl(weather_db, "addresses") %>% collect()

    
### Unzipping station files ###


unzip(
    zipfile = filename_zip,
    files = station_name,
    exdir = "Hourly Files"
)
    
    
## cleaning the data ##


station_unzip <- 
    
    read_delim(paste0("Hourly Files/",
        station_name), 
        delim = "|", 
        col_types = cols(WBAN = col_integer())
        ) %>% 
    
    `attr<-`("problems", NULL) %>% 

    transmute(
        obs_year = station_name %>% 
            str_sub(1, 4) %>% 
            as.integer(), 
        obs_month = station_name %>% 
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


## removing redundant data ##


most_recent_data <-
    tbl(weather_db, "address_station") %>% 
    filter(
        obs_year %in% address_station$obs_year &
        obs_month %in% address_station$obs_month) %>% 
    collect()


address_station <-
    address_station %>% 
    mutate(
        wmo = as.character(wmo),
        altitude = as.character(altitude),
        time_zone = as.character(time_zone)
        )


address_station_distinct <- 
    anti_join(
        address_station, 
        most_recent_data)


## writing to database ##


dbWriteTable(
    weather_db,
    "address_station",
    value = address_station_distinct,
    append = TRUE,
    temporary = FALSE
)


cat("\n-------------------------------\n")
cat(nrow(address_station_distinct), "rows added")
cat("\n-------------------------------\n")
cat("|-- ")
cat(
    address_station_distinct$obs_year, "-", 
    address_station_distinct$obs_month, sep = "")
cat(" --|")
cat("\n-------------------------------\n")  


## removing the unzipped files ##


file.remove(paste0("Hourly Files/", station_name))

gc()
    

#========================================#
#### Unzipping hourly weather files ####
#========================================#


#---------------------------------#
#---- Initializing databases ----
#---------------------------------#

weather_db <- dbConnect(RSQLite::SQLite(), "weather_db.sqlite3")
pitchRx_db <-
    dbConnect(
        RSQLite::SQLite(),
        "~/BASP/R analyses/Baseball Data/Data Files/pitchRx_db.sqlite3")


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
#---- Chunking ----
#---------------------------------#

unzip(
    zipfile = filename_zip,
    files = hourly_name,
    exdir = "Hourly Files"
)


### cleaning the data ###

tryCatch({


### Hourly weather files ###


hourly_unzip_0 <-

    read_csv(
        paste0("Hourly Files/", hourly_name), 
        col_types = cols(WBAN = col_integer())
    ) %>% 
    
    `attr<-`("problems", NULL)
    

### Extracting variables ###


hourly_unzip_1 <- 
    hourly_unzip_0 %>% 
    
    transmute(
        wban = as.integer(WBAN),
        # wmo = as.character(WMO),
        obs_year = hourly_name %>% str_sub(1, 4) %>% as.integer(),
        obs_month = hourly_name %>% str_sub(5, 6) %>% as.integer(),
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
        address_station, .,
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


name_drop <-
    hourly_unzip_1 %>%
    extract(!colnames(hourly_unzip_1) %in% colnames(tbl(weather_db, "hourly_with_address"))) %>%
    colnames()

hourly_unzip_2 <- hourly_unzip_1 %>% select(-one_of(name_drop))


rm(hourly_unzip_0)
rm(hourly_unzip_1)
gc()


cat("\n-------------------------------\n")
cat("|-- ")
cat(
    min(hourly_unzip_2$obs_date_chr, na.rm = TRUE), 
    " - ", 
    max(hourly_unzip_2$obs_date_chr, na.rm = TRUE), 
    sep = "")
cat(" --|")
cat("\n-------------------------------\n")
cat(nrow(hourly_unzip_2), "rows added\n")
object_size(hourly_unzip_2) %>% print()
cat("-------------------------------\n")


dbWriteTable(
    weather_db,
    "hourly_with_address",
    value = hourly_unzip_2,
    append = TRUE,
    temporary = FALSE
)


file.remove(paste0("Hourly Files/", hourly_name))

gc()

},
    
    finally = next
    
)
    

dbDisconnect(weather_db)


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


#--------------------------------#
#---- Finding dates ----
#--------------------------------#


begin_date <-
    tbl(baseball_weather_db, "atbat_weather") %>% 
    select(game_date) %>% 
    collect() %>% 
    pull(game_date) %>% 
    max() %>% 
    as_date() - 1

end_date <- today() - 1

unique_dates <- seq(begin_date, end_date, 1)


#--------------------------------#
#---- Combining by chunking ----
#--------------------------------#


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
    cat(min(atbat_weather$game_date, na.rm = TRUE),
        "-",
        max(atbat_weather$game_date, na.rm = TRUE))
    cat(" --|")
    cat("\n-------------------------------\n")
    cat(nrow(atbat_weather), "rows added\n")
    cat("Size: ")
    object_size(atbat_weather) %>% print()
    cat("-------------------------------\n")    
    
    ###---###---###---###---###---###---###---###---###---###---###---###---###
    
    dbWriteTable(
        baseball_weather_db,
        "atbat_weather",
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


#--------------------------------#
#---- Finding dates ----
#--------------------------------#


begin_date <-
    tbl(baseball_weather_db, "pitch_weather") %>% 
    filter(game_year == "2017") %>% 
    select(game_date) %>% 
    collect() %>% 
    pull(game_date) %>% 
    max() %>% 
    as_date() - 1

end_date <- today() - 1

unique_dates <- seq(begin_date, end_date, 1)


#--------------------------------#
#---- Combining by chunking ----
#--------------------------------#


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
    cat(min(pitch_weather$game_date, na.rm = TRUE),
        "-",
        max(pitch_weather$game_date, na.rm = TRUE))
    cat(" --|")
    cat("\n-------------------------------\n")
    cat(nrow(pitch_weather), "rows added\n")
    cat("Size: ")
    object_size(pitch_weather) %>% print()
    cat("-------------------------------\n")    
    
    ###---###---###---###---###---###---###---###---###---###---###---###---###
    
    dbWriteTable(
        baseball_weather_db,
        "pitch_weather",
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


#--------------------------------#
#---- Finding dates ----
#--------------------------------#


begin_date <-
    tbl(baseball_weather_db, "statcast_weather") %>% 
    filter(game_year == "2017") %>% 
    select(game_date) %>% 
    collect() %>% 
    pull(game_date) %>% 
    max() %>% 
    as_date() + 1

# begin_date1 <- floor_date(begin_date, unit = "month")
end_date <- ceiling_date(begin_date, unit = "month") - 1

unique_dates <- seq(begin_date, end_date, 1)


#--------------------------------#
#---- Combining by chunking ----
#--------------------------------#


error_ls <- list()


tryCatch({

for (each_day in 14:length(unique_dates)) {

    
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
        left_join(
        # full_join(
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
    cat(min(statcast_weather$game_date, na.rm = TRUE),
        "-",
        max(statcast_weather$game_date, na.rm = TRUE))
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
        "statcast_weather",
        value = statcast_weather,
        append = TRUE,
        temporary = FALSE
    )
    
    gc()
    
}
    
},
    
    finally = next
    
)

################################################################################
################################################################################
################################################################################
################################################################################


dates_to_delete <-
    c(
        17353,
        17352,
        17351
        )

dbExecute(baseball_weather_db, paste0("DELETE FROM statcast_weather WHERE 'game_date' = ", dates_to_delete[7]))

dbExecute(
    baseball_weather_db,
    "DELETE FROM statcast_weather WHERE obs_year = '2017' AND obs_month = '7'"
)

dbExecute(
    baseball_weather_db,
    "DELETE FROM statcast_weather WHERE game_date = '2017-07-01'"
)

dbGetRowsAffected(rs)

dbClearResult(rs)


# Error in mutate_impl(.data, dots) : 
#   Column `pitch_number_ab` must be length 0 (the number of rows) or one, not 2

tbl(baseball_weather_db, "statcast_weather") %>% filter(obs_year == 2017) %>% select(game_date, obs_date, obs_date_chr)


################################################################################
################################################################################
################################################################################
################################################################################

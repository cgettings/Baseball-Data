
#-----------------------------------------------------------------------------------------#
# Putting into real database ----
#-----------------------------------------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Connecting to databases
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

statcast_db <- dbConnect(SQLite(), "data/statcast_db_rebuilt.sqlite3")
statcast_db_chunk <- dbConnect(SQLite(), "data/statcast_db_rebuilt_chunk.sqlite3")

statcast_db %>% db_drop_table("statcast_data")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Putting into real databsse ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

statcast_res <- dbSendQuery(statcast_db_chunk, "SELECT * FROM statcast_data")

while (!dbHasCompleted(statcast_res)) {
    
    statcast_chunk <- 
        dbFetch(statcast_res, n = 1e2) %>% 
        as_tibble() %>% 
        
        mutate(
            sv_id = case_when(sv_id == "null" ~ NA_character_ ,
                              TRUE            ~ sv_id)
        ) %>%
        
        mutate(
            obs_date_time_loc =
                make_datetime(
                    year = str_c("20", substr(sv_id, 1, 2)) %>% as.integer(),
                    month = substr(sv_id, 3, 4) %>% as.integer(),
                    day = substr(sv_id, 5, 6) %>% as.integer(),
                    hour = substr(sv_id, 8, 9) %>% as.integer(),
                    min = substr(sv_id, 10, 11) %>% as.integer(),
                    sec = substr(sv_id, 12, 13) %>% as.integer(),
                    tz = "UTC"
                )
        ) %>% 
        
        mutate(
            obs_date_time_utc =
                case_when(
                    ballpark %in% c(
                        "Busch Stadium",
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
                    
                    ballpark %in% c(
                        "Camden Yards",
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
                    
                    ballpark %in% c(
                        "Coors Field") ~ 
                        force_tz(obs_date_time_loc, tzone = "US/Mountain") %>% 
                        with_tz(., tzone = "UTC"),
                    
                    ballpark %in% c(
                        "Chase Field") ~ 
                        force_tz(obs_date_time_loc, tzone = "America/Phoenix") %>% 
                        with_tz(., tzone = "UTC"),
                    
                    ballpark %in% c(
                        "Dodger Stadium",
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
        )

    
    dbWriteTable(
        statcast_db,
        "statcast_data",
        value = statcast_chunk,
        append = TRUE,
        temporary = FALSE)
    
}

dbClearResult(statcast_res)

dbDisconnect(statcast_db)
dbDisconnect(statcast_db_chunk)

##############################################################################################################

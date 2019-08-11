


get_savant_pitches_data <-
    
    function(
        start_date = lubridate::today(), 
        end_date   = lubridate::today() - 1,
        home_road  = NULL) {
        
        start_year <- ymd(start_date) %>% year()
        
        statcast_data <-
            read_csv(
                glue(
                    "https://baseballsavant.mlb.com/statcast_search/csv?all=true&hfPT=&hfAB=&hfBBT=&hfPR=",
                    
                    "&hfZ=&stadium=&hfBBL=&hfNewZones=&hfGT=&hfC=&hfSea=",
                    
                    "{start_year}",
                    
                    "%7C&hfSit=&player_type=batter&hfOuts=&opponent=&pitcher_throws=&batter_stands=&hfSA=",
                    
                    "&game_date_gt=",
                    "{end_date}",
                    
                    "&game_date_lt=",
                    "{start_date}",
                    
                    "&hfInfield=&team=&position=&hfRO=",
                    
                    "&home_road=",
                    "{home_road}",
                    
                    "&hfFlag=&metric_1=&hfInn=&min_pitches=0&min_results=0&group_by=name",
                    "&sort_col=pitches&player_event_sort=h_launch_speed&sort_order=desc&min_abs=0&type=details&"
                    
                ),
                col_types = cols(game_date = col_character()),
                col_names = TRUE
            ) %>%
            mutate_all("as.character")
        
        if (ncol(statcast_data) == 1) {
            
            stop(statcast_data, statcast_data$error)
        }
        
        return(statcast_data)
}

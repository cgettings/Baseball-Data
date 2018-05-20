


get_savant_pitches_data <-
    
    function(
        # pitch_type = NULL, 
        from = lubridate::today(), 
        back_to = lubridate::today() - 1,
        home_road = NULL) {
        
        year_1 <- ymd(from) %>% year()
        
        
        statcast_data <-
            read_csv(paste0(
                "https://baseballsavant.mlb.com/statcast_search/csv?all=true&hfPT=&hfAB=&hfBBT=&hfPR=",
                
                # pitch_type,
                
                "&hfZ=&stadium=&hfBBL=&hfNewZones=&hfGT=&hfC=&hfSea=",
                
                year_1,
                
                "%7C&hfSit=&player_type=batter&hfOuts=&opponent=&pitcher_throws=&batter_stands=&hfSA=",
                
                "&game_date_gt=",
                back_to,
                
                "&game_date_lt=",
                from,
                
                "&hfInfield=&team=&position=&hfRO=",
                
                "&home_road=",
                home_road,
                
                "&hfFlag=&metric_1=&hfInn=&min_pitches=0&min_results=0&group_by=name&sort_col=pitches&player_event_sort=h_launch_speed&sort_order=desc&min_abs=0&type=details&"
                
        ), 
        col_types = cols(game_date = col_character()))
        
        return(statcast_data)
}

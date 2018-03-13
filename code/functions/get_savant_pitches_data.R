


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
    #     na = "null",
    #     col_types = c(
    #         hit_distance_sc = col_integer(),
    #         launch_angle = col_number(),
    #         launch_speed = col_number(),
    #         effective_speed = col_number(),
    #         release_spin_rate = col_integer(),
    #         release_extension = col_number(),
    #         release_pos_x = col_number(),
    #         on_3b = col_integer(),
    #         on_2b = col_integer(),
    #         on_1b = col_integer())
    # ) #%>% 
            # mutate(
            #     release_pos_x = as.numeric(release_pos_x),
            #     release_pos_y = as.numeric(release_pos_y),
            #     release_pos_z = as.numeric(release_pos_z),
            #     release_extension = as.numeric(release_extension),
            #     pos1_person_id = as.integer(pos1_person_id),
            #     pos2_person_id = as.integer(pos2_person_id),
            #     pos2_person_id_1 = as.integer(pos2_person_id_1),
            #     pos3_person_id = as.integer(pos3_person_id),
            #     pos4_person_id = as.integer(pos4_person_id),
            #     pos5_person_id = as.integer(pos5_person_id),
            #     pos6_person_id = as.integer(pos6_person_id),
            #     pos7_person_id = as.integer(pos7_person_id),
            #     pos8_person_id = as.integer(pos8_person_id),
            #     pos9_person_id = as.integer(pos9_person_id),
            #     estimated_ba_using_speedangle = as.numeric(estimated_ba_using_speedangle),
            #     estimated_woba_using_speedangle = as.numeric(estimated_woba_using_speedangle),
            #     launch_speed = as.numeric(launch_speed),
            #     launch_angle = as.numeric(launch_angle),
            #     launch_speed_angle = as.integer(launch_speed_angle),
            #     effective_speed = as.numeric(effective_speed),
            #     hit_distance_sc = as.integer(hit_distance_sc),
            #     release_spin_rate = as.integer(release_spin_rate)
            #     )
        
        return(statcast_data)
}

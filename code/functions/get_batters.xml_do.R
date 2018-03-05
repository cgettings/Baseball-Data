#############################################
#############################################
##
## Diwnloading batters data from mlb gameday
##
#############################################
#############################################

get_batters.xml_do <- function(gid) {
    
    # gid <- "gid_2011_04_04_minmlb_nyamlb_1"
    

    gids2urls <- function (gid) {
        root <- "http://gd2.mlb.com/components/game/"
        league <- rep("mlb", length(gid))
        not.mlb <- !grepl("mlb", gid)
        if (any(not.mlb))
            league[not.mlb] <- substr(gid[not.mlb], 26, 28)
        base <- paste0(root, league)
        paste0(
            base,
            "/year_",
            substr(gid, 5, 8),
            "/month_",
            substr(gid,
                10, 11),
            "/day_",
            substr(gid, 13, 14),
            "/",
            gid
        )
    }
    
    #####
    
    batter_list_to_df <-
        function(x)
            x %>% attributes() %>% as.data.frame()
    
    #####

    batter_df <- data.frame()
    
    game_url <- gids2urls(gid)
    
    batters_url <- paste0(game_url, "/batters/")
    
    tryCatch(
        page_1 <- read_html(batters_url),
        error = function(e) {
            page_1 <<- NULL
        }
    )
    
    if (is.null(page_1))
        return(batter_df)
    
    batter_xml <-
        read_html(batters_url) %>%
        html_nodes("body") %>%
        html_nodes("li") %>%
        html_nodes("a") %>%
        xml_text() %>%
        str_subset(pattern = ".xml") %>%
        str_trim() %>% 
        extract(str_length(.) == 10)
    
    
    if (length(batter_xml) == 0)
        return(batter_df)
    
    else
        
        batter_xml_df <- data.frame(gid, batter_xml1 = batter_xml)
        
        batter_df <-
        batter_xml_df %>% 
        group_by(batter_xml1) %>%
        do(
            read_html(paste0(batters_url, .$batter_xml1)) %>%
                as_list() %>%
                extract2("body") %>% 
                extract2("player") %>%
                # lapply(X = ., FUN = batter_list_to_df) %>%
                lapply(FUN = batter_list_to_df) %>%
                extract(!names(.) %in% c("pitch", "faced", "atbats")) %>%
                bind_rows(.id = "id") %>%
                gather(column_1, value, avg:des) %>%
                unite(column_2, id, column_1, sep = "_") %>%
                spread(column_2, value) %>%
                add_column(gid, .before = 1)
            ) %>% 
            ungroup() %>% 
            mutate(batter = str_trunc(batter_xml1 %>% as.character(), 6, "right", "")) %>% 
            select(-batter_xml1)
    
    return(batter_df)
    
}



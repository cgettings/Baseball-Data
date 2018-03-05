
get_game.xml <- function(gid_df, i) {
    
    game.url <- url(paste0(gid_df$url[i], "/game.xml"))
    
    game.xml <- xml2::read_html(game.url)
    
    game <-
        game.xml %>%
        rvest::html_nodes("game") %>%
        purrr::map(xml2::xml_attrs) %>%
        purrr::flatten() %>%
        dplyr::as_tibble()
    
    team_home <-
        game.xml %>% 
        rvest::html_nodes("team") %>%
        purrr::map(xml2::xml_attrs) %>%
        magrittr::extract(1) %>%
        purrr::flatten() %>%
        dplyr::as_tibble() %>%
        magrittr::set_colnames(stringr::str_c(colnames(.), "_home"))
    
    team_away <-
        game.xml %>% 
        rvest::html_nodes("team") %>%
        purrr::map(xml2::xml_attrs) %>%
        magrittr::extract(2) %>%
        purrr::flatten() %>%
        dplyr::as_tibble() %>%
        magrittr::set_colnames(stringr::str_c(colnames(.), "_away"))
    
    stadium <-
        game.xml %>% 
        rvest::html_nodes("stadium") %>%
        purrr::map(xml2::xml_attrs) %>%
        purrr::flatten() %>%
        dplyr::as_tibble()
    
    game.xml_df <-
        bind_cols(
            game,
            team_home,
            team_away,
            stadium)
    
    gid_game.xml_df <-
        bind_cols(
            gid_df[i,], 
            game.xml_df)
    
    return(gid_game.xml_df)
    
    close(game.url)

}

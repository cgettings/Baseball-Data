###########################################
###########################################
##
## Turn gameday_links into gameday urls
##
###########################################
###########################################

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

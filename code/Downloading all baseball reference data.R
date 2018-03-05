###########################################
###########################################
##
## Auto downloading baseball reference data
##
###########################################
###########################################

#=========================#
#### Loading packages ####
#=========================#

library(QuantPsyc)
library(readr)
library(rvest)
library(tidyr)
library(stringr)
library(pitchRx)
library(lubridate)
library(stringdist)
library(dplyr)
library(tibble)

source("~/BASP/R analyses/Baseball Data/get_br_data.R")

setwd("~/BASP/R analyses/Baseball Data/Data Files")

#=========================#
#### Downloading data ####
#=========================#

teams <-
    c("ANA", "ARI", "ATL", "BAL", "BOS", "CHC", "CHW", "CIN", "CLE", "COL", "DET", 
        "FLA", "HOU", "KCR", "LAD", "MIL", "MIN", "NYM", "NYY", "OAK", "PHI", "PIT", 
        "SDP", "SEA", "SFG", "STL", "TBD", "TEX", "TOR", "WSN")


###################


br_data_pa.2014.2 <- data.frame()

now() %>% print()

    
for (i in 1:length(teams)) {
    
    br_data_pa.2014.2 <-
        get_br_data(
            events = "pa",
            from_year = 2014,
            back_to_year = 2014,
            team = teams[i]
        ) %>%
        bind_rows(br_data_pa.2014.2)
    
}

now() %>% print()


##########################


# br_data_pa.2014.1 %>% count(batter_team) %>% print(n = 30)
# 
# br_data_pa.2014.1 <- br_data_pa.2014.1 %>% arrange(desc(game_date))
# 
# write_rds(br_data_pa.2014.1, "br_data_pa.2014.1.RDS")



br_data_pa.2014.3 <- bind_rows(br_data_pa.2014.2, br_data_pa.2014.1)

br_data_pa.2014.3 %>% count(batter_team) %>% print(n = 30)

br_data_pa.2014.3 <- br_data_pa.2014.3 %>% arrange(desc(game_date))

write_rds(br_data_pa.2014.3, "br_data_pa.2014.3.RDS")

# br_data_pa.2015.3 <- read_rds("br_data_pa.2015.3.RDS")


###################


br_data_pa.2013.1 <- data.frame()

now() %>% print()

br_time_pa.2013 <- system.time(
    
    for (i in 1:length(teams)) {
        
        br_data_pa.2013.1 <-
            get_br_data(
                events = "pa",
                from_year = 2013,
                back_to_year = 2013,
                team = teams[i]
            ) %>%
            bind_rows(br_data_pa.2013.1)
        
    }
)
now() %>% print()

br_data_pa.2013.1 %>% count(batter_team) %>% print(n = 30)

br_data_pa.2013.1 <- br_data_pa.2013.1 %>% arrange(desc(game_date))

write_rds(br_data_pa.2013.1, "br_data_pa.2013.1.RDS")

##########################


br_data_pa.2012.1 <- data.frame()

now() %>% print()

br_time_pa.2012 <- system.time(
    
    for (i in 1:length(teams)) {
        
        br_data_pa.2012.1 <-
            get_br_data(
                events = "pa",
                from_year = 2012,
                back_to_year = 2012,
                team = teams[i]
            ) %>%
            bind_rows(br_data_pa.2012.1)
        
    }
)
now() %>% print()

br_data_pa.2012.1 %>% count(batter_team) %>% print(n = 30)

br_data_pa.2012.1 <- br_data_pa.2012.1 %>% arrange(desc(game_date))

write_rds(br_data_pa.2012.1, "br_data_pa.2012.1.RDS")

##########################


br_data_pa.2011.1 <- data.frame()

now() %>% print()

br_time_pa.2011 <- system.time(
    
    for (i in 1:length(teams)) {
        
        br_data_pa.2011.1 <-
            get_br_data(
                events = "pa",
                from_year = 2011,
                back_to_year = 2011,
                team = teams[i]
            ) %>%
            bind_rows(br_data_pa.2011.1)
        
    }
)
now() %>% print()

br_data_pa.2011.1 %>% count(batter_team) %>% print(n = 30)

br_data_pa.2011.1 <- br_data_pa.2011.1 %>% arrange(desc(game_date))

write_rds(br_data_pa.2011.1, "br_data_pa.2011.1.RDS")

##########################


br_data_pa.2010.1 <- data.frame()

now() %>% print()

br_time_pa.2010 <- system.time(
    
    for (i in 1:length(teams)) {
        
        br_data_pa.2010.1 <-
            get_br_data(
                events = "pa",
                from_year = 2010,
                back_to_year = 2010,
                team = teams[i]
            ) %>%
            bind_rows(br_data_pa.2010.1)
        
    }
)
now() %>% print()

br_data_pa.2010.1 %>% count(batter_team) %>% print(n = 30)

br_data_pa.2010.1 <- br_data_pa.2010.1 %>% arrange(desc(game_date))

write_rds(br_data_pa.2010.1, "br_data_pa.2010.1.RDS")

##########################


br_data_pa.2009.2 <- data.frame()


now() %>% print()

br_time_pa.2009 <- system.time(
    
    for (i in 5:length(teams)) {
        
        br_data_pa.2009.2 <-
            get_br_data(
                events = "pa",
                from_year = 2009,
                back_to_year = 2009,
                team = teams[i]
            ) %>%
            bind_rows(br_data_pa.2009.2)
        
    }
)

now() %>% print()


br_data_pa.2009.2 %>% count(batter_team) %>% print(n = 30)

br_data_pa.2009.2 <- br_data_pa.2009.2 %>% arrange(desc(game_date))

write_rds(br_data_pa.2009.2, "br_data_pa.2009.2.RDS")

#####

br_data_pa.2009.3 <- bind_rows(br_data_pa.2009.2, br_data_pa.2009.1)

br_data_pa.2009.3 %>% count(batter_team) %>% print(n = 30)

br_data_pa.2009.3 <- br_data_pa.2009.3 %>% arrange(desc(game_date))

write_rds(br_data_pa.2009.3, "br_data_pa.2009.RDS")

##########################


br_data_pa.2008.1 <- data.frame()

now() %>% print()

br_time_pa.2008 <- system.time(
    
    for (i in 1:length(teams)) {
        
        br_data_pa.2008.1 <-
            get_br_data(
                events = "pa",
                from_year = 2008,
                back_to_year = 2008,
                team = teams[i]
            ) %>%
            bind_rows(br_data_pa.2008.1)
        
    }
)
now() %>% print()

br_data_pa.2008.1 %>% count(batter_team) %>% print(n = 30)

br_data_pa.2008.1 <- br_data_pa.2008.1 %>% arrange(desc(game_date))

write_rds(br_data_pa.2008.1, "br_data_pa.2008.1.RDS")

##########################


br_data_pa.2007.1 <- data.frame()

now() %>% print()

br_time_pa.2007 <- system.time(
    
    for (i in 1:length(teams)) {
        
        br_data_pa.2007.1 <-
            get_br_data(
                events = "pa",
                from_year = 2007,
                back_to_year = 2007,
                team = teams[i]
            ) %>%
            bind_rows(br_data_pa.2007.1)
        
    }
)
now() %>% print()

br_data_pa.2007.1 %>% count(batter_team) %>% print(n = 30)

br_data_pa.2007.1 <- br_data_pa.2007.1 %>% arrange(desc(game_date))

write_rds(br_data_pa.2007.1, "br_data_pa.2007.1.RDS")

##########################


br_data_pa.2006.1 <- data.frame()

now() %>% print()

br_time_pa.2006 <- system.time(
    
    for (i in 1:length(teams)) {
        
        br_data_pa.2006.1 <-
            get_br_data(
                events = "pa",
                from_year = 2006,
                back_to_year = 2006,
                team = teams[i]
            ) %>%
            bind_rows(br_data_pa.2006.1)
        
    }
)
now() %>% print()

br_data_pa.2006.1 %>% count(batter_team) %>% print(n = 30)

br_data_pa.2006.1 <- br_data_pa.2006.1 %>% arrange(desc(game_date))

write_rds(br_data_pa.2006.1, "br_data_pa.2006.1.RDS")

##########################


###################


'
Pit(cnt) -- Pitches (Balls-Strikes)
Pitches Seen in PA with abbreviations
Click to see the non-abbreviated pitch-by-pitch sequence
C--called strike
S--swinging strike
F--foul
B--ball
X--ball put into play by batter

T--foul tip
K--strike (unknown type)
I--intentional ball
H--hit batter
L--foul bunt
M--missed bunt attempt
N--no pitch (on balks and interference calls)
O--foul tip on bunt
P--pitchout
Q--swinging on pitchout
R--foul ball on pitchout
U--unknown or missed pitch
V--called ball because pitcher went to his mouth
Y--ball put into play on pitchout
1--pickoff throw to first
2--pickoff throw to second
3--pickoff throw to third
>--Indicates a runner going on the pitch
+--following pickoff throw by the catcher
*--indicates the following pitch was blocked by the catcher.
.--marker for play not involving the batter

'
###################################################

# <h4>Custom Statistic Report: Team Batting</h4><form action='index.php' method='post'>
# <input type='hidden' name='mystatslist' value='TEAM,LG,year,G,PA,BATTED_BALL_TYPE_KNOWN,FB,GB,LINEDR,POPUP,BABIP,DP_PERCENT,FB_PERCENT,GB_PERCENT,LINEDR_PERCENT,POPUP_PERCENT'>
# <input type='hidden' name='category' value='team_batting'>
# <input type='hidden' name='tablename' value='dyna_team_batting'>
# <input type='hidden' name='stage' value='data'>

# TEAM,LG,year,G,PA,BATTED_BALL_TYPE_KNOWN,FB,GB,LINEDR,POPUP,BABIP,DP_PERCENT,FB_PERCENT,GB_PERCENT,LINEDR_PERCENT,POPUP_PERCENT


###################################################

# http://www.baseballprospectus.com/sortable/index.php?mystatslist=NAME,year,LVL,G,PA,AB,BATTED_BALL_TYPE_KNOWN,FB,GB,LINEDR,POPUP,BUNT,BABIP,DP_PERCENT,FB_PERCENT,GB_PERCENT,LINEDR_PERCENT,POPUP_PERCENT,BUNT_HIT,R,H,B1,B2,B3,HR,TB,BB,IBB,SO,HBP,SF,SH,RBI,DP,NETDP,SB,CS,AVG,OBP,SLG,OPS,ISO,BPF,OPP_QUAL_OPS,EQA,VORP,FRAA,BWARP,OPP_QUAL_AVG,OPP_QUAL_OBP,OPP_QUAL_SLG,OPP_QUAL_TAV,OPP_QUAL_RPA_PLUS,PITCHES,ZONE_RT,SWING_RT,CONTACT_RT,Z_SWING_RT,O_SWING_RT,Z_CONTACT_RT,O_CONTACT_RT,SW_STRK_RT&category=batter_season&tablename=dyna_batter_season&stage=data&year=2017&group_LVL=MLB&group_PRIMARY_POS_LABEL=*&minimum=0&sort1column=NAME&sort1order=ASC&page_limit=20&glossary_terms=*&tt_team=*&show_ttroster=1&show_ttwatched=1&viewdata=View%20Data&start_num=0


###################################################

years <- 2005:1950

###################################################

# bp_data <- data.frame()
        
for (i in 1:length(years)) {
    
    bp_data <-
        xml2::read_html(
            paste(
                "http://www.baseballprospectus.com/sortable/index.php?mystatslist=NAME,year,LVL,G,PA,AB,BATTED_BALL_TYPE_KNOWN,FB,GB,LINEDR,POPUP,BUNT,BABIP,DP_PERCENT,FB_PERCENT,GB_PERCENT,LINEDR_PERCENT,POPUP_PERCENT,BUNT_HIT,R,H,B1,B2,B3,HR,TB,BB,IBB,SO,HBP,SF,SH,RBI,DP,NETDP,SB,CS,AVG,OBP,SLG,OPS,ISO,BPF,OPP_QUAL_OPS,EQA,VORP,FRAA,BWARP,OPP_QUAL_AVG,OPP_QUAL_OBP,OPP_QUAL_SLG,OPP_QUAL_TAV,OPP_QUAL_RPA_PLUS,PITCHES,ZONE_RT,SWING_RT,CONTACT_RT,Z_SWING_RT,O_SWING_RT,Z_CONTACT_RT,O_CONTACT_RT,SW_STRK_RT&category=batter_season&tablename=dyna_batter_season&stage=data",
                
                "&year=",
                years[i],
                
                "&group_LVL=MLB&group_PRIMARY_POS_LABEL=*&minimum=0&sort1column=NAME&sort1order=ASC&page_limit=2000&glossary_terms=*&tt_team=*&show_ttroster=1&show_ttwatched=1&viewdata=View%20Data&start_num=0",
                
                sep = ""
            )
        ) %>%
        
        rvest::html_nodes("table") %>%
        magrittr::extract2(5) %>%
        rvest::html_table(header = TRUE) %>%
        bind_rows(bp_data)
    
}

###################################################


bp_data %>% count(year) %>% print(n = 100)

bp_data %>% count(year) %>% nrow()

write_rds(bp_data, "baseball_prospectus_data.RDS")



bp_data %>% count(NAME) %>% print(n = 10)

bp_data %>% count(NAME) %>% nrow()

bp_data %>% count(NAME) %>% count(n) %>% print(n = 100)



###################################################

expected_runs <- function(years) {
    
    bp_data <-
        xml2::read_html(
            paste(
                "http://www.baseballprospectus.com/sortable/index.php?mystatslist=year,LVL,runners,exp_r_outs_0,exp_r_outs_1,exp_r_outs_2&category=expected_runs&tablename=dyna_expected_runs&stage=data",
                
                "&year=",
                years,
                
                sep = ""
            )
        ) %>%
        
        rvest::html_nodes("body") %>%
        magrittr::extract2(1) %>%
        rvest::html_nodes("table") %>%
        magrittr::extract(5) %>%
        rvest::html_table(header = TRUE)
    
    return(bp_data)
    
}

###################################################


years <- seq(1950, 2017, 1) %>% rev()


bp_data_er <- sapply(years, expected_runs)

bp_data_er_all <- 
    bind_rows(bp_data_er) %>% 
    select(-`#`, -LVL) %>% 
    rename(
        year = YEAR,
        runners = RUNNERS,
        exp_r_outs_0 = EXP_R_OUTS_0,
        exp_r_outs_1 = EXP_R_OUTS_1,
        exp_r_outs_2 = EXP_R_OUTS_2
        ) %>% 
    mutate(
        runners = as.character(runners),
        runners = as.factor(runners),
        runners = recode_factor(
            runners,
            "0"   = "000",
            "100" = "100",
            "20"  = "020",
            "3"   = "003",
            "120" = "120",
            "102" = "103",
            "23"  = "023",
            "123" = "123"
        )
    ) %>% 
    as_tibble()

write_csv(bp_data_er_all, "bp_data_er_all.csv")


bp_data_er_all_long <-
    bp_data_er_all %>%
    
    gather(
        outs,
        exp_r,
        exp_r_outs_0,
        exp_r_outs_1,
        exp_r_outs_2) %>% 
    mutate(
        outs = recode(
            outs,
            "exp_r_outs_0" = "outs_0",
            "exp_r_outs_1" = "outs_1",
            "exp_r_outs_2" = "outs_2"
        )
    ) %>% 
    mutate(
        year = as.factor(year),
        outs = as.factor(outs)
    ) %>%
    as_tibble()

bp_data_er_all_long$year %>% levels()
bp_data_er_all_long$runners %>% levels()
bp_data_er_all_long$outs %>% levels()

###################################################

bp_data_er_all <- read_csv("bp_data_er_all.csv")

bp_data_er_all <- 
    bp_data_er_all %>% 
    mutate(
        runners = factor(
            runners,
            levels = c(
                "000",
                "100",
                "020",
                "003",
                "120",
                "103",
                "023",
                "123")),
        year = as.factor(year))

bp_data_er_all$year %>% levels()
bp_data_er_all$runners %>% levels()

write_rds(bp_data_er_all, "bp_data_er_all.RDS")

###################################################


bp_data_er_all_long <-
    bp_data_er_all %>%
    
    gather(
        outs,
        exp_r,
        exp_r_outs_0,
        exp_r_outs_1,
        exp_r_outs_2) %>% 
    mutate(
        outs = recode(
            outs,
            "exp_r_outs_0" = "outs_0",
            "exp_r_outs_1" = "outs_1",
            "exp_r_outs_2" = "outs_2"
        )
    ) %>% 
    mutate(
        year = as.factor(year),
        outs = as.factor(outs)
    ) %>%
    as_tibble()

bp_data_er_all_long$year %>% levels()
bp_data_er_all_long$runners %>% levels()
bp_data_er_all_long$outs %>% levels()

write_csv(bp_data_er_all_long, "bp_data_er_all_long.csv")
write_rds(bp_data_er_all_long, "bp_data_er_all_long.RDS")


###################################################


bp_data_er_all <- read_rds("bp_data_er_all.RDS")


bp_data_er_all %>% 
    group_by(runners) %>%
    # group_by(runners, outs) %>% 
    summarize(
        exp_r_outs_0 = mean(exp_r_outs_0),
        exp_r_outs_1 = mean(exp_r_outs_1),
        exp_r_outs_2 = mean(exp_r_outs_2)
    ) %>%
    # arrange(outs, runners) %>% 
    arrange(runners) %>% 
    mutate_if(is.double, round, digits = 3)




###################################################

bp_data_er_all_long %>% 
    group_by(outs, runners) %>%
    # group_by(runners, outs) %>% 
    summarize(exp_r_mean = mean(exp_r)) %>% 
    # arrange(outs, runners) %>% 
    arrange(runners, outs) %>% 
    mutate_if(is.double, round, digits = 3) %>% 
    write_csv("bp_data_er_means.csv")



###################################################

bp_data_er_all_long %>% 
    filter(runners %in% c("020", "003", "000")) %>%
    group_by(outs, runners) %>%
    # group_by(runners, outs) %>% 
    summarize(exp_r_mean = mean(exp_r)) %>% 
    # arrange(outs, runners) %>% 
    arrange(runners, outs) %>% 
    mutate_if(is.double, round, digits = 3) %>% 
    print(n = 30)

# not advancing to 3rd:    
# RE = -0.418
# advancing safely to 3rd: 
# RE = -0.176
# = 0.242

# not advancing to 3rd:      
# RE = -0.418
# getting thrown out at 3rd: 
# RE = -0.570
# = -0.152

# failing to advance to 3rd: RE = -0.418
# getting thrown out at 3rd (vs. staying at 2nd): RE = -0.570

# 


###################################################

library(MASS)
library(car)
library(lme4)
library(broom)
library(merTools)
detach(package:dplyr)
library(dplyr)
detach(package:tidyr)
library(tidyr)

setwd("~/BASP/R analyses")
source("tidy.lmerTest.R")
source("ICC_comp.R")
setwd("~/BASP/R analyses/Baseball Data/Data Files")



bp_data_er_all_long %>% glimpse()

contrasts(bp_data_er_all_long$year) <- "contr.Sum"
contrasts(bp_data_er_all_long$runners) <- "contr.Sum"
contrasts(bp_data_er_all_long$outs) <- "contr.sdif"


###############################


mod.1 <- lmer(exp_r ~ runners * outs + (1 | year), data = bp_data_er_all_long)

ICC_comp(mod.1)
summary(mod.1)
arm::display(mod.1)
tidy(mod.1)
glance(mod.1)


coef(mod.1) %>% 
    extract2(1) %>% 
    as.data.frame(.) %>% 
    round(digits = 2) %>% 
    # t(.) %>% 
    as.data.frame() %>% 
    rownames_to_column() %>% 
    as_tibble() 


fixef(mod.1) %>% 
    round(digits = 2) %>% 
    as.data.frame() %>% 
    rownames_to_column(.) %>% 
    as_tibble()


ranef(mod.1) %>% 
    extract2(1) %>% 
    round(digits = 2) %>% 
    as.data.frame() %>% 
    rownames_to_column(.) %>% 
    as_tibble() %>% print(n = 70)


plot(mod.1, year ~ resid(., scaled = TRUE), alpha = .25, pch = 1)

plot(mod.1, 
     resid(., scaled = TRUE) ~ fitted(.) | year, 
     alpha = .25, 
     pch = 16)


plotREsim(REsim(mod.1, n.sims = 500, seed = 726),
          stat = "median",
          labs = TRUE)




###############################


mod.2 <- lmer(exp_r ~ 1 + (runners + outs | year), data = bp_data_er_all_long)

ICC_comp(mod.2)
summary(mod.2)
arm::display(mod.2)
tidy(mod.2)
glance(mod.2)


coef(mod.2) %>% 
    extract2(1) %>% 
    as.data.frame(.) %>% 
    round(digits = 2) %>% 
    # t(.) %>% 
    as.data.frame() %>% 
    rownames_to_column() %>% 
    as_tibble() 


fixef(mod.2) %>% 
    round(digits = 2) %>% 
    as.data.frame() %>% 
    rownames_to_column(.) %>% 
    as_tibble()


ranef(mod.2) %>% 
    extract2(1) %>% 
    round(digits = 2) %>% 
    as.data.frame() %>% 
    rownames_to_column(.) %>% 
    as_tibble() %>% print(n = 70)


plot(mod.2, year ~ resid(., scaled = TRUE), alpha = .25, pch = 1)

plot(mod.2, 
     resid(., scaled = TRUE) ~ fitted(.) | year, 
     alpha = .25, 
     pch = 16)


plotREsim(REsim(mod.2, n.sims = 500, seed = 726),
          stat = "median",
          labs = TRUE)



###################################################



unzip(
    zipfile = "war_archive-2013-05-03.zip",
    exdir = "WAR files/war_archive-2013-05-03"
)

war_archive.2013.05.03 <-
    read_csv("WAR files/war_archive-2013-05-03/war_daily_bat.txt") %>% 
    as.tibble()


unzip(
    zipfile = "war_archive-2013-05-02.zip",
    exdir = "WAR files/war_archive-2013-05-02"
)

war_archive.2013.05.02 <-
    read_csv("WAR files/war_archive-2013-05-02/war_daily_bat.txt") %>% 
    as.tibble()


unzip(
    zipfile = "war_archive-2013-05-01.zip",
    exdir = "WAR files/war_archive-2013-05-01"
)

war_archive.2013.05.01 <-
    read_csv("WAR files/war_archive-2013-05-01/war_daily_bat.txt") %>% 
    as.tibble()



glimpse(war_archive.2013.05.01)
glimpse(war_archive.2013.05.02)
glimpse(war_archive.2013.05.03)

war_archive.2013.05.01 %>% count(name_common) %>% arrange(-n)


war_archive.2013.05.01 %>% filter(player_ID == "thomafr04") %>% glimpse()


library(cfbplotR)
library(cfbfastR)
library(tidyverse)
Sys.setenv(CFBD_API_KEY = "x1C/67YV6Sy98uENGd+tSvJSr82NfDxHFTmWk4QB5wGl2qxogM53QKLB5T4l6kPn")

team_info= cfbd_team_info()
write.csv(team_info,"C:\\Users\\Harrison Eller\\CFB_Analytics\\team_info.csv")

ht=c('Alabama')
at=c("Texas")

input_season = 2023
week = 2
include_scarlett = 'yes'

df = data.frame()
#conference = c('SEC')

fbs = cfbd_conferences()
fbs = subset(fbs, classification == 'fbs')
fbs = fbs[1:10,]
fbs_teams = load_cfb_teams()
fbs_teams = subset(fbs_teams, classification == 'fbs')


# team setup
# ht = cfbd_game_info(2022, week = week, conference = conference)$home_team[1]
# home_team_filter = ht = cfbd_game_info(2022, week = week, conference = conference)$home_team[1]
# at = cfbd_game_info(2022, week = week, conference = conference)$away_team[1]
ht=c('Tulane')
at=c("Ole Miss")



stats <- cfbd_stats_season_team(input_season)
TEAMS <- cfbd_stats_season_advanced(input_season)
TEAMS <- inner_join(stats,TEAMS,by = 'team')
TEAMS$Total_YPG <- TEAMS$total_yds / (week-1)
TEAMS$Passing_YPG <- TEAMS$net_pass_yds / (week-1)
TEAMS$Rushing_YPG <- TEAMS$rush_yds / (week-1)
TEAMS$Passing_YPA <- TEAMS$net_pass_yds / TEAMS$pass_atts
TEAMS$Passing_YPC <- TEAMS$net_pass_yds / TEAMS$pass_comps
TEAMS$Rushing_YPA <- TEAMS$rush_yds / TEAMS$rush_atts
TEAMS$third_down_conv_rate<- TEAMS$third_down_convs / TEAMS$third_downs




lines <- cfbd_betting_lines(year = 2023, week = week, team = ht)
line <- subset(lines,provider == "DraftKings")
if (nrow(line) < 1 | is.na(line$over_under[1]) | is.na(line$spread[1]) ){line <- subset(lines,provider == "Bovada")}
# bet$home_score <- ifelse(as.numeric(bet$spread) < 0, as.numeric(bet$over_under) + as.numeric(bet$spread),  as.numeric(bet$over_under) - as.numeric(bet$spread))
# 
# bet$away_score <- as.numeric(bet$over_under) - as.numeric(bet$home_score)



bet <- data.frame(home_score=0,away_score=0,total=0,home_team=0,away_team=0,spread=0, home_conference=0)
bet$total <- as.numeric(line$over_under[1])
bet$spread <- as.numeric(line$spread[1])
bet$home_score <- ifelse(bet$spread  < 0, (bet$total/2)-(bet$spread/2), (bet$total/2)+(bet$spread/2))
bet$away_score <- bet$total - bet$home_score 

bet$home_team <- ht
bet$away_team <- at



df <- inner_join(team_info,TEAMS,by = c("school"="team"))
lines <- cfbd_betting_lines(year = 2023, week = week)
write.csv(lines,"game_lines.csv")


library(dplyr)
library(readr)
library(cfbfastR)
#remotes::install_github(repo = "sportsdataverse/cfbfastR")
#remotes::install_github(repo = "sportsdataverse/cfbplotR")
library(cfbplotR)
Sys.setenv(CFBD_API_KEY = "x1C/67YV6Sy98uENGd+tSvJSr82NfDxHFTmWk4QB5wGl2qxogM53QKLB5T4l6kPn")
#Sys.setenv(CFBD_API_KEY = "OMFtwopAS5WexLsewwy5BKQsUIzguwFqGz6KkjiUc6zcpKNYphzld/71fWW7pt8j")
library(stringr)
library(tidyverse)
library(nnet)
library(mgcv)
library(texreg)
library(aod)
library(xtable)
library(xgboost)
library(readr)
library(stringr)
library(caret)
library(car)
library(tidyverse)
library(gt)
#setwd("C:/Users/harri/OneDrive/Desktop/LTB/CFB Analysis")
#setwd("C:/Users/Harrison Eller/CFB_Analytics")


conferences<-c('SEC','ACC','B1G','PAC','B12')
year <-2024
week<-1
current_elo <- cfbd_ratings_elo(year = year-1) #cfbd_ratings_elo(year = year, week = week)
###### gather team game by game data #######

df = tibble()
for (conf in conferences) {
  
  temp_df_1<-cfbd_game_team_stats(year = year-1,conference = conf)
  temp_df_1['season'] = year-1
  temp_df_2<-cfbd_game_team_stats(year = year,conference = conf)
  temp_df_2['season'] = year
  temp_df<-rbind(temp_df_1,temp_df_2)
  df<-rbind(df,temp_df)
}


temp1<-cfbd_stats_game_advanced(year=year-1) # use for individual teams as well 
temp2<-cfbd_stats_game_advanced(year=year)
temp<-rbind(temp1,temp2)
#temp2 <- df %>% distinct(game_id,.keep_all = TRUE)
df2 <- df %>% inner_join(temp,by= c('school'='team', 'game_id'='game_id'))
df2<-df2 %>% rename('opponent'='opponent.x')
###### gather year over year elo list #######

df2['team_elo'] = 0
df2['opponent_elo'] = 0
df2['recruting_rating'] = 0
df2['qb_rating'] = 0
df2['opponent_recruiting_rating'] = 0
df2['opponent_qb_rating'] = 0



for (y in (year-1):year){
  recruiting <- cfbd_recruiting_position(start_year = year-4,end_year = year)
  
for (j in 1:max(df2$week)){
  temp_elo = cfbd_ratings_elo(year=y, week=j)
  for (i in 1:nrow(df2[which(df2$week == j & df2$season== y),] )) {
    
    school<-  pull(df2[which(df2$week == j & df2$season == y),][i,'school'])
    opponent <-  pull(df2[which(df2$week == j & df2$season == y),][i,'opponent'])
    
    ### getting team elo ###
    if(pull(df2[which(df2$week == j & df2$season == y),][i,'school']) %in% temp_elo$team ){
        df2[which(df2$week == j  & df2$season == y),][i,'team_elo'] <- pull(temp_elo[which(temp_elo$team == school),'elo'])}
    else{ df2[which(df2$week == j & df2$season == y),][i,'team_elo'] <- 1000}
    
    ### getting opponent elo ###
    if(pull(df2[which(df2$week == j & df2$season == y),][i,'opponent']) %in% temp_elo$team ){
      df2[which(df2$week == j & df2$season == y),][i,'opponent_elo'] <- pull(temp_elo[which(temp_elo$team == opponent),'elo'])}
    else{ df2[which(df2$week == j & df2$season == y),][i,'opponent_elo'] <- 1000
    }
    
    ### getting team recuriting ###
    if(pull(df2[which(df2$week == j & df2$season == y),][i,'school']) %in% recruiting$team ){
      df2[which(df2$week == j  & df2$season == y),][i,'recruting_rating'] <- pull(recruiting[which(recruiting$team == school & recruiting$position_group == 'All Positions'),'avg_rating'])
      df2[which(df2$week == j  & df2$season == y),][i,'qb_rating'] <- pull(recruiting[which(recruiting$team == school & recruiting$position_group == 'Quarterback'),'avg_rating'])}
    else{ df2[which(df2$week == j  & df2$season == y),][i,'recruting_rating'] <- .6
          df2[which(df2$week == j  & df2$season == y),][i,'qb_rating'] <- .6
    }
    
    ### getting opponent recuriting ###
    if(pull(df2[which(df2$week == j & df2$season == y),][i,'opponent']) %in% recruiting$team ){
      df2[which(df2$week == j  & df2$season == y),][i,'opponent_recruiting_rating'] <- pull(recruiting[which(recruiting$team == opponent & recruiting$position_group == 'All Positions'),'avg_rating'])
      df2[which(df2$week == j  & df2$season == y),][i,'opponent_qb_rating'] <- pull(recruiting[which(recruiting$team == opponent & recruiting$position_group == 'Quarterback'),'avg_rating'])}
    else{ df2[which(df2$week == j  & df2$season == y),][i,'opponent_recruiting_rating'] <- .6
    df2[which(df2$week == j  & df2$season == y),][i,'opponent_qb_rating'] <- .6
    }

    
  } #end of game loop 
} #end of week loop
  
}#end year loop

cols <- c('points', 'points_allowed', 'team_elo', 'opponent_elo','recruting_rating','qb_rating','opponent_recruiting_rating','opponent_qb_rating',
       'yards_per_pass','yards_per_pass_allowed', 'yards_per_rush_attempt','yards_per_rush_attempt_allowed','turnovers', 'turnovers_allowed',
       'off_success_rate','def_success_rate','def_drives','off_drives')
for (col in cols){
if (is.character(df2[col])){
    print(col)
  df2[col]<-as.double(df2[col])
  }
}

data <- df2 %>% 
  select(points, points_allowed, team_elo, opponent_elo,recruting_rating,qb_rating,opponent_recruiting_rating,opponent_qb_rating,
         yards_per_pass,yards_per_pass_allowed, yards_per_rush_attempt,yards_per_rush_attempt_allowed,turnovers, turnovers_allowed,
         home_away,off_success_rate,def_success_rate,def_drives,off_drives)

data$yards_per_pass <- as.numeric(data$yards_per_pass)
data$yards_per_pass_allowed <- as.numeric(data$yards_per_pass_allowed)
data$yards_per_rush_attempt <- as.numeric(data$yards_per_rush_attempt)
data$yards_per_rush_attempt_allowed <- as.numeric(data$yards_per_rush_attempt_allowed)
data$turnovers <- as.numeric(data$turnovers)
data$turnovers_allowed <- as.numeric(data$turnovers_allowed)

model <- lm(points ~ ., data = data)
summary(model)

















######################### generate projections ######################

ht <- c('Tennessee','Tennessee')
at <- c('Alabama','Oklahoma')
projections <- tibble()
conference_input = c('SEC')
if (length(conference_input) > 0){
ht<-cfbd_game_info(2024, week = week, conference = 'SEC')$home_team
at<-cfbd_game_info(2024, week = week, conference = 'SEC')$away_team
}
for (j in 1:length(ht)){



# team_elo
# recruting_rating,
# qb_rating,
# opponent_recruiting_rating,
# opponent_qb_rating,
# home_away

home <- df2 %>% 
  filter(school == ht[j]) %>% 
  summarize(
    points = median(as.double(points), na.rm =TRUE),
    points_allowed = median(as.double(points_allowed), na.rm =TRUE), 
    yards_per_pass = median(as.double(yards_per_pass), na.rm =TRUE),
    yards_per_pass_allowed = median(as.double(yards_per_pass_allowed), na.rm =TRUE), 
    yards_per_rush_attempt = median(as.double(yards_per_rush_attempt), na.rm =TRUE),
    yards_per_rush_attempt_allowed = median(as.double(yards_per_rush_attempt_allowed), na.rm =TRUE),
    turnovers = median(as.double(turnovers), na.rm =TRUE), 
    turnovers_allowed = median(as.double(turnovers_allowed), na.rm =TRUE),
    off_success_rate = median(as.double(off_success_rate), na.rm =TRUE),
    def_success_rate = median(as.double(def_success_rate), na.rm =TRUE),
    def_drives = median(as.double(def_drives), na.rm =TRUE),
    off_drives = median(as.double(off_drives), na.rm =TRUE)
  )



away <- df2 %>% 
  filter(school == at[j]) %>% 
  summarize(
    points = median(as.double(points), na.rm =TRUE),
    points_allowed = median(as.double(points_allowed), na.rm =TRUE), 
    yards_per_pass = median(as.double(yards_per_pass), na.rm =TRUE),
    yards_per_pass_allowed = median(as.double(yards_per_pass_allowed), na.rm =TRUE), 
    yards_per_rush_attempt = median(as.double(yards_per_rush_attempt), na.rm =TRUE),
    yards_per_rush_attempt_allowed = median(as.double(yards_per_rush_attempt_allowed), na.rm =TRUE),
    turnovers = median(as.double(turnovers), na.rm =TRUE), 
    turnovers_allowed = median(as.double(turnovers_allowed), na.rm =TRUE),
    off_success_rate = median(as.double(off_success_rate), na.rm =TRUE),
    def_success_rate = median(as.double(def_success_rate), na.rm =TRUE),
    def_drives = median(as.double(def_drives), na.rm =TRUE),
    off_drives = median(as.double(off_drives), na.rm =TRUE)
  )



new_data_home<- tibble(points=(median(home$points)+median(away$points_allowed))/2, 
              points_allowed = (median(home$points_allowed)+median(away$points))/2,
              yards_per_pass = (median(home$yards_per_pass)+median(away$yards_per_pass_allowed))/2,
              yards_per_pass_allowed = (median(home$yards_per_pass_allowed)+median(away$yards_per_pass))/2,
              yards_per_rush_attempt = (median(home$yards_per_rush_attempt)+median(away$yards_per_rush_attempt_allowed))/2,
              yards_per_rush_attempt_allowed = (median(home$yards_per_rush_attempt_allowed)+median(away$yards_per_rush_attempt))/2,
              turnovers = (median(home$turnovers)+median(away$turnovers_allowed))/2,
              turnovers_allowed = (median(home$turnovers_allowed)+median(away$turnovers))/2,
              off_success_rate = (median(home$off_success_rate)+median(away$def_success_rate))/2,
              def_success_rate = (median(home$def_success_rate)+median(away$off_success_rate))/2,
              def_drives = (median(home$def_drives)+median(away$off_drives))/2,
              off_drives = (median(home$off_drives)+median(away$def_drives))/2, 
              home_away = 'home',
              team_elo = pull(current_elo[which(current_elo$team == ht[j]),'elo']),
              opponent_elo = pull(current_elo[which(current_elo$team == at[j]),'elo']),
              recruting_rating = pull(recruiting[which(recruiting$position_group == 'All Positions' & recruiting$team == ht[j]),'avg_rating']),
              qb_rating = pull(recruiting[which(recruiting$position_group == 'Quarterback' & recruiting$team == ht[j]),'avg_rating']),
              opponent_recruiting_rating = pull(recruiting[which(recruiting$position_group == 'All Positions' & recruiting$team == at[j]),'avg_rating']),
              opponent_qb_rating = pull(recruiting[which(recruiting$position_group == 'Quarterback' & recruiting$team == at[j]),'avg_rating']),
)



new_data_away <- tibble(points=(median(away$points)+median(home$points_allowed))/2, 
                       points_allowed = (median(away$points_allowed)+median(home$points))/2,
                       yards_per_pass = (median(away$yards_per_pass)+median(home$yards_per_pass_allowed))/2,
                       yards_per_pass_allowed = (median(away$yards_per_pass_allowed)+median(home$yards_per_pass))/2,
                       yards_per_rush_attempt = (median(away$yards_per_rush_attempt)+median(home$yards_per_rush_attempt_allowed))/2,
                       yards_per_rush_attempt_allowed = (median(away$yards_per_rush_attempt_allowed)+median(home$yards_per_rush_attempt))/2,
                       turnovers = (median(away$turnovers)+median(home$turnovers_allowed))/2,
                       turnovers_allowed = (median(away$turnovers_allowed)+median(home$turnovers))/2,
                       off_success_rate = (median(away$off_success_rate)+median(home$def_success_rate))/2,
                       def_success_rate = (median(away$def_success_rate)+median(home$off_success_rate))/2,
                       def_drives = (median(away$def_drives)+median(home$off_drives))/2,
                       off_drives = (median(away$off_drives)+median(home$def_drives))/2, 
                       home_away = 'away',
                       team_elo = pull(current_elo[which(current_elo$team == at[j]),'elo']),
                       opponent_elo = pull(current_elo[which(current_elo$team == ht[j]),'elo']),
                       recruting_rating = pull(recruiting[which(recruiting$position_group == 'All Positions' & recruiting$team == at[j]),'avg_rating']),
                       qb_rating = pull(recruiting[which(recruiting$position_group == 'Quarterback' & recruiting$team == at[j]),'avg_rating']),
                       opponent_recruiting_rating = pull(recruiting[which(recruiting$position_group == 'All Positions' & recruiting$team == ht[j]),'avg_rating']),
                       opponent_qb_rating = pull(recruiting[which(recruiting$position_group == 'Quarterback' & recruiting$team == ht[j]),'avg_rating']),
)





y<- tibble( home = ht[j],
            home_score = predict(model,new_data_home),
            away = at[j],
            away_score = predict(model,new_data_away),
            home_spread = -(home_score - away_score),
            total = (home_score + away_score)
)

projections <- rbind(projections,y)

}






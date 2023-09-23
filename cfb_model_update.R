library(dplyr)
library(readr)
library(cfbfastR)
#remotes::install_github(repo = "sportsdataverse/cfbfastR")
remotes::install_github(repo = "sportsdataverse/cfbplotR")
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
#setwd("C:/Users/harri/OneDrive/Desktop/LTB/CFB Analysis")
#setwd("C:/Users/Harrison Eller/CFB_Analytics")




CFB_PROJECTIONS <- function(ht=c(),at=c(),input_week,input_season,conferences = c(),previous_season=1,remove_fcs = FALSE){
  
  
  week = input_week
  df = data.frame()
  games <- tibble()
  #conferences = c()
  # 'Ind','MAC','MWC','SBC'
  
  fbs = cfbd_conferences()
  fbs = subset(fbs, classification == 'fbs')
  fbs = fbs[1:10,]
  fbs_teams = load_cfb_teams()
  
  fbs_teams = subset(fbs_teams, classification == 'fbs')
  fbs_teams$conference_abbr <-''
  

  
  
  
  for (i in 1:nrow(fbs_teams)){
    fbs_teams$conference_abbr[i] <- fbs$abbreviation[which(fbs$name == fbs_teams$conference[i])][1]
  }


    
    if(length(conferences) >= 1){
      #ht = cfbd_game_info(2023, week = week, conference = conference)$home_team
      #at = cfbd_game_info(2023, week = week, conference = conference)$away_team
      
      
      
      
      
      for (c in conferences){
        group<- cfbd_game_info(2023, week = week, conference = c)
        games <- rbind(games,group,fill=TRUE)
      }
      
      games <- games[!duplicated(games$game_id),]
      ht <- games$home_team
      at <- games$away_team
      
      ht <- ht[-which(ht == TRUE)]
      at <- at[-which(at == TRUE)]
      
    }else{
      
      ht = ht
      at = at
      
    }
    if(remove_fcs == TRUE){
      remove = c()
      for(i in 1:length(ht)){
        if(ht[i] %in% fbs_teams$school && at[i] %in% fbs_teams$school ){print('!')}else{remove[i] = i}
      }
      remove<-remove[!is.na(remove)]
      if(is.null(remove) == F){ht = ht[-remove]}
      if(is.null(remove) == F){at = at[-remove]}
      
    }
    

    
    y = data.frame(matrix(ncol = 5, nrow = length(ht)))
    colnames(y) <- c('ht', 'at', 'ht_score', 'at_score', 'ht_spread') # nolint
    


    #######################  model #############################
    for (j in 1:length(ht)){
      
      
      home1 <- 0
      away1 <- 0
      
      print(ht[j])
      print(at[j])
      
      #enable current season
      if(previous_season == 1){
        home_stats1 = cfbd_game_team_stats(input_season-2, team = ht[j])
        away_stats1 = cfbd_game_team_stats(input_season-2, team = at[j])
        
        home_advanced1 = cfbd_stats_game_advanced(input_season-2, team = ht[j])
        away_advanced1 = cfbd_stats_game_advanced(input_season-2, team = at[j])
        
        home_ppa1 = cfbd_metrics_ppa_games(input_season-2, team = ht[j])
        away_ppa1 = cfbd_metrics_ppa_games(input_season-2, team = at[j])
        
        
        home_elo1 = cfbd_ratings_elo(year = input_season-2,team = ht[j])
        if (nrow(home_elo1) != 0){print("good elo data for home1")}else{
          home_elo1<-cfbd_ratings_elo(year = input_season-2)[1,]
          home_elo1$team <- ht[j]
          ifelse(nrow(games)<1,home_elo1$conference <-"NCAA" ,home_elo1$conference <- games$home_conference[which(games$home_team == ht[j])])
          home_elo1$elo <- 1300
        }
        away_elo1 = cfbd_ratings_elo(year = input_season-2,team = at[j])
        if (nrow(away_elo1) != 0){print("good elo data for away1")}else{
          away_elo1<-cfbd_ratings_elo(year = input_season-2)[1,]
          away_elo1$team <- at[j]
          ifelse(nrow(games)<1,away_elo1$conference <-"NCAA",away_elo1$conference <- games$away_conference[which(games$away_team == at[j])])
          away_elo1$elo <- 1300
        }
        
        home_stats1$game_id <- as.numeric(home_stats1$game_id )
        away_stats1$game_id <- as.numeric(away_stats1$game_id )
        if(length(home_stats1)>0){home1 = inner_join(home_stats1,home_ppa1,by = 'game_id')}
        if(length(away_stats1)>0){away1 = inner_join(away_stats1,away_ppa1,by = 'game_id')}
        
        
        
        if(length(home_elo1) >0){home1 = inner_join(home1,home_elo1, by = 'team')}
        if(length(away_elo1) >0){away1= inner_join(away1,away_elo1, by = 'team')}
        
        home_advanced1$game_id <- as.numeric(home_advanced1$game_id )
        away_advanced1$game_id <- as.numeric(away_advanced1$game_id )
        if(length(home_advanced1)>0){home1 = inner_join(home1,home_advanced1,by = 'game_id')}
        if(length(away_advanced1)>0){away1 = inner_join(away1,away_advanced1,by = 'game_id')}
        
        

        
        
      }
      # 
      
      home_stats2 = cfbd_game_team_stats(input_season-1, team = ht[j])
      away_stats2 = cfbd_game_team_stats(input_season-1, team = at[j])
      
      home_advanced2 = cfbd_stats_game_advanced(input_season-1, team = ht[j])
      away_advanced2 = cfbd_stats_game_advanced(input_season-1, team = at[j])
      
      
      home_ppa2 = cfbd_metrics_ppa_games(input_season-1, team = ht[j])
      away_ppa2 = cfbd_metrics_ppa_games(input_season-1, team = at[j])
      
      
      home_elo2 = cfbd_ratings_elo(year = input_season-1,team = ht[j])
      if (nrow(home_elo2) != 0){print("good elo data for home team")}else{
        home_elo2<-cfbd_ratings_elo(year = input_season-1)[1,]
        home_elo2$team <- ht[j]
        ifelse(nrow(games)<1,home_elo2$conference <- "NCAA" ,home_elo2$conference <- games$home_conference[which(games$home_team == ht[j])])
        home_elo2$elo <- 1300
      }
      away_elo2 = cfbd_ratings_elo(year = input_season-1,team = at[j])
      if (nrow(away_elo2) != 0){away_elo2 = cfbd_ratings_elo(year = input_season-1,team = at[j])}else{
        away_elo2<-cfbd_ratings_elo(year = input_season-1)[1,]
        away_elo2$team <- at[j]
        ifelse(nrow(games)<1,away_elo2$conference <-"NCAA",away_elo2$conference <- games$away_conference[which(games$away_team == at[j])])
        away_elo2$elo <- 1300
      }
      
      
      
      home2 = inner_join(home_stats2,home_ppa2,by = 'game_id')
      away2 = inner_join(away_stats2,away_ppa2,by = 'game_id')
      
      
      home2 = inner_join(home2,home_elo2, by = 'team')
      away2= inner_join(away2,away_elo2, by = 'team')
      
      home2 = inner_join(home2,home_advanced2,by = 'game_id')
      away2 = inner_join(away2,away_advanced2,by = 'game_id')
      
      if (previous_season==1){
        if(length(home1) > 0){home = rbind(home1, home2, fill = T)}else{home = home2}
        if(length(away1) > 0){away = rbind(away1, away2, fill = T)}else{away = away2}
      }
      
      
      
      
      home_stats3 = cfbd_game_team_stats(input_season, team = ht[j])
      away_stats3 = cfbd_game_team_stats(input_season, team = at[j])
      
      home_advanced3 = cfbd_stats_game_advanced(input_season, team = ht[j])
      away_advanced3 = cfbd_stats_game_advanced(input_season, team = at[j])
      
      
      home_ppa3 = cfbd_metrics_ppa_games(input_season, team = ht[j])
      away_ppa3 = cfbd_metrics_ppa_games(input_season, team = at[j])
      
      
      home_elo3 = cfbd_ratings_elo(year = input_season,team = ht[j])
      if (nrow(home_elo3) != 0){print("good elo data for home team")}else{
        home_elo3<-cfbd_ratings_elo(year = input_season)[1,]
        home_elo3$team <- ht[j]
        ifelse(nrow(games)<1,home_elo3$conference <- "NCAA" ,home_elo3$conference <- games$home_conference[which(games$home_team == ht[j])])
        home_elo3$elo <- 1300
      }
      away_elo3 = cfbd_ratings_elo(year = input_season,team = at[j])
      if (nrow(away_elo3) != 0){away_elo3 = cfbd_ratings_elo(year = input_season,team = at[j])}else{
        away_elo3<-cfbd_ratings_elo(year = input_season)[1,]
        away_elo3$team <- at[j]
        ifelse(nrow(games)<1,away_elo3$conference <-"NCAA",away_elo3$conference <- games$away_conference[which(games$away_team == at[j])])
        away_elo3$elo <- 1300
      }
      
      
      
      home3 = inner_join(home_stats3,home_ppa3,by = 'game_id')
      away3 = inner_join(away_stats3,away_ppa3,by = 'game_id')
      
      
      
      home3 = inner_join(home3,home_elo3, by = 'team')
      away3= inner_join(away3,away_elo3, by = 'team')
      
      home3 = inner_join(home3,home_advanced3,by = 'game_id')
      away3 = inner_join(away3,away_advanced3,by = 'game_id')
      
      if (previous_season == 1){
        if(length(home) > 0){home = rbind(home, home3, fill = T)}else{home = home}
        if(length(away) > 0){away = rbind(away, away3, fill = T)}else{away = away}
      }else{
        if(length(home2) > 0){home = rbind(home2, home3, fill = T)}else{home = home}
        if(length(away2) > 0){away = rbind(away2, away3, fill = T)}else{away = away}
      }
      
      home3 = as.data.frame(home3)
      away3= as.data.frame(away3)
      home = as.data.frame(home)
      away = as.data.frame(away)
 

      home['opp_elo'] = 0
      away['opp_elo'] = 0
      home['opp_def_overall'] = 0
      away['opp_def_overall'] = 0
      home['opp_def_rushing'] = 0
      home['opp_def_passing'] = 0
      away['opp_def_rushing'] = 0
      away['opp_def_passing'] = 0
      home['opp_off_rushing'] = 0
      home['opp_off_passing'] = 0
      away['opp_off_rushing'] = 0
      away['opp_off_passing'] = 0

      home['opp_turnovers'] = 0
      away['opp_turnovers'] = 0
      home['opp_total_yards'] = 0
      away['opp_total_yards'] = 0
      home['opp_ypp'] = 0
      away['opp_ypp'] = 0
      home['opp_ypr'] = 0
      away['opp_ypr'] = 0
      
      home['opp_off_success_rate'] = 0
      away['opp_off_success_rate'] = 0
      home['opp_off_explosiveness'] = 0
      away['opp_off_explosiveness'] = 0
      home['opp_off_power_success'] = 0
      away['opp_off_power_success'] = 0
      home['opp_def_success_rate'] = 0
      away['opp_def_success_rate'] = 0
      home['opp_def_explosiveness'] = 0
      away['opp_def_explosiveness'] = 0
      home['opp_def_power_success'] = 0
      away['opp_def_power_success'] = 0
      #changes
      
      
      home$opponent = home$opponent.x
      away$opponent = away$opponent.x
      home$home_dummy = 0
      away$home_dummy = 0
      
      
      for(i in 1:nrow(home)){
        if(is.na(home$year[i]) == T){home$year[i] = input_season-1}
        if(home$home_away[i] == 'home'){home$home_dummy[i]= 1}
      }
      for(i in 1:nrow(away)){
        if(is.na(away$year[i]) == T){away$year[i] = input_season-1}
        if(away$home_away[i] == 'home'){away$home_dummy[i] = 1}
      }
      
      
      historical_elo = data.frame()
      metrics_ppa = data.frame()
      game_stats_home = data.frame()
      game_stats_away = data.frame()
      advanced_stats = data.frame()
      #advanced_stats_away = data.frame()
      if(previous_season == 1){prev <- 2}else{prev<-1}
      
      for (s in (input_season-prev):(input_season)){
        temp1=cfbd_ratings_elo(year = s)
        historical_elo = rbind(historical_elo,temp1, fill = TRUE)
        
        temp2 = cfbd_metrics_ppa_games(year = s)
        metrics_ppa = rbind(metrics_ppa,temp2, fill = TRUE)
        
        temp3 = cfbd_game_team_stats(year = s, team = at[j])
        temp3['season'] = s
        game_stats_away = rbind(game_stats_away,temp3,fill=TRUE)
        
        temp5 = cfbd_game_team_stats(year = s,team = ht[j])
        temp5['season'] = s
        game_stats_home = rbind(game_stats_home,temp5,fill=TRUE)
        
        temp4 = cfbd_stats_game_advanced(year = s)
        temp4['season'] = s
        advanced_stats = rbind(advanced_stats,temp4,fill=TRUE)
        
        #    temp5 = cfbd_stats_game_advanced(year = s,team = at[j])
        #    advanced_stats_away = rbind(advanced_stats_away,temp5)
      }
      
      

      
      for (i in 1:nrow(home)){

        ################################### home
        
        home$opp_elo[i] = ifelse(is.null(historical_elo[which(historical_elo$team == home$opponent[i] & historical_elo$year == home$year[i]),]$elo) == T, 1000, historical_elo[which(historical_elo$team == home$opponent[i] & historical_elo$year == home$year[i]),]$elo)
        home$opp_def_overall[i] = ifelse(is.null(metrics_ppa[which(metrics_ppa$season == home$year[i] & metrics_ppa$team == home$opponent[i] & metrics_ppa$opponent == ht[j]),]$def_overall) == T, 0, metrics_ppa[which(metrics_ppa$season == home$year[i] & metrics_ppa$team == home$opponent[i] & metrics_ppa$opponent == ht[j]),]$def_overall)
        home$opp_off_overall[i] = ifelse(is.null(metrics_ppa[which(metrics_ppa$season == home$year[i] & metrics_ppa$team == home$opponent[i] & metrics_ppa$opponent == ht[j]),]$off_overall) == T, 0, metrics_ppa[which(metrics_ppa$season == home$year[i] & metrics_ppa$team == home$opponent[i] & metrics_ppa$opponent == ht[j]),]$off_overall)
        home$opp_off_rushing[i] = ifelse(is.null(metrics_ppa[which(metrics_ppa$season == home$year[i] & metrics_ppa$team == home$opponent[i] & metrics_ppa$opponent == ht[j]),]$off_rushing) == T, 0, metrics_ppa[which(metrics_ppa$season == home$year[i] & metrics_ppa$team == home$opponent[i] & metrics_ppa$opponent == ht[j]),]$off_rushing)
        home$opp_off_passing[i] = ifelse(is.null(metrics_ppa[which(metrics_ppa$season == home$year[i] & metrics_ppa$team == home$opponent[i] & metrics_ppa$opponent == ht[j]),]$off_passing) == T, 0, metrics_ppa[which(metrics_ppa$season == home$year[i] & metrics_ppa$team == home$opponent[i] & metrics_ppa$opponent == ht[j]),]$off_passing)
        home$opp_def_rushing[i] = ifelse(is.null(metrics_ppa[which(metrics_ppa$season == home$year[i] & metrics_ppa$team == home$opponent[i] & metrics_ppa$opponent == ht[j]),]$def_rushing) == T, 0, metrics_ppa[which(metrics_ppa$season == home$year[i] & metrics_ppa$team == home$opponent[i] & metrics_ppa$opponent == ht[j]),]$def_rushing)
        home$opp_def_passing[i] = ifelse(is.null(metrics_ppa[which(metrics_ppa$season == home$year[i] & metrics_ppa$team == home$opponent[i] & metrics_ppa$opponent == ht[j]),]$def_passing) == T, 0, metrics_ppa[which(metrics_ppa$season == home$year[i] & metrics_ppa$team == home$opponent[i] & metrics_ppa$opponent == ht[j]),]$def_passing)
        
        
        home$opp_turnovers[i] = ifelse(is.null(game_stats_home[which(game_stats_home$opponent == home$opponent[i] ),]$turnovers) == T, 0, game_stats_home[which(game_stats_home$opponent == home$opponent[i] ),]$turnovers)
        home$opp_total_yards[i] = ifelse(is.null(game_stats_home[which(game_stats_home$opponent == home$opponent[i] ),]$total_yards_allowed) == T, 0, game_stats_home[which(game_stats_home$opponent == home$opponent[i] ),]$total_yards_allowed)
        home$opp_ypp[i] = ifelse(is.null(game_stats_home[which(game_stats_home$opponent == home$opponent[i] ),]$yards_per_pass_allowed) == T, 0, game_stats_home[which(game_stats_home$opponent == home$opponent[i] ),]$yards_per_pass_allowed)
        home$opp_ypr[i] = ifelse(is.null(game_stats_home[which(game_stats_home$opponent == home$opponent[i] ),]$yards_per_rush_attempt_allowed) == T, 0,game_stats_home[which(game_stats_home$opponent == home$opponent[i] ),]$yards_per_rush_attempt_allowed)
        
        
        home$opp_off_success_rate[i]  = ifelse(is.null(advanced_stats[which(advanced_stats$team == home$opponent[i] & advanced_stats$season == home$year[i] & advanced_stats$opponent == ht[j]),]$off_success_rate) == T, 0, advanced_stats[which(advanced_stats$team == home$opponent[i] & advanced_stats$season == home$year[i] & advanced_stats$opponent == ht[j]),]$off_success_rate)
        home$opp_off_power_success[i]  = ifelse(is.null(advanced_stats[which(advanced_stats$team == home$opponent[i] & advanced_stats$season == home$year[i] & advanced_stats$opponent == ht[j]),]$off_power_success) == T, 0, advanced_stats[which(advanced_stats$team == home$opponent[i] & advanced_stats$season == home$year[i] & advanced_stats$opponent == ht[j]),]$off_power_success)
        home$opp_off_explosiveness[i]  = ifelse(is.null(advanced_stats[which(advanced_stats$team == home$opponent[i] & advanced_stats$season == home$year[i] & advanced_stats$opponent == ht[j]),]$off_explosiveness) == T, 0, advanced_stats[which(advanced_stats$team == home$opponent[i] & advanced_stats$season == home$year[i] & advanced_stats$opponent == ht[j]),]$off_explosiveness)
        home$opp_def_success_rate[i]  = ifelse(is.null(advanced_stats[which(advanced_stats$team == home$opponent[i] & advanced_stats$season == home$year[i] & advanced_stats$opponent == ht[j]),]$def_success_rate) == T, 0, advanced_stats[which(advanced_stats$team == home$opponent[i] & advanced_stats$season == home$year[i] & advanced_stats$opponent == ht[j]),]$def_success_rate)
        home$opp_def_power_success[i]  = ifelse(is.null(advanced_stats[which(advanced_stats$team == home$opponent[i] & advanced_stats$season == home$year[i] & advanced_stats$opponent == ht[j]),]$def_power_success) == T, 0, advanced_stats[which(advanced_stats$team == home$opponent[i] & advanced_stats$season == home$year[i] & advanced_stats$opponent == ht[j]),]$def_power_success)
        home$opp_def_explosiveness[i] = ifelse(is.null(advanced_stats[which(advanced_stats$team == home$opponent[i] & advanced_stats$season == home$year[i] & advanced_stats$opponent == ht[j]),]$def_explosiveness) == T, 0, advanced_stats[which(advanced_stats$team == home$opponent[i] & advanced_stats$season == home$year[i] & advanced_stats$opponent == ht[j]),]$def_explosiveness)
        
        
        
        
        print(i/nrow(home))
      }
      for (i in 1:nrow(away)){

        
        away$opp_elo[i] = ifelse(is.null(historical_elo[which(historical_elo$team == away$opponent[i] & historical_elo$year == away$year[i]),]$elo) == T, 1000, historical_elo[which(historical_elo$team == away$opponent[i] & historical_elo$year == away$year[i]),]$elo)
        away$opp_def_overall[i] = ifelse(is.null(metrics_ppa[which(metrics_ppa$season == away$year[i] & metrics_ppa$team == away$opponent[i] & metrics_ppa$opponent == at[j]),]$def_overall) == T, 0, metrics_ppa[which(metrics_ppa$season == away$year[i] & metrics_ppa$team == away$opponent[i] & metrics_ppa$opponent == at[j]),]$def_overall)
        away$opp_off_overall[i] = ifelse(is.null(metrics_ppa[which(metrics_ppa$season == away$year[i] & metrics_ppa$team == away$opponent[i] & metrics_ppa$opponent == at[j]),]$off_overall) == T, 0, metrics_ppa[which(metrics_ppa$season == away$year[i] & metrics_ppa$team == away$opponent[i] & metrics_ppa$opponent == at[j]),]$off_overall)
        away$opp_off_rushing[i] = ifelse(is.null(metrics_ppa[which(metrics_ppa$season == away$year[i] & metrics_ppa$team == away$opponent[i] & metrics_ppa$opponent == at[j]),]$off_rushing) == T, 0, metrics_ppa[which(metrics_ppa$season == away$year[i] & metrics_ppa$team == away$opponent[i] & metrics_ppa$opponent == at[j]),]$off_rushing)
        away$opp_off_passing[i] = ifelse(is.null(metrics_ppa[which(metrics_ppa$season == away$year[i] & metrics_ppa$team == away$opponent[i] & metrics_ppa$opponent == at[j]),]$off_passing) == T, 0, metrics_ppa[which(metrics_ppa$season == away$year[i] & metrics_ppa$team == away$opponent[i] & metrics_ppa$opponent == at[j]),]$off_passing)
        away$opp_def_rushing[i] = ifelse(is.null(metrics_ppa[which(metrics_ppa$season == away$year[i] & metrics_ppa$team == away$opponent[i] & metrics_ppa$opponent == at[j]),]$def_rushing) == T, 0, metrics_ppa[which(metrics_ppa$season == away$year[i] & metrics_ppa$team == away$opponent[i] & metrics_ppa$opponent == at[j]),]$def_rushing)
        away$opp_def_passing[i] = ifelse(is.null(metrics_ppa[which(metrics_ppa$season == away$year[i] & metrics_ppa$team == away$opponent[i] & metrics_ppa$opponent == at[j]),]$def_passing) == T, 0, metrics_ppa[which(metrics_ppa$season == away$year[i] & metrics_ppa$team == away$opponent[i] & metrics_ppa$opponent == at[j]),]$def_passing)
        
        
        away$opp_turnovers[i] = ifelse(is.null(game_stats_away[which(game_stats_away$opponent == away$opponent[i] ),]$turnovers) == T, 0, game_stats_away[which(game_stats_away$opponent == away$opponent[i] ),]$turnovers)
        away$opp_total_yards[i] = ifelse(is.null(game_stats_away[which(game_stats_away$opponent == away$opponent[i] ),]$total_yards_allowed) == T, 0, game_stats_away[which(game_stats_away$opponent == away$opponent[i] ),]$total_yards_allowed)
        away$opp_ypp[i] = ifelse(is.null(game_stats_away[which(game_stats_away$opponent == away$opponent[i] ),]$yards_per_pass_allowed) == T, 0, game_stats_away[which(game_stats_away$opponent == away$opponent[i] ),]$yards_per_pass_allowed)
        away$opp_ypr[i] = ifelse(is.null(game_stats_away[which(game_stats_away$opponent == away$opponent[i] ),]$yards_per_rush_attempt_allowed) == T, 0,game_stats_away[which(game_stats_away$opponent == away$opponent[i] ),]$yards_per_rush_attempt_allowed)
        
        
        away$opp_off_success_rate[i]  = ifelse(is.null(advanced_stats[which(advanced_stats$team == away$opponent[i] & advanced_stats$season == away$year[i] & advanced_stats$opponent == at[j]),]$off_success_rate) == T, 0, advanced_stats[which(advanced_stats$team == away$opponent[i] & advanced_stats$season == away$year[i] & advanced_stats$opponent == at[j]),]$off_success_rate)
        away$opp_off_power_success[i]  = ifelse(is.null(advanced_stats[which(advanced_stats$team == away$opponent[i] & advanced_stats$season == away$year[i] & advanced_stats$opponent == at[j]),]$off_power_success) == T, 0, advanced_stats[which(advanced_stats$team == away$opponent[i] & advanced_stats$season == away$year[i] & advanced_stats$opponent == at[j]),]$off_power_success)
        away$opp_off_explosiveness[i]  = ifelse(is.null(advanced_stats[which(advanced_stats$team == away$opponent[i] & advanced_stats$season == away$year[i] & advanced_stats$opponent == at[j]),]$off_explosiveness) == T, 0, advanced_stats[which(advanced_stats$team == away$opponent[i] & advanced_stats$season == away$year[i] & advanced_stats$opponent == at[j]),]$off_explosiveness)
        away$opp_def_success_rate[i]  = ifelse(is.null(advanced_stats[which(advanced_stats$team == away$opponent[i] & advanced_stats$season == away$year[i] & advanced_stats$opponent == at[j]),]$def_success_rate) == T, 0, advanced_stats[which(advanced_stats$team == away$opponent[i] & advanced_stats$season == away$year[i] & advanced_stats$opponent == at[j]),]$def_success_rate)
        away$opp_def_power_success[i]  = ifelse(is.null(advanced_stats[which(advanced_stats$team == away$opponent[i] & advanced_stats$season == away$year[i] & advanced_stats$opponent == at[j]),]$def_power_success) == T, 0, advanced_stats[which(advanced_stats$team == away$opponent[i] & advanced_stats$season == away$year[i] & advanced_stats$opponent == at[j]),]$def_power_success)
        away$opp_def_explosiveness[i] = ifelse(is.null(advanced_stats[which(advanced_stats$team == away$opponent[i] & advanced_stats$season == away$year[i] & advanced_stats$opponent == at[j]),]$def_explosiveness) == T, 0, advanced_stats[which(advanced_stats$team == away$opponent[i] & advanced_stats$season == away$year[i] & advanced_stats$opponent == at[j]),]$def_explosiveness)
        
        
        print(i/nrow(away))
      }
      
      
      
      home['takeaways'] <- (as.numeric(home$interceptions) + as.numeric(home$fumbles_recovered))
      away['takeaways'] <- (as.numeric(away$interceptions) + as.numeric(away$fumbles_recovered))
      recent_home <- subset(home,season==2023)
      recent_away <- subset(away,season==2023)
      # recent_home <- home[(nrow(home)-10):nrow(home),]
      # recent_away <- home[(nrow(home)-10):nrow(home),]
      
      new_data_home = data.frame(#elo = recent_home$elo[nrow(recent_home)], 
                                 opp_elo = recent_away$elo[nrow(recent_away)], 
                                 off_overall = (median(as.numeric(recent_home$off_overall)) + median(as.numeric(recent_away$def_overall)))/2 ,
                                 #def_overall = ( median(as.numeric(recent_home$def_overall)) + median(as.numeric(recent_away$off_overall)) )/2 , 
                                 opp_def_overall = (median(as.numeric(recent_home$def_overall)) + median(as.numeric(recent_away$off_overall)))/2 ,
                                 #opp_off_overall = ( median(as.numeric(recent_home$def_overall)) + median(as.numeric(recent_away$off_overall)) )/2,
                                 sacks_allowed = (median(as.numeric(recent_home$sacks_allowed)) + median(as.numeric(recent_away$sacks)))/2,
                                 #sacks = (median(as.numeric(recent_home$sacks)) + median(as.numeric(recent_away$sacks_allowed)))/2 ,
                                 net_passing_yards = (median(as.numeric(recent_home$net_passing_yards)) + median(as.numeric(recent_away$net_passing_yards_allowed)))/2 ,
                                 rushing_yards = (median(as.numeric(recent_home$rushing_yards)) + median(as.numeric(recent_away$rushing_yards_allowed)))/2 ,

                                 home_dummy = 1, 
                                 turnovers = (median(as.numeric(recent_home$turnovers)) + median(as.numeric(recent_away$takeaways)))/2,
                                 #opp_turnovers =  (median(as.numeric(recent_away$turnovers)) + median(as.numeric(recent_home$takeaways)))/2,
                                 #total_yards = (median(as.numeric(recent_home$total_yards)) + median(as.numeric(recent_away$total_yards_allowed)))/2, 
                                 #opp_total_yards = (median(as.numeric(recent_away$total_yards)) + median(as.numeric(recent_home$total_yards_allowed)))/2, 

                                 off_success_rate = (median(as.numeric(recent_home$off_success_rate)) + median(as.numeric(recent_away$def_success_rate)))/2,

                                 def_success_rate = (median(as.numeric(recent_home$def_success_rate)) + median(as.numeric(recent_away$off_success_rate)))/2 )
   # 
   #                                  def_rushing = (median(as.numeric(recent_home$def_rushing)) + median(as.numeric(recent_away$off_rushing)))/2,
   #                                  opp_def_rushing =(median(as.numeric(recent_away$def_rushing)) + median(as.numeric(recent_home$off_rushing)))/2,
   #                                  def_passing = (median(as.numeric(recent_home$def_passing)) + median(as.numeric(recent_away$off_passing)))/2,
   #                                  opp_def_passing = (median(as.numeric(recent_home$off_passing)) + median(as.numeric(recent_away$def_passing)))/2,
   #                                  off_rushing = (median(as.numeric(recent_home$off_rushing)) + median(as.numeric(recent_away$def_rushing)))/2,
   #                                  opp_off_rushing =(median(as.numeric(recent_home$def_rushing)) + median(as.numeric(recent_away$off_rushing)))/2,
   #                                  off_passing = (median(as.numeric(recent_home$off_passing)) + median(as.numeric(recent_away$def_passing)))/2,
   #                                  opp_off_passing = (median(as.numeric(recent_home$def_passing)) + median(as.numeric(recent_away$off_passing)))/2, 
                                # opp_off_success_rate = (median(as.numeric(recent_away$off_success_rate)) + median(as.numeric(recent_home$def_success_rate)))/2, 
                                # opp_off_explosiveness = (median(as.numeric(recent_away$off_explosiveness)) + median(as.numeric(recent_home$def_explosiveness)))/2,
                                # opp_off_power_success =(median(as.numeric(recent_away$off_power_success)) + median(as.numeric(recent_home$def_power_success)))/2,
                                # opp_def_success_rate = (median(as.numeric(recent_away$def_success_rate)) + median(as.numeric(recent_home$off_success_rate)))/2, 
                                # opp_def_explosiveness = (median(as.numeric(recent_away$def_explosiveness)) + median(as.numeric(recent_home$off_explosiveness)))/2,
                                # opp_def_power_success = (median(as.numeric(recent_away$def_power_success)) + median(as.numeric(recent_home$off_power_success)))/2,
                                # off_explosiveness = (median(as.numeric(recent_home$off_explosiveness)) + median(as.numeric(recent_away$def_explosiveness)))/2,
                                # off_power_success = (median(as.numeric(recent_home$off_power_success)) + median(as.numeric(recent_away$def_power_success)))/2,
                                 # def_explosiveness = (median(as.numeric(recent_home$def_explosiveness)) + median(as.numeric(recent_away$off_explosiveness)))/2,
                                 # def_power_success = (median(as.numeric(recent_home$def_power_success)) + median(as.numeric(recent_away$off_power_success)))/2 

      
      
   
      new_data_away = data.frame(#elo = recent_away$elo[nrow(recent_away)], 
                                 opp_elo = recent_home$elo[nrow(recent_home)], 
                                 off_overall = (median(as.numeric(recent_away$off_overall)) + median(as.numeric(recent_home$def_overall)))/2 ,
                                 #def_overall = ( median(as.numeric(recent_away$def_overall)) + median(as.numeric(recent_home$off_overall)) )/2 , 
                                 opp_def_overall = (median(as.numeric(recent_away$def_overall)) + median(as.numeric(recent_home$off_overall)))/2 ,
                                 #opp_off_overall = ( median(as.numeric(recent_away$def_overall)) + median(as.numeric(recent_home$off_overall)) )/2,
                                 sacks_allowed = (median(as.numeric(recent_away$sacks_allowed)) + median(as.numeric(recent_home$sacks)))/2,
                                 #sacks = (median(as.numeric(recent_away$sacks)) + median(as.numeric(recent_home$sacks_allowed)))/2 ,
                                 net_passing_yards = (median(as.numeric(recent_away$net_passing_yards)) + median(as.numeric(recent_home$net_passing_yards_allowed)))/2 ,
                                 rushing_yards = (median(as.numeric(recent_away$rushing_yards)) + median(as.numeric(recent_home$rushing_yards_allowed)))/2 ,

                                 home_dummy = 0, 
                                 turnovers = (median(as.numeric(recent_away$turnovers)) + median(as.numeric(recent_home$takeaways)))/2,
                                 #opp_turnovers =  (median(as.numeric(recent_home$turnovers)) + median(as.numeric(recent_away$takeaways)))/2,
                                 #total_yards = (median(as.numeric(recent_away$total_yards)) + median(as.numeric(recent_home$total_yards_allowed)))/2, 
                                 #opp_total_yards = (median(as.numeric(recent_home$total_yards)) + median(as.numeric(recent_away$total_yards_allowed)))/2, 

                                 off_success_rate = (median(as.numeric(recent_away$off_success_rate)) + median(as.numeric(recent_home$def_success_rate)))/2,

                                 def_success_rate = (median(as.numeric(recent_away$def_success_rate)) + median(as.numeric(recent_home$off_success_rate)))/2)
                                 # def_rushing = (median(as.numeric(recent_away$def_rushing)) + median(as.numeric(recent_home$off_rushing)))/2,
                                 # opp_def_rushing =(median(as.numeric(recent_home$def_rushing)) + median(as.numeric(recent_away$off_rushing)))/2,
                                 # def_passing = (median(as.numeric(recent_away$def_passing)) + median(as.numeric(recent_home$off_passing)))/2,
                                 # opp_def_passing = (median(as.numeric(recent_away$off_passing)) + median(as.numeric(recent_home$def_passing)))/2,
                                 # off_rushing = (median(as.numeric(recent_away$off_rushing)) + median(as.numeric(recent_home$def_rushing)))/2,
                                 # opp_off_rushing =(median(as.numeric(recent_away$def_rushing)) + median(as.numeric(recent_home$off_rushing)))/2,
                                 # off_passing = (median(as.numeric(recent_away$off_passing)) + median(as.numeric(recent_home$def_passing)))/2,
                                 # opp_off_passing = (median(as.numeric(recent_away$def_passing)) + median(as.numeric(recent_home$off_passing)))/2, 
                                 # opp_off_success_rate = (median(as.numeric(recent_home$off_success_rate)) + median(as.numeric(recent_away$def_success_rate)))/2, 
                                 # opp_off_explosiveness = (median(as.numeric(recent_home$off_explosiveness)) + median(as.numeric(recent_away$def_explosiveness)))/2,
                                 # opp_off_power_success =(median(as.numeric(recent_home$off_power_success)) + median(as.numeric(recent_away$def_power_success)))/2,
                                 # opp_def_success_rate = (median(as.numeric(recent_home$def_success_rate)) + median(as.numeric(recent_away$off_success_rate)))/2, 
                                 # opp_def_explosiveness = (median(as.numeric(recent_home$def_explosiveness)) + median(as.numeric(recent_away$off_explosiveness)))/2,
                                 # opp_def_power_success = (median(as.numeric(recent_home$def_power_success)) + median(as.numeric(recent_away$off_power_success)))/2,
                                 # off_explosiveness = (median(as.numeric(recent_away$off_explosiveness)) + median(as.numeric(recent_home$def_explosiveness)))/2,
                                 # off_power_success = (median(as.numeric(recent_away$off_power_success)) + median(as.numeric(recent_home$def_power_success)))/2,
                                 # def_explosiveness = (median(as.numeric(recent_away$def_explosiveness)) + median(as.numeric(recent_home$off_explosiveness)))/2,
                                 # def_power_success = (median(as.numeric(recent_away$def_power_success)) + median(as.numeric(recent_home$off_power_success)))/2 
                                 
      
      
      cols <-colnames(new_data_home)
      #cols <- c('elo','opp_elo','off_overall','sacks_allowed', 'sacks' , 'net_passing_yards' , 'rushing_yards' , 'home_dummy' , 'turnovers' , 'off_success_rate' )
      
      home$weights <- ifelse(as.numeric(substr(as.character(home$season),4,4))==2,as.numeric(substr(as.character(home$season),4,4)),as.numeric(substr(as.character(home$season),4,4))*2)
      away$weights <- ifelse(as.numeric(substr(as.character(away$season),4,4))==2,as.numeric(substr(as.character(away$season),4,4)),as.numeric(substr(as.character(away$season),4,4))*2)
      

      ######### home #############
      
      home<-home[which(home$opponent.x %in% fbs_teams$school),]
      away<-away[which(away$opponent.x %in% fbs_teams$school),]
      #home<-tibble(home)

      # 
      # for (col in cols){
      #   home[col]<-as.numeric(unlist(home[col]))
      # }
      # 
      # for (col in cols){
      #   for (i in nrow(home)){
      #     if(is.na(home[i,col])){home[i,col] <- sum(home[col],na.rm=TRUE)/nrow(na.omit(home[col]))}
      #   }
      # }
      
      X = home[cols]
      Y = home['points']
      


    
      # new_data_home<-new_data_home[cols]
      # 
      #                                                
      # model_home = lm(points ~ elo+opp_elo+off_overall+
      #                   sacks_allowed + sacks +net_passing_yards + rushing_yards +
      #                   home_dummy + turnovers + off_success_rate 
      #                  ,data = home ,weights = home$weights)
      # summary(model_home)
      # model_home$coefficients
      # predict(model_home,new_data_home)
      
      
     
      
      for(i in 1:ncol(X)){
        X[,i] = as.numeric(X[,i])
      }
      
      
     
      hp<-c()
      for(k in 1:5){
        r = seq(1,nrow(X), 1)
        s = sample(r, 3 ,replace = F)
        train_x = data.matrix(X[-s,])
        train_y = data.matrix(Y[-s,])
        test_x = data.matrix(X[s,])
        test_y = data.matrix(Y[s,])
        train_weights = data.matrix(home$weights[-s])
        test_weights = data.matrix(home$weights[s])
        
        # set up the cross-validated hyper-parameter search
        xgb_grid_1 = expand.grid(
          nrounds = 1000,
          eta = c(0.01, 0.001, 0.0001),
          max_depth = c(2, 4, 6, 8, 10),
          gamma = 1
        )
        # pack the training control parameters
        xgb_trcontrol_1 = trainControl(
          method = "cv",
          number = 5,
          verboseIter = TRUE,
          returnData = FALSE,
          returnResamp = "all",                                                        # save losses across all models
          classProbs = FALSE,                                                           # set to TRUE for AUC to be computed
          summaryFunction = defaultSummary,
          allowParallel = TRUE
        )
        
        
        #define predictor and response variables in testing se
        #define final training and testing sets
        xgb_train = xgb.DMatrix(data = train_x, label = train_y, weight=train_weights)
        xgb_test = xgb.DMatrix(data = test_x, label = test_y,weight = test_weights)
        
        
        watchlist = list(train=xgb_train, test=xgb_test)
        
        params <- list(booster = "gblinear",
          objective = "reg:absoluteerror")
        
        xgb_base <- 0
        xgb_base <- xgb.train(params = params,
                              data = xgb_train,
                              nrounds =100,
                              print_every_n = 100,
                              max_depth = 3,
                              eval_metric = "rmse",
                              early_stopping_rounds = 20,
                              trControl = xgb_trcontrol_1,
                              tuneGrid = xgb_grid_1,
                              watchlist = watchlist)
        #model = xgb.train(data = xgb_train, max.depth = 3, nrounds = 70)
        #final = xgboost(data = xgb_train, max.depth = 3, nrounds = 60)
        new_data_home = data.matrix(new_data_home)
        
        
        # predict home points #
        hp[k] = predict(xgb_base,new_data_home)
      }
      
      
      home_points<-mean(hp)
      
      
      
      
      
      ########## away #############

      X = away[cols]
      Y = away['points']
      for(i in 1:ncol(X)){
        X[,i] = as.numeric(X[,i])
      }
      
      
      
      
      
      ap<-c()
      
      for(k in 1:5){
        r = seq(1,nrow(X), 1)
        s = sample(r, 3 ,replace = F)
        train_x = data.matrix(X[-s,])
        train_y = data.matrix(Y[-s,])
        test_x = data.matrix(X[s,])
        test_y = data.matrix(Y[s,])
        train_weights = data.matrix(away$weights[-s])
        test_weights = data.matrix(away$weights[s])
        
        # set up the cross-validated hyper-parameter search
        xgb_grid_1 = expand.grid(
          nrounds = 1000,
          eta = c(0.01, 0.001, 0.0001),
          max_depth = c(2, 4, 6, 8, 10),
          gamma = 1
        )
        # pack the training control parameters
        xgb_trcontrol_1 = trainControl(
          method = "cv",
          number = 5,
          verboseIter = TRUE,
          returnData = FALSE,
          returnResamp = "all",                                                        # save losses across all models
          classProbs = FALSE,                                                           # set to TRUE for AUC to be computed
          summaryFunction = defaultSummary,
          allowParallel = TRUE
        )
        
        
        #define predictor and response variables in testing se
        #define final training and testing sets
        xgb_train = xgb.DMatrix(data = train_x, label = train_y,weight = train_weights)
        xgb_test = xgb.DMatrix(data = test_x, label = test_y,weight = test_weights)
        
        
        watchlist = list(train=xgb_train, test=xgb_test)
        
        params <- list(booster = "gblinear",
          objective = "reg:absoluteerror")
        
        xgb_base <- 0
        xgb_base <- xgb.train(params = params,
                              data = xgb_train,
                              nrounds =100,
                              print_every_n = 100,
                              max_depth = 4,
                              eval_metric = "rmse",
                              early_stopping_rounds = 20,
                              trControl = xgb_trcontrol_1,
                              tuneGrid = xgb_grid_1,
                              watchlist = watchlist)
        # predict away points #
        new_data_away = data.matrix(new_data_away)
        
        ap[k] = predict(xgb_base,new_data_away)
        
      }
      
      away_points<-mean(ap)
      
      
      
      # store #
      y$ht[j] =  ht[j]
      y$at[j] = at[j]
      y$ht_score[j] = home_points
      y$at_score[j] = away_points
      y$ht_spread[j] = ifelse(is.null(cfbd_betting_lines(year = 2023, week = week, team = ht[j])$spread), 0, cfbd_betting_lines(year = 2023, week = week, team = ht[j])$spread)
      #y$ht_spread[j] = -11
      print(ht[j]);print(home_points);print(at[j]);print(away_points)
      #setTxtProgressBar(pb2, j)
      
    }
    
    
    
    
    
    ######## Find Value ######
    y$diff = (y$at_score - y$ht_score)
    # y$value = as.numeric(y$ht_spread) - y$ht_diff
    #y$value_rating = (y$value)/100
    y$value_side = 0
    y$value_diff = 0
    y$x_axis = 0
    for (i in 1: nrow(y)){
      if(y$diff[i] > 0 && as.numeric(y$ht_spread[i]) < 0){ y$value_side[i] = y$at[i]}
      if(y$diff[i] > 0 && as.numeric(y$ht_spread[i]) >= 0 && as.numeric(y$ht_spread[i]) > y$diff[i] ){ y$value_side[i] = y$ht[i]}
      if(y$diff[i] < 0 && as.numeric(y$ht_spread[i]) < y$diff[i]){ y$value_side[i] = y$at[i]}
      if(y$diff[i] > 0 && as.numeric(y$ht_spread[i]) < y$diff[i]){ y$value_side[i] = y$at[i]}
      if(y$diff[i] < 0 && as.numeric(y$ht_spread[i]) > y$diff[i]){ y$value_side[i] = y$ht[i]}
      
      if(y$diff[i] > 0 && as.numeric(y$ht_spread[i]) < 0){ y$value_diff[i] = y$diff[i] - as.numeric(y$ht_spread[i])}
      if(y$diff[i] > 0 && as.numeric(y$ht_spread[i]) > 0 && y$diff[i] < as.numeric(y$ht_spread[i])){ y$value_diff[i] = as.numeric(y$ht_spread[i]) - y$diff[i]}
      if(y$diff[i] < 0 && as.numeric(y$ht_spread[i]) < y$diff[i]){ y$value_diff[i] = abs(as.numeric(y$ht_spread[i])) - abs(as.numeric(y$diff[i]))}
      if(y$diff[i] > 0 && as.numeric(y$ht_spread[i]) < y$diff[i]){ y$value_diff[i] = y$diff[i] - as.numeric(y$ht_spread[i]) }
      if(y$diff[i] < 0 && as.numeric(y$ht_spread[i]) > y$diff[i]){ y$value_diff[i] = abs(as.numeric(y$ht_spread[i])) + abs(as.numeric(y$diff[i]))}
    }
    
    
    for(i in 1:nrow(y)){
      if(y$ht[i] == y$value_side[i]){y$x_axis[i] = y$at[i]}
      if(y$at[i] == y$value_side[i]){y$x_axis[i] = y$ht[i] }
    }
    
    y$value_rating = abs((y$diff)/100)
    y$value_rating = abs((y$value_diff)/100)
    #df = rbind(df,y, fill = TRUE)
    

  y
  
  return(y)
  
  
  
  
  
}






# games<-cfbd_game_info(2023, week = 3)
# games <- subset(games,start_date < "2023-09-16")
# 
# 
# ht = games$home_team
# at = games$away_team
# 
# y <- CFB_PROJECTIONS(ht=ht,at=at,input_week=3,input_season=2023,conferences = c(),previous_season=0,remove_fcs = TRUE)
# 




############################################### run model #################################

# df <- data.frame()
# 
# for (conf in c("SEC","ACC","B1G","B12",'PAC')){
# y <- CFB_PROJECTIONS(ht=c(),at=c(),input_week=4,input_season=2023,conferences = c(conf),previous_season=0,remove_fcs = TRUE)
# df<-rbind(df,y,fill=TRUE)
# }
# 
# 
# p5<-df

############################################### run model #################################

#team = cfbd_team_info(year = 2023)

#p5 <- data.frame()
#conferences<-c("ACC","SEC","B12","B1G","PAC")

# ht = c('Alabama','Colorado','Iowa State','Miami','Oklahoma','NC State')#,"Jacksonville State")
# at = c('Texas','Nebraska','Iowa','Texas A&M','SMU','Notre Dame')
# df <- CFB_MODEL(ht=ht,at=at,input_week=2,input_season=2023,conferences = c(),previous_season=1,remove_fcs = TRUE)













# 
# p5<-p5[-which(p5$ht == TRUE),]
# p5 = p5[!duplicated(p5$ht),]
# y<-p5

# team_plot_data = y
# team_plot_data$conference = 0
# 
# for(i in 1:nrow(team_plot_data)){
#    team_plot_data$conference[i] = cfbd_stats_season_team(year = 2023, team = team_plot_data$ht[i])$conference
# 
# }

# 
# 
# library(gt)
# 
#  team_info <- cfbd_team_info()
#  team_info <- team_info %>%
#        filter(conference %in% c("SEC","Pac-12","Big Ten","Big 12","ACC"))

# 
# # ############################ gt table ####################################
# 
# 
#  
#  
# 
#  conf = "Pac-12"
#  temp <- subset(team_info,conference == conf)
# 
#  team_plot_data <- p5 %>%
#          filter(ht %in% temp$school | at %in% temp$school)
# 
# team_plot_data$conference <- conf
# 
# #team_plot_data$at_score[9]<-17.78
# 
# 
#  team_plot_data %>%
#    transmute(Conference = conference, Home_Team = ht,
#              Home_Score = round(ht_score,2),
#              Away_Score = round(at_score,2), Away_Team = at, Spread_Pick = value_side) %>%
#    arrange(desc(Conference)) %>%
#    gt() %>%
#    gt_fmt_cfb_logo(columns = c("Conference", "Spread_Pick")) %>%
#    gt_fmt_cfb_wordmark(columns = c("Home_Team","Away_Team")) %>%
#    cols_align(
#      align = c('center'),
#      columns = everything()
#    ) %>%
#    tab_header(
#      title = md("**Scarlett Score Predictions**"),
#      subtitle = md("Harrison Eller")
#    ) %>%
#    fmt_number(
#      columns = c("Home_Score", "Away_Score")
#    ) %>%
#    tab_style(
#      style = list(
#        cell_text(weight = "bold")
#      ),
#      locations = cells_body(
#        columns = c("Home_Score", "Away_Score")
#      )
#   )






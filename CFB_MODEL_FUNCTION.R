CFB_MODEL <- function(ht=c(),at=c(),input_week,conferences = c(),previous_season=1,remove_fcs = FALSE){
  
  
  
  library(dplyr)
  library(readr)
  library(cfbfastR)
  #remotes::install_github(repo = "sportsdataverse/cfbfastR")
  remotes::install_github(repo = "Kazink36/cfbplotR")
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
  setwd("C:/Users/harri/OneDrive/Desktop/LTB/CFB Analysis")
  
  week = input_week
  df = data.frame()
  conferences = c()
  # 'Ind','MAC','MWC','SBC'
  
  fbs = cfbd_conferences()
  fbs = subset(fbs, classification == 'fbs')
  fbs = fbs[1:10,]
  fbs_teams = load_cfb_teams()
  fbs_teams = subset(fbs_teams, classification == 'fbs')
  
  for(conf in 1:length(conferences)){
    
    conference= conferences[conf]
    week = week
    
    if(length(conferences) >= 1){
    ht = cfbd_game_info(2022, week = week, conference = conference)$home_team
    at = cfbd_game_info(2022, week = week, conference = conference)$away_team
    #ht = cfbd_game_info(2022)$home_team
    #at = cfbd_game_info(2022, week = week)$away_team
    #ht = ht[1:17]
    #at = at[1:17]
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
    
    #at = c('Eastern Michigan', 'Ohio', 'Ball State', 'Northern Illinois', 'Buffalo', 'Kent State', 'Tulsa', 'Georgia Southern', 'East Carolina', 'Colorado', 'Fresno State')
    #ht = c('Akron', 'Miami (OH)', 'Toledo', 'Western Michigan', 'Central Michigan', 'Bowling Green', 'Memphis', 'Louisiana', 'Cincinnati', 'USC', 'UNLV')
    pb2 = txtProgressBar(min = 0, max = length(ht), initial = 0)
    
    y = data.frame(matrix(ncol = 5, nrow = length(ht)))
    colnames(y) <- c('ht', 'at', 'ht_score', 'at_score', 'ht_spread')
    
    #######################  model #############################
    for (j in 1:length(ht)){
      
      home1 <- 0
      away1 <- 0
      
      #enable current season
      # if(previous_season == 1){
      #   home_stats1 = cfbd_game_team_stats(2022, team = ht[j])
      #   away_stats1 = cfbd_game_team_stats(2022, team = at[j])
      # 
      #   home_advanced1 = cfbd_stats_game_advanced(2022, team = ht[j])
      #   away_advanced1 = cfbd_stats_game_advanced(2022, team = at[j])
      # 
      #   home_ppa1 = cfbd_metrics_ppa_games(2022, team = ht[j])
      #   away_ppa1 = cfbd_metrics_ppa_games(2022, team = at[j])
      # 
      # 
      # 
      #   home_elo1 = cfbd_ratings_elo(year = 2022,team = ht[j])
      #   away_elo1 = cfbd_ratings_elo(year = 2022,team = at[j])
      # 
      # 
      #   if(length(home_stats1)>0){home1 = inner_join(home_stats1,home_ppa1,by = 'game_id')}
      #   if(length(away_stats1)>0){away1 = inner_join(away_stats1,away_ppa1,by = 'game_id')}
      # 
      #   if(length(home_elo1) >0){home1 = inner_join(home1,home_elo1, by = 'team')}
      #   if(length(away_elo1) >0){away1= inner_join(away1,away_elo1, by = 'team')}
      # 
      # 
      #   if(length(home_advanced1)>0){home1 = inner_join(home1,home_advanced1,by = 'game_id')}
      #   if(length(away_advanced1)>0){away1 = inner_join(away1,away_advanced1,by = 'game_id')}
      # 
      # }
      # 
      # # ### current season ###
      # # home_stats2 = cfbd_game_team_stats(2023, team = ht[j])
      # # away_stats2 = cfbd_game_team_stats(2023, team = at[j])
      # # 
      # # home_advanced2 = cfbd_stats_game_advanced(2023, team = ht[j])
      # # away_advanced2 = cfbd_stats_game_advanced(2023, team = at[j])
      # # 
      # # 
      # # home_ppa2 = cfbd_metrics_ppa_games(2023, team = ht[j])
      # # away_ppa2 = cfbd_metrics_ppa_games(2023, team = at[j])
      # # 
      # # 
      # # home_elo2 = cfbd_ratings_elo(year = 2023,team = ht[j])
      # # away_elo2 = cfbd_ratings_elo(year = 2023,team = at[j])
      # # 
      # # 
      # # 
      # # home2 = inner_join(home_stats2,home_ppa2,by = 'game_id')
      # # away2 = inner_join(away_stats2,away_ppa2,by = 'game_id')
      # # 
      # # 
      # # home2 = inner_join(home2,home_elo2, by = 'team')
      # # away2= inner_join(away2,away_elo2, by = 'team')
      # # 
      # # home2 = inner_join(home2,home_advanced2,by = 'game_id')
      # # away2 = inner_join(away2,away_advanced2,by = 'game_id')
      # # 
      home_stats2 = cfbd_game_team_stats(2022, team = ht[j])
      away_stats2 = cfbd_game_team_stats(2022, team = at[j])
      
      home_advanced2 = cfbd_stats_game_advanced(2022, team = ht[j])
      away_advanced2 = cfbd_stats_game_advanced(2022, team = at[j])
      
      
      home_ppa2 = cfbd_metrics_ppa_games(2022, team = ht[j])
      away_ppa2 = cfbd_metrics_ppa_games(2022, team = at[j])
      
      
      home_elo2 = cfbd_ratings_elo(year = 2022,team = ht[j])
      away_elo2 = cfbd_ratings_elo(year = 2022,team = at[j])
      
      
      
      home2 = inner_join(home_stats2,home_ppa2,by = 'game_id')
      away2 = inner_join(away_stats2,away_ppa2,by = 'game_id')
      
      
      home2 = inner_join(home2,home_elo2, by = 'team')
      away2= inner_join(away2,away_elo2, by = 'team')
      
      home2 = inner_join(home2,home_advanced2,by = 'game_id')
      away2 = inner_join(away2,away_advanced2,by = 'game_id')
      
      if(length(home1) > 0){home = rbind(home1, home2, fill = T)}else{home = home2}
      if(length(home1) > 0){away = rbind(away1, away2, fill = T)}else{away = away2}
      
      home2 = as.data.frame(home2)
      away2= as.data.frame(away2)
      home = as.data.frame(home)
      away = as.data.frame(away)
      home=home2
      away=away2
      
      # non = c()
      # for(i in 1:nrow(home)){
      #   if(home$opponent_conference[i] %in%  fbs$name){print(i)}else{non[i] = i}
      # }
      #  
      #  non<-non[!is.na(non)]
      #   if(is.null(non) == F){home = home[-non,]}
      #
      #
      # non = c()
      # for(i in 1:nrow(away)){
      #   if(away$opponent_conference[i] %in%  fbs$name){print(i)}else{non[i] = i}
      # }
      #
      # non<-non[!is.na(non)]
      # if(is.null(non) == F){away = away[-non,]}
      #
      #
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
      # away['opp_sacks_allowed'] = 0
      # home['opp_sacks_allowed'] = 0
      #  away['opp_qb_hurries_allowed'] = 0
      #  home['opp_qb_hurries_allowed'] = 0
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
      
      
      
      home$opponent = home$opponent.x
      away$opponent = away$opponent.x
      home$home_dummy = 0
      away$home_dummy = 0
      
      for(i in 1:nrow(home)){
        if(is.na(home$year[i]) == T){home$year[i] = 2021}
        if(home$home_away[i] == 'home'){home$home_dummy[i]= 1}
      }
      for(i in 1:nrow(away)){
        if(is.na(away$year[i]) == T){away$year[i] = 2021}
        if(away$home_away[i] == 'home'){away$home_dummy[i] = 1}
      }
      
      #
      # for(i in 1:nrow(home)){
      #   if(home$year[i] == 2021){ifelse()home$fpi[i] <- fpi_2021$fpi[which(fpi_2021$name == ht[j])]}
      #   if(home$year[i] == 2022){home$fpi[i] <- fpi_2022$fpi[which(fpi_2022$name == ht[j])]}
      #   if(home$year[i] == 2021){if(home$opponent[i] %in% fpi_2021$name){home$opp_fpi[i] <- fpi_2021$fpi[which(fpi_2021$name == home$opponent[i])]}}
      #   if(home$year[i] == 2022){if(home$opponent[i] %in% fpi_2022$name){home$opp_fpi[i] <- fpi_2022$fpi[which(fpi_2022$name == home$opponent[i])]}}
      # }
      # for(i in 1:nrow(away)){
      #   if(away$year[i] == 2021){away$fpi[i] <- fpi_2021$fpi[which(fpi_2021$name == at[j])]}
      #   if(away$year[i] == 2022){away$fpi[i] <- fpi_2022$fpi[which(fpi_2022$name == at[j])]}
      #   if(away$year[i] == 2021){if(away$opponent[i] %in% fpi_2021$away){away$opp_fpi[i] <- fpi_2021$fpi[which(fpi_2021$name == away$opponent[i])]}}
      #   if(away$year[i] == 2022){if(away$opponent[i] %in% fpi_2022$name){away$opp_fpi[i] <- fpi_2022$fpi[which(fpi_2022$name == away$opponent[i])]}}
      # }
      #
      
      
      for (i in 1:nrow(home)){
        home$opp_elo[i] = ifelse(is.null(cfbd_ratings_elo(year = home$year[i], team = home$opponent[i])$elo) == T, 1000, cfbd_ratings_elo(year = 2022, team = home$opponent[i])$elo)
        home$opp_def_overall[i] = ifelse(is.null(cfbd_metrics_ppa_games(year = home$year[i], team = home$opponent[i])$def_overall) == T, 0, cfbd_metrics_ppa_games(year = home$year[i], team = home$opponent[i])$def_overall)
        home$opp_off_overall[i] = ifelse(is.null(cfbd_metrics_ppa_games(year = home$year[i], team = home$opponent[i])$off_overall) == T, 0, cfbd_metrics_ppa_games(year = home$year[i], team = home$opponent[i])$off_overall)
        home$opp_off_rushing[i] = ifelse(is.null(cfbd_metrics_ppa_games(year = home$year[i], team = home$opponent[i])$off_rushing) == T, 0, cfbd_metrics_ppa_games(year = home$year[i], team = home$opponent[i])$off_rushing)
        home$opp_off_passing[i] = ifelse(is.null(cfbd_metrics_ppa_games(year = home$year[i], team = home$opponent[i])$off_passing) == T, 0, cfbd_metrics_ppa_games(year = home$year[i], team = home$opponent[i])$off_passing)
        home$opp_def_rushing[i] = ifelse(is.null(cfbd_metrics_ppa_games(year = home$year[i], team = home$opponent[i])$def_rushing) == T, 0, cfbd_metrics_ppa_games(year = home$year[i], team = home$opponent[i])$def_rushing)
        home$opp_def_passing[i] = ifelse(is.null(cfbd_metrics_ppa_games(year = home$year[i], team = home$opponent[i])$def_passing) == T, 0, cfbd_metrics_ppa_games(year = home$year[i], team = home$opponent[i])$def_passing)
        home$opp_turnovers[i] = ifelse(is.null(cfbd_game_team_stats(year = home$year[i], team = home$opponent[i])$turnovers) == T, 0, cfbd_game_team_stats(year = home$year[i], team = home$opponent[i])$turnovers)
        home$opp_total_yards[i] = ifelse(is.null(cfbd_game_team_stats(year = home$year[i], team = home$opponent[i])$total_yards) == T, 0, cfbd_game_team_stats(year = home$year[i], team = home$opponent[i])$total_yards)
        home$opp_ypp[i] = ifelse(is.null(cfbd_game_team_stats(year = home$year[i], team = home$opponent[i])$yards_per_pass) == T, 0, cfbd_game_team_stats(year = home$year[i], team = home$opponent[i])$yards_per_pass)
        home$opp_ypr[i] = ifelse(is.null(cfbd_game_team_stats(year = home$year[i], team = home$opponent[i])$yards_per_rush_attempt) == T, 0, cfbd_game_team_stats(year = home$year[i], team = home$opponent[i])$yards_per_rush_attempt)
        
        home$opp_off_success_rate[i]  = ifelse(is.null(cfbd_stats_game_advanced(year = home$year[i], team = home$opponent[i])$off_success_rate) == T, 0, cfbd_stats_game_advanced(year = home$year[i], team = home$opponent[i])$off_success_rate)
        home$opp_off_power_success[i]  = ifelse(is.null(cfbd_stats_game_advanced(year = home$year[i], team = home$opponent[i])$off_power_success) == T, 0, cfbd_stats_game_advanced(year = home$year[i], team = home$opponent[i])$off_power_success)
        home$opp_off_explosiveness[i]  = ifelse(is.null(cfbd_stats_game_advanced(year = home$year[i], team = home$opponent[i])$off_explosiveness) == T, 0, cfbd_stats_game_advanced(year = home$year[i], team = home$opponent[i])$off_explosiveness)
        home$opp_def_success_rate[i]  = ifelse(is.null(cfbd_stats_game_advanced(year = home$year[i], team = home$opponent[i])$def_success_rate) == T, 0, cfbd_stats_game_advanced(year = home$year[i], team = home$opponent[i])$def_success_rate)
        home$opp_def_power_success[i]  = ifelse(is.null(cfbd_stats_game_advanced(year = home$year[i], team = home$opponent[i])$def_power_success) == T, 0, cfbd_stats_game_advanced(year = home$year[i], team = home$opponent[i])$def_power_success)
        home$opp_def_explosiveness[i] = ifelse(is.null(cfbd_stats_game_advanced(year = home$year[i], team = home$opponent[i])$def_explosiveness) == T, 0, cfbd_stats_game_advanced(year = home$year[i], team = home$opponent[i])$def_explosiveness)
        
        
        
        
        print(i/nrow(home))
      }
      for (i in 1:nrow(away)){
        away$opp_elo[i] = ifelse(is.null(cfbd_ratings_elo(year = away$year[i], team = away$opponent[i])$elo) == T, 1000, cfbd_ratings_elo(year = away$year[i], team = away$opponent[i])$elo)
        away$opp_def_overall[i] = ifelse(is.null(cfbd_metrics_ppa_games(year = away$year[i], team = away$opponent[i])$def_overall) == T, 0, cfbd_metrics_ppa_games(year = away$year[i], team = away$opponent[i])$def_overall)
        away$opp_off_overall[i] = ifelse(is.null(cfbd_metrics_ppa_games(year = away$year[i], team = away$opponent[i])$off_overall) == T, 0, cfbd_metrics_ppa_games(year = away$year[i], team = away$opponent[i])$off_overall)
        away$opp_off_rushing[i] = ifelse(is.null(cfbd_metrics_ppa_games(year = away$year[i], team = away$opponent[i])$off_rushing) == T, 0, cfbd_metrics_ppa_games(year = away$year[i], team = away$opponent[i])$off_rushing)
        away$opp_off_passing[i] = ifelse(is.null(cfbd_metrics_ppa_games(year = away$year[i], team = away$opponent[i])$off_passing) == T, 0, cfbd_metrics_ppa_games(year = away$year[i], team = away$opponent[i])$off_passing)
        away$opp_def_rushing[i] = ifelse(is.null(cfbd_metrics_ppa_games(year = away$year[i], team = away$opponent[i])$def_rushing) == T, 0, cfbd_metrics_ppa_games(year = away$year[i], team = away$opponent[i])$def_rushing)
        away$opp_def_passing[i] = ifelse(is.null(cfbd_metrics_ppa_games(year = away$year[i], team = away$opponent[i])$def_passing) == T, 0, cfbd_metrics_ppa_games(year = away$year[i], team = away$opponent[i])$def_passing)
        # away$opp_sacks_allowed[i] = ifelse(is.null(cfbd_game_team_stats(year = away$year[i], team = away$opponent[i])$sacks_allowed) == T, 0, cfbd_game_team_stats(year = away$year[i], team = away$opponent[i])$sacks_allowed)
        # away$opp_qb_hurries_allowed[i] = ifelse(is.null(cfbd_game_team_stats(year = away$year[i], team = away$opponent[i])$qb_hurries_allowed) == T, 0, cfbd_game_team_stats(year = away$year[i], team = away$opponent[i])$qb_hurries_allowed)
        away$opp_turnovers[i] = ifelse(is.null(cfbd_game_team_stats(year = away$year[i], team = away$opponent[i])$turnovers) == T, 0, cfbd_game_team_stats(year = away$year[i], team = away$opponent[i])$turnovers)
        away$opp_total_yards[i] = ifelse(is.null(cfbd_game_team_stats(year = away$year[i], team = away$opponent[i])$total_yards) == T, 0, cfbd_game_team_stats(year = away$year[i], team = away$opponent[i])$total_yards)
        away$opp_ypp[i] = ifelse(is.null(cfbd_game_team_stats(year = away$year[i], team = away$opponent[i])$yards_per_pass) == T, 0, cfbd_game_team_stats(year = away$year[i], team = away$opponent[i])$yards_per_pass)
        away$opp_ypr[i] = ifelse(is.null(cfbd_game_team_stats(year = away$year[i], team = away$opponent[i])$yards_per_rush_attempt) == T, 0, cfbd_game_team_stats(year = away$year[i], team = away$opponent[i])$yards_per_rush_attempt)
        
        away$opp_off_success_rate[i]  = ifelse(is.null(cfbd_stats_game_advanced(year = away$year[i], team = away$opponent[i])$off_success_rate) == T, 0, cfbd_stats_game_advanced(year = away$year[i], team = away$opponent[i])$off_success_rate)
        away$opp_off_power_success[i]  = ifelse(is.null(cfbd_stats_game_advanced(year = away$year[i], team = away$opponent[i])$off_power_success) == T, 0, cfbd_stats_game_advanced(year = away$year[i], team = away$opponent[i])$off_power_success)
        away$opp_off_explosiveness[i]  = ifelse(is.null(cfbd_stats_game_advanced(year = away$year[i], team = away$opponent[i])$off_explosiveness) == T, 0, cfbd_stats_game_advanced(year = away$year[i], team = away$opponent[i])$off_explosiveness)
        away$opp_def_success_rate[i]  = ifelse(is.null(cfbd_stats_game_advanced(year = away$year[i], team = away$opponent[i])$def_success_rate) == T, 0, cfbd_stats_game_advanced(year = away$year[i], team = away$opponent[i])$def_success_rate)
        away$opp_def_power_success[i]  = ifelse(is.null(cfbd_stats_game_advanced(year = away$year[i], team = away$opponent[i])$def_power_success) == T, 0, cfbd_stats_game_advanced(year = away$year[i], team = away$opponent[i])$def_power_success)
        away$opp_def_explosiveness[i]  = ifelse(is.null(cfbd_stats_game_advanced(year = away$year[i], team = away$opponent[i])$def_explosiveness) == T, 0, cfbd_stats_game_advanced(year = away$year[i], team = away$opponent[i])$def_explosiveness)
        
        
        
        print(i/nrow(away))
      }
      
      
      
      new_data_home = data.frame(elo = home$elo[nrow(home)], opp_elo = away$elo[nrow(away)], off_overall = median(as.numeric(home2$off_overall)),
                                 def_overall = median(as.numeric(home2$def_overall)), opp_def_overall = median(as.numeric(away2$def_overall)),
                                 opp_off_overall = median(as.numeric(away2$off_overall)),
                                 sacks_allowed = median(as.numeric(home2$sacks_allowed)),
                                 #opp_sacks_allowed = median(as.numeric(away$sacks_allowed)),
                                 qb_hurries_allowed = median(as.numeric(home2$qb_hurries_allowed)),
                                 #opp_qb_hurries_allowed = median(as.numeric(away$qb_hurries_allowed)),
                                 def_rushing = median(as.numeric(home2$def_rushing)),
                                 opp_def_rushing = median(as.numeric(away2$def_rushing)),
                                 def_passing = median(as.numeric(home2$def_passing)),
                                 opp_def_passing = median(as.numeric(away2$def_passing)),
                                 off_rushing = median(as.numeric(home2$off_rushing)),
                                 opp_off_rushing = median(as.numeric(away2$off_rushing)),
                                 off_passing = median(as.numeric(home2$off_passing)),
                                 opp_off_passing = median(as.numeric(away2$off_passing)), home_dummy = 1, turnovers = median(as.numeric(home2$turnovers)), opp_turnovers = median(as.numeric(away2$turnovers)),
                                 total_yards = median(as.numeric(home$total_yards)), opp_total_yards = median(as.numeric(home$opp_total_yards)), yards_per_pass = median(as.numeric(home$yards_per_pass)) , opp_ypp = median(as.numeric(home$opp_ypp)),
                                 opp_ypr = median(as.numeric(home2$yards_per_rush_attempt)), yards_per_rush_attempt = median(as.numeric(home2$yards_per_rush_attempt)),
                                 opp_off_success_rate = median(as.numeric(home2$opp_off_success_rate)), opp_off_explosiveness = median(as.numeric(home2$opp_off_explosiveness)),
                                 opp_off_power_success = median(as.numeric(home2$opp_off_power_success)),
                                 opp_def_success_rate = median(as.numeric(home2$opp_def_success_rate)), opp_def_explosiveness = median(as.numeric(home2$opp_def_explosiveness)),
                                 opp_def_power_success = median(as.numeric(home2$opp_def_power_success)),
                                 off_success_rate = median(as.numeric(home2$off_success_rate)),off_explosiveness = median(as.numeric(home2$off_explosiveness)),
                                 off_power_success = median(as.numeric(home2$off_power_success)),
                                 def_success_rate = median(as.numeric(home2$def_success_rate)), def_explosiveness = median(as.numeric(home2$def_explosiveness)),
                                 def_power_success = median(as.numeric(home2$def_power_success)))
      
      
      
      
      new_data_away = data.frame(elo = away$elo[nrow(away)], opp_elo = home$elo[nrow(home)],
                                 off_overall = median(as.numeric(away2$off_overall)),
                                 def_overall = median(as.numeric(away2$def_overall)),
                                 opp_def_overall = median(as.numeric(home2$def_overall)),
                                 opp_off_overall = median(as.numeric(home2$off_overall)),
                                 sacks_allowed = median(as.numeric(away2$sacks_allowed)),
                                 #opp_sacks_allowed = median(as.numeric(home$sacks_allowed)),
                                 qb_hurries_allowed = median(as.numeric(away2$qb_hurries_allowed)),
                                 #opp_qb_hurries_allowed = median(as.numeric(home$qb_hurries_allowed)),
                                 def_rushing = median(as.numeric(away2$def_rushing)),
                                 opp_def_rushing = median(as.numeric(home2$def_rushing)),
                                 def_passing = median(as.numeric(away2$def_passing)),
                                 opp_def_passing = median(as.numeric(home2$def_passing)),
                                 off_rushing = median(as.numeric(away2$off_rushing)),
                                 opp_off_rushing = median(as.numeric(home2$off_rushing)),
                                 off_passing = median(as.numeric(away2$off_passing)),
                                 opp_off_passing = median(as.numeric(home2$off_passing)), home_dummy = 0, turnovers = median(as.numeric(away2$turnovers)) ,opp_turnovers = median(as.numeric(home2$turnovers)),
                                 total_yards = median(as.numeric(away2$total_yards)), opp_total_yards = median(as.numeric(away2$opp_total_yards)), yards_per_pass = median(as.numeric(away2$yards_per_pass)) , opp_ypp = median(as.numeric(away2$opp_ypp)),
                                 opp_ypr = median(as.numeric(away2$yards_per_rush_attempt)),yards_per_rush_attempt = median(as.numeric(home2$yards_per_rush_attempt)),
                                 opp_off_success_rate = median(as.numeric(away2$opp_off_success_rate)), opp_off_explosiveness = median(as.numeric(away2$opp_off_explosiveness)),
                                 opp_off_power_success = median(as.numeric(away2$opp_off_power_success)),
                                 opp_def_success_rate = median(as.numeric(away2$opp_def_success_rate)), opp_def_explosiveness = median(as.numeric(away2$opp_def_explosiveness)),
                                 opp_def_power_success = median(as.numeric(away2$opp_def_power_success)),
                                 off_success_rate = median(as.numeric(away2$off_success_rate)),off_explosiveness = median(as.numeric(away2$off_explosiveness)),
                                 off_power_success = median(as.numeric(away2$off_power_success)),
                                 def_success_rate = median(as.numeric(away2$def_success_rate)), def_explosiveness = median(as.numeric(away2$def_explosiveness)),
                                 def_power_success = median(as.numeric(away2$def_power_success)))
      
      
      
      
      ######### home #############
      X = home[c('elo','opp_elo',
                 'off_overall',
                 'def_overall',
                 'opp_def_overall',
                 'opp_off_overall',
                 'sacks_allowed',
                 'qb_hurries_allowed',
                 'def_rushing',
                 'opp_def_rushing',
                 'def_passing',
                 'opp_def_passing',
                 'off_rushing',
                 'opp_off_rushing',
                 'off_passing',
                 'opp_off_passing', 'home_dummy', 'turnovers', 'opp_turnovers',
                 'total_yards', 'opp_total_yards', 'yards_per_pass','opp_ypp', 'opp_ypr', 'yards_per_rush_attempt',
                 'opp_off_success_rate', 'opp_off_explosiveness', 'opp_off_power_success',
                 'opp_def_success_rate', 'opp_def_explosiveness', 'opp_def_power_success',
                 'off_success_rate', 'off_explosiveness', 'off_power_success',
                 'def_success_rate', 'def_explosiveness', 'def_power_success')]
      Y = home['points']
      for(i in 1:ncol(X)){
        X[,i] = as.numeric(X[,i])
      }
      
      
      
      hp<-c()
      for(k in 1:50){
        r = seq(1,nrow(X), 1)
        s = sample(r, 3 ,replace = F)
        train_x = data.matrix(X[-s,])
        train_y = data.matrix(Y[-s,])
        test_x = data.matrix(X[s,])
        test_y = data.matrix(Y[s,])
        
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
        xgb_train = xgb.DMatrix(data = train_x, label = train_y)
        xgb_test = xgb.DMatrix(data = test_x, label = test_y)
        
        
        watchlist = list(train=xgb_train, test=xgb_test)
        
        params <- list(#booster = "gblinear",
          objective = "reg:squarederror")
        
        xgb_base <- 0
        xgb_base <- xgb.train(params = params,
                              data = xgb_train,
                              nrounds =1000,
                              print_every_n = 100,
                              max_depth = 10,
                              eval_metric = "rmse",
                              early_stopping_rounds = 100,
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
      X = away[c('elo','opp_elo',
                 'off_overall',
                 'def_overall',
                 'opp_def_overall',
                 'opp_off_overall',
                 'sacks_allowed',
                 'qb_hurries_allowed',
                 'def_rushing',
                 'opp_def_rushing',
                 'def_passing',
                 'opp_def_passing',
                 'off_rushing',
                 'opp_off_rushing',
                 'off_passing',
                 'opp_off_passing', 'home_dummy', 'turnovers', 'opp_turnovers',
                 'total_yards', 'opp_total_yards', 'yards_per_pass','opp_ypp', 'opp_ypr', 'yards_per_rush_attempt',
                 'opp_off_success_rate', 'opp_off_explosiveness', 'opp_off_power_success',
                 'opp_def_success_rate', 'opp_def_explosiveness', 'opp_def_power_success',
                 'off_success_rate', 'off_explosiveness', 'off_power_success',
                 'def_success_rate', 'def_explosiveness', 'def_power_success')]
      Y = away['points']
      for(i in 1:ncol(X)){
        X[,i] = as.numeric(X[,i])
      }
      
      
      
      
      
      ap<-c()
      
      for(k in 1:50){
        r = seq(1,nrow(X), 1)
        s = sample(r, 3 ,replace = F)
        train_x = data.matrix(X)
        train_y = data.matrix(Y)
        test_x = data.matrix(X[s,])
        test_y = data.matrix(Y[s,])
        
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
        xgb_train = xgb.DMatrix(data = train_x, label = train_y)
        xgb_test = xgb.DMatrix(data = test_x, label = test_y)
        
        
        watchlist = list(train=xgb_train, test=xgb_test)
        
        params <- list(#booster = "gblinear",
          objective = "reg:squarederror")
        
        xgb_base <- 0
        xgb_base <- xgb.train(params = params,
                              data = xgb_train,
                              nrounds =1000,
                              print_every_n = 100,
                              max_depth = 10,
                              eval_metric = "rmse",
                              early_stopping_rounds = 100,
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
      y$ht_spread[j] = ifelse(is.null(cfbd_betting_lines(year = 2022, week = week, team = ht[j])$spread), 0, cfbd_betting_lines(year = 2022, week = week, team = ht[j])$spread)
      #y$ht_spread[j] = -11
      print(ht[j]);print(home_points);print(at[j]);print(away_points)
      setTxtProgressBar(pb2, j)
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
    df = rbind(df,y)
  }
  y

  # library(writexl)
  # write_xlsx(df,"natty.xlsx")
  # df
  # 
  
  ggplot(y, aes(x = seq(1,nrow(y),1),y = value_rating)) +
    geom_cfb_logos(aes(team = value_side),y = y$value_rating +.025, width = 0.075) +
    geom_cfb_logos(aes(team = x_axis),y =-.02, width = 0.075)+
    #geom_median_lines(aes( h_var = 0), size = 2) +
    geom_col(aes(fill = value_side, color = value_side),size = 1.5) +
    annotate(cfbplotR::GeomCFBlogo,x = 1.55,y = .42 + .03,team = 'B12',height = .2,alpha = .3) +
    labs(x = 'Opponent', y = 'Value (Model Favorite)', title = 'CFB Value Finder', subtitle = 'Picks Against The Spread')+
    scale_fill_cfb(alpha = .8) +
    scale_color_cfb(alt_colors = ht) +
    ylim(-.02, .5)+
    scale_x_cfb(size = 16) +
    theme_light()
  
  
  
  
  
  
  
  
  
  
}
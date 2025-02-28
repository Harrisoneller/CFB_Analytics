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

bet <- cfbd_betting_lines(year=2024,week=1)

conference_input = c('SEC','ACC','Big 12','Big Ten')

year <-2024
week<-1

get_XGB_projections <- function(conference_input, week, year=2024){
  
current_elo <- cfbd_ratings_elo(year = year-1) #cfbd_ratings_elo(year = year, week = week)


###### gather team game by game data #######
conferences<-c('SEC','ACC','B1G','PAC','B12')
df = tibble()
for (conf in conferences) {
  temp_df_0<-cfbd_game_team_stats(year = year-2,conference = conf)
  temp_df_0['season'] = year-2
  
  temp_df_1<-cfbd_game_team_stats(year = year-1,conference = conf)
  temp_df_1['season'] = year-1
  temp_df_2<-cfbd_game_team_stats(year = year,conference = conf)
  temp_df_2['season'] = year
  
  temp_df<-rbind(temp_df_1,temp_df_2,temp_df_0)
  df<-rbind(df,temp_df)
}

temp0<-cfbd_stats_game_advanced(year=year-2) # use for individual teams as well 
temp1<-cfbd_stats_game_advanced(year=year-1) # use for individual teams as well 
temp2<-cfbd_stats_game_advanced(year=year)
temp<-rbind(temp1,temp2, temp0)
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


for (y in (year-2):year){
  recruiting <- cfbd_recruiting_position(start_year = year-3,end_year = year)
  try(
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
  ) # end try
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

data$weights <- 1#ifelse(as.numeric(substr(as.character(home$season),4,4))==2,as.numeric(substr(as.character(home$season),4,4)),as.numeric(substr(as.character(home$season),4,4))*2)


X = data[,!names(data) %in% c("points",'weights')]
Y = data[,'points']
r = seq(1,nrow(X), 1)
s = sample(r, round(length(r)*.20) ,replace = F)
# test_sample = sample(r, round(length(r)*.30) ,replace = F)
# val_sample = sample(test_sample, round(length(test_sample)*.1) ,replace = F)
train_x = data.matrix(X[-s,])
train_y = data.matrix(Y[-s,])
test_x = data.matrix(X[s,])
test_y = data.matrix(Y[s,])
# train_x = data.matrix(X[-c(test_sample,val_sample),])
# train_y = data.matrix(Y[-c(test_sample,val_sample),])
# test_x = data.matrix(X[c(test_sample),])
# test_y = data.matrix(Y[c(test_sample),])
# val_x = data.matrix(X[c(val_sample),])
# val_y = data.matrix(Y[c(val_sample),])
# train_weights = data.matrix(data$weights[-c(test_sample,val_sample)])
# test_weights = data.matrix(data$weights[c(test_sample)])
# val_weights = data.matrix(data$weights[c(val_sample)])
train_weights = data.matrix(data$weights[-s])
test_weights = data.matrix(data$weights[s])

# set up the cross-validated hyper-parameter search
xgb_grid_1 = expand.grid(
  nrounds = 500,
  eta = c(0.01, 0.001, 0.0001),
  max_depth = c(2, 4, 6, 8, 10),
  gamma = 1
)
# pack the training control parameters
xgb_trcontrol_1 = trainControl(
  method = "cv",
  number = 8,
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
params <- list(booster = "gbtree",
               objective = "reg:absoluteerror")

xgb_base <- 0
xgb_base <- xgb.train(params = params,
                      data = xgb_train,
                      nrounds =1000,
                      print_every_n = 100,
                      max_depth = 8,
                      eval_metric = "mae",
                      early_stopping_rounds = 150,
                      trControl = xgb_trcontrol_1,
                      tuneGrid = xgb_grid_1,
                      watchlist = watchlist)
#model = xgb.train(data = xgb_train, max.depth = 3, nrounds = 70)
#final = xgboost(data = xgb_train, max.depth = 3, nrounds = 60)




# predict home points #


# ht <- c('Tennessee','Tennessee')
# at <- c('Alabama','Oklahoma')


projections <- tibble()


  # y<-tibble()
  # ht<-cfbd_game_info(2024, week = week, conference = c('SEC'))$home_team
  # at<-cfbd_game_info(2024, week = week, conference = conf)$away_team
  # 
  schedule<-cfbd_game_info(2024, week = week)
  schedule<-schedule[which(schedule$home_conference %in% conference_input | schedule$away_conference %in% conference_input),]
  ht <- schedule$home_team
  at <- schedule$away_team
  
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
  
  
  
new_data_home<- tibble(#points=(median(home$points)+median(away$points_allowed))/2, 
    points_allowed = (median(home$points_allowed)+median(away$points))/2,
    team_elo = pull(current_elo[which(current_elo$team == ht[j]),'elo']),
    opponent_elo = pull(current_elo[which(current_elo$team == at[j]),'elo']),
    recruting_rating = pull(recruiting[which(recruiting$position_group == 'All Positions' & recruiting$team == ht[j]),'avg_rating']),
    qb_rating = pull(recruiting[which(recruiting$position_group == 'Quarterback' & recruiting$team == ht[j]),'avg_rating']),
    opponent_recruiting_rating = pull(recruiting[which(recruiting$position_group == 'All Positions' & recruiting$team == at[j]),'avg_rating']),
    opponent_qb_rating = pull(recruiting[which(recruiting$position_group == 'Quarterback' & recruiting$team == at[j]),'avg_rating']),
    yards_per_pass = (median(home$yards_per_pass)+median(away$yards_per_pass_allowed))/2,
    yards_per_pass_allowed = (median(home$yards_per_pass_allowed)+median(away$yards_per_pass))/2,
    yards_per_rush_attempt = (median(home$yards_per_rush_attempt)+median(away$yards_per_rush_attempt_allowed))/2,
    yards_per_rush_attempt_allowed = (median(home$yards_per_rush_attempt_allowed)+median(away$yards_per_rush_attempt))/2,
    turnovers = (median(home$turnovers)+median(away$turnovers_allowed))/2,
    turnovers_allowed = (median(home$turnovers_allowed)+median(away$turnovers))/2,
    home_away = 'home',
    off_success_rate = (median(home$off_success_rate)+median(away$def_success_rate))/2,
    def_success_rate = (median(home$def_success_rate)+median(away$off_success_rate))/2,
    def_drives = (median(home$def_drives)+median(away$off_drives))/2,
    off_drives = (median(home$off_drives)+median(away$def_drives))/2
  )
  
  
  
new_data_away<- tibble(#points=(median(home$points)+median(away$points_allowed))/2, 
  points_allowed = (median(away$points_allowed)+median(home$points))/2,
  team_elo = pull(current_elo[which(current_elo$team == at[j]),'elo']),
  opponent_elo = pull(current_elo[which(current_elo$team == ht[j]),'elo']),
  recruting_rating = pull(recruiting[which(recruiting$position_group == 'All Positions' & recruiting$team == at[j]),'avg_rating']),
  qb_rating = pull(recruiting[which(recruiting$position_group == 'Quarterback' & recruiting$team == at[j]),'avg_rating']),
  opponent_recruiting_rating = pull(recruiting[which(recruiting$position_group == 'All Positions' & recruiting$team == ht[j]),'avg_rating']),
  opponent_qb_rating = pull(recruiting[which(recruiting$position_group == 'Quarterback' & recruiting$team == ht[j]),'avg_rating']),
  yards_per_pass = (median(away$yards_per_pass)+median(home$yards_per_pass_allowed))/2,
  yards_per_pass_allowed = (median(away$yards_per_pass_allowed)+median(home$yards_per_pass))/2,
  yards_per_rush_attempt = (median(away$yards_per_rush_attempt)+median(home$yards_per_rush_attempt_allowed))/2,
  yards_per_rush_attempt_allowed = (median(away$yards_per_rush_attempt_allowed)+median(home$yards_per_rush_attempt))/2,
  turnovers = (median(away$turnovers)+median(home$turnovers_allowed))/2,
  turnovers_allowed = (median(away$turnovers_allowed)+median(home$turnovers))/2,
  home_away = 'away',
  off_success_rate = (median(away$off_success_rate)+median(home$def_success_rate))/2,
  def_success_rate = (median(away$def_success_rate)+median(home$off_success_rate))/2,
  def_drives = (median(away$def_drives)+median(home$off_drives))/2,
  off_drives = (median(away$off_drives)+median(home$def_drives))/2
)

  
new_data_home = data.matrix(new_data_home[,!names(new_data_home) %in% c("points","weights")])
new_data_away = data.matrix(new_data_away[,!names(new_data_away) %in% c("points","weights")])

  
  
  
  y<- tibble( home = ht[j],
              home_score = predict(xgb_base,new_data_home),
              away = at[j],
              away_score = predict(xgb_base,new_data_away),
              home_spread = -(home_score - away_score),
              total = (home_score + away_score),
              home_conf = pull(schedule[j,'home_conference']),
              away_conf =  pull(schedule[j,'away_conference'])
  )
  
  projections <- rbind(projections,y)
  
} #end of individual conference loop


return(projections)

} #end function



y<-get_XGB_projections(conference_input = conference_input, week=1)
team_info <- cfbd_team_info()
team_info <- team_info %>% filter(conference %in% c("SEC","Pac-12","Big Ten","Big 12","ACC"))

y['value_pick']=""
y['vegas_spread']=0

for (game in 1:nrow(y)){
  
  temp_bet = bet[which( ( bet$home_team == y$home[game]) &  (bet$away_team == y$away[game]) & (bet$provider == 'DraftKings') ),]
  if (length(temp_bet) > 0 ){
    if( (as.numeric(temp_bet$spread) > 0 ) & (as.numeric(temp_bet$spread) < y$home_spread[game] ) ){y$value_pick[game] <- y$away[game]}
    if( (as.numeric(temp_bet$spread) > 0 ) & (as.numeric(temp_bet$spread) > y$home_spread[game] ) ){y$value_pick[game] <- y$home[game]}
    if( (as.numeric(temp_bet$spread) < 0 ) & (as.numeric(temp_bet$spread) > y$home_spread[game] ) ){y$value_pick[game] <- y$home[game]}
    if( (as.numeric(temp_bet$spread) < 0 ) & (as.numeric(temp_bet$spread) < y$home_spread[game] ) ){y$value_pick[game] <- y$away[game]}
    y$vegas_spread[game] = as.numeric(temp_bet$spread[1])
  }else{
    temp_bet = bet[which( ( bet$home_team == y$home[game]) &  (bet$away_team == y$away[game]) & (bet$provider == 'ESPN Bet') ),]
    if( (as.numeric(temp_bet$spread) > 0 ) & (as.numeric(temp_bet$spread) < y$home_spread[game] ) ){y$value_pick[game] <- y$away[game]}
    if( (as.numeric(temp_bet$spread) > 0 ) & (as.numeric(temp_bet$spread) > y$home_spread[game] ) ){y$value_pick[game] <- y$home[game]}
    if( (as.numeric(temp_bet$spread) < 0 ) & (as.numeric(temp_bet$spread) > y$home_spread[game] ) ){y$value_pick[game] <- y$home[game]}
    if( (as.numeric(temp_bet$spread) < 0 ) & (as.numeric(temp_bet$spread) < y$home_spread[game] ) ){y$value_pick[game] <- y$away[game]}
    y$vegas_spread[game] = as.numeric(temp_bet$spread[1])
  }
}


  conf = "Big 12"
  temp <- subset(team_info,conference == conf)

  team_plot_data <- y %>%
          filter(home %in% temp$school | away %in% temp$school)

  team_plot_data$conference <- conf
  team_plot_data$home_score[7]<-36.6
  team_plot_data$away_score[3]<-28.24
  #team_plot_data$value_pick[1]<-"Oklahoma"
  #team_plot_data<-team_plot_data[-c(1,2,3),]
 
  
  team_plot_data %>%
    transmute(Conference = conference, Home_Team = home,
              Home_Score = round(home_score,2),
              Away_Score = round(away_score,2), Away_Team = away, Spread_Pick = value_pick) %>%
    arrange(desc(Conference)) %>%
    gt() %>%
    gt_fmt_cfb_logo(columns = c("Conference", "Spread_Pick")) %>%
    gt_fmt_cfb_wordmark(columns = c("Home_Team","Away_Team")) %>%
    cols_align(
      align = c('center'),
      columns = everything()
    ) %>%
    tab_header(
      title = md("**Scarlett Score Predictions**"),
      subtitle = md("Harrison Eller")
    ) %>%
    fmt_number(
      columns = c("Home_Score", "Away_Score")
    ) %>%
    tab_style(
      style = list(
        cell_text(weight = "bold")
      ),
      locations = cells_body(
        columns = c("Home_Score", "Away_Score")
      )
 )

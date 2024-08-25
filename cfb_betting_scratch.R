library(cfbfastR)
library(tidyverse)
library(lubridate)
library(dplyr)
library(readr)
#remotes::install_github(repo = "sportsdataverse/cfbfastR")
remotes::install_github(repo = "Kazink36/cfbplotR")
library(cfbplotR)
Sys.setenv(CFBD_API_KEY = "x1C/67YV6Sy98uENGd+tSvJSr82NfDxHFTmWk4QB5wGl2qxogM53QKLB5T4l6kPn")
#Sys.setenv(CFBD_API_KEY = "OMFtwopAS5WexLsewwy5BKQsUIzguwFqGz6KkjiUc6zcpKNYphzld/71fWW7pt8j")
library(stringdist)
library(tidyverse)
library(nnet)
library(mgcv)
library(texreg)
library(aod)
library(xtable)
library(xgboost)
library(readxl)
library(stringr)
library(caret)
library(car)
library(tidyverse)
setwd("C:/Users/harri/OneDrive/Desktop/LTB/CFB Analysis/Data Visualization")

df <- read_xlsx('Over_percentage.xlsx')
teams = cfbd_team_info(only_fbs = TRUE)
df$y = df$Over_Percentage - .5

conferences = c("SEC","Big Ten","Big 12","ACC","Pac-12","FBS Independents")

P5 = subset(teams, conference %in% conferences)
G5 = subset(teams, conference %in% conferences)
# for(i in 1:nrow(df)){
#   df$team_name[i] <- teams$school[amatch(df$Team[i], teams$school,maxDist = 3)]
# }
df <- subset(df,!(Team %in% P5$school))

df

ggplot(df,aes(x = reorder(Team,-y),y = y)) +
  geom_cfb_logos(aes(team = Team),y=ifelse(df$y >0 ,df$y + .02, df$y -.02), width = 0.015)+
  geom_col(aes(fill = Team, color = Team),size = .5) +
  #coord_flip(ylim= c(0,.95))+
  scale_fill_cfb(alpha = .8) +
  scale_color_cfb(alt_colors = df$Team)+
  labs(x = 'Team', y = 'Differential', title = 'O/U Cover Percentage (G5)', subtitle = "Harrison Eller")+
  theme(axis.ticks.x=element_blank(),axis.text.x=element_blank()) 





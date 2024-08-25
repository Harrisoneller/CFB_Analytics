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
library(gt)


team_plot_data = read.csv('tpd.csv')


team_plot_data$conference = 0

for(i in 1:nrow(team_plot_data)){
  team_plot_data$conference[i] = cfbd_stats_season_team(year = 2022, team = team_plot_data$ht[i])$conference
  
}

team_plot_data$ht_score[3]=42.2
team_plot_data$value_side[3] = "Vanderbilt"
team_plot_data$at_score[2]=16.2
team_plot_data$value_side[2] = "Ohio"
team_plot_data$value_side[4] = "Louisiana Tech"

team_plot_data %>%
  transmute(Conference = conference, Home_Team = ht,
            Home_Score = round(ht_score,2),
            Away_Score = round(at_score,2), Away_Team = at, Spread_Pick = value_side) %>%
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
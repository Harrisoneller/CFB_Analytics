library(nflfastR)
library(tidyverse)
library(nflplotR)
library(ggplot2)
library(extrafont)
library(ggrepel)
library(ggimage)
library(ggridges)
library(ggtext)
library(ggfx)
library(geomtextpath)
library(cropcircles)
library(magick)
library(glue)
library(gt)
library(gtExtras)
Sys.setenv(CFBD_API_KEY = "x1C/67YV6Sy98uENGd+tSvJSr82NfDxHFTmWk4QB5wGl2qxogM53QKLB5T4l6kPn")

players<-cfbd_stats_season_player(2023)
qb<-c("Caleb Williams","Jayden Daniels","Drake Maye","Michael Penix Jr.","Bo Nix","J.J. McCarthy")

temp <- players %>% 
  filter(player %in% qb, category %in% c('passing','rushing'))



pass <- c('player',"passing_completions", "passing_att" ,  "passing_pct", "passing_yds"  ,"passing_td" ,       
   "passing_int","passing_ypa")

rush<-c("rushing_car"  ,       "rushing_yds"   ,      "rushing_td"  ,       
        "rushing_ypc"  ,       "rushing_long"  , "fumbles_lost")



p <- temp %>% 
  filter(category == "passing") %>% 
  select(all_of(pass))

p %>%
  transmute(Player = player, Comp_PCT = passing_pct, Passing_TDs = round(passing_td,0), Pass_Yds = round(passing_yds,0),  INTs = round(passing_int,0),Pass_YPA = round(passing_ypa,1),Completions = passing_completions, Att = passing_att) %>% 
  gt(rowname_col = c("Player")) %>% 
  #gt_fmt_cfb_logo(columns = c('Team')) %>% 
  cols_label(
    Player = md("**Player**"),
    Comp_PCT = md("**Comp PCNT**"),
    Passing_TDs = md("**Pass TDS**"),
    Pass_Yds = md("**Pass YDS**"),
    INTs = md("**INT**"),
    Pass_YPA = md("**YPA**"),
    Completions = md("**Completions**"),
    Att = md("**Attempts**")
  ) %>% 
  fmt_number(
    columns = c(Passing_TDs,Pass_Yds,INTs,Completions, Att),
    decimals = 0
  ) %>% 
  data_color(
    columns = c(Passing_TDs),
    colors = scales::col_numeric(
      palette = c('red','white','green'),
      domain = c(min(p$passing_td),max(p$passing_td)
      )
    )
  ) %>% 
  data_color(
    columns = c(Comp_PCT),
    colors = scales::col_numeric(
      palette = c('red','white','green'),
      domain = c(min(p$passing_pct),max(p$passing_pct)
      )
    )
  )  %>% 
  data_color(
    columns = c(INTs),
    colors = scales::col_numeric(
      palette = c('green','white','red'),
      domain = c(min(p$passing_int),max(p$passing_int)
      )
    )
  ) %>% 
  data_color(
    columns = c(Pass_YPA),
    colors = scales::col_numeric(
      palette = c('red','white','green'),
      domain = c(min(p$passing_ypa),max(p$passing_ypa)
      )
    )
  ) %>% 
  data_color(
    columns = c(Completions),
    colors = scales::col_numeric(
      palette = c('red','white','green'),
      domain = c(min(p$passing_completions),max(p$passing_completions)
      )
    )
  )  %>% 
  data_color(
    columns = c(Att),
    colors = scales::col_numeric(
      palette = c('red','white','green'),
      domain = c(min(p$passing_att),max(p$passing_att)
      )
    )
  ) %>% 
  data_color(
    columns = c(Pass_Yds),
    colors = scales::col_numeric(
      palette = c('red','white','green'),
      domain = c(min(p$passing_yds),max(p$passing_yds)
      )
    )
  ) %>% 
  tab_header(
    title = md("**Statistical Comparison**"),
    subtitle = md("2023 Season (Statletics)")
  ) 








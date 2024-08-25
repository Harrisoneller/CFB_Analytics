library(gt)
library(dplyr)
library(readr)
library(cfbfastR)
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
library(stringdist)
library(readxl)
library(cfbplotR)
library(writexl)
#setwd("C:/Users/Harrison Eller/OneDrive/Desktop/LTB/CBB Analytics/CSVs")
Sys.setenv(CFBD_API_KEY = "x1C/67YV6Sy98uENGd+tSvJSr82NfDxHFTmWk4QB5wGl2qxogM53QKLB5T4l6kPn")


teams  = cfbd_team_info(conference = 'PAC')

school = teams$school


write_xlsx(teams,"C:/Users/harri/OneDrive/Desktop/LTB/CFB Analysis/PAC.xlsx")

df<-read_xlsx("C:/Users/harri/OneDrive/Desktop/LTB/CFB Analysis/PAC.xlsx")
df$Conf_Winner_Odds<- 1/ df$Conf_Winner_Odds

df$prob <-  df$Conf_Winner_Odds/sum(df$Conf_Winner_Odds)

df$percent_chance <-df$prob*100


ggplot(df, aes(x = reorder(school,-percent_chance) ,y = percent_chance)) +
  geom_cfb_logos(aes(team = school),y = df$percent_chance + 2.5, width = 0.06) +
  #geom_median_lines(aes( h_var = .5), size = 1) +
  #geom_median_lines(aes( v_var = .5), size = 1) +
  geom_col(aes(fill = school, color = school),size = 1.5) +
  #annotate(cfbplotR::GeomCFBlogo,x = min(ats$MOV) + 2,y = .9,team = 'NCAA',height = .2,alpha = .3) +
  labs(x = 'Team', y = 'Probability To Win Conference (%)', title = 'PAC 12 Odds To Win Division', subtitle = 'Harrison Eller')+
  scale_fill_cfb(alpha = .8) +
  scale_color_cfb(alt_colors = school) +
  ylim(0, 40)+
  scale_x_cfb(size = 14) +
  theme_light()+
  theme(axis.ticks.x=element_blank(), axis.text.x=element_blank()) 



dfc <- df %>% 
  select(school,percent_chance,mascot,conference)
dfc$Win_Probability <- dfc$percent_chance
dfc$team <- dfc$school
dfc$Mascot <- dfc$mascot
dfc <- dfc %>% 
  select(team,Win_Probability,conference,Mascot)


dfc %>% 
  dplyr::transmute(.data$conference, 
                   .data$team, 
                   logo = team, 
                   .data$Mascot, 
                   #wordmark = team,
                   Win_Probability = round(Win_Probability, 2)) %>% 
  dplyr::arrange(desc(Win_Probability)) %>% 
  head(14) %>% 
  gt() %>% 
  gt_fmt_cfb_logo(columns = c("conference","logo")) %>% 
  #gt_fmt_cfb_wordmark(columns = "wordmark") %>% 
  gt_merge_stack_team_color("team", "Mascot", "team") %>% 
  cols_label(
    conference = "Conference",
    team = "Team",
    logo = "Logo",
    Win_Probability = "Win Probability (%)"
  ) %>% 
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      columns = Win_Probability
    )
  )
  

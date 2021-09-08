library(tidyverse)
library(lubridate)
library(ggthemes)
library(knitr)
library(plotly)



plays <- read_csv('game_plays.csv')
plays$year <- as.numeric(substring(plays$game_id, 1, 4))

player_info <-  read_csv('player_info.csv')
games <-  read_csv('game.csv')
teams <-  read_csv('team_info.csv')
player_plays <- read_csv('game_plays_players.csv')


#--------------Goals--------------------#

Goals <- plays %>% filter(event == 'Goal')


Goals <- Goals %>% 
  left_join(player_plays %>% filter(playerType == 'Scorer') %>% 
              select(play_id, player_id), 
      by = 'play_id')
  
Goals <- Goals %>% 
  left_join(player_info %>% select(player_id, firstName, lastName, primaryPosition, birthDate),
      by = 'player_id')


Goals %>% group_by(year) %>% count(event== 'Goal') 

Goals <- Goals %>% 
          left_join(games %>% select(game_id, season, type),
            by = 'game_id')

Goals %>% 
  group_by(year) %>% 
  filter(type == 'R') %>% 
  count(event == 'Goal')


Goals$season <- as.character(Goals$season)

  
#Remove any duplicates based on play_id, 
#Was getting up to 3 times amount of Goals for 2018 - Present Seasons
Goals = Goals[!duplicated(Goals$play_id),]






Goals %>% 
  ggplot(aes(season)) + 
  geom_bar() + 
  labs(x = "Season", y = "Goals") +
  coord_flip()









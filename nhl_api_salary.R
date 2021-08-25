library(nhlapi)
library(rvest)
library(xml2)
library(tidyverse)
library(httr)
library(jsonlite)

# Reference Examples data using Steven Stamkos
stamkos_2010_stats <- as_tibble(nhl_players_seasons('Steven Stamkos', 2010))
stamkos_career_stats <-  nhl_players_allseasons('Steven Stamkos')
stamkos_career_regular_season_stats <- 'https://statsapi.web.nhl.com/api/v1/people/8474564/stats?stats=statscareerRegularSeason&season=20202021'

# Scrape hockey reference for salary table -----------------------------------------------
nhl_salary_2021_2022_url<- 'https://www.hockey-reference.com/friv/current_nhl_salaries.cgi'
nhl_salary_2021_2022_html <- read_html(nhl_salary_2021_2022_url)

# Tidy the salary table
salary_table <- html_table(nhl_salary_2021_2022_html) %>% 
  rbindlist() %>% 
  as_tibble() %>% 
  select( -c('Tm', 'Cap Hit'))

# Scrape the salary alternative
player_salary <- nhl_salary_2021_2022_html %>% 
  html_nodes('body') %>% 
  xml_find_all("//td[contains(@data-stat, 'salary')]") %>% 
  html_text()

# Get season stats for all skaters in 2020 season-------------------------------------------
players_names <- salary_table$Player[1:704]
all_skaters_stats <- nhl_players_seasons(players_names, 2020)

# Get team Rosters-------------------------------------------------------------------------
nhl_teams <- nhl_teams()
rosters <- nhl_teams_rosters(nhl_teams$id)

# Extract player ids from rosters.-----------------------------------------------------------
player_ids <- rosters$roster.roster %>% 
  lapply('[', c('person.id')) %>% 
  flatten() %>% 
  flatten()

# Combine all player_ids to api url.--------------------------------------------------------------
url_start <- "https://statsapi.web.nhl.com/api/v1/people/"
url_end <-  "/stats?stats=careerRegularSeason&season=20192020"
full_urls <- paste0(url_start, player_ids$value, url_end) %>% 
  as_tibble()


# Get career regular season stats for all skaters in all seasons til present.--------------------
get_url_example <- "https://statsapi.web.nhl.com/api/v1/people/8478421/stats?stats=careerRegularSeason&season=20192020"
get_career_regular_season_stats_all <- lmap(full_urls$value[1:5], fromJSON)



get_career_regular_season_stats_all$stats$splits[[1]]$stat$assists
get_career_regular_season_stats_all$stats$splits[[1]]$stat$goals
  


player_stats <- vector(mode= 'list', length=0)

test_dataframe  <- data.frame(assists=double(),
                 goals=double(),
                 playerid= integer())


nhl_api <- function(i) {
  
  resp <- GET(full_urls$value[i])
  if (http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  
  parsed <- fromJSON(content(resp, 'text'), simplifyVector = FALSE)
  
  structure(
    list(
      content = parsed,
      responce = resp
    ),
    class = 'nhl_api'
  )
# print.nhl_api <- function(x, ...) {
#   str(x$content)
#   invisible(x)
# }
}







  



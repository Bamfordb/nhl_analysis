library(nhlapi)
library(rvest)
library(xml2)
library(tidyverse)
library(httr)
library(jsonlite)

# Use the NHL Api to retrieve data and merge with salary data in CSV.

# # Reference Examples data using Steven Stamkos.--------------------------------------------
# stamkos_2010_stats <- as_tibble(nhl_players_seasons('Steven Stamkos', 2010))
# stamkos_career_stats <-  nhl_players_allseasons('Steven Stamkos')
# stamkos_career_regular_season_stats <- 'https://statsapi.web.nhl.com/api/v1/people/8474564/stats?stats=statscareerRegularSeason&season=20202021'
# 
# # Get season stats for all skaters in 2020 season-------------------------------------------
# players_names <- salary_table$Player[1:704]
# all_skaters_stats <- nhl_players_seasons(players_names, 2020)

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


# Get team Rosters-------------------------------------------------------------------------
nhl_teams <- nhl_teams()
rosters <- nhl_teams_rosters(nhl_teams$id)

# Extract player ids from rosters.-----------------------------------------------------------
basic_player_info <- ldply(rosters$roster.roster, rbind())
player_ids <- as_tibble(basic_player_info$person.id)


# Combine all player_ids to api url.--------------------------------------------------------------
url_start <- "https://statsapi.web.nhl.com/api/v1/people/"
url_end <-  "/stats?stats=careerRegularSeason&season=20192020"
full_urls <- paste0(url_start, player_ids$value, url_end) %>% 
  as_tibble()


# Get career regular season stats for all skaters in all seasons til present.------------------------------------
get_url_example <- "https://statsapi.web.nhl.com/api/v1/people/8478421/stats?stats=careerRegularSeason&season=20192020"


# Function from GET request and parse down to a more usable format.-----------------------------------
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
    class = 'list'
  )
# print.nhl_api <- function(x, ...) {
#   str(x$content)
#   invisible(x)
# }
}


# Run all player_ids through nhl_api function.------------------------------------------------------------
# Filter down to stats and make each entry its own row.
get_player_stats_regular_all <- nhl_api(1)

all_player_career_regular_stats <-   enframe(unlist(get_player_stats_regular_all)) %>% 
                                     filter(grepl('^content.stats.splits', name))

all_player_career_regular_stats$name <-  str_remove(all_player_career_regular_stats$name, 'content.stats.splits.stat.')

all_player_career_regular_stats <- pivot_wider(all_player_career_regular_stats, 
                                               names_from = name, 
                                               values_from = value)



# Make table of all seasons group by player_id based on unique url.
all_seasons_stats <- nhl_players_allseasons(playerIds = player_ids$value[1:856])
nhl_all_seasons_stats <- test %>% filter(league.name == 'National Hockey League') %>% 
    group_by(url) %>% 



nhl_career_table_url <- 'http://www.nhl.com/stats/skaters?reportType=season&seasonFrom=19901991&seasonTo=20202021&gameType=2&status=active&filter=gamesPlayed,gte,1&sort=points,goals,assists&page=0&pageSize=100'





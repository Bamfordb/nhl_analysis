library(tidyverse)
library(rvest)
library(data.table)
library(nhlapi)
library(lubridate)

# This script takes data from predownloaded csvs,
# and combines it with data scrapped from a salary table on hockey-reference
# fixes some errors and formats to be more tidy.

# Read in data ---------------------------------------------------------------------------
stat_data <- read_csv('all_stats_combined.csv')
bio_data <- read_csv('all_bio_combined.csv')
goalie_stat_data <- read_csv('goalie_sum.csv')
goalie_bio_data <- read_csv('goalie_bio.csv')

# Join csvs, Minimal removal duplicated rows.
# We will join goalie csvs after cleaning others.
half_data_join <- left_join(bio_data, stat_data, by = c('Player', 'GP')) %>% 
  select(-contains('.y')) 

# Change both players names due to corrupted special character.
half_data_join$Player[484] <- c('Alexis Lafreniere')
half_data_join$Player[822] <- c('Tim Stützle')

  
# Scrape hockey reference for salary table -----------------------------------------------
nhl_salary_2021_2022_url<- 'https://www.hockey-reference.com/friv/current_nhl_salaries.cgi'
nhl_salary_2021_2022_html <- read_html(nhl_salary_2021_2022_url)


# Tidy the salary table ------------------------------------------------------------------------------
salary_table <- html_table(nhl_salary_2021_2022_html) %>% 
  rbindlist() %>% 
  as_tibble() %>% 
  select( -c('Tm', 'Cap Hit'))

# Debug names that are not registering based on NHL Stats.-------------------------------------- 
# 47 current missing some due to name errors, others have multiple Ids
# 32 after fixing  15 misspelled names
# The remaining errors occur due to same name or 
# NHL Api doesn't recognize Player Name without PlayerId
nhl_players(merged_salary_flipped_table$Player)


# Finding difference between both Player names columns, Trying to match 707 entries with 926 total.
name_bio <- bio_data$Player %>% as_tibble()

sorted_name_bio <- name_bio %>% 
  separate(value, into = c('first_name', 'last_i'), sep = '[\\s]', extra = 'merge') %>% 
  arrange_at(vars(c(2, 1)))

name_salary <- salary_table %>% as_tibble()
colnames(name_salary) <- c('value1', 'Salary') 

sorted_name_salary <- name_salary  %>% 
  separate(value1, into = c('first_name1', 'last_i1'), sep = '[\\s]', extra = 'merge') %>% 
  arrange_at(vars(c(2, 1)))


# Place names where order is reversed and replace.
wrong_name_table <- sorted_name_salary %>% filter(str_detect(first_name1, ',$'))

wrong_name_table$first_name1 <- 
  str_remove(wrong_name_table$first_name1,  ',$')

flipped_wrong_name_table <- 
  rename(wrong_name_table, last_i1 = first_name1, first_name1 = last_i1, Salary = Salary)

flipped_wrong_name_table <- flipped_wrong_name_table[c('first_name1', 'last_i1', 'Salary')]

merged_salary_flipped_table <- sorted_name_salary %>% 
  filter(!str_detect(first_name1, ',$')) %>% 
  bind_rows(flipped_wrong_name_table) %>% 
  unite(Player, first_name1, last_i1, sep = ' ', remove = TRUE, na.rm = FALSE)

# Dictionary of misspelled names and their correct spelling.

misspelled_names_correct_names <- 
  c('Anthony DeAngelo'= 'Tony DeAngelo', 
    'Mitch Marner' = 'Mitchell Marner',
    'Alexander Nylander' = 'Alex Nylander',
    'Cal Petersen' = 'Calvin Petersen',
    'Evgeni Dadonov' = 'Evgenii Dadonov',
    'Joshua Brown' = 'Josh Brown',
    'Joshua Morrissey' = 'Josh Morrissey',
    'Matthew Benning' = 'Matt Benning',
    'Maxime Comtois' = 'Max Comtois',
    'Michael Matheson' = 'Mike Matheson',
    'Nicholas Paul' = 'Nick Paul',
    'Patrick Maroon' = 'Pat Maroon',
    'Samuel Blais' = 'Sammy Blais',
    'Zachary Sanford' = 'Zach Sanford',
    'Zachary Werenski' = 'Zach Werenski',
    'Michael Anderson' =  'Mikey Anderson',
    'Henrik Borgström' = 'Henrik Borgstrom',
    'Zachary Jones' = 'Zac Jones',
    'Dominik Kubalík' = 'Dominik Kubalik',
    'Joshua Norris' = 'Josh Norris'
    )

# Fix the spelling errors.
merged_salary_flipped_table$Player <- 
  merged_salary_flipped_table$Player %>% 
  str_replace_all(misspelled_names_correct_names)

clean_salary_table <- merged_salary_flipped_table


# Join tables together---------------------------------------------------------------------
full_data <- left_join(clean_salary_table, half_data_join, by = 'Player')


# Clean table of duplicate columns,--------------------------------------------------------
# Rename and mutate columns,
# Other cleaning and notable changes to table.
# Drop Sebastian Aho duplicates as there are two unique players
full_data <- full_data[-c(3, 4), ]

full_data <- full_data %>% 
  select(-c('HOF'))

full_data <- full_data %>% 
  rename(Laterality = 'S/C.x', 
         Position = Pos.x, 
         Country = Ctry, 
        Nationality = Ntnlty, 
        Height_cm = Ht, 
        Weight_lbs = Wt,
        Games_Played = GP,
        Goals = G.x, 
        Assists = A.x, 
        Points = P.x, 
        First_Season = '1st Season')

full_data$First_Season <- str_sub(full_data$First_Season, 1, 4)
full_data$DOB <- mdy(full_data$DOB)
full_data$DOB <- parse_date_time(full_data$DOB, 'ymd')



# See all columns that registered NAs, Most appear to be Goalies, 
# since goalie stats recorded in different place.
na <- full_data[rowSums(is.na(full_data)) > 0,]


# Join goalie csvs ---------------------------------------------------------------------
# Impute goalie data into missing columns
goalie_data <- left_join(goalie_bio_data, goalie_stat_data, by = c('Player', 'GP')) %>% 
  select(-contains('.y'),
         -c('HOF'))

# Clean to match column names in full_data
goalie_data <- goalie_data %>%   
  rename(Laterality = 'S/C.x',
         Country = Ctry, 
         Nationality = Ntnlty, 
         Height_cm = Ht, 
         Weight_lbs = Wt,
         Games_Played = GP, 
         Goals = G, 
         Assists = A, 
         Points = P, 
         First_Season = '1st Season',
         Wins = W.x, 
         Losses = L.x, 
         OTL = OT.x,
         SOL = SO.x, 
         Ties = T.x)

goalie_data$First_Season <- str_sub(goalie_data$First_Season, 1, 4)

# Get list of goalies that are on the na table 
# Find 56 goalies
na_goalie_merge <- merge(na, goalie_data, by = 'Player')

na_goalie_merge <- na_goalie_merge %>% 
  select(
    !contains('.x'))

na_goalie_merge <- na_goalie_merge %>% 
  setNames(gsub('\\.y', '', names(.)))


# Change Position from NA to G
na_goalie_merge[, 'Position'] = 'G'

na_goalie_merge$DOB <-  parse_date_time(na_goalie_merge$DOB, 'ymd')


# Coalesce duplicated columns together for final merge ------------------------------------------ 
full_plus_goalie <- full_join(full_data, na_goalie_merge, by = 'Player')


clean_final_table <- full_plus_goalie %>%  
  mutate(
        Salary = coalesce(Salary.x, Salary.y),
        Laterality = coalesce(Laterality.x, Laterality.y),
        Position = coalesce(Position.x, Position.y),
        DOB = coalesce(DOB.x, DOB.y),
        Birth_City = coalesce(`Birth City.x`, `Birth City.y`),
        'S/P' = coalesce(`S/P.x`, `S/P.y`),
        Country = coalesce(Country.x, Country.y),
        Nationality = coalesce(Nationality.x, Nationality.y),
        Height_cm = coalesce(Height_cm.x, Height_cm.y),
        Weight_lbs = coalesce(Weight_lbs.x, Weight_lbs.y),
        Draft_Year = coalesce(`Draft Yr.x`, `Draft Yr.y`),
        Round = coalesce(Round.x, Round.y),
        Overall = coalesce(Overall.x, Overall.y),
        First_Season = coalesce(First_Season.x, First_Season.y),
        Games_Played = coalesce(Games_Played.x, Games_Played.y),
        Goals = coalesce(Goals.x, Goals.y),
        Assists = coalesce(Assists.x, Assists.y),
        Points = coalesce(Points.x, Points.y),
        PPG = coalesce(PPG.x, PPG.y),
        PPP = coalesce(PPP.x, PPP.y),
        SHG = coalesce(SHG.x, SHG.y),
        SHP = coalesce(SHP.x, SHP.y),
        OTG = coalesce(OTG.x, OTG.y),
        GWG = coalesce(GWG.x, GWG.y),
        
        '+/-' = coalesce(`+/-.x`, `+/-.y`),
        PIM = coalesce(PIM.x, PIM.y),
        'P/GP' = coalesce(`P/GP.x`, `P/GP.y`),
        EVG = coalesce(EVG.x, EVG.y),
        GWG = coalesce(GWG.x, GWG.y),
        S = coalesce(S.x, S.y),
        'S%' = coalesce(`S%.x`, `S%.y`),
        'TOI/GP' = coalesce(`TOI/GP.x`, `TOI/GP.y`),
        'FOW%' = coalesce(`FOW%.x`, `FOW%.y`)
        ) %>% 
  select(
    !contains(c('.x', '.y')))
  
# Now just to find some of the remaining rows where information on player is missing.
players_no_data <- 
clean_final_table %>% 
  filter(is.na(Laterality)) %>% 
  select(Player) %>% 
  as_tibble()

clean_final_table <- filter(clean_final_table, 
                            !(Player %in% players_no_data$Player))


#  Find 13 of the 15 missing players data try and enter into existing table. 
# missing_bio_data <-  nhl_players(players_no_data$Player)  %>% 
#   as_tibble() %>% 
#   select(birthDate, birthCity, birthStateProvince, birthCountry, nationality, height, weight, primaryPosition.abbreviation)
# 
# 
# missing_stat_data <- nhl_players_allseasons(players_no_data$Player) %>% as_tibble() 
# names(missing_stat_data) <- gsub(pattern = '^stat.', replacement = '', names(missing_stat_data))
# 
# missing_stat_data <- missing_stat_data %>% 
#   select(
#     -c(season, sequenceNumber, blocked, shifts, team.name, team.link, team.id, league.name, league.link, league.id, 
#        url, copyright, seasonStart, shutouts, powerPlaySaves, shortHandedSaves, shortHandedShots,
#        powerPlaySavePercentage, shortHandedSavePercentage, evenStrengthSavePercentage))
# 
# 
# temp <- missing_stat_data %>% group_by(playerId)


# Players with no reported salary are ending there 3 year entry level contracts. All values at $832,500, 
players_no_salary <- 
  clean_final_table %>% 
  filter(Salary == '') 

# Fill blank salaries with correct salary.
clean_final_table$Salary[clean_final_table$Salary == ''] = '832,500'


# 694 of the 709 salaries, are reported in this table along with basic biographical data, and total career regular season stats.
# Some stats are not recorded for goalies, as goalie stats are measuring different metrics.
unorganized_final_table <- filter(clean_final_table, !(Player %in% players_no_data$Player))

# Last bit of cleaning, to reorganize the order of the columns in a more logical way.

# Move goalies stats to last column as most of these columns will be NA for non-goalies.
unorganized_final_table <- unorganized_final_table %>% 
  relocate(2:13, .after = last_col())

# Last is to move the Salary to the left, as that is the variable we are interested in.

clean_bio_stat_goalie_data <- unorganized_final_table %>% 
  relocate(Salary, .before = Player)



write_csv(clean_bio_stat_goalie_data, 'nhl_active_skater_salary_stats_2021.csv', col_names = TRUE)





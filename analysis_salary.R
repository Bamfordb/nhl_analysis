library(tidyverse)
library(rvest)
library(data.table)
library(nhlapi)

# Read in data ---------------------------------------------------------------------------
warin
bio_data <- read_csv('all_bio_combined.csv', encoding = 'UTF-8')

# Minimal cleaning duplicated rows 
half_data_join <- merge(bio_data, stat_data)
half_data_join <- half_data_join[!duplicated(half_data_join[c('Player', 'DOB')]), ]


# Scrape hockey reference for salary table -----------------------------------------------
nhl_salary_2021_2022_url<- 'https://www.hockey-reference.com/friv/current_nhl_salaries.cgi'
nhl_salary_2021_2022_html <- read_html(nhl_salary_2021_2022_url)


# Tidy the salary table ------------------------------------------------------------------------------
salary_table <- html_table(nhl_salary_2021_2022_html) %>% 
  rbindlist() %>% 
  as_tibble() %>% 
  select( -c('Tm', 'Cap Hit'))

# Fix some columns that contain incorrect names, or added punctuation.
salary_table$Player <- gsub(',', '', salary_table$Player) 

# Finding difference between both Player names columns, Trying to match 705 entries with 926 total.
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


# Debug names that are not registering based on NHL Stats.-------------------------------------- 
#47 current missing some due to name errors, others have multiple Ids
nhl_players(merged_salary_flipped_table$Player)
 

# Join tables together---------------------------------------------------------------------
full_data <- left_join(salary_table, half_data_join, by = 'Player') 
  

# Clean table of duplicate columns,--------------------------------------------------------
# Rename and mutate columns,
# Other cleaning and notable changes to table.
full_data <- full_data %>% 
  select(-c('HOF', 'GP.y', 'G.y', 'A.y', 'P.y', 'S/C.y', 'Pos.y'))

full_data <- full_data %>% 
  rename(Laterality = 'S/C.x', Postion = Pos.x, Country = Ctry, 
        Nationality = Ntnlty, Height_cm = Ht, Weight_lbs = Wt,
        Games_Played = GP.x, Goals = G.x, Assists = A.x, 
        Points = P.x, First_Season = '1st Season')

full_data$First_Season <- str_sub(full_data$First_Season, 1, 4)


# See all 96 columns that registered NAs, Most appear to be Goalies, since goalie stats are measured differently
na <- full_data[rowSums(is.na(full_data)) > 0,]





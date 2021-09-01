library(tidyverse)
library(rvest)
library(data.table)
library(nhlapi)

# Read in data ---------------------------------------------------------------------------
stat_data <- read_csv('all_stats_combined.csv')
bio_data <- read_csv('all_bio_combined.csv')


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

# Finding difference between both Player names columns, Trying to match 704 entries with 926 total.
name_bio <- bio_data$Player %>% as_tibble()

sorted_name_bio <- name_bio %>% 
  separate(value, into = c('first_name', 'last_i'), sep = '[\\s]', extra = 'merge') %>% 
  arrange_at(vars(c(2, 1)))

name_salary <- salary_table$Player %>% as_tibble()
colnames(name_salary) <- c('value1') 

sorted_name_salary <- name_salary  %>% 
  separate(value1, into = c('first_name1', 'last_i1'), sep = '[\\s]', extra = 'merge') %>% 
  arrange_at(vars(c(2, 1))) %>% 
  unique() #There is one duplicate Sebastian Aho %>% 

# Get names where order is swapped, reverse and replace.
wrong_name_table <- sorted_name_salary %>% filter(str_detect(first_name1, ',$'))

wrong_name_table$first_name1 <- str_remove(wrong_name_table$first_name1,  ',$')

flipped_wrong_name_table <- wrong_name_table[c('last_i1', 'first_name1')] %>% 
  rename(flipped_wrong_name_table, last_i1 = first_name1, first_name1 = last_i1)






# sorted_names_combined <- max(length(sorted_name_bio), length(sorted_name_salary))
# length(sorted_name_bio) <- names_sorted_combined
# length(sorted_name_salary) <- names_sorted_combined
# 
# clean_names <- bind_rows(sorted_name_bio, sorted_name_salary)




# Join tables together---------------------------------------------------------------------
half_data_join <- inner_join(bio_data, stat_data, by = 'Player')
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





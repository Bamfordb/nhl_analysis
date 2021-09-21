library(tidyverse)
library(DataExplorer)
library(janitor)
library(lubridate)
library(tidymodels)
library(ranger)


# Set directory, different every user.
setwd("~/R Scripts/NHL Analysis/NHL Player Stats")


# Read in NHL Skater Stats.
# Data current to end of 2021 regular season.
data <- read_csv('nhl_active_skater_salary_stats_2021.csv', 
                 na = c('--', 'NA'))

data$TOI <- data$TOI %>% lubridate::ms()
data$`TOI/GP` <- data$`TOI/GP` %>% lubridate::ms()

# Early EDA and cleaning of Data------------------------------------------------
dim(data)
glimpse(data)
# Shows percentages of all columns with NAs
DataExplorer::plot_missing(data)
# Majority of variables are from the Goalie position, as stats are recorded 
# differently for different positions in hockey.

data %>% count(Position == 'G')
# There are 57 Goalies in this data set, it may be worthwhile to keep the 'G' 
# rows while simply dropping that majority of goalies variables. 

# Lets try and impute 0s for all non-goalies in their respective variables.
wins_col_num <- 34
toi_col_num <-45   

data <- data %>% 
  mutate_at(wins_col_num:toi_col_num, 
          ~replace(., is.na(.), 0))

data$TOI <- data$TOI %>% 
  as.numeric() %>%  replace(., is.na(.), 0)

# Now do the opposite, and impute 0s for all skaters stats for goalies. 
ppg_col_num <- 20
fow_col_num <- 33

data <- data %>% 
  mutate_at(ppg_col_num:fow_col_num, 
            ~replace(., is.na(.), 0))

data$`TOI/GP` <- data$`TOI/GP` %>% 
  as.numeric() %>% replace(., is.na(.), 0)

# Impute INT for all NAs in State/Province column, for non North American players.
data$`S/P` <- data$`S/P` %>% replace_na('INT')

data$Birth_City <- data$Birth_City %>% replace_na('INT')

# Impute ND for non-drafted players, where NA is in its place.
draft_col_num <- 12
overall_col_num <- 14

data <- data %>% mutate_at(draft_col_num:overall_col_num,
                   ~replace(., is.na(.), 0))

# Drop dupe Nationality column.
data <- data %>% select(!c('Nationality'))

# Impute the remaining NAs as 0
data$`TOI/GP` <- replace_na(data$`TOI/GP`, 0)

# With the majority of NAs now at 0, we can now graphically represent out data.
# Now for some graphs and breakdown of different categorical difference.
players_by_country <- data %>% tabyl(Country) %>% arrange(desc(n))

ggplot(data, aes(x = reorder(Country, Country,
                             function(x) - length(x)))) + # Orders columns by count
  labs(title = 'Players per Country',
       x = 'Country abbr.',
       y = 'Number of players') +
         geom_bar()

# Create Age column 
end_of_season_date <- as.POSIXct('2021-07-07')

data <- data %>%
  mutate(Age = (end_of_season_date - DOB) / 365, 
         .after = DOB)

data$Age <- as.numeric(data$Age) %>% round(digits = 0)

# Create Years_in_league column
end_of_season_year <- 2021

data <- data %>% 
  mutate(Years_in_League = end_of_season_year - First_Season, 
                .after = First_Season)

data <- data %>% mutate(ID = row_number(), .before = 'Player')

# Data is model ready ----------------------------------------------------------
# Get rid of unnecessary character columns and change valuable characters to factors.
data_model_ready <- data %>% select(!c('Player', 'Birth_City', `S/P`)) 
data_model_ready$Laterality <- as_factor(data_model_ready$Laterality)
data_model_ready$Position <- as_factor(data_model_ready$Position)
data_model_ready$Country <- as_factor(data_model_ready$Country)

# Graphs -----------------------------------------------------------------------
# Plot Age Distribution and Salary 
ggplot(data, aes(x = Age)) +
  geom_bar()
summary(data$Age)  

#Data is very right-skewed 
ggplot(data, aes(x = Salary)) +
  scale_x_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n = 10)) +
  geom_histogram()

ggplot(data, aes(x= Salary)) +
  scale_x_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n = 10)) +
  geom_boxplot()

summary(data$Salary)

# Lets see if transforming salary with a log scale normalizes this. 
ggplot(data, aes(x = Salary)) +
  scale_x_log10() +
  geom_density()


ggplot(data, aes(x = Salary, y = Goals)) +
  scale_x_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n = 10)) +
  geom_point() +
  geom_smooth()


ggplot(data, aes(x = Salary, y = Years_in_League)) +
  scale_x_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n = 10)) +
  geom_point()+
  geom_smooth()


# Model Time -------------------------------------------------------------------
# Time to split data into training and test splits.
set.seed(4224)
nhl_split <- initial_split(data_model_ready, prop = 0.8, strata = 'Salary')

train_data <- training(nhl_split)
test_data <- testing(nhl_split)

# Random Forest 
rand_forest_model <- rand_forest(mode = 'regression', mtry =.preds(), trees = 1000) %>% 
  set_engine('ranger') %>% 
  fit(
    log10(Salary) ~ .,
    data = train_data
  )

# Linear Regression Model

lm_model <- 
 linear_reg() %>% 
  set_engine('lm')

lm_workflow <- workflow() %>% 
  add_model(lm_model) %>% 
  add_formula(Salary ~ Goals + Draft_Year)

lm_fit <- fit(lm_workflow, train_data)








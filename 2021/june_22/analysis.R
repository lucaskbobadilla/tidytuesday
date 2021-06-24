# Tidytuesday Jun 22, 2021

# packages

library(tidyverse)
library(tidytuesdayR)
library(corrplot)
# load data
tuesdata <- tidytuesdayR::tt_load('2021-06-22')
df_parks <- tuesdata$parks

# check data structure
glimpse(df_parks)

# data wrangling

# convert numerics
df_parks <- df_parks %>% 
  mutate(park_pct_city_data = as.numeric(str_remove(park_pct_city_data,"\\%")),
         pct_near_park_data = as.numeric(str_remove(pct_near_park_data, "\\%")),
         spend_per_resident_data = as.numeric(str_remove(spend_per_resident_data, "\\$")))

summary(df_parks)
# get variable correlations
num <- select_if(df_parks, is.numeric) %>% replace(is.na(.), 0) %>% 
  select(-year)
corrplot(cor(num))

### What variables seems to affect most the rank

# Variables correlated with rank:
  # pct_near_park_data - negative correlated 
  # spend_per_resident_data - negative correlated
  # park_benches - Negative correlated
  # med_park_size_data - Positive correlated
  # med_park_size_points - Positive correlated
  # park_pct_city_points - Negative correlated
  # park_pct_city_data - Negative correlated

# Variable slighly correlated with rank:
  # dogpark_data - neg cor
  # playground_data - neg cor


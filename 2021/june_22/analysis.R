# Tidytuesday Jun 22, 2021

# packages

library(tidyverse)
library(tidytuesdayR)

# load data
tuesdata <- tidytuesdayR::tt_load('2021-06-22')
df_parks <- tuesdata$parks

# check data structure
glimpse(df_parks)


# Tidytuesday Jun 22, 2021

# packages

library(tidyverse)
library(tidytuesdayR)
library(corrplot)
library(hrbrthemes)
library(patchwork)
library(colorspace)
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

# Variables correlated with rank plots

rank_cor <- df_parks %>% 
  select(rank,year,pct_near_park_data, spend_per_resident_data,
         park_benches, med_park_size_data, park_pct_city_data) %>% replace(is.na(.), 0)


# A few constants
size_col <- "#69b3a2"
size_city_col <- rgb(0.2, 0.6, 0.9, 1)

# percentages plot
pct_plot <- rank_cor %>% 
  select(year,rank,park_pct_city_data,pct_near_park_data) %>% 
  gather(park_pct_city_data:pct_near_park_data, key = "data", value = "percentage") %>% 
  filter(year >= 2017) %>% 
  ggplot(aes(x = rank, y = percentage, colour = data)) +
  geom_point() + 
  geom_smooth(method = 'lm') +
  facet_wrap(~ year, scales = "free_y", labeller = label_both) +
  scale_x_reverse(limits = c(100, 1), breaks = seq(1, 98, by = 23))  + 
  labs(x = "Park quality rank",
       y = "Percentage",
       caption = "Visualization: Lucas K. Bobadilla;\nData: The trust for public land - ParkScore index",
       title = "US Park quality rank according to size and accessibility") +
  scale_colour_manual(values = c(size_col, size_city_col),
                      name = "Metric",
                      labels = c("Park size\n(City size %)", "Park Accessibility\n(10 min walk %)"))  +
  theme_ipsum(axis_title_size = 18,
              plot_title_face = "bold") + 
  theme(legend.position = "top",
        legend.justification='left',
        legend.direction='horizontal') +
  theme(plot.background = element_rect(fill = "white"),
        legend.title = element_text(size=18, face ="bold"),
        legend.text = element_text(size=18))

ggsave(pct_plot, filename = "size_access_plot.jpeg",dpi = 300, height = 21, width = 36, units = "cm")


# spend plot
spend_plot <- rank_cor %>% filter(year >= 2017) %>% 
  ggplot(aes(x = rank, y = spend_per_resident_data)) +
  geom_point(colour = "#52b331") + 
  geom_smooth(method = 'lm', colour = "#b5a67d") +
  facet_wrap(~ year, scales = "free_y", labeller = label_both) +
  scale_x_reverse(limits = c(100, 1), breaks = seq(1, 98, by = 23))  + 
  labs(x = "Park quality rank",
       y = "Spending (USD)",
       caption = "Visualization: Lucas K. Bobadilla;\nData: The trust for public land - ParkScore index",
       title = "US Park quality rank according to spending per resident")  +
  theme_ipsum(axis_title_size = 18,
              plot_title_face = "bold") + 
  theme(legend.position = "top",
        legend.justification='left',
        legend.direction='horizontal') +
  theme(plot.background = element_rect(fill = "white"),
        legend.title = element_text(size=18, face ="bold"),
        legend.text = element_text(size=18))

spend_plot + pct_plot

ggsave(spend_plot + pct_plot, filename = "test.jpeg",dpi = 300, height = 21, width = 36, units = "cm")

# cities with the best parks overall
best_col <- "#4dbd74"
best_park <- df_parks %>% 
  filter(year >= 2017) %>%
  group_by(city) %>% 
  summarize(n_parks = n(),
            avg_points = mean(total_points, na.rm = T))  %>% 
  arrange(desc(avg_points)) %>% 
  head(n = 10) %>% 
  ggplot(aes(y = reorder(city, avg_points),  x = avg_points)) +
  geom_col(width=0.5, fill = best_col)  + 
  labs(x = "Average park index points",
       y = "City",
       title = "US Top-10 cities: Best Park Index score\n2017-2020") +
  scale_colour_manual(values = c(size_col, size_city_col),
                      name = "Metric",
                      labels = c("Park size (City size %)", "Park Accessibility (10 min walk %)"))  +
  theme_ipsum(axis_title_size = 18,
              plot_title_face = "bold"
              ,axis_text_size = 16) + 
  theme(legend.position = "top",
        legend.justification='left',
        legend.direction='horizontal') 

# cities with the best parks overall
worst_col <- "#d43f3f"
worst_park <- df_parks %>% 
  filter(year >= 2017) %>%
  group_by(city) %>% 
  summarize(n_parks = n(),
            avg_points = mean(total_points, na.rm = T))  %>% 
  arrange(avg_points) %>% 
  head(n = 10) %>% 
  ggplot(aes(y = reorder(city, desc(avg_points)),  x = avg_points)) +
  geom_col(width=0.5, fill = worst_col)  + 
  labs(x = "Average park index points",
       y = "City",
       #caption = "Visualization: Lucas K. Bobadilla;\nData: The trust for public land - ParkScore index",
       title = "US Top-10 cities: Worst Park Index score\n2017-2020")  +
  theme_ipsum(axis_title_size = 18,
              plot_title_face = "bold",
              axis_text_size = 16) + 
  theme(legend.position = "top",
        legend.justification='left',
        legend.direction='horizontal')

best_park + worst_park


# making a map
cities_states <- read_csv("cities_states.csv")

df_parks <- df_parks %>% 
  right_join(cities_states %>% select(city = City, `State full`)) %>% 
  distinct(rank, .keep_all = T)
df_parks <- df_parks %>% rename(state = `State full`)


states_avg <- df_parks %>% 
  filter(year >= 2017) %>% 
  group_by(state) %>% 
  summarize(n = n(), 
            avg_point = mean(total_points, na.rm = T)) %>% 
  rename(region = state) %>% 
  mutate(region = tolower(region))

library(maps)
library(mapdata)

states_park <- state %>% inner_join(states_avg, by = 'region')

state <- map_data("state")
map_state <- ggplot() +
  geom_polygon(data=state, aes(x=long, y=lat, group=group)) + 
  geom_polygon(data = states_park, aes(fill = avg_point,x=long, y=lat, group=group) 
               , colour = "grey")  +
  scale_fill_continuous_sequential(palette = "Heat", name = "Score") +
  theme_ipsum(axis_title_size = 18,
              plot_title_face = "bold") + 
  theme(legend.position = "bottom",
        legend.justification='center',
        legend.direction='horizontal') +
  labs(x = "",
       y = "",
       caption = "Visualization: Lucas K. Bobadilla;\nData: The trust for public land - ParkScore index",
       title = "US Park quality score average per state 2017-2020") +
  theme(plot.background = element_rect(fill = "white"),
        legend.title = element_text(size=18, face ="bold"))

ggsave(map_state, filename = "map.jpeg", dpi = 300, height = 21, width = 36, units = "cm")

top_10 <- (best_park +worst_park)
ggsave(top_10, filename = "top-10.jpeg", dpi = 300, height = 21, width = 46, units = "cm")

library(tidyr)
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(schrute)


office_ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-17/office_ratings.csv')


## Based on a graph from @drob's analysis

office_ratings  <- office_ratings %>%
  mutate(id = paste(season, episode, sep = "_")) %>% ### using id as ggplot x axis works for everything but geom_smooth() as it expects as numeric/computable input(?), so had to use row_number instead; other geoms work fine with id
  mutate(episode_number = row_number())

popularity_plot <- office_ratings %>%
  ggplot(aes(x = episode_number, y = imdb_rating)) +
  geom_point(aes(color = factor(season), size = total_votes)) + 
  geom_line(group = 1) + 
  geom_smooth() +
  geom_text_repel(data = subset(office_ratings, imdb_rating > 9.25 | imdb_rating < 7.5), aes(label = title)) + 
  scale_color_viridis_d() +
  theme_minimal() + 
    theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "none"
  ) + 
  labs(title = "Popularity of episode of the sitcom, The Office, over 9 seasons", 
       subtitle = "Color indicates season, size indicates number of votes on IMDB", 
       y = "IMDB Rating", 
       caption = "Made for the #TidyTuesday project | @anu9109") 

ggsave(filename = "2020-03-17_theoffice_popularity_plot.png", plot = popularity_plot, device = "png") 

#### split by season
popularity_season_plot <- office_ratings %>% 
  ggplot(aes(x = episode_number, y = imdb_rating)) + 
  geom_point(aes(color = factor(season), size = total_votes)) +
  geom_smooth() + 
  facet_wrap(~season, scales = "free_x") + 
  scale_color_viridis_d() +
  theme_light() + 
  theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "none"
  ) + 
  labs(title = "Popularity of episode of the sitcom, The Office, over 9 seasons", 
       subtitle = "Color indicates season, size indicates number of votes on IMDB", 
       y = "IMDB Rating", 
       caption = "Made for the #TidyTuesday project | @anu9109") 

ggsave(filename = "2020-03-17_theoffice_popularity_season_plot.png", plot = popularity_season_plot, device = "png") 







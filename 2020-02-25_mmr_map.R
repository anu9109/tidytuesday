library(tidyr)
library(maps)
library(dplyr)
library(ggplot2)


# load data
measles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-25/measles.csv')

# clean data + calculate averages by county
measles <- measles %>% 
  filter(mmr >= 0) %>%
  mutate(region = tolower(state), subregion = tolower(county)) %>%
  group_by(county) %>% 
  mutate(mmr_avg = mean(mmr), overall_avg = mean(overall)) %>% 
  ungroup() %>% distinct(region, subregion, mmr_avg)

# get USA county data for plotting
county <- map_data("county")

# merge data for plotting
county_measles <- left_join(county, measles)

map_plot <- ggplot(county_measles, aes(x = long, y = lat)) + 
  geom_polygon(aes(group = group, fill = mmr_avg)) + 
  #guides(fill = FALSE) + 
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) + 
  scale_fill_viridis_c("plasma", direction = -1) +
  theme_void() + 
  labs(title = "MMR Vaccination Rates across (some) US states",
       subtitle = "data shown is average rate per county",
       fill = "MMR Rate") + 
  theme(plot.title = element_text(hjust = 0.5,),
        plot.subtitle = element_text(hjust = 0.5))

ggsave(filename = "2020-02-25_map_plot.png", plot = map_plot, device = "png")  




  
  
library(tidyr)
library(dplyr)
library(ggplot2)

# load data
measles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-25/measles.csv')

## clean data
# remove negative values for vaccination rates
# filtering for both mmr and overall to be positive values removes too many states. Only 6 states left. So will focus only on mmr rates for now. 
measles <- measles %>%
  filter(mmr >= 0) 


# plotting mmr vs overall rates
measles %>%
  ggplot(aes(x = mmr, y = overall)) +
  geom_point(alpha = 0.3) 

# plotting mmr rates per state and seeing how many schools are under/over the 95% vaacination rate that is required for the MMR vaccine to work effectively in the context of herd immunity
jitter_plot <- measles %>%
  subset(enroll > 10) %>%
  mutate(color = if_else(mmr >= 95, "green", "#990000")) %>%
  ggplot(aes(mmr, enroll)) + 
  geom_jitter(alpha = 0.3, aes(color = color)) +
  geom_vline(xintercept = 95, color = "darkgray", linetype = "longdash") +
  facet_wrap(~ state, scales = "free_y") + 
  theme_minimal() + 
  theme(legend.position = "none") + 
  labs(title = "MMR Vaccination Rates across (some) US states",
       subtitle = "Vertical line indicates an MMR rate of 95% that is required for herd immunity to be effective",
       x = "Rate of MMR vaccination",
       y = "No. of students enrolled in a given school") 

ggsave(filename = "2020-02-25_jitter_plot.png", plot = jitter_plot, device = "png", height = 5.3, width = 8.3)  

  
  

  
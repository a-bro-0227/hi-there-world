

# library bank ----

library(tidyverse)
library(ggmap)
library(lubridate)
library(scales)


# read in data ----

park_visits <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-17/national_parks.csv')
state_pop <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-17/state_pop.csv')
gas_price <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-17/gas_price.csv')


state_map <- map_data('state') %>% as_tibble()

# cleaning ----

park_visits <- park_visits %>% 
  filter(year != 'Total') %>% 
  mutate(year = as.integer(year))

# exploration ----



park_visits %>%
  group_by(year) %>% 
  summarise(total_visitors = sum(visitors)) %>% 
  ggplot(aes(year, total_visitors)) +
  # geom_hline(yintercept = 1000, lty = 2) +
  geom_line(col = "darkgreen", size = 1.5, group = 1) +
  geom_area(fill = "lightgreen", alpha = 0.25, group = 1) +
  scale_x_continuous(breaks = seq(1900, 2010, 10)) +
  scale_y_continuous(breaks = seq(0, 1200, 200)) +
  labs(title = "How popular have national parks been?",
       subtitle = "1904-2015. Dashed line represents an equal number of visitors and population",
       x = "",
       y = "Visitors per thousand US population")

park_visits %>% group_by(unit_name) %>%
  summarize(sum_visitors = sum(visitors, na.rm = T)) %>% top_n(n = 20, sum_visitors) %>% 
  ggplot(aes(x = reorder(unit_name, sum_visitors), y = sum_visitors)) +
  geom_bar(stat='identity', color = 'black', fill = 'orange') + 
  ggtitle('Top 20 National Parks Visited') +
  labs(x = '', y = '') +
  scale_y_continuous(label = comma) +
  coord_flip() +
  theme_light()

park_visits %>% count(unit_type) %>% 
  ggplot(aes(x = reorder(unit_type, n), y = n)) +
  geom_bar(stat='identity', color = 'black', fill = 'lightgreen') + 
  coord_flip() +
  theme_light() +
  ggtitle('Differnt Types of National Parks') +
  ylab('') +
  xlab('')


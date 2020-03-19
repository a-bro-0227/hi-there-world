
# clear workspace
rm(list = ls())

# library bank ----
library(tidyverse)
library(ggmap)

# read in data ----
park_visits <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-17/national_parks.csv")
state_pop <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-17/state_pop.csv")
gas_price <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-17/gas_price.csv")


states <- map_data("state") %>% as_tibble()

# data exploration ----

park_visits %>% count(unit_type) %>% 
  ggplot(aes(x = reorder(unit_type, n), y = n)) +
  geom_bar(stat="identity", color = "black", fill = "grey") + 
  coord_flip() +
  theme_light()

park_visits %>% group_by(unit_type) %>% summarize(sum_visitors = sum(visitors, na.rm = T)) %>% top_n(n = 20, sum_visitors) %>% 
  ggplot(aes(x = reorder(unit_type, sum_visitors), y = sum_visitors)) +
  geom_bar(stat="identity", color = "black", fill = "grey") + 
  coord_flip() +
  theme_light()

park_visits %>% group_by(unit_type) %>% summarize(sum_visitors = sum(visitors, na.rm = T)) %>% arrange(sum_visitors) %>% data.frame()

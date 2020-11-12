

library(tidyverse)
library(scales)
library(lubridate)
library(tidycensus)


shootings <- read.csv("~/GitHub/tidy_tuesday/police_shootings/datasets_723010_1257097_fatal-police-shootings-data.csv", stringsAsFactors = F, na = "")

colnames(shootings)

glimpse(shootings)

shootings <- shootings %>%
  mutate(date = as.Date(date),
         year_month = format(as.Date(date), "%Y-%m"),
         year = as.numeric(format(as.Date(date), "%Y")))

get_acs("state", year = 2016, output = "tidy")

# counts ----

# overall yaerly
shootings %>% 
  ggplot(aes(x = year)) +
  geom_bar()

# by year-month
shootings %>% 
  count(year_month = floor_date(date, unit = "month"), name = "count") %>%
  ggplot(aes(x = year_month, y = count)) +
  geom_line()

# by year-month-race
shootings %>% 
  count(year_month = floor_date(date, unit = "month"), race, name = "count") %>%
  ggplot(aes(x = year_month, y = count, color = race)) +
  geom_line()

# by year-month-race - stacked-fill
shootings %>% 
  count(year_month = floor_date(date, unit = "month"), race, name = "count") %>%
  ggplot(aes(x = year_month, y = count, fill = race)) +
  geom_bar(stat = "identity", position = "fill")

# by year-race
shootings %>% 
  ggplot(aes(x = year, fill = race)) +
  geom_bar(position = "dodge")

# by month
shootings %>% 
  ggplot(aes(x = month(date, label = T))) +
  geom_bar()

# avgs ----

# overall monthly avg
shootings %>% 
  count(year_month, month = month(date, label = T), name = "count") %>% 
  ggplot(aes(y = count)) +
  geom_boxplot()

# by month
shootings %>% 
  count(year_month, month = month(date, label = T), name = "count") %>% 
  ggplot(aes(x = month, y = count)) +
  geom_boxplot()

# by race-month
shootings %>% 
  count(year_month, month = month(date, label = T), race, name = "count") %>% 
  ggplot(aes(x = race, y = count)) +
  geom_boxplot()

# by race-age
shootings %>% 
  ggplot(aes(x = race, y = age)) +
  geom_boxplot()


# random ----

# age histo
shootings %>% 
  ggplot(aes(x = age)) +
  geom_histogram()

# histo by age and race
shootings %>% filter(race == "W" | race == "B") %>% 
  ggplot(aes(y = ..density.., x = age, fill = race)) +
  geom_histogram(position = "identity", alpha = 0.5) +
  geom_density(alpha = 0.6)




# library bank ----

library(tidyverse)
library(data.table)

# read in data ----

beer <- fread("beers.csv") %>% rename(row_numb = V1, beer_id = id)
breweries <- fread("breweries.csv", header = T) %>%
  rename(brewery_id = V1, brewery_name = name) %>%
  mutate(brewery_name = str_replace(brewery_name, "Company", "Co."))
beer_reviews <- fread("beer_reviews.csv") %>% rename(beer_id = beer_beerid)


# top brewery reviews
beer_reviews %>% count(brewery_name) %>% top_n(n = 10, wt = n) %>% arrange(desc(n))

# top beer reviews
beer_reviews %>% count(brewery_name, beer_name) %>% top_n(n = 10, wt = n) %>% arrange(desc(n))

# breweries with most number of beers
beer_reviews %>% distinct(brewery_name, beer_name) %>% count(brewery_name) %>% top_n(n = 10, wt = n) %>% arrange(desc(n))

beer_reviews %>% count(review_profilename) %>% top_n(n = 10, wt = n) %>% arrange(desc(n))
beer_reviews %>% filter(review_profilename == "northyorksammy") %>% count(brewery_name, beer_name) %>% top_n(n = 10, wt = n) %>% arrange(desc(n))

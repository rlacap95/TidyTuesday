### Tidy Tuesday Week 12
### Created by Roland Lacap
### Created on 2021-04-20

### Load Libraries
library(tidyverse)
library(here)
library(wordcloud2)

### Load Data
netflix_titles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-20/netflix_titles.csv')
map <- ne_countries()

### Clean data
netflix_locations <- netflix_titles %>% 
  select(title,country) %>% 
  separate_rows(country, sep = ", ") %>% 
  mutate(country = recode(country,
                         "Bahamas" = "The Bahamas",
                         "Cambodia," = "Cambodia" ,
                         "East Germany" = "Germany", 
                         "Poland," = "Poland",
                         "Serbia" = "Republic of Serbia",
                         "Soviet Union" = "Russia",
                         "United Kingdom," = "United Kingdom",
                         "United States," = "United States",
                         "Vatican City" = "Vatican",
                         "West Germany" = "Germany")) %>%
  group_by(country) %>% 
  count(country) %>% 
  filter(!is.na(country))

### Create a wordcloud
netflix_word <-
  wordcloud2(netflix_locations, shape = "circle",
             size = .3)

netflix_word
7
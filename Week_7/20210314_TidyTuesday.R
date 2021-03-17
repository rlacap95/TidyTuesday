### Tidy Tuesday Week 7
### Created By: Roland Lacap
### Creadted on: 2021-03-12

### Load Libraries
library(tidyverse)
library(beepr)
library(here)
library(tidytuesdayR)
library(lubridate)
library(ggplot2)
library(extrafont)
library(tvthemes)
library(ggstream)

### Load Data
movies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-09/movies.csv')


moviesFiltered <- movies %>% 
  select(year, title, budget_2013,domgross_2013, intgross_2013, metascore, imdb_rating) %>% 
  filter(complete.cases(.)) %>% 
  arrange(desc(imdb_rating)) %>% 
  filter(domgross_2013 != "#N/A") %>% 
  as.numeric(unlist(3))


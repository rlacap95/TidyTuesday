### Tidy Tuesday Week 7
### Created By: Roland Lacap
### Created on: 2021-03-12
### Updated on: 2021-03-18

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

moviesStarwars <- movies %>%
  filter(title %in% c("Star Wars",
                      "Star Wars: Episode V - The Empire Strikes Back",
                      "Star Wars: Episode VI - Return of the Jedi",
                      "Star Wars: Episode III - Revenge of the Sith",
                      "Star Wars: Episode II - Attack of the Clones",
                      "Star Wars: Episode I - The Phantom Menace")) %>% 
  select(title, year, budget_2013, domgross_2013) 

newStarwars <- moviesStarwars %>% 
  as.data.frame(sapply("domgross_2013", as.numeric))

StarwarsPlot <- moviesStarwars %>% 
  ggplot(aes(y = domgross_2013,
             x = budget_2013,
             fill = title))+
  geom_bar(stat = "identity")

StarwarsPlot

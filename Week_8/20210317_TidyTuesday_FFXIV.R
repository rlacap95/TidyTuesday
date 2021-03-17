### Tidy Tuesday Week 8
### Created By: Roland Lacap
### Creadted on: 2021-03-16

### Load Libraries
library(tidyverse)
library(lubridate)
library(here)
library(tidytuesdayR)
library(ggstream)

### Get Data
games <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-16/games.csv')

gamesFF <- games %>% 
  filter(avg_peak_perc != "NaN%",
         gamename == "FINAL FANTASY XIV Online",
         year > 2014) %>% 
  select(gamename, year, month, avg, gain) %>% 
  unite(col = "y_m",
        c(year, month),
        sep = " ")

gameFFplot <- gamesFF %>% 
  ggplot(aes(x=y_m,
             y=gain,
             fill=avg))+
  geom_stream()

gameFFplot  

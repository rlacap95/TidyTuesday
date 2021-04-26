### Tidy Tuesday Week 12
### Created by Roland Lacap
### Created on 2021-04-22

### Load Libraries
library(tidyverse)
library(here)
library(lubridate)

### Load Data
netflix_titles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-20/netflix_titles.csv')

### Clean data
netflix_korea <- netflix_titles %>% 
  select(title,country,date_added,listed_in) %>% 
  separate_rows(country, sep = ", ") %>% 
  filter(country == "South Korea") %>% 
  mutate(date_added = mdy(date_added)) %>%
  mutate(year = year(date_added)) %>% 
  mutate(year = factor(year,
         levels = c("2016","2017",
                    "2018","2019",
                    "2020","2021"))) %>% 
  group_by(year) %>% 
  count(year)

### List the data
nf_kor_plot <- netflix_korea %>% 
  ggplot(aes(x = year, y = n))+
  geom_bar(stat = "identity")+
  labs(title = "Korean Content increasing in popularity",
       captions = "Source: Kaggle | @LacapRoland")

nf_kor_plot


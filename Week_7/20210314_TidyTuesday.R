### Tidy Tuesday Week 7
### Created By: Roland Lacap
### Created on: 2021-03-12
### Updated on: 2021-03-27

### Load Libraries
library(tidyverse)
library(here)
library(tidytuesdayR)
library(ghibli)
library(tvthemes)
library(magick)

### Load Data
movies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-09/movies.csv')

moviesStarwars <- movies %>%
  filter(title %in% c("Star Wars", #filter only star wars movies
                      "Star Wars: Episode V - The Empire Strikes Back",
                      "Star Wars: Episode VI - Return of the Jedi",
                      "Star Wars: Episode III - Revenge of the Sith",
                      "Star Wars: Episode II - Attack of the Clones",
                      "Star Wars: Episode I - The Phantom Menace")) %>% 
  mutate(title = recode(title, 'Star Wars' = "Star Wars: Episode IV - A New Hope")) %>% #rename "star wars" to actual episode
  select(title, budget_2013, domgross_2013) %>% #select only relevant columns
  mutate(domgross_2013 = (as.numeric(domgross_2013))) %>% #change from character to numeric
  mutate(gain = domgross_2013-budget_2013) %>% #obtain total gain from for each movie
  mutate(total_gain = sum(domgross_2013)) %>%  #find the total gain from all movies
  mutate(perc_gain = (domgross_2013/total_gain)) #take percentages from each movie
  

pal <- ghibli_palette("LaputaMedium") #color palette

starwars <- image_read("https://1000logos.net/wp-content/uploads/2017/06/StarWars-logo.png") #star wars logo

StarwarsPlot <- moviesStarwars %>% #create plot
  ggplot(aes(y = title,
             x = perc_gain,
             fill = title))+
  geom_bar(stat = "identity")+ #choose bar 
  theme_hildaNight()+ #theming
  guides(fill = FALSE)+ #remove legend
  scale_fill_manual(values = pal)+ # choose color palette
  labs(title = "Star Wars Success", #add labels 
       subtitle = "Percent contribution to the total gain ($4.8 billion) of the Star Wars franchise from 2013.",
       x = "Percent Gain",
       y = " ",
       caption = "Source: FiveThirtyEight | @LacapRoland")+
  theme(panel.background = element_rect(fill = "#F6F6F4"), #change background
        plot.title = element_text(face="bold", #change font of title 
                                family = "mono"), 
        plot.subtitle = element_text(size = 10), 
        plot.caption = element_text(hjust = 1.9), #right justify caption
        axis.title.y = element_text(face = 2), #bold axis title
        axis.text.y = element_text(size = 10), #change font size of axis text
        axis.title.x = element_text(vjust = -1))+ #change the position of axis title
  scale_y_discrete(position = "right")+ #change axis text to the right
  scale_x_continuous(labels = percent)+ #change axis scale to percentages
  ggsave(here("Week_7","Output","starwarsplot.png")) #save plot

StarwarsPlot

starwarsplot <- image_read(here("Week_7","Output","starwarsplot.png")) #import plot
  
starwarscomplete <- image_composite(starwarsplot, image_scale(starwars,"x200"), #combine star wars logo and plot
                                    offset = "+200+10",
                                    gravity = "northeast") %>% 
  image_write(here("Week_7","Output","20210327_TidyTuesday_Starwars.png"))

starwarscomplete


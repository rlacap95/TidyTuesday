### Tidy Tuesday Week 8
### Created By: Roland Lacap
### Creadted on: 2021-03-18

### Load Libraries
library(tidyverse)
library(lubridate)
library(here)
library(tidytuesdayR)
library(ggstream)
library(PNWColors)
library(tvthemes)

### Get Data
games <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-16/games.csv')

gamesFF <- games %>% 
  filter(gamename == "FINAL FANTASY XIV Online") %>% # filter only FFXIV Online 
  mutate(avgperc = (avg/sum(avg)), #take percentages of avg
         month = factor(month, #factor the months 
                        levels = c("January",
                                   "February",
                                   "March",
                                   "April",
                                   "May",
                                   "June",
                                   "July",
                                   "August",
                                   "September",
                                   "October",
                                   "November",
                                   "December"))) %>% 
  select(gamename, year, avgperc, month)  # select only necessary col

### Make Plots  
gameFFplot <- gamesFF %>% 
  ggplot(aes(y=avgperc,
             x=year,
             fill=month))+
  geom_stream()+ # use ggstream for this plot
  theme_avatar()+ # use tvthemes
  theme(plot.title=element_text(hjust=0.5, #centered figure title in bold
                                         face="bold"),
        plot.subtitle = element_text (hjust = 0.5), #center subtitle
        axis.title.x = element_text(face = 2), #bold axes titles
        axis.title.y = element_text(face = 2))+
  labs(title = "FINAL FANTASY XIV Online", #add titles and change axis labels
       subtitle = "Player growth per year where the biggest growth appears during the pandemic.",
       x = "Year",
       y = "Player growth",
       caption = "Source: SteamCharts | @LacapRoland")+
  guides(fill = guide_legend(title="Month"))+
  coord_flip()+ #flip x and y axes
  scale_x_continuous(labels=c(2014,2015,2016,2017,2018,2019,2020,2021), #changed labels for the x axis to represent only years
                     breaks=c(2014,2015,2016,2017,2018,2019,2020,2021))+
  scale_y_continuous(labels=scales::percent_format())+ # change y axis to percentages
  scale_fill_manual(values = pnw_palette("Bay",12))+ #color palette
  ggsave(here("Week_8","Output","20210318_FFXIV_Plot.png")) #save photo
 

gameFFplot  


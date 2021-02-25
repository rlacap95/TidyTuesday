### Tidy Tuesday - Employment and Earnings ######
### Created by: Roland Lacap ####################
### Created on: 2021-02-22 ######################

### Load Libraries ##############################
library(tidyverse)
library(here)
library(tidytuesdayR)
library(patchwork)
library(ghibli)
library(lubridate)

### Load Data ###################################
employed <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-23/employed.csv')
earn <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-23/earn.csv')

earnClean <- earn %>% 
  filter(complete.cases(.)) #filters out incomplete rows

employedClean <- employed %>% 
  filter(complete.cases(.)) #filters out incomplete rows

glimpse(earnClean)
glimpse(employedClean)

#Employed Figure
employedClean_race<- employedClean %>% 
  mutate(race = case_when(race_gender == "Black or African American" ~ "Black", #changed data to say "Black"
                          race_gender == "White" ~ "White")) %>%  #changed data to say "White"
  filter(race %in% c("Black", "White")) %>% #filtered out race to only show Black and White
  select(race,employ_n,year) %>% #selected columns for race, employment number, and year
  ggplot(aes(x=employ_n, #created ggplot
             y=race,
             fill=race))+
  geom_bar(stat = "identity")+ #created bar chart
  theme_classic()+ #theme for the figure 
  labs(x= "Number of Persons Employed", #labeled axis titles and figure
       title="Disparity in Employment by Race")+
  theme(axis.title.y=element_blank(), #removed y-axis title
        axis.title.x = element_text(vjust = -0.5), #adjusted the x-axis title to go lower
        plot.title=element_text(hjust=0.5, #centered the figure title and bolded it
                                face="bold"),
        legend.title = element_blank())+ #removed legend title
  scale_fill_manual(values=ghibli_palette("PonyoMedium")[c(3,4)])+ #color scale 
  scale_x_continuous(labels=scales::unit_format(unit="M", 
                                                scale = 1e-6)) #changed x-axis title to millions

employedClean_race

#Earn Figure
earnClean_race<- earnClean %>%
  filter(race %in% c("Black or African American", "White")) %>%
  filter(year %in% c(2015,2020)) %>% 
  ggplot(aes(x=year,
             y=median_weekly_earn,
             fill=race,
             color=race))+
  guides(fill=FALSE,
         color=FALSE)+
  geom_smooth()+
  theme_bw()+
  labs(y = "Median Weekly Earn",
       x = " ",
       title = "Average Weekly Earnings by Race",
       caption="Data from 2015-2020 | Source: U.S. Bureau of Labor Statistics")+
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        plot.title=element_text(hjust=0.5,
                                face="bold"))+
  scale_color_manual(values=ghibli_palette("PonyoMedium")[c(3,4)])+
  scale_fill_manual(values=ghibli_palette("PonyoMedium")[c(3,4)])+
  scale_x_continuous(labels=c(2010,2012,2014,2016,2018,2020),
                     breaks=c(2010,2012,2014,2016,2018,2020))+
  scale_y_continuous(labels=scales::dollar_format(),
                     position = "right")
earnClean_race

plot<-employedClean_race+earnClean_race + plot_annotation(tag_levels = 'A')+
  plot_layout(guides = "collect")&theme(legend.position = "bottom")+
  ggsave(here("Week_5","Output","20210223_Employment_and_Earnings_Disparities.png"))

plot



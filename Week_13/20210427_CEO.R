### Tidy Tuesday Week 13
### Created by Roland Lacap
### Created on 2021-04-27

### Load Libraries
library(tidyverse)
library(here)
library(ggridges)
library(wesanderson)

### Load Data
departures <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-27/departures.csv')

### Wrangle Data
dep <- departures %>% 
  mutate(
    reason = case_when( #recode the departure codes to its reasons
      departure_code == 1 ~ "Death",
      departure_code == 2 ~ "Illness",
      departure_code == 3 ~ "Dismissed for job performance",
      departure_code == 4 ~ "Dismissed for legal violations",
      departure_code == 5 ~ "Retired",
      departure_code == 6 ~ "New Opportunity",
      departure_code == 7 ~ "Other",
      departure_code == 8 ~ "Unknown",
      departure_code == 9 ~ "Execucomp error",
    )) %>% 
  filter(reason != "NA") %>% #remove NA
  filter(reason != "Execucomp error") %>% #remove this because I'm unsure what it is
  filter(fyear_gone != "2997") %>% # removed this year
  select(coname, reason, fyear_gone) %>% #selected only relevant col
  filter(complete.cases(.)) %>% #filter our complete cases only
  mutate(reason = factor(reason, #create levels for the reasons
                         levels = c("Unknown", "Other", "New Opportunity",
                                    "Dismissed for legal violations",
                                    "Dismissed for job performance",
                                    "Illness", "Death", "Retired")))

### Making the plot
pal <- wes_palette("GrandBudapest2", 8, type = "continuous") #color palette

depplot <- dep %>% 
  ggplot(aes(x = fyear_gone, #create plot
             y = reason,
             fill = reason))+
  geom_density_ridges()+ #use ggridges
  guides(fill = FALSE)+ #removed legend
  scale_fill_manual(values = pal)+ #apply color
  labs(title = "Reasons for CEO leaving their position", #labels
       subtitle = "From Fiscal Year 1980-2021",
       caption = "Source: Gentry et al. by way of DataIsPlural | @LacapRoland",
       x = " ", y = " ")+
  scale_y_discrete(position = "right")+ #switch position of y axis to right
  theme_ridges() + #theme
  ggsave(here("Week_13","Output","20210428_CEO_plot.png"))

depplot




### Tidy Tuesday Week 10
### Created by Roland Lacpa

### Load Libraries
library(tidyverse)
library(tidytuesdayR)
library(here)
library(tvthemes)
library(PNWColors)


### Load Data

brazil_loss <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-06/brazil_loss.csv')

brazil <- brazil_loss %>% 
  pivot_longer(cols = 4:14, #pivot data to show loss by each cause
               names_to = "cause",
               values_to = "loss") %>% 
  select(year,cause,loss) %>% #select only these colums
  group_by(year) %>% #group by year to summarise each loss per year
  mutate(loss_perc = (loss/sum(loss))) %>% #calculate ratio of loss per year
  mutate(cause = recode(cause, #recoded all the causes
                        'commercial_crops' = "Commericial Crops",
                        'mining' = "Mining",
                        'pasture' = "Pasture",
                        'small_scale_clearing' = "Small Scale Clearing",
                        'fire' = "Fire",
                        'natural_disturbances' = "Natural Disturbances",
                        'roads' = "Roads",
                        'tree_plantations_including_palm' = "Tree Plantations",
                        'flooding_due_to_dams' = "Flooding by dams",
                        'other_infrastructure' = "Other Infrastructure",
                        'selective_logging' = "Selective Logging"))

brazilLoss <- brazil %>% 
  ggplot(aes(x=year, y=loss_perc,fill=cause))+ #create plot
  geom_area()+
  scale_x_continuous(labels=c(2001,2002,2003,2004,2005,2006,2007,
                              2008,2009,2010,2011,2012,2013), #changed labels for the x axis to represent only years
                     breaks=c(2001,2002,2003,2004,2005,2006,2007,
                              2008,2009,2010,2011,2012,2013))+ 
  labs(title = "Brazil Deforestation leading cause are pastures", #titles
       caption = "Our World in Data | @LacapRoland",
       x = " ", y = "Total Loss")+
  guides(fill = guide_legend(title="Cause"))+ #rename legend title
  theme_hildaNight()+ #theme
  scale_y_continuous(labels = scales::percent)+ #change y axis to percents
  scale_fill_manual(values = pnw_palette("Sunset",11)) + #color theme
  ggsave(here("Week_10","2021_04_10_Brazil_Deforestation.png"))

brazilLoss
  
  
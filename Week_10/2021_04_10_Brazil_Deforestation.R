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
  pivot_longer(cols = 4:14,
               names_to = "cause",
               values_to = "loss") %>% 
  select(year,cause,loss) %>% 
  group_by(cause,year) %>% 
  summarise(total_loss = sum(loss))

brazilLoss <- brazil %>% 
  ggplot(aes(x=year, y=total_loss,fill=cause))+
  geom_area()+
  scale_x_continuous(labels=c(2001,2002,2003,2004,2005,2006,2007,
                              2008,2009,2010,2011,2012,2013), #changed labels for the x axis to represent only years
                     breaks=c(2001,2002,2003,2004,2005,2006,2007,
                              2008,2009,2010,2011,2012,2013))+
  labs(title = "Brazil Deforestation")+
  theme_avatar()+
  scale_fill_manual(values = pnw_palette("Bay",11))

brazilLoss
  
  
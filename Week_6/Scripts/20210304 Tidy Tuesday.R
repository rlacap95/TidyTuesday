### Tidy Tuesday Superbowl Ads ###
### Created by: Roland Lacap
### Created on: 2021-03-04

### Load Libraries
library(tidyverse)
library(beepr)
library(here)
library(tidytuesdayR)
library(ggplot2)
library(extrafont)
library(tvthemes)

### Load Data
youtube <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-02/youtube.csv')

youtubeClean <- youtube %>% #create new data set
  select(brand, funny:use_sex, view_count,like_count) %>% #selected columns
  pivot_longer(cols = funny:use_sex, names_to = "category") %>% #combine all categories to one column
  filter(value == "TRUE", # filters only by soda
         brand %in% c("Coca-Cola", "Pepsi"),
         category != "show_product_quickly") %>% #remove this one bc I wanted to keep it to six categories
  mutate(category = recode(category, #renamed all of the categories
                      'funny' = "Contains Humor",
                      'animals' = "Contains Animals",
                      'celebrity' = "Contains Celebrities",
                      'danger' = "Contains Danger",
                      'patriotic' = "Patriotic",
                      'use_sex' = "Sexual Appeal")) %>% 
  group_by(brand, category) %>% #planned to only summaries with these
  summarise(likeview = sum(like_count, na.rm = T)/sum(view_count, na.rm = T), #create like:view ratio
            .groups = "drop") %>% #for some reason, it had a grouped multiples, so i had to drop them.
  mutate(likeview = likeview*100) %>% #multiplied ratios to percentages
  filter(likeview != "NaN") %>% #removed NAs
  ggplot(aes(x = category, #create plot
             y = likeview,
             fill = category)) +
  geom_col()+
  coord_flip()+ #flipped coordinates
  facet_wrap(~brand)+ #facet wrapped to show both sodas
  scale_fill_rickAndMorty()+ #new scale fill
  theme_bw()+ #themes for the plot
  theme(panel.background = element_rect(fill = "#E5E5ED"), #change plot background color
        axis.text = element_text(face = "bold"))+ #bold axes
  guides(fill=FALSE)+ # removed legend to avoid redundancy
  labs(title = "Superbowl Commercials: Coca-Cola vs Pepsi", #titling
       subtitle = "Which category brought each soda more audience attention?",
       caption = "Uses like:view ratios. From years 2001-2020, \n
       source: FiveThirtyEight | TidyTuesday",
       x = "", y = "Audience Attention")+
  ggsave(here("Week_6", "Outputs", "2021_03_04_SodaSuperboal.png"))

youtubeClean


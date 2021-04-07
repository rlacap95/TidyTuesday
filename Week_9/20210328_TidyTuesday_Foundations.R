### Tidy Tuesday Week 9
### Created by: Roland Lacap
### Created on: 2021-03-28

### Load Libraries
library(tidyverse)
library(here)
library(tidytuesdayR)
library(magick)
library(tvthemes)
library(ggplot2)

### Get Data
allShades <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-30/allShades.csv')

### Wrangle this massive data set!

shadeSep <- allShades %>% 
  select(brand, name, lightness) %>% #select only these columns
  mutate(shade = case_when( #separates data based on the lightness of the foundation
  lightness >= .75 ~ "Light", 
lightness > 0.45 & lightness < 0.75 ~ "Medium",
lightness <= 0.45 ~ "Deep"
  )) %>%
  group_by(brand) %>% #grouped by brand
  count(shade) %>% #counted the number of shades per brand
  filter(brand %in% c("Anastasia Beverly Hills", #only chose these brands 
                      "bareMinerals",
                      "Clinique",
                      "CoverGirl",
                      "Dior",
                      "e.l.f. Cosmetics",
                      "FENTY BEAUTY by Rihanna",
                      "HUDA BEAUTY",
                      "Lancôme",
                      "MAC",
                      "MAKE UP FOR EVER",
                      "Maybelline",
                      "NARS",
                      "NYX Professional Makeup",
                      "SEPHORA COLLECTION",
                      "Shiseido",
                      "Smashbox",
                      "Tarte",
                      "Too Faced",
                      "Urban Decay Cosmetics",
                      "Yves Saint Laurent",
                      "ULTA",
                      "Wet n Wild",
                      "L'Oréal")) %>% 
  mutate(brand = recode(brand, #renamed the brand names to shorten the name
                        'NYX Professional Makeup' = "NYX",
                        'FENTY BEAUTY by Rihanna' = "FENTY BEAUTY",
                        'SEPHORA COLLECTION' = "SEPHORA",
                        'Urban Decay Cossmetics' = "Urban Decay"))

### Make a plot

FoundationPlot <- shadeSep %>% #create plot
  ggplot(aes(x = shade,
             y = n,
             fill = shade))+
  geom_col()+
  scale_x_discrete(limits = c("Light", #made it so that it goes from light to deep
                              "Medium",
                              "Deep"))+
  guides(fill=FALSE)+
  facet_wrap(~brand, #faceted the plot to display per brand
             scales = "free", #made the y-axis free scaled
             ncol = 4)+ #number of columns for the grid
  theme_classic()+ #theme
  labs(title = "Which brand has your shade?", #labeling the title, subtitle, and caption
       subtitle = "Number of foundation shades for each brand.",
       caption = "Source: ThePudding | @LacapRoland",
       x = " ", y = " ")+ #removed axes labels to avoid redundancy
  theme(plot.title=element_text(hjust=0.5, #centered figure title in bold
                                size = 18, face= 2,
                                family = "serif"),
        plot.subtitle = element_text (hjust = 0.5), #center subtitle
        plot.background = element_rect(fill = "#e09f3e"), #changed background color
        panel.background = element_rect(fill = "#fff3b0"), #changed plot background color
        axis.text.x = element_text(color = "black", #changed font for axis text
                                   size = 10, face = 2),
        axis.text.y = element_text(color = "black",
                                   size = 10, face = 2), 
        strip.text.x = element_text(size = 12, face = 2, 
                                    family = "serif"))+ #changed font for facet text
  scale_fill_manual(values = c("#335c67","#9e2a2b","#540b0e"))+ #color scale
  ggsave(here("Week_8","Output","20210330_Foundations_Plot.png"),
         width = 10, height = 16)

FoundationPlot
         

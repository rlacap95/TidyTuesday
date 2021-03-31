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
  select(brand, name, lightness, hex) %>% 
  mutate(shade = case_when(
  lightness >= .75 ~ "Light",
lightness > 0.45 & lightness < 0.75 ~ "Medium",
lightness <= 0.45 ~ "Deep"
  )) %>%
  group_by(brand) %>% 
  count(shade) %>% 
  filter(brand %in% c("Anastasia Beverly Hills",
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
  mutate(brand = recode(brand,
                        'NYX Professional Makeup' = "NYX",
                        'FENTY BEAUTY by Rihanna' = "FENTY BEAUTY",
                        'SEPHORA COLLECTION' = "SEPHORA",
                        'Urban Decay Cossmetics' = "Urban Decay"))

### Make a plot

FoundationPlot <- shadeSep %>% 
  ggplot(aes(x = shade,
             y = n,
             fill = shade))+
  geom_col()+
  scale_x_discrete(limits = c("Light",
                              "Medium",
                              "Deep"))+
  guides(fill=FALSE)+
  facet_wrap(~brand,
             scales = "free",
             ncol = 4)+
  theme_classic()+
  labs(title = "Which brand has your shade?",
       subtitle = "Number of foundation shades for each brand.",
       caption = "Source: ThePudding | @LacapRoland",
       x = " ", y = " ")+
  theme(plot.title=element_text(hjust=0.5, #centered figure title in bold
                                size = 18, face= 2,
                                family = "serif"),
        plot.subtitle = element_text (hjust = 0.5), #center subtitle
        plot.background = element_rect(fill = "#e09f3e"),
        panel.background = element_rect(fill = "#fff3b0"),
        axis.text.x = element_text(color = "black",
                                   size = 10, face = 2),
        axis.text.y = element_text(color = "black",
                                   size = 10, face = 2), #change font size of axis text
        strip.text.x = element_text(size = 12, face = 2, 
                                    family = "serif"))+ #change the position of axis title
  scale_fill_manual(values = c("#335c67","#9e2a2b","#540b0e"))+
  ggsave(here("Week_8","Output","20210330_Foundations_Plot.png"),
         width = 10, height = 16)

FoundationPlot
         

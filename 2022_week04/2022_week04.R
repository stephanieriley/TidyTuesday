#################################################
###               TidyTuesday                 ###
###                 Week 4                    ###
#################################################

#Packages
library(tidyverse)
library(ggtext)
library(grid)
library(showtext)

#Data
tuesdata <- tidytuesdayR::tt_load(2022, week = 4)
details<- tuesdata$details
ratings <- tuesdata$ratings

#Wrangling
str(details)
slice_sample(details, n=10)
str(ratings)
slice_sample(ratings, n=10)

games<- inner_join(ratings, details, by="id") #not all ratings info in details

games<- games %>%
  mutate(card = case_when(str_detect(boardgamecategory, "Card Game") ~ "Card game",
                          is.na(boardgamecategory) ~ "NA",
                          TRUE ~ "Non-card game"))



#Plot of mean playing time of games by card category
games %>%
  filter(yearpublished>1990 & yearpublished<2022,
         card!="NA") %>%
  group_by(card, yearpublished) %>%
  mutate(meanplay = mean(playingtime)) %>%
  ggplot(aes(x = yearpublished, y = meanplay, col=card)) +
  geom_line(show.legend = F) +
  scale_fill_manual(values = c("#769BD1", "#F63C26")) +
  geom_point(shape=21, show.legend = F) +
  scale_color_manual(values = c("#769BD1", "#F63C26")) +
  scale_y_continuous(breaks = seq(0,200, by=50), limits = c(0,200)) +
  labs(x = "Year", y = "Mean playing time (minutes)",
       title = "Mean playing time of games over time",
       subtitle = "On average, do 
       <span style ='color:#769BD1'>card games</span> 
       last longer than 
       <span style ='color:#F63C26'>non-card games</span>
       ?",
       caption = "Data source: Kaggle via TidyTuesday") +
  theme_classic() +
  theme(plot.subtitle = element_markdown(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "#ffefa5"))



#Plot proportion of card games over time
font_add_google(name = "Nunito Sans", family = "nunsans")
showtext_auto()

games %>%
  filter(yearpublished>1970 & yearpublished<2022,
         card!="NA") %>%
  group_by(yearpublished, card) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n) *100) %>%
  ggplot(aes(x=yearpublished, y = prop, fill = card)) +
  geom_area(position = "stack", show.legend = F) +
  scale_fill_manual(values = c("#769BD1", "#F63C26")) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +
  labs(x="Year", y="Percetnage of games",
       title = "Change in percentage of card games versus non-card games",
       subtitle = "Board games published from 1970 - present",
       caption = "Data source: Kaggle via TidyTuesday") +
  theme_classic() +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_blank()) +
  annotation_custom(grobTree(textGrob("Card game", x=0.7,  y=0.75,
                                      gp=gpar(col="white", fontsize=16)))) +
  annotation_custom(grobTree(textGrob("Non-card game", x=0.7,  y=0.5,
                                      gp=gpar(col="white", fontsize=16))))
  




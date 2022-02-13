#################################################
###               TidyTuesday                 ###
###                 Week 4                    ###
#################################################

#Packages
library(tidyverse)
library(png)
library(ggtext)

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

#Plot
snakelad<- readPNG(getURLContent("https://cdn.shopify.com/s/files/1/0876/1176/files/i984_pimgpsh_fullsize_distr.png?v=1525140332"))

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
       ?"
       ) +
  theme_classic() +
  theme(plot.subtitle = element_markdown(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = ""))






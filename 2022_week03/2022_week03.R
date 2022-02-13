#################################################
###               TidyTuesday                 ###
###                 Week 3                    ###
#################################################

#Packages
library(tidyverse)
library(palettetown)
library(showtext)


#Data
tuesdata <- tidytuesdayR::tt_load(2022, week = 3)
choc<- tuesdata$chocolate

#Wrangling
str(choc)
slice_sample(choc, n=10)

choc <- choc %>% 
  separate(col = "ingredients", into = c("number_ingredients", "list_ingredients"), sep = "- *") %>%
  mutate(number_ingredients = as.numeric(number_ingredients),
         cocoa_percent = str_remove(cocoa_percent, "%"),
         cocoa_percent = as.numeric(cocoa_percent),
         across(where(is.character), as_factor))

#Plot
font_add_google(name = "Poppins", family = "poppins")
showtext_auto()

choc %>%
  group_by(country_of_bean_origin) %>%
  summarise(meancocoa = mean(cocoa_percent)) %>%
  arrange(desc(meancocoa)) %>%
  slice_max(meancocoa, n=10) %>%
  ggplot(aes(x=meancocoa, y=fct_reorder(country_of_bean_origin, meancocoa), fill=country_of_bean_origin)) +
  geom_col(show.legend = F)+
  scale_fill_poke(pokemon = 258) +
  geom_text(aes(label = country_of_bean_origin, hjust=1.1)) +
  labs(x = "Mean cocoa percentage", y = element_blank(),
       title = "Top 10 countries by mean cocoa percentage",
       caption = "Data source: Flavors of Cacao via TidyTuesday\nPalette: Mudkip") +
  theme(panel.background = element_blank(),
        axis.line.y = element_blank(), axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(family = "poppins"))


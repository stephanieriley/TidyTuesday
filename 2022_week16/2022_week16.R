#################################################
###               TidyTuesday                 ###
###                Week 16                    ###
#################################################

#Packages
library(readr)
library(tidyverse)
library(wordcloud); library(ggwordcloud)
library(palettetown) #Pokemon colour palette!

#Read in data
big_dave <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-19/big_dave.csv')
times <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-19/times.csv')

str(big_dave)
str(times)

#Combine datasets
allcross<- rbind(big_dave, times)


#Wordcloud to show top definitions
def_freq<- allcross %>%
  drop_na(definition) %>%
  mutate(definition = str_to_lower(definition), #so that Bird and bird are not considered different
         ) %>% 
  group_by(definition) %>%
  summarise(count=n())

set.seed(369)
wordcloud(words = def_freq$definition, freq = def_freq$count, 
          min.freq = 1, max.words = 100, random.order = FALSE,
          colors = pokepal(pokemon = "Quilava", spread = 8))
wordcloud(words = def_freq$definition, freq = def_freq$count, 
          min.freq = 1, max.words = 100, random.order = FALSE,
          colors = pokepal(pokemon = "Porygon", spread = 8))


set.seed(369)
def_freq %>%
  slice_max(count, n=100) %>%
  mutate(angle = 90 * sample(c(0, 1), n(), replace = TRUE, prob = c(75, 25))) %>%
  ggplot(aes(label = definition, size = count, angle = angle, colour = count)) +
  geom_text_wordcloud(area_corr_power = 1, shape = "circle") +
  scale_size_area(max_size = 10) +
  scale_color_poke(pokemon = "Quilava", spread=3) +
  #scale_color_gradient(low = "darkred", high = "red") +
  #scale_color_brewer(type = )
  theme_minimal()

#################################################
###               TidyTuesday                 ###
###                 Week 7                    ###
#################################################

#Packages
library(tidyverse)



#Data
dubois10<- readr:::read_csv('https://raw.githubusercontent.com/ajstarks/dubois-data-portraits/master/challenge/2022/challenge10/data.csv')


#Wrangling
str(dubois10)
dubois10
dubois10<- dubois10 %>%
  rename(enrolled = `Percent Enrolled`) %>%
  mutate(Year = as.factor(Year),
         length = c(100, 80, 100),
         prop_enrol = enrolled/100,
         enrolled = -prop_enrol*length,
         not = enrolled - 1) %>%
  pivot_longer(cols = c(enrolled, not), names_to = "enrolled", values_to = "perc")


#Plot
ggplot(dubois10, aes(x= Year, y=perc, fill = enrolled)) +
  geom_col(show.legend = F) +
  scale_fill_manual(values = c("#141510", "#ac002d")) +
  theme_classic() +
  theme(panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "#e0d8cd"),
        axis.line = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank()) +
  geom_text(aes(label = paste0(unique(prop_enrol*100), "%")))

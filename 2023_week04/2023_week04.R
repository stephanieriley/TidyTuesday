

#Packages
library(tidyverse)
library(showtext)
library(ggtext)

#Data
tuesdata <- tidytuesdayR::tt_load(2023, week = 4)
survivalists <- tuesdata$survivalists
seasons<- tuesdata$seasons
loadout<- tuesdata$loadouts

#Explore
glimpse(seasons)
unique(seasons$country)
unique(seasons$lat)

glimpse(survivalists)
unique(survivalists$profession)
unique(survivalists$state)
unique(survivalists$result)
group_by(survivalists, result) %>% count()
length(unique(survivalists$name))

glimpse(loadout)
unique(loadout$item)
length(unique(loadout$name))


#how long you survive given your loadout item
#text next to it with how many used this item
item_join<- survivalists %>%
  select(name, days_lasted) %>%
  left_join(select(loadout, name, item), by = "name")

item_join %>%
  count(item, name)



item_summary<- survivalists %>%
  select(name, days_lasted) %>%
  left_join(select(loadout, name, item), by = "name") %>% #Join datasets
  #Some contestants have the same item multiple times (e.g. multiple Axes)
  #Keep only distinct items for each person
  group_by(name) %>%
  distinct(item, days_lasted) %>%
  ungroup() %>%
  #Now we can count how many contestants picked each item
  group_by(item) %>%
  summarise(meandays = mean(days_lasted),
            sddays = sd(days_lasted),
            usedby = n()) %>%
  mutate(item = factor(item),
         #Where only one person used item, sd is record as NA -> change to 0
         sddays_noNA = case_when(is.na(sddays) ~ 0,
                                 TRUE ~ sddays),
         lower = meandays - 1.96*sddays_noNA,
         upper = meandays + 1.96*sddays_noNA,
         #Cut off confidence intervals at 0
         lower = pmax(lower, 0)) %>%
  arrange(desc(meandays)) %>%
  slice_max(meandays, n = 15)

#Most used items
item_summary %>%
  slice_max(usedby, n = 5)
#Pots (92) and fishing gear (91) top used 

#Set the font
font_add_google("Poppins", "poppins")
showtext_auto()

#Plot
p<- ggplot(item_summary, aes(x = meandays, y = fct_reorder(item, meandays))) +
  geom_linerange(aes(xmin = lower, xmax = upper), colour = "#c5832b") +
  geom_point(aes(size = usedby), colour = "#c5832b") + 
  #Add outline around the points (pch in geom_point above gives squiggly lines for some reason)
  geom_point(aes(size = usedby), shape = 1, colour = "#e4fde1") +
  scale_x_continuous(breaks = seq(0,120, by = 20),
                     limits = c(0,140)) +
  labs(title = "One item to rule them all?",
       subtitle = "No one item yielded more survival time on average<br>Size of point indicates how many contestants used the item",
       y = "",
       x = "Average number of days survived (with 95% confidence interval)",
       caption = "@stephril3y
       Data from the {alone} package by Dan Oehm via TidyTuesday") +
  theme_classic() +
  theme(panel.background = element_rect(fill = "#4b543b"),
        plot.background = element_rect(fill = "#4b543b",
                                       colour = "#4b543b"),
        legend.position = "none",
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 65, 
                                  family = "poppins",
                                  colour = "#e4fde1"),
        plot.subtitle = element_markdown(size = 40,
                                     colour = "#e4fde1",
                                     lineheight = 0.35),
        plot.caption = element_text(size = 40,
                                    colour = "#e4fde1",
                                    lineheight = 0.35),
        axis.title.x = element_text(size = 40,
                                    colour = "#e4fde1",
                                    hjust = 0.3,
                                    margin = margin(0.5,0,0.5,0, "cm")),
        axis.text = element_text(size = 40,
                                 colour = "#e4fde1"),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm")) +
  geom_curve(aes(x = 110, y = 8.1, xend = 95, yend = 4.1),
             arrow = arrow(length = unit(0.03, "npc")),
             colour = "#e4fde1",
             curvature = -0.15,
             size = 0.5) +
  #Need to find a different way to reduce line space in annotate
  annotate(geom = "text", label = "The most commonly used item",
           x = 110, y = 10,
           size = 12,
           family = "poppins",
           colour = "#e4fde1") +
  annotate(geom = "text", label = "was a pot, used by 92",
           x = 110, y = 9.3,
           size = 12,
           family = "poppins",
           colour = "#e4fde1") +
  annotate(geom = "text", label = "contestants",
           x = 110, y = 8.6,
           size = 12,
           family = "poppins",
           colour = "#e4fde1")

ggsave("2023_week04/item_survival.png", p)



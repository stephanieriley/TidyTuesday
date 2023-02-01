######################################################
###                 Tidy Tuesday                   ###
###                   Week 5                       ###
######################################################

#Packages
library(tidyverse); library(png); library(grid); library(showtext); library(ggshadow)

#Read in data
tuesdata <- tidytuesdayR::tt_load(2023, week = 5)
cats_uk <- tuesdata$cats_uk
cats_uk_reference <- tuesdata$cats_uk_reference


#Quick explore
glimpse(cats_uk); head(cats_uk)
glimpse(cats_uk_reference); head(cats_uk_reference)

unique(cats_uk$tag_id)
unique(cats_uk_reference$tag_id)
#Some cat names have -Tag at the end -> may need to remove if included names

unique(cats_uk_reference$animal_taxon)



#Plot speed against prey caught to see whether faster cats catch more prey
#Firstly we will find the average speed of each cat
cat_speed<- cats_uk %>%
  group_by(tag_id) %>%
  summarise(meanspeed = mean(ground_speed))

#Now we want to join this with the dataframe containing the approximate prey per month
cat_speed_prey<- cats_uk_reference %>%
  dplyr::select(tag_id, prey_p_month, animal_sex) %>%
  full_join(cat_speed, by = "tag_id")

#Want to add icon to plot
caticon<- png::readPNG("2023_week05/caticon_nobg.png", native = T)
catgrob<- grid::rasterGrob(caticon)

#Which cat is fastest
fastcat<- slice_max(cat_speed_prey, meanspeed, n = 1) %>%
  select(tag_id) %>%
  as.character() %>%
  gsub("-Tag", "",.) #Remove the -Tag at the end of the name

#Select font for plot using showtext package
font_add_google(name = "Nunito Sans", family = "nunsans")
showtext_auto()

#Data frame now contains all we need so time to plot
p<- ggplot(cat_speed_prey, aes(x = prey_p_month, y = meanspeed)) +
  ggshadow::geom_glowpoint(col = "#124559", size = 1.3) +
  geom_point(col = "#124559",
             size = 3) +
  scale_x_continuous(limits = c(0,20)) +
  labs(title = "The early bird may catch the worm, but faster cats don't catch more prey!",
       x = "Approximate number of prey caught per month",
       y = "Average ground speed (m/s)", 
       caption = "Data from Kays et al. (doi: 10.1111/acv.12563) via TidyTuesday") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "#aec3b0"),
        plot.background = element_rect(fill = "#aec3b0"),
        plot.margin = margin(0.5,3,0.5,0.5, "cm"),
        axis.line = element_blank(),
        text = element_text(colour = "#124559", 
                            family = "nunsans",
                            size = 45),
        axis.title.y = element_text(margin = margin(0,0.5,0,0, "cm")),
        axis.title.x = element_text(margin = margin(0.5,0,0.5,0, "cm")),
        plot.caption = element_text(hjust = -0.4)
        ) +
  annotation_custom(catgrob,
                    xmin = 1.2, xmax = 10, ymax = 5200, ymin = 3500) + 
  annotate(geom = "text", 
           label = c(paste0("The fastest cat, ", fastcat, ", perhaps "), "often had a case of the zoomies"),
           x = c(11, 11), y = c(4700, 4500),
           size = 14)

ggsave("2023_week05/speed_prey.png", p, dpi = 300)





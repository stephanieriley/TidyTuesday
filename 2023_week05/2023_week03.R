######################################################
###                 Tidy Tuesday                   ###
###                   Week 5                       ###
######################################################

#Packages
library(tidyverse); library(png); library(grid); library(showtext); library(ggshadow); library(lubridate); library(hms); library(ggtext)

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


######################################################
#Plot speed against prey caught to see whether faster 
#cats catch more prey
######################################################
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



######################################################
#Look at when cats are moving
######################################################
#Need to get data in correct format
#Want to look at number of movement within each hour on each day of week
cats_movement<- cats_uk %>%
  mutate(date = as.Date(timestamp), #Collect date
         day = wday(date, label = T, abbr = F), #Collect weekday
         timehms = hms::hms(hours = hour(timestamp)), #Collect time
         hour= factor(format(lubridate::parse_date_time(timehms, c("HMS", "HM")), "%H:%M")), #Collect time rounded to nearest hour
         ) %>%
  group_by(day, hour) %>%
  count()

#x axis labels
xlabs<- rep("", 24)
xlabs[c(1,7,13,19,24)]<- as.character(cats_movement$hour[c(1,7,13,19,24)])


#Plot
p2<- ggplot(cats_movement, aes(y = day, x = hour, fill = n)) +
  geom_tile(color = "#ffffff",
            lwd = 1,
            linetype = 1) +
  coord_equal() +
  scale_x_discrete(labels = xlabs) + #Looks too busy with all times
  scale_y_discrete(limits = rev) +
  scale_fill_gradient(low = "white", high = "#124559") +
  labs(title = "When are cats the least and <span style='color:#124559;'>most</span> busy?",
       caption = "@stephril3y
       Data from Kays et al. (doi: 10.1111/acv.12563) via TidyTuesday") +
  theme(axis.text.x = element_text(angle = 90, vjust=0.5),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.background = element_rect(fill = "black", colour = "black"),
        panel.background = element_rect(fill = "black"),
        legend.position = "none",
        plot.title = element_markdown(colour = "white", 
                                  hjust = 1.5,
                                  size = 20),
        axis.text = element_text(colour = "white",
                            family = "nunsans",
                            size = 16),
        plot.caption = element_text(colour = "white",
                                    family = "nunsans",
                                    size = 12),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
        axis.title.x = element_text(margin = margin(0,0,0.5,0, "cm"))
        )

ggsave("2023_week05/cats_movement.png", p2, dpi = 300)






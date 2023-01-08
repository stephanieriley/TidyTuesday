##################################
###        TidyTuesday         ###
###           Week 1           ###
##################################

#Packages
library(tidyverse)
library(StatsBombR)
library(grid)
library(gtable)
library(showtext)
library(ggtext)

#Read in data provided by StatsBomb (https://statsbomb.com/)
wwc<- FreeCompetitions() %>%
  filter(competition_name == "Women's World Cup")
matches<- FreeMatches(wwc)

#Select entries from matches where goal is scored
matchgoal<- data.frame()
for(i in 1:nrow(matches)) {
  matchgoal<- rbind(matchgoal, 
                    get.matchFree(matches[i,]) %>%
                      filter(shot.outcome.name == "Goal") %>%
                      select(match_id, id:second, related_events, location, play_pattern.name, team.name, player.name:position.name, shot.type.name, shot.body_part.name, shot.technique.name))
}

#Pull out pitch location and turn into 2 variables
matchgoal$loc_x<- NULL
matchgoal$loc_y<- NULL
for(i in 1:nrow(matchgoal)) {
  matchgoal$loc_x[i]<- as.data.frame(matchgoal$location[i])[1,]
  matchgoal$loc_y[i]<- as.data.frame(matchgoal$location[i])[2,]
}


###### Pitch functions ######
#Function to create blank pitch
blank_pitch<- function(grass_col="#538032",
                       line_col="#ffffff",
                       background_col="#538032",
                       size=12,
                       pitchlength=120,
                       pitchwidth=80) {
  p<- ggplot() +
    #Blank pitch  
    geom_rect(aes(xmin = 0, xmax = pitchlength,
                  ymin = 0, ymax = pitchwidth),
              colour = line_col, fill = grass_col)+
    xlim(c(-10,pitchlength+10))+
    ylim(c(0,pitchwidth))+
    theme(
      axis.text=element_blank(),
      axis.ticks.length=unit(0, "lines"), 
      axis.title=element_blank(),
      legend.position="none",
      strip.background = element_rect(colour = background_col, fill = background_col, size = 0.5),
      panel.background=element_rect(fill=background_col,colour=background_col), 
      panel.grid.major=element_blank(), 
      panel.grid.minor=element_blank(), 
      panel.spacing=element_blank(), 
      plot.background=element_blank(), 
      plot.margin=unit(c(0, 0, 0, 0), "cm"), 
      plot.title=element_text(size=size*1.2), 
      strip.text.y=element_text(colour=background_col,size=size,angle=270),
      strip.text.x=element_text(size=size*1))
  
  return(p)
}

#Function to add pitch markings
pitch_markings<- function(grass_col="#538032",
                          line_col="#ffffff",
                          background_col="#538032",
                          size=12,
                          pitchlength=120,
                          pitchwidth=80) {
  
  #Create data frames for penalty box D
  circleFun <- function(center = c(0,0),diameter = 1, npoints = 1000){
    r = diameter / 2
    tt <- seq(0,2*pi,length.out = npoints)
    xx <- center[1] + r * cos(tt)
    yy <- center[2] + r * sin(tt)
    return(data.frame(x = xx, y = yy))
  }
  
  center_circle<- circleFun(center = c(0.5 * pitchlength, 0.5 * pitchwidth), diameter=30)
  
  Dleft<- circleFun(center = c(0.1 * pitchlength, 0.5 * pitchwidth), diameter = 22)
  Dleft<- Dleft[which(Dleft$x >= 0.17 * pitchlength),]
  
  Dright<- circleFun(center = c((1 - 0.1) * pitchlength, 0.5 * pitchwidth), diameter = 22)
  Dright<- Dright[which(Dright$x <= (1 - 0.17) * pitchlength),]
  
  #Plot
  p<- list(
    #Center circle
    geom_path(data = center_circle, aes(x = x, y = y), colour = line_col),
    #Middle spot
    geom_point(aes(x = 0.5 * pitchlength, y = 0.5 * pitchwidth), colour = line_col),
    #Halfway line
    geom_segment(aes(x = 0.5*pitchlength, xend = 0.5*pitchlength, 
                     y = 0, yend = pitchwidth),colour = line_col) ,
    #RHS penalty box
    geom_rect(aes(xmin = 0.83 * pitchlength, xmax = pitchlength,
                  ymin = 0.211 * pitchwidth, ymax = 0.799 * pitchwidth),
              colour = line_col, fill = grass_col, alpha = 0) ,
    #RHS penalty spot
    geom_point(aes(x=0.885 * pitchlength, y = 0.5 * pitchwidth), colour = line_col),
    #LHS penalty box
    geom_rect(aes(xmin = 0, xmax = 0.17 * pitchlength,
                  ymin = 0.211 * pitchwidth, ymax = 0.799 * pitchwidth),
              colour = line_col, fill = grass_col, alpha = 0) ,
    #LHS penalty spot
    geom_point(aes(x=0.115 * pitchlength, y = 0.5 * pitchwidth), colour = line_col),
    #RHS 6-yard box
    geom_rect(aes(xmin = (1 - 0.058) * pitchlength, xmax = pitchlength,
                  ymin = 0.368 * pitchwidth, ymax = (1 - 0.368) * pitchwidth),
              colour = line_col, fill = grass_col, alpha = 0) ,
    #LHS 6-yard box
    geom_rect(aes(xmin = 0, xmax = 0.058 * pitchlength,
                  ymin = 0.368 * pitchwidth, ymax = (1 - 0.368) * pitchwidth),
              colour = line_col, fill = grass_col, alpha = 0) ,
    #Left D
    geom_path(data = Dleft, aes(x = x, y = y), colour = line_col) ,
    #Right D
    geom_path(data = Dright, aes(x = x, y = y), colour = line_col)
  )
  
  return(p)
  
}


###### Type of shots ######
#Register font
font_add_google(name = "Nunito Sans", family = "nunsans")
showtext_auto()

#Plot which body part used to score goal
blank_pitch(grass_col = "#000000", background_col = "#000000") +
  xlim(c(90,120)) +
  coord_flip() +
  geom_rect(aes(xmin=90, xmax=120,
                ymin=0, ymax=80),
            colour="#ffffff", fill="#000000", alpha=0.2) +
  geom_point(data = matchgoal, aes(x = loc_x, y = loc_y, col = shot.body_part.name), size=4) +
  scale_colour_manual(breaks = c("Left Foot", "Right Foot", "Head", "Other"),
                      values = c("#07b1e8", "#d30107", "#fee518", "#68ff3f")) +
  pitch_markings(grass_col = "#000000", background_col = "#000000") +
  annotate("richtext", x=94.5, y=0.2, hjust=0, 
           label="Goals scored with<br><span style='color:#07b1e8;'>left foot</span>, <span style='color:#d30107;'>right foot</span>, <span style='color:#fee518;'>head</span>, <br>or <span style='color:#68ff3f;'>other</span> body part in the<br>2019 Women's World Cup", 
           colour = "#ffffff",
           family = "nunsans",
           size = 6,
           parse=T,
           fill = NA, label.color = NA) +
  labs(caption = "Data provided by StatsBomb (https://statsbomb.com/)   \n") +
  theme(plot.background = element_rect(fill = "#000000", color = "#000000"),
        plot.caption = element_text(colour = "#ffffff", 
                                    family = "nunsans",
                                    size = 12),
        plot.margin = unit(c(0, 1, 0.5, 1), "pt"))


###### Where did the top scorers score from? ######
#Top scorers of tournament
topgoalscorers<- matchgoal %>%
  count(player.name) %>%
  top_n(n, n = 4) %>%
  select(player.name) %>%
  as.vector()

#Plot where the goals were scored from
#Function to create plot
playergoal<- function(player, data) {
  #player: character containing player name
  #data: data frame containing goal information
  dat<- filter(data, player.name == player)
  p<- blank_pitch() +
    xlim(c(90,120)) +
    coord_flip() +
    geom_rect(aes(xmin=90, xmax=120,
                  ymin=0, ymax=80),
              colour="#ffffff", fill="#538032", alpha=0.2) +
    geom_density_2d_filled(data = dat, aes(x = loc_x, y = loc_y, fill = ..level..),
                           contour_var = "ndensity", 
                           breaks = seq(0.1, 1.0, length.out = 10)) +
    scale_fill_brewer(palette = "YlOrRd") +
    geom_point(data = dat, aes(x = loc_x, y = loc_y)) +
    pitch_markings() +
    annotate("text", x=92, y=1, hjust=0, label=paste0("Goals scored by\n", player), colour = "#ffffff") +
    labs(caption = "Data provided by StatsBomb (https://statsbomb.com/)")
  
  return(p)
}

playergoal(topgoalscorers[[1]][1], data = matchgoal)



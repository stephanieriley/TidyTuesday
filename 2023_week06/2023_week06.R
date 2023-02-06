

#Packages
library(tidyverse); library(lubridate); library(showtext)

#Data
#Usual way to read in isn't working
big_tech_stock_prices <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-07/big_tech_stock_prices.csv')
big_tech_companies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-07/big_tech_companies.csv')


#Explore
names(big_tech_companies)
glimpse(big_tech_companies)

glimpse(big_tech_stock_prices)
max(big_tech_stock_prices$date)

#Remove unneccessary words in company name
name_remove<- c(", Inc.", "Inc.", "Corporation", ".com")
big_tech_companies_tidy<- big_tech_companies %>%
  mutate(name_noinc = str_remove_all(company, paste(name_remove, collapse = "|")),
         name = str_trim(name_noinc, side = "right")) %>%
  select(-company)

#Join with stock info
big_tech_join<- big_tech_stock_prices %>%
  inner_join(big_tech_companies_tidy, by = "stock_symbol")

#To make plot not so busy only look at top 5 mean average volume
big_tech_top<- big_tech_join %>%
  group_by(name) %>%
  summarise(meanvol = mean(volume)) %>% #Get the mean volume by company
  slice_max(meanvol, n = 5) %>% #Select only the top5 companies
  left_join(big_tech_join, by = "name") %>% #Join with original stock join to get all entires for those top 5 companies
  mutate(year = year(date)) %>% #Plot will be busy on daily volume so sum up for yearly volume
  group_by(name, year) %>%
  summarise(year_vol = sum(volume)) %>%
  ungroup() %>%
  mutate(year_vol_bil = year_vol /1e+9) #Show volume in billions

#Check it worked
unique(big_tech_top$name)

#Dataframe with locations for text
textloc<- data.frame(name = unique(big_tech_top$name),
                     x = rep(2022.5, 5),
                     y = filter(big_tech_top, year == 2022)$year_vol_bil) %>%
  #Tesla, Apple and Amazon look close so going to move them a bit
  mutate(yjust = case_when(name == "Apple" ~ y + 10.5,
                           name == "Tesla" ~ y + 4,
                           name == "Amazon" ~ y,
                           name == "NVIDIA" ~ y - 1,
                           name == "Alphabet" ~ y - 3))

#Set up text
font_add_google(name = "Orbitron", family = "orb")
showtext_auto()

#Line graph of volume traded over time
p<- ggplot(big_tech_top) +
  geom_smooth(data = big_tech_top, aes(x = year, y = year_vol_bil), 
              method = "lm", se = F,
              col = "#ffffff", linetype = "dashed") +
  #glow line from {ggshadow} would look cool here but getting error :(
  geom_line(data = big_tech_top, aes(x = year, y = year_vol_bil, col = name)) +
  scale_x_continuous(breaks = seq(2010, 2023, by = 2),
                     limits = c(2010, 2025)) +
  geom_text(data = textloc, aes(x = x, y = yjust, label = name, col = name),
            hjust = 0,
            size = 12,
            family = "orb") +
  
  labs(title = "Volume of stocks traded per year",
       subtitle = "The dashed white line indicates that overall volume of stocks\ntraded is (slightly) decreasing over time",
       caption = "@stephril3y\nData from Yahoo Finance via TidyTuesday",
       x = "",
       y = "Volume of stocks traded (billions)") +
  theme_classic() +
  theme(plot.background = element_rect(fill = "#152238",
                                       colour = "#152238"),
        panel.background = element_rect(fill = "#152238"),
        plot.caption = element_text(size = 40,
                            colour = "#ffffff",
                            family = "orb",
                            lineheight = 0.4),
        title = element_text(size = 50,
                             colour = "#ffffff",
                             family = "orb",
                             margin = margin(1,0,0,0, unit = "cm")),
        plot.subtitle = element_text(size = 45,
                                     colour = "#ffffff",
                                     family = "orb",
                                     lineheight = 0.3),
        axis.title.y = element_text(angle = 90,
                                    hjust = 1,
                                    size = 45,
                                    lineheight = 0.3,
                                    family = "orb",
                                    margin = margin(0,1,0,0.5, unit = "cm")),
        axis.text = element_text(colour = "#ffffff",
                                 family = "orb",
                                 size = 30),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        legend.position = "none"
        )

ggsave("2023_week06/volume_traded.png", p)




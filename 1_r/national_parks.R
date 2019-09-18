###########################################

# R script used in National Park Tidy Tuesday analysis
# Author: Cory Sauve 
# Updated: 9/17/19

# Sections follow order in .Rmd file 

###########################################

library(tidyverse)
library(here)
library(gganimate)

#

park_visits <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-17/national_parks.csv")

#

park_visits$year <- as.numeric(park_visits$year)

p_total_visits <- park_visits %>% 
  filter(unit_type == "National Park", !year == "Total") %>% 
  mutate(visitors = visitors / 1000000) %>% 
  group_by(year) %>% 
  summarize(total_visitors = sum(visitors)) %>% 
  ggplot(aes(x = year, y = total_visitors)) +
  geom_path(size = 2, color = "olivedrab") +
  geom_area(fill = "olivedrab", alpha = 0.5) +
  transition_reveal(year) +
  scale_x_continuous(name = "Year", breaks = seq(1900, 2010, 10)) +
  scale_y_continuous(name = "Total visitors (million)") +
  ggtitle("National Park Visitors from 1904 to 2016") +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 16, hjust = -0.2)) +
  my.functions::theme_awesome()

anim_save("total_park_visits.gif", p_total_visits)

#

p_top_25 <- park_visits %>% 
  filter(unit_type == "National Park",
         !year == "Total",
         year >= 1995) %>% 
  mutate(unit_name = str_remove(unit_name, "National Park"))  %>% 
  group_by(unit_name) %>% 
  summarize(mean_visitors = round(mean(visitors))) %>% 
  arrange(desc(mean_visitors)) %>% 
  head(25) %>% 
  ggplot(aes(x = reorder(unit_name, mean_visitors), y = mean_visitors)) +
  geom_col(fill = "darkolivegreen") +
  coord_flip(ylim = c(0, 10000000)) +
  scale_y_continuous(labels = c("0", "2,000,000", "5,000,000", "7,500,000", "10,000,000")) +
  labs(y = "Mean visitors (million)", x = "") +
  ggtitle("Most popular national parks since 1995") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.55),
        plot.title = element_text(lineheight=.8, face="bold", size = 16, hjust = -1.5)) +
  my.functions::theme_awesome()

ggsave(p_top_25, file = "top_25_parks.png")

#

top_four <- c("GRSM", "GRCA", "YOSE", "OLYM")

national_parks <- park_visits %>% 
  filter(unit_type == "National Park",
         !year == "Total") %>% 
  mutate(unit_name = str_remove(unit_name, "National Park"))


top_parks <- national_parks %>% 
  filter(unit_code %in% top_four)

library(gganimate)

p_top_four_parks <- top_parks %>% 
  mutate(visitors = visitors / 1000000) %>% 
  ggplot(aes(x = year, y = visitors, group = unit_name)) +
  geom_line(aes(color = unit_name), size = 2, show.legend = FALSE) +
  facet_wrap(~unit_name) +
  transition_reveal(year) +
  coord_cartesian(xlim = c(1900, 2020), ylim = c(0, 12)) +
  scale_x_continuous(name = "Year", breaks = seq(1900, 2010, 20)) +
  scale_y_continuous(name = "Number of Visitors (M)", breaks = seq(0, 12, 2)) +
  scale_color_manual(values=c("darkorange3", "olivedrab", "steelblue3", "slateblue3")) +
  ggtitle("Vistors to Top National Parks") +
  theme(plot.title = element_text(lineheight=.8, face="bold", size = 16),
        strip.text.x = element_text(size = 12, face = "bold")) +
  my.functions::theme_awesome()

anim_save("top_four_parks.gif", p_top_four_parks)

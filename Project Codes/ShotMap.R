library(tidyverse)
library(dplyr)
library(worldfootballR)
library(ggplot2)
library(ggsoccer)
library(ggimage)
library(showtext)
library(plotly)

font_add_google("Cairo", "cairo")
showtext_auto()

shots <- understat_match_shots(match_url = "https://understat.com/match/26952")

shots$X <- shots$X * 100
shots$X <- case_when(shots$home_away == "a" ~ 100 - shots$X, shots$home_away == "h" ~ shots$X)
shots$Y <- shots$Y * 100
shots$Y <- case_when(shots$home_away == "h" ~ 100 - shots$Y, shots$home_away == "a" ~ shots$Y)

shot_map <- ggplot(shots) + aes(x = X, y = Y) +
  annotate_pitch(fill = "#01470c", colour = "white") +
  geom_point(aes(colour = result, size = xG)) +
  coord_flip(xlim = c(0,100),
             ylim = c(0,100)) +
  theme_pitch(aspect_ratio = 1.3/1) +
  labs(title = paste(shots$home_team, "v", shots$away_team),
       subtitle = paste(shots$home_goals, "-", shots$away_goals, "FT | Shot Locations"),
       caption = "Data: Understat | Created by: Tristan Leblanc") +
  scale_colour_manual(values = c("#e6e60b", "#53c213", "#c21327", "#07e3be", "#a714c4"),
                      name = "Shot Result",
                      labels = c("Blocked", "Goal", "Missed", "Saved", "Woodwork")) +
  geom_point(data = shots %>% filter(result == "Goal"),
             aes(x = X, y = Y, size = xG),
             colour = "#53c213") +
  theme(plot.background = element_rect(fill = "gray15"),
        text = element_text(family = "Cairo"),
        plot.title = element_text(colour = "white", size = "18", face = "bold"),
        plot.subtitle = element_text(color = "white", size = "12"),
        plot.caption = element_text(colour = "white", size = "10"),
        legend.background = element_rect(fill = "gray15"),
        legend.title = element_text(colour = "white", size = "12", face = "bold"),
        legend.text = element_text(colour = "white", size = "10")) +
  geom_text(x = 98, y = 1, label = shots$home_team, colour = "white", size = 3.5, hjust = 0) +
  geom_text(x = 2, y = 99, label = shots$away_team, colour = "white", size = 3.5, hjust = 1)

shot_map

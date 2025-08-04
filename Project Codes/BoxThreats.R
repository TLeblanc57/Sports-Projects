library(tidyverse)
library(worldfootballR)
library(showtext)

font_add_google("Cairo", "cairo", "bell")
showtext_auto()

sho_big5 <- fb_big5_advanced_season_stats(season_end_year = '2025',
                                          stat_type = 'shooting',
                                          team_or_player = 'player')

poss_big5 <- fb_big5_advanced_season_stats(season_end_year = '2025',
                                           stat_type = 'possession',
                                           team_or_player = 'player')

stats_big5 <- merge(sho_big5, poss_big5, 'Url', all = TRUE)
top_touches <- stats_big5 %>% filter(`Att Pen_Touches` > 99,
                                     Mins_Per_90.x > 15,
                                     Comp.x == 'Premier League')

box_players <- top_touches %>%
  ggplot(aes(x = `Att Pen_Touches`, y = Sh_per_90_Standard, label = Player.x)) +
  geom_point(size = 4, color = 'red3', fill = 'beige', shape = 21) +
  ylim(0, 5) + xlim(99, 380) +
  geom_label_repel(aes(label = as.character(Player.x)), box.padding = 0.6) +
  theme_classic() +
  labs(title = "Premier League's Top Box Threats",
       subtitle = "Box touches and shot leaders for the 2024/2025 season",
       caption = "Data: FBRef | Created by: Tristan Leblanc",
       x = "Box Touches",
       y = "Shots/90") +
  theme(text = element_text(family = "bell"),
        plot.title = element_text(colour = 'white', size = "18", face = "bold"),
        plot.subtitle = element_text(color = 'white', size = "12"),
        plot.caption = element_text(colour = 'white', size = "10"),
        axis.title = element_text(colour =  'white', size = '12'),
        axis.line = element_line(colour = 'white'),
        axis.text = element_text(colour = 'white'),
        plot.background = element_rect(fill = 'grey20'),
        panel.background = element_rect(fill = 'grey20'))

box_players

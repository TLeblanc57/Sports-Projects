library(tidyverse)
library(worldfootballR)
library(showtext)
library(ggrepel)

font_add_google("Cairo", "cairo")
showtext_auto()

teams <- c('Arsenal', 'Aston Villa', 'Bournemouth', 'Brentford', 'Brighton', 'Chelsea',
           'Crystal Palace', 'Everton', 'Fulham', 'Ipswich Town', 'Leicester City',
           'Liverpool', 'Manchester City', 'Manchester Utd', 'Newcastle Utd', 'Nottingham Forest',
           'Southampton', 'Tottenham', 'West Ham', 'Wolves')

team_urls <- understat_team_meta(team_names = teams)

players <- understat_team_players_stats(team_url = c(team_urls$url))

top_players <- players %>%
  filter(season == '2024/2025') %>%
  mutate(xC = xG + (xA)) %>%
  slice_max(order_by = xC, n = 50, with_ties = TRUE)

creation <- top_players %>%
  ggplot(aes(x = xG, y = xA, label = player_name)) +
  geom_point(size = 3, color = 'skyblue', fill = 'grey20', shape = 23) +
  ylim(0, 17) + xlim(0, 30) +
  geom_label_repel(aes(label = ifelse(xC > '17.99', as.character(player_name), '')), box.padding = 0.5) +
  theme_classic() +
  labs(title = "Premier League's Best Expected Contributors",
       subtitle = "xG and xA leaders for the 2024/2025 season",
       caption = "Data: Understat | Created by: Tristan Leblanc") +
  theme(text = element_text(family = "Cairo"),
        plot.title = element_text(colour = "grey20", size = "18", face = "bold"),
        plot.subtitle = element_text(color = "grey20", size = "12"),
        plot.caption = element_text(colour = "grey20", size = "10"))

creation

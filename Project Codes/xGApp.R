library(tidyverse)
library(worldfootballR)
library(showtext)
library(ggrepel)
library(zoo)
library(shiny)
library(gt)
font_add_google("Cairo", "cairo")
showtext_auto()

teams <- c('Arsenal', 'Aston Villa', 'Bournemouth', 'Brentford', 'Brighton', 'Chelsea',
           'Crystal Palace', 'Everton', 'Fulham', 'Ipswich Town', 'Leicester City',
           'Liverpool', 'Manchester City', 'Manchester Utd', 'Newcastle Utd', 'Nottingham Forest',
           'Southampton', 'Tottenham', 'West Ham', 'Wolves')

urls <- c('https://fbref.com/en/squads/18bb7c10/Arsenal-Stats',
          'https://fbref.com/en/squads/8602292d/Aston-Villa-Stats',
          'https://fbref.com/en/squads/4ba7cbea/Bournemouth-Stats',
          'https://fbref.com/en/squads/cd051869/Brentford-Stats',
          'https://fbref.com/en/squads/d07537b9/Brighton-and-Hove-Albion-Stats',
          'https://fbref.com/en/squads/cff3d9bb/Chelsea-Stats',
          'https://fbref.com/en/squads/47c64c55/Crystal-Palace-Stats',
          'https://fbref.com/en/squads/d3fd31cc/Everton-Stats',
          'https://fbref.com/en/squads/fd962109/Fulham-Stats',
          'https://fbref.com/en/squads/b74092de/Ipswich-Town-Stats',
          'https://fbref.com/en/squads/a2d435b3/Leicester-City-Stats',
          'https://fbref.com/en/squads/822bd0ba/Liverpool-Stats',
          'https://fbref.com/en/squads/b8fd03ef/Manchester-City-Stats',
          'https://fbref.com/en/squads/19538871/Manchester-United-Stats',
          'https://fbref.com/en/squads/b2b47a98/Newcastle-United-Stats',
          'https://fbref.com/en/squads/e4a775cb/Nottingham-Forest-Stats',
          'https://fbref.com/en/squads/33c895d4/Southampton-Stats',
          'https://fbref.com/en/squads/361ca564/Tottenham-Hotspur-Stats',
          'https://fbref.com/en/squads/7c21e445/West-Ham-United-Stats',
          'https://fbref.com/en/squads/8cec06e1/Wolverhampton-Wanderers-Stats')

colors <- c('#EF0107', '#95BFE5', '#DA291C', '#E30613', '#0057B8', '#034694',
            '#1B458F', '#003399', '#000000', '#3A64A3', '#003090', '#C8102E',
            '#6CABDD', '#DA291C', '#241F20', '#DD0000', '#D71920', '#132257',
            '#7A263A', '#FDB913')

teamURLs <- as.data.frame(tibble(teams, urls, colors))

premteam_stats <- function(TEAM){
  url <- teamURLs$urls[teamURLs$teams == TEAM]
  fb_team_match_results(url) %>% filter(Comp == 'Premier League')}

TeamxG <- function(TEAM){
  team_data <- premteam_stats(TEAM)
  team_data %>%
  mutate(xGD = xG-xGA) %>%
  ggplot(aes(Date, xGD)) +
  geom_line(aes(y=rollmean(xGD, 5, na.pad = TRUE), group = 1), size = 2, color = teamURLs$colors[teamURLs$teams == TEAM]) +
  geom_point(size = 5, color = teamURLs$colors[teamURLs$teams == TEAM], fill = 'white', alpha = 0.7, shape = 21) +
  labs(title = paste(TEAM, "xG Difference per Game"),
       caption = 'Data: FBref.com | Created by: Tristan Leblanc', x = "Game", y = "xG Difference") +
  theme(text = element_text(family = 'cairo'), plot.title = element_text(size = 18, face = "bold"),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 12),
        panel.background = element_blank(),
        plot.background = element_rect(fill = 'grey95'),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()) +
  scale_x_discrete(labels = team_data$Opponent, guide = guide_axis(n.dodge = 2)) + 
  ylim(-4, 4)}

## Shiny App

ui <- fluidPage(
  titlePanel(h1("xG Difference with 5 Game Rolling Average", align = "center")),
  mainPanel(selectInput("TEAM",
                           label = "Choose a team:",
                           choices = teams,
                           selected = NULL,
                           multiple = FALSE), align = "center", width = 12),
  fluidRow(column(width = 1, offset = 1, plotOutput(outputId = "plt"))))

server <- function(input, output){
  tm <- reactive({input$TEAM})
  output$plt <- renderPlot(TeamxG(tm()),height = 600, width = 1200)
}

shinyApp(ui, server)

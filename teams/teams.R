# przygotowanie
library(showtext)
library(dplyr)
library(ggplot2)
my_white <- rgb(255, 255, 255, 150, maxColorValue = 255)
font_add("knul", "C:/Users/tymek/Desktop/Studia/3_sem/TWD/project_fifa/data/Knul/Knul-Regular.otf")
showtext_auto()
teams <- read.csv('teams_fifa23.csv')
# wersja pop to wersja z recznie poprawionymi wartosciami spi
spi <- read.csv('spi_global_rankings_pop.csv')

# przygotowanie danych

spitop5 <- spi %>%
  filter(league %in% c("Barclays Premier League", "German Bundesliga",
                       "French Ligue 1", "Spanish Primera Division",
                       "Italy Serie A")) %>%
  select(name, league, spi) %>%
  arrange(name)

teamstop5 <- teams %>%
  filter(League %in% c("English Premier League (1)", 
                       "French Ligue 1 (1)", 
                       "German 1. Bundesliga (1)",
                       "Italian Serie A (1)",
                       "Spain Primera Division (1)")) %>%
  select(Name, League, Overall) %>%
  arrange(Name)

write.csv(teamstop5,"teams.csv", row.names = FALSE)

# Reczna zamiana nazw w pliku by SPI pasowalo do Fify 

teamstop5f <- read.csv('teams_f.csv', sep = ',')

# przygotowanie ramki do wykresu

df <- merge(teamstop5f, spitop5, by.x = "Name", by.y = "name")

df[1,1] <- "Union Berlin"
df[17,1] <- "Bayern"
df[23,1] <- "Brighton"
df[79,1] <- "Sevilla"
df[85,1] <- "Tottenham"
df[55,1] <- "Man City"

bad = c("Salernitana", "Nottingham Forest", "Getafe", "Wolverhampton", "Juventus",
         "Manchester United", "Sevilla", "Everton", "Monza", "Fulham",
        "Atletico Madrid", "Sampdoria", "Elche", "Empoli", "Brest", "Nantes",
        "AFC Bournemouth", "Verona", "Real Valladolid", "Cadiz", "Montpellier",
        "Bologna", "Nice", "VfL Wolfsburg", "Lazio", "Leicester City",
        "Bayer Leverkusen")
good = c("Arsenal", "Napoli", "Man City", "Brighton",
         "Union Berlin", "Werder Bremen", "Bayern", "Tottenham",
         "Hertha Berlin", "Torino", "VfB Stuttgart", "FC Cologne", "Lille",
         "Lens", "Brentford", "Mainz", "Udinese", "SC Freiburg", "Valencia",
         "TSG Hoffenheim", "Crystal Palace", "Stade Rennes", "Real Sociedad",
         "Newcastle", "Marseille", "Villarreal", "Borussia Dortmund")
names2 = c("Manchester United", "Juventus", "Arsenal", "Napoli", "Man City",
           "Brighton", "Union Berlin", "Getafe",
           "Salernitana", "Werder Bremen", "Wolverhampton", "Real Madrid", 
           "Nottingham Forest",
           "Monza", "Atletico Madrid", "Barcelona")

df_choose <- df %>%
  mutate(ifwant = if_else(Name %in% names2, 1, 0)) %>%
  mutate(good_bad = case_when(Name %in% good ~ "Underrated",
                              Name %in% bad ~ "Overrated",
                              TRUE ~ "Properly rated")) %>%
  filter(Overall > 72)

df_choose
df

# wykres różne kolory

team_plot <- df_choose %>%
  ggplot(aes(x = Overall, y = spi, label = Name)) +
  geom_smooth(data = df_choose, aes(x = Overall, y = spi), inherit.aes = FALSE,
              color = 'white', se = F) +
  geom_point(aes(color = good_bad), size = 1) + 
  geom_text(aes(label=ifelse(ifwant == 1,as.character(Name),'')),
              hjust=-0.02,vjust=-0.1, size = 10, color='white', family='knul') +
  xlim(72.5, 87.5) + ylim(45, 95) +
  labs(y = "SPI", x = "Rating", color= "") +
  scale_color_manual(values = c("#cb3df0", "white", "#ccf53f")) +
  scale_fill_discrete(labels=c('Underrated', 'Properly rated', 'Overrated')) +
  theme(
    rect = element_rect(fill = 'transparent'),
    text = element_text(family = 'knul'),
    panel.background = element_rect(fill = 'transparent'),
    panel.grid.major.y = element_line(size = 0.2, color = my_white),
    panel.grid.minor.y = element_line(size = 0.2, color = my_white),
    panel.grid.major.x = element_line(size = 0.2, color = my_white),
    panel.grid.minor.x = element_line(size = 0.2, color = my_white),
    panel.border = element_blank(),
    axis.text.x = element_text(size = 30, color = 'white'),
    axis.text.y = element_text(size = 30, color = 'white'),
    axis.title = element_text(size = 35, color = 'white'),
    axis.ticks = element_blank(),
    legend.text = element_text(size = 30, color = 'white', hjust = -5),
    legend.position="top",
    legend.justification="right", 
    legend.box.spacing = unit(0, "pt"),
    legend.margin=margin(0,0,0,0),
    legend.key = element_blank()
  )

team_plot

ggsave(plot = team_plot, file = "teams.png",
       type = "cairo-png",  bg = "transparent",
       width = 1800, height = 1300, units = 'px')

# wykres jeden kolor

df2 <- df %>%
  mutate(ifwant = if_else(Name %in% names2, 1, 0))

team_plot_2 <- df_choose %>%
  ggplot(aes(x = Overall, y = spi, label = Name)) +
  geom_smooth(data = df_choose, aes(x = Overall, y = spi), inherit.aes = FALSE,
              color = '#cb3df0', se = F) +
  geom_point(size = 1, color = '#ccf53f') +
  geom_text(aes(label=ifelse(ifwant == 1,as.character(Name),'')),
            hjust=-0.02,vjust=-0.1, size = 10, color='white', family='knul') +
  xlim(72.5, 87.5) + ylim(45, 95) +
  labs(y = "SPI", x = "Rating") +
  theme(
    rect = element_rect(fill = 'transparent'),
    text = element_text(family = 'knul'),
    panel.background = element_rect(fill = 'transparent'),
    panel.grid.major.y = element_line(size = 0.2, color = my_white),
    panel.grid.minor.y = element_line(size = 0.2, color = my_white),
    panel.grid.major.x = element_line(size = 0.2, color = my_white),
    panel.grid.minor.x = element_line(size = 0.2, color = my_white),
    panel.border = element_blank(),
    axis.text.x = element_text(size = 30, color = 'white'),
    axis.text.y = element_text(size = 30, color = 'white'),
    axis.title = element_text(size = 35, color = 'white'),
    axis.ticks = element_blank(),
  )

team_plot_2

ggsave(plot = team_plot_2, file = "teams_2.png",
       type = "cairo-png",  bg = "transparent",
       width = 1500, height = 1300, units = 'px')

# df %>%
#   ggplot(aes(x = Overall, y = spi, label = Name)) +
#   geom_point(size = 2) +
#   geom_text(aes(label=as.character(Name), color = League)) +
#   labs(title = "Fifa teams ratings vs SPI",
#        subtitle = "Team overall rating in Fifa 23, Soccer Power Index on the start of the 2022/23 season, top 5 leagues",
#        y = "SPI", x = "Rating") +
#   geom_smooth(data = df_choose, aes(x = Overall, y = spi), inherit.aes = FALSE,
#               color = 'black') +
#   xlim(72.5, 87.5) + ylim(45, 95) +
#   theme_minimal()
            


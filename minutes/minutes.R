# przygotowanie

library(dplyr)
library(ggplot2)
library(showtext)
appearances <- read.csv("appearances")
font_add("knul", "project_fifa/data/Knul/Knul-Regular.otf")
showtext_auto()
my_white <- rgb(255, 255, 255, 150, maxColorValue = 255)

# przygotowanie ramki

## minuty

players <- c("Cristiano Ronaldo", "Sergio Ramos", "Gerard Pique",
             "Eden Hazard", "Mats Hummels", "Luis Suarez", "Lionel Messi",
             "Gianluigi Buffon", "Sergio Aguero",
             "Jordi Alba", "Raphael Varane", "Giorgio Chiellini",
             "Thiago Silva", "Marco Reus", "David Silva", "Leonardo Bonucci",
             "Zlatan Ibrahimovic", "Gareth Bale", "Pepe", "Isco", 
             )

minutes <- appearances %>%
  filter(player_pretty_name %in% players) %>%
  mutate(year = substr(date, 1, 4)) %>%
  filter(year != 2014) %>%
  group_by(player_pretty_name, year) %>%
  summarise(summ = sum(minutes_played)) %>%
  group_by(year) %>%
  summarise(mean_min = mean(summ)) %>%
  mutate(min_g = case_when(year == 2018 ~ mean_min / 1.1,
                           year == 2020 ~ mean_min * 1.1,
                           year == 2022 ~ mean_min * 1.3,
                           TRUE ~ mean_min)) %>%
  select(year, min_g)

minutes

## rating, stamina

fifa15 <- read.csv("players_15.csv")

fifa15 %>%
  filter(overall > 80, age > 25) %>%
  merge(fifa22, by = "sofifa_id") %>%
  select(sofifa_id, short_name.x)
  

playersfifa <- c("Cristiano Ronaldo", "Sergio Ramos", "PiquĂ©",
             "E. Hazard", "M. Hummels", "L. SuĂˇrez", "L. Messi",
             "G. Buffon", "S. AgĂĽero",
             "Jordi Alba", "R. Varane", "G. Chiellini",
             "Thiago Silva", "M. Reus", "David Silva", "L. Bonucci",
             "Z. IbrahimoviÄ‡", "G. Bale", "Pepe", "Isco")

fifa_stat <- fifa15 %>%
  filter(short_name %in% playersfifa) %>%
  filter(overall > 70) %>%
  select(short_name, sofifa_id, overall, power_stamina) %>%
  mutate(over_stam = (overall + power_stamina) / 2) %>%
  summarise(mean_a = mean(over_stam)) %>%
  mutate(year = 2015)

for (yer in 16:22) {
  fifa <- read.csv(paste("players_",yer,".csv", sep = ""))
  fifa <- fifa %>%
    filter(short_name %in% playersfifa) %>%
    filter(overall > 70) %>%
    select(short_name, sofifa_id, overall, power_stamina) %>%
    mutate(over_stam = (overall + power_stamina) / 2) %>%
    summarise(mean_a = mean(over_stam)) %>%
    mutate(year = 2000 + yer)
  fifa_stat <- rbind(fifa_stat, fifa)
}

fifa_stat

## cała

df <- merge(minutes, fifa_stat, by = "year")

df

# wykres

minutes_plot <- ggplot(df, aes(x=year, group = 1)) +
  geom_smooth(aes(y=min_g), method="loess", col="#cb3df0", se = F, size = 2) +
  geom_smooth(aes(y=(mean_a * 50)), method="loess", col="#ccf53f", se = F, size = 2) +
  scale_y_continuous(name="Minutes played", sec.axis=sec_axis(~./50, 
                       name="Overall and stamina"), limits=c(0, 4100)) +
  scale_x_discrete(name = "Year") +
  theme(
    rect = element_rect(fill = 'transparent'),
    text = element_text(family = 'knul'),
    panel.background = element_rect(fill = 'transparent'),
    panel.grid.major.y = element_line(size = 0.2, color = my_white),
    panel.grid.minor.y = element_line(size = 0.2, color = my_white),
    panel.grid.major.x = element_line(size = 0.2, color = my_white),
    panel.grid.minor.x = element_line(size = 0.2, color = my_white),
    panel.border = element_blank(),
    axis.title.y.left=element_text(color="#cb3df0", size = 35),
    axis.title.y.right=element_text(color="#ccf53f", size = 35),
    axis.text.y = element_text(size = 30, color = 'white'),
    axis.title.x = element_text(color = 'white', size = 35),
    axis.text.x = element_text(size = 30, color = 'white'),
    axis.ticks = element_blank(),
  )

minutes_plot

ggsave(plot = minutes_plot, file = "minutes.png",
       type = "cairo-png",  bg = "transparent",
       width = 1500, height = 1500, units = 'px')

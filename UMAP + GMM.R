# Libraries

library(umap)
library(worldfootballR)
library(tidyverse)
library(extrafont)
library(ggtext)
library(mclust)
library(ggrepel)
library(ggforce)
library(MetBrewer)
library(concaveman)
library(viridis)

# Scraping 

data1 <- fb_big5_advanced_season_stats(season_end_year= 2022, stat_type= "possession", team_or_player= "player")
data2 <- fb_big5_advanced_season_stats(season_end_year= 2022, stat_type= "passing", team_or_player= "player")
data3 <- fb_big5_advanced_season_stats(season_end_year= 2022, stat_type= "passing_types", team_or_player= "player")
data4 <- fb_big5_advanced_season_stats(season_end_year= 2022, stat_type= "gca", team_or_player= "player")

# Data Wrangling

data1 <- data1 %>%
  filter(Pos == "MF") %>%
  mutate(passrec = Rec_Receiving/Mins_Per_90) %>%
  rename(def = "Def 3rd_Touches") %>%
  rename(mid = "Mid 3rd_Touches") %>%
  mutate(touch = (def + mid)/Touches_Touches) %>%
  mutate(touch = touch * 100) %>%
  mutate(targ = Targ_Receiving/Mins_Per_90) %>%
  select(passrec, touch, targ)

data2 <- data2 %>%
  filter(Pos == "MF") %>%
  mutate(fint = Final_Third/Mins_Per_90) %>%
  mutate(prog = Prog/Mins_Per_90) %>%
  select(Player, Mins_Per_90, Comp, fint, prog, Cmp_percent_Total)

data3 <- data3 %>%
  filter(Pos == "MF") %>%
  mutate(live = Live_Pass/Mins_Per_90) %>%
  mutate(press = Press_Pass/Mins_Per_90) %>%
  mutate(ground = (Ground_Height/Att)/Mins_Per_90) %>%
  select(live, press, ground)

data4 <- data4 %>%
  filter(Pos == "MF") %>%
  mutate(livesca = PassLive_SCA/Mins_Per_90) %>%
  select(livesca)

# Join and more data manipulation 

data <- cbind(data2, data1, data3, data4)
data <- data %>%
  filter(Mins_Per_90 >= 10) %>%
  filter(Comp == "Ligue 1")
Player <- data$Player 
cmp <- data$Cmp_percent_Total
data <- subset(data, select = -c(Player, Mins_Per_90, Comp))

# UMAP

data <- umap(data)
data <- as.data.frame(data$layout)

# GMM

gmm <- Mclust(data, 5)
data$Player <- Player
data$cluster <- gmm$classification
data$cmp <- cmp

data$cluster[data$cluster == 1] <- "Cluster 1"
data$cluster[data$cluster == 2] <- "Cluster 2"
data$cluster[data$cluster == 3] <- "Cluster 3"
data$cluster[data$cluster == 4] <- "Cluster 4"
data$cluster[data$cluster == 5] <- "Cluster 5"

# Custom theme

theme_athletic <- function() {
  theme_minimal() +
    theme(plot.background = element_rect(colour = "#151515", fill = "#151515"),
          panel.background = element_rect(colour = "#151515", fill = "#151515")) +
    theme(plot.title = element_text(colour = "white", size = 24, family = "Fried Chicken Bold", hjust = 0.5),
          plot.subtitle = element_markdown(colour = "#525252", size = 18, hjust = 0.5),
          plot.caption = element_text(colour = "white", size = 12, hjust = 1),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank()) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    theme(panel.grid.major.x = element_blank(),
          panel.background = element_blank()) +
    theme(legend.title = element_text(colour = "white"),
          legend.text = element_text(colour = "white"))
}

# Plotting (big 5) 

ggplot() +
  geom_text_repel(data = data, aes(x = V1, y = V2, label = Player, colour = cluster), segment.color = "#151515", size = 4.5) +
  scale_colour_manual(values = met.brewer(name = "", n = 5, type = "discrete")) + ## use scale_colour_viridis_c() for continous
  labs(title = "Who are the 'Tempo Controllers' in Football?",
       subtitle = "Minimum 12 90's Played",
       caption = "Data from StatsBomb via FBref\nCreated by Harsh Krishna") +
  theme_athletic() +
  theme(legend.position = "none")

# Plotting (league)

ggplot() +
  geom_text_repel(data = data, aes(x = V1, y = V2, label = Player, colour = cluster), segment.color = "#151515", size = 4.5) +
  geom_mark_hull(data = data, aes(x = V1, y = V2, fill = cluster, colour = cluster)) +
  scale_colour_manual(values = met.brewer(name = "Isfahan2", n = 5, type = "discrete")) +
  scale_fill_manual(values = met.brewer(name = "Isfahan2", n = 5, type = "discrete")) +
  labs(title = "Different Types of Midfielder's in Ligue 1",
       subtitle = "Minimum 10 90's Played",
       caption = "Data from StatsBomb via FBref\nCreated by Harsh Krishna") +
  theme_athletic() +
  theme(legend.position = "none")

# Save

setwd("C:/Users/harsh_1mwi2o4/Downloads")
ggsave("fig.png", width = 4000, height = 2000, units = "px")

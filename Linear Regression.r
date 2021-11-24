Linear Regression

# Libraries

library(tidyverse)
library(worldfootballR)

# Scraping

data <- fb_big5_advanced_season_stats(season_end_year= c(2018:2022), stat_type= "shooting", team_or_player= "team")

# Data Manipulation

df <- data %>%
filter(Team_or_Opponent == "team")

x <- df[, "npxG_Expected", drop = FALSE]
y <- df[, "np:G_minus_xG_Expected", drop = FALSE]

x <- as.vector(unlist(x))
y <- as.vector(unlist(y))

# Modelling 

lrmodel <- lm(y~x)
print(lrmodel)

# Testing

a <- data.frame(x = 70)
predict(lrmodel, a)
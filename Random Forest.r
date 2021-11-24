Random Forest

# Importing data

setwd("C:/Users/harsh_1mwi2o4/Downloads")
data <- read.csv("RegressionPractice.csv")

# Libraries

library(rsample)       
library(randomForest)        
library(caret)        
library(h2o)

# Splitting into testing and training datasets

set.seed(31419)

data_split <- initial_split(data, prop = .8)
data_train <- training(data_split)
data_test <- testing(data_split)

# Model

rfmodel <- randomForest(
  formula = Standings ~ .,
  data    = data_train
)

# Prediction

pred <-predict(rfmodel, data_test)
PredictedStandings <- pred
ActualStandings <- data_test$Standings
data_test$PS <- PredictedStandings

rsq <- function (x, y) {cor(x, y) ^ 2}
rsq(ActualStandings, PredictedStandings)
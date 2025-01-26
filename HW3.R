# Problem 5
library(tree)
library(caret)
library(gbm)
library(randomForest)

data(iris)
set.seed(123)

train_index <- createDataPartition(iris$Species, p = 0.8, list = FALSE)
iris_train <- iris[train_index, ]
iris_test <- iris[-train_index, ]

iris_tree <- tree(Species ~ ., data = iris_train)
summary(iris_tree)

predictions <- predict(iris_tree, iris_test, type = "class")
confusion_matrix <- table(predictions, iris_test$Species)
print(confusion_matrix)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
accuracy

#Problem 6:

NFL_train <- read.csv("NFL_train.csv")
NFL_test <- read.csv("NFL_test.csv")

linear_model <- lm(wp ~ ., data = NFL_train)
linear_predictions <- predict(linear_model, NFL_test)
linear_rmse <- sqrt(mean((linear_predictions - NFL_test$wp)^2))
linear_rmse

boost_model <- gbm(wp ~ ., data = NFL_train, distribution = "gaussian", n.trees = 1000, interaction.depth = 4)
boost_predictions <- predict(boost_model, NFL_test, n.trees = 1000)
boost_rmse <- sqrt(mean((boost_predictions - NFL_test$wp)^2))
boost_rmse

bagging_model <- randomForest(wp ~ ., data = NFL_train, mtry = ncol(NFL_train) - 1, ntree = 100)
bagging_predictions <- predict(bagging_model, NFL_test)
bagging_rmse <- sqrt(mean((bagging_predictions - NFL_test$wp)^2))
bagging_rmse

rf_model <- randomForest(wp ~ ., data = NFL_train, ntree = 100)
rf_predictions <- predict(rf_model, NFL_test)
rf_rmse <- sqrt(mean((rf_predictions - NFL_test$wp)^2))
rf_rmse

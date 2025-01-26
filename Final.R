# Load necessary libraries
library(caret)
library(randomForest)
library(gbm)
library(MASS)
library(e1071)

# Load the data
data <- read.csv("breast-cancer-wisconsin.csv")
data$Cancer <- as.factor(data$Cancer)  # Convert Cancer to factor

# Split the data into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(data$Cancer, p = .8, 
                                  list = FALSE, 
                                  times = 1)
dataTrain <- data[trainIndex, ]
dataTest <- data[-trainIndex, ]

# a) K-NN
knn_results <- train(Cancer ~ . - id.number, data = dataTrain, method = "knn", tuneLength = 20)
knn_pred <- predict(knn_results, newdata = dataTest)
knn_confusion <- confusionMatrix(knn_pred, dataTest$Cancer)
knn_mcc <- confusionMatrix(knn_pred, dataTest$Cancer)$overall['Kappa']

# b) Decision Trees with Random Forests and Boosting
rf_model <- randomForest(Cancer ~ . - id.number, data = dataTrain)
rf_pred <- predict(rf_model, newdata = dataTest)
rf_confusion <- confusionMatrix(rf_pred, dataTest$Cancer)
rf_mcc <- confusionMatrix(rf_pred, dataTest$Cancer)$overall['Kappa']

boost_model <- gbm(Cancer ~ . - id.number, data = dataTrain, distribution = "bernoulli", n.trees = 100)
boost_pred <- predict(boost_model, newdata = dataTest, n.trees = 100, type = "response")
boost_pred_class <- ifelse(boost_pred > 0.5, 4, 2)
boost_confusion <- confusionMatrix(as.factor(boost_pred_class), dataTest$Cancer)
boost_mcc <- confusionMatrix(as.factor(boost_pred_class), dataTest$Cancer)$overall['Kappa']

# c) Logistic Regression
log_model <- glm(Cancer ~ . - id.number, data = dataTrain, family = binomial)
log_pred <- predict(log_model, newdata = dataTest, type = "response")
log_pred_class <- ifelse(log_pred > 0.5, 4, 2)
log_confusion <- confusionMatrix(as.factor(log_pred_class), dataTest$Cancer)
log_mcc <- confusionMatrix(as.factor(log_pred_class), dataTest$Cancer)$overall['Kappa']

# d) QDA and LDA
qda_model <- qda(Cancer ~ . - id.number, data = dataTrain)
qda_pred <- predict(qda_model, newdata = dataTest)$class
qda_confusion <- confusionMatrix(qda_pred, dataTest$Cancer)
qda_mcc <- confusionMatrix(qda_pred, dataTest$Cancer)$overall['Kappa']

lda_model <- lda(Cancer ~ . - id.number, data = dataTrain)
lda_pred <- predict(lda_model, newdata = dataTest)$class
lda_confusion <- confusionMatrix(lda_pred, dataTest$Cancer)
lda_mcc <- confusionMatrix(lda_pred, dataTest$Cancer)$overall['Kappa']

# e) Support Vector Machines
svm_model <- svm(Cancer ~ . - id.number, data = dataTrain)
svm_pred <- predict(svm_model, newdata = dataTest)
svm_confusion <- confusionMatrix(svm_pred, dataTest$Cancer)
svm_mcc <- confusionMatrix(svm_pred, dataTest$Cancer)$overall['Kappa']

# Output results
results <- data.frame(
  Model = c("K-NN", "Random Forest", "Boosting", "Logistic Regression", "QDA", "LDA", "SVM"),
  MCC = c(knn_mcc, rf_mcc, boost_mcc, log_mcc, qda_mcc, lda_mcc, svm_mcc)
)

print(results)
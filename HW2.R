#I've half written my answers on paper and half in R so I'll be stitching these
#Answers together on the PDF. On R are the answers to 3 - 5, 1-2 and 6 are on paper.
setwd("C:/Users/Marko/WIT/MATH4050")
library(readr)
#A
xt <- read_csv("NFL_train.csv")
xtest <- read_csv("NFL_test.csv")
wpt <- xtest[,1]
wpt <- as.matrix(wpt)
#B
names(wpt) = NULL
PAl <- lm(wp ~ PA, data = xt)
PFl <- lm(wp ~ PF, data = xt)
NYAl <- lm(wp ~ NYA, data = xt)
NYFl <- lm(wp ~ NYF, data = xt)
penDEl <- lm(wp ~ penDE, data = xt)
penOFl <- lm(wp ~ penOF, data = xt)
summary(PAl)$coefficients[,4] #Significant
summary(PFl)$coefficients[,4] #Significant
summary(NYAl)$coefficients[,4] #Significant
summary(NYFl)$coefficients[,4] #Significant
summary(penDEl)$coefficients[,4] #NOT Significant
summary(penOFl)$coefficients[,4] #NOT Significant
BestT <- lm(wp ~ PA + PF + NYA + NYF, data = xt)
summary(BestT)$coefficients[,4] #PA, PF only significant values
Best <- lm(wp ~ PA + PF, data = xt)
summary(Best)$coefficients[,4]
predicted <- c(predict(Best, newdata = xtest))
names(predicted) = NULL
real <- c(xtest[,1])
#C
TP <- 0
FP <- 0
TN <- 0
FN <- 0
i <- 0
TP = 0
for (i in 1:length(predicted)) {
  if (wpt[i] > 0.5) {  
    if (predicted[i] > 0.5) {
      TP = TP + 1
    }
  }
}
for (i in 1:length(predicted)) {
  if (wpt[i] < 0.5) {  
    if (predicted[i] < 0.5) {
      TN = TN + 1
    }
  }
}
for (i in 1:length(predicted)) {
  if (wpt[i] > 0.5) {  
    if (predicted[i] < 0.5) {
      FP = FP + 1
    }
  }
}
for (i in 1:length(predicted)) {
  if (wpt[i] < 0.5) {  
    if (predicted[i] > 0.5) {
      FN = FN + 1
    }
  }
}
#MCC: .79

#D) Linear
linear_model <- lm(wp ~ PA + PF, data = xt)
linear_predictions <- predict(linear_model, newdata = xtest)
linear_residuals <- xtest$wp - linear_predictions
linear_rmse <- sqrt(mean(linear_residuals^2))
linear_rmse
#Logistic
logistic_predictions <- predict(Best, newdata = xtest)
logistic_residuals <- xtest$wp - logistic_predictions
logistic_rmse <- sqrt(mean(logistic_residuals^2))
logistic_rmse
#These two models perform equally well as they derive from similar models
#E
threshold <- 0.7
logistic_classifications <- ifelse(logistic_predictions >= threshold, 1, 0)
linear_classifications <- ifelse(linear_predictions >= threshold, 1, 0)

confusion_logistic <- table(Predicted = logistic_classifications, Actual = wpt)
confusion_linear <- table(Predicted = linear_classifications, Actual = wpt)

mcc_logistic <- (confusion_logistic[1, 1] * confusion_logistic[2, 2] - 
                   confusion_logistic[1, 2] * confusion_logistic[2, 1]) / 
  sqrt((confusion_logistic[1, 1] + confusion_logistic[1, 2]) * 
         (confusion_logistic[1, 1] + confusion_logistic[2, 1]) * 
         (confusion_logistic[2, 2] + confusion_logistic[1, 2]) * 
         (confusion_logistic[2, 2] + confusion_logistic[2, 1]))

mcc_linear <- (confusion_linear[1, 1] * confusion_linear[2, 2] - 
                 confusion_linear[1, 2] * confusion_linear[2, 1]) / 
  sqrt((confusion_linear[1, 1] + confusion_linear[1, 2]) * 
         (confusion_linear[1, 1] + confusion_linear[2, 1]) * 
         (confusion_linear[2, 2] + confusion_linear[1, 2]) * 
         (confusion_linear[2, 2] + confusion_linear[2, 1]))

mcc_logistic
mcc_linear
misclassification_rate_logistic <- (confusion_logistic[1, 2] + confusion_logistic[2, 1]) / sum(confusion_logistic)
misclassification_rate_linear <- (confusion_linear[1, 2] + confusion_linear[2, 1]) / sum(confusion_linear)
misclassification_rate_logistic
misclassification_rate_linear
#As we see from the MCC, there isn't a difference between these two models, hence, the best model could be either in this scenario

#4
data(iris)
iris_subset <- iris[iris$Species %in% c("setosa", "versicolor"), ]
set.seed(123)
train_indices <- sample(1:nrow(iris_subset), size = 0.8 * nrow(iris_subset))
train_data <- iris_subset[train_indices, ]
test_data <- iris_subset[-train_indices, ]

gaussian_pdf <- function(x, mean, sd) {
  return(dnorm(x, mean, sd))
}

prior_prob <- function(class, data) {
  return(nrow(data[data$Species == class, ]) / nrow(data))
}

naive_bayes_single <- function(train_data, test_data, feature) {
  classes <- unique(train_data$Species)
  predictions <- c()
  
  for (i in 1:nrow(test_data)) {
    posteriors <- c()
    
    for (class in classes) {
      mean_val <- mean(train_data[train_data$Species == class, feature])
      sd_val <- sd(train_data[train_data$Species == class, feature])
      prior <- prior_prob(class, train_data)
      likelihood <- gaussian_pdf(test_data[i, feature], mean_val, sd_val)
      
      posterior <- prior * likelihood
      posteriors <- c(posteriors, posterior)
    }
    
    predictions[i] <- classes[which.max(posteriors)]
  }
  
  return(predictions)
}

calculate_metrics <- function(predictions, actual) {
  confusion_matrix <- table(Predicted = predictions, Actual = actual)
  print(confusion_matrix)
  
  TP <- confusion_matrix[2, 2]
  TN <- confusion_matrix[1, 1]
  FP <- confusion_matrix[2, 1]
  FN <- confusion_matrix[1, 2]
  
  mcc <- (TP * TN - FP * FN) / sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))
  return(mcc)
}
#A
predictions_length <- naive_bayes_single(train_data, test_data, "Sepal.Length")
mcc_length <- calculate_metrics(predictions_length, test_data$Species)
mcc_length

#B
predictions_width <- naive_bayes_single(train_data, test_data, "Sepal.Width")
mcc_width <- calculate_metrics(predictions_width, test_data$Species)
mcc_width

#C
naive_bayes_double <- function(train_data, test_data) {
  classes <- unique(train_data$Species)
  predictions <- c()
  
  for (i in 1:nrow(test_data)) {
    posteriors <- c()
    
    for (class in classes) {
      mean_length <- mean(train_data[train_data$Species == class, "Sepal.Length"])
      sd_length <- sd(train_data[train_data$Species == class, "Sepal.Length"])
      mean_width <- mean(train_data[train_data$Species == class, "Sepal.Width"])
      sd_width <- sd(train_data[train_data$Species == class, "Sepal.Width"])
      
      prior <- prior_prob(class, train_data)
      
      likelihood_length <- gaussian_pdf(test_data[i, "Sepal.Length"], mean_length, sd_length)
      likelihood_width <- gaussian_pdf(test_data[i, "Sepal.Width"], mean_width, sd_width)
      
      posterior <- prior * likelihood_length * likelihood_width
      posteriors <- c(posteriors, posterior)
    }
    
    predictions[i] <- classes[which.max(posteriors)]
  }
  
  return(predictions)
}

predictions_both <- naive_bayes_double(train_data, test_data)
mcc_both <- calculate_metrics(predictions_both, test_data$Species)
mcc_both

mcc_length
mcc_width
mcc_both

#compare
mcc_values <- c(mcc_length, mcc_width, mcc_both)
best_model_index <- which.max(mcc_values)
best_model_name <- c("Sepal Length", "Sepal Width", "Sepal Length and Width")[best_model_index]

mcc_values[best_model_index]

#5
library(MASS)
train_indices <- sample(1:nrow(iris), size = 0.8 * nrow(iris))
train_data <- iris[train_indices, ]
test_data <- iris[-train_indices, ]

#A
lda_model <- lda(Species ~ ., data = train_data)
lda_predictions <- predict(lda_model, newdata = test_data)$class

lda_misclassification <- mean(lda_predictions != test_data$Species)
lda_misclassification

#B
qda_model <- qda(Species ~ ., data = train_data)
qda_predictions <- predict(qda_model, newdata = test_data)$class

qda_misclassification <- mean(qda_predictions != test_data$Species)
qda_misclassification

#C
#We can see according to LDA and QDA, LDA performs better with a lower missclassification rate

#D
#The difference between model 1 and 2 is LDA assumes all predictors have the same variance-covariance matrix for all classes while QDA does not.

#E
#Averaging individual class variances, using the pooled covariance matrix, and using Naive Bayes approach

#F
#QDA is more nonlinear than LDA because it considers the different variance-covariance matrices for every class
---
title: "Machine Learning Final"
author: "Marko Tyhansky"
date: "2024-12-11"
output: pdf_document
---
Marko Tyhansky

1.	(35 points) For the cancer data (breast-cancer-wisconsin.csv) on Brightspace analysis the data with the following machine learning techniques: 
a)	K-NN (find the optimal K for this model using either misclassification rate or MCC) 
b)	Decision trees with random forests and boosting  
c)	Logistic Regression 
d)	QDA and LDA 
e)	Support Vector Machines. 
Let the output variable be cancer, which is either a 2 for benign, or 4 for malignant, while all the other variables are predictors except id number.  For the five machine learning techniques above, which one performs the best. Please explain and provide evidence (confusion matrices for all models, and either misclassification rate or MCC for all models) for your choice.  For all these models use 80% for training and 20% for testing. For this data set you do not have to normalize all the features/predictors by a z-score. 
```{r}
setwd("C:/Users/Marko/WIT/MATH4050/Final")
library(caret)
library(class)
library(rpart)
library(rpart.plot)
library(MASS)
library(e1071)

data <- read.csv("breast-cancer-wisconsin.csv")

# Remove the 'id' column as it's not a predictor
data <- data[, -1]

# Convert cancer column to factor
data$Cancer <- as.factor(data$Cancer)

# Split data into training (80%) and testing (20%)
set.seed(123)
trainIndex <- sample(seq_len(nrow(data)), size = 0.7 * nrow(data))
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]

# Function to compute misclassification rate and MCC
evaluate_model <- function(model, testData) {
  pred <- predict(model, newdata = testData)
  cm <- confusionMatrix(pred, testData$cancer)
  misclassification_rate <- 1 - cm$overall['Accuracy']
  mcc <- mcc(cm)
  return(list(cm = cm, misclassification_rate = misclassification_rate, mcc = mcc))
}
# a) K-NN Model
# Define predictors for training and testing
train_x <- data.frame(
  clump.thickness = trainData$clump.thickness,
  Uniformity.of.cell.size = trainData$Uniformity.of.cell.size,
  Uniformity.of.cell.shape = trainData$Uniformity.of.cell.shape,
  marginal.Adhesion = trainData$marginal.Adhesion,
  Single.Epithelial.cell.size = trainData$Single.Epithelial.cell.size,
  Bare.nucleoli = trainData$Bare.nucleoli,
  Bland.chromatin = trainData$Bland.chromatin,
  Normal.nucleoli = trainData$Normal.nucleoli,
  mitoses = trainData$mitoses
)

train_y <- trainData$Cancer

test_x <- data.frame(
  clump.thickness = testData$clump.thickness,
  Uniformity.of.cell.size = testData$Uniformity.of.cell.size,
  Uniformity.of.cell.shape = testData$Uniformity.of.cell.shape,
  marginal.Adhesion = testData$marginal.Adhesion,
  Single.Epithelial.cell.size = testData$Single.Epithelial.cell.size,
  Bare.nucleoli = testData$Bare.nucleoli,
  Bland.chromatin = testData$Bland.chromatin,
  Normal.nucleoli = testData$Normal.nucleoli,
  mitoses = testData$mitoses
)

test_y <- testData$Cancer

# Scale predictors
train_x <- scale(train_x)
test_x <- scale(test_x, center = attr(train_x, "scaled:center"), scale = attr(train_x, "scaled:scale"))

# Train the KNN model
knn_model <- knn3(train_x, train_y, k = 3)

# Predict on test data
knn_predictions <- predict(knn_model, test_x, type = "class")

# Confusion matrix and accuracy
confusion_matrix <- table(Predicted = knn_predictions, Actual = test_y)
print(confusion_matrix)
 
knn_accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
accuracy <- c(paste("KNN Accuracy =", knn_accuracy))

# b) Decision Tree Model
# Train a decision tree model
tree_model <- rpart(
  Cancer ~ clump.thickness + Uniformity.of.cell.size + Uniformity.of.cell.shape +
    marginal.Adhesion + Single.Epithelial.cell.size + Bare.nucleoli +
    Bland.chromatin + Normal.nucleoli + mitoses,
  data = trainData,
  method = "class"
)

# Plot the decision tree
rpart.plot(tree_model, type = 2, extra = 104, under = TRUE, faclen = 0, cex = 0.8) 
# Make predictions
predictions <- predict(tree_model, testData, type = "class")
# Confusion matrix
confusion_matrix <- table(Predicted = predictions, Actual = testData$Cancer)
print(confusion_matrix)
 
# Calculate accuracy
tree_accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
accuracy <- append(accuracy, paste("Tree Accuracy =", tree_accuracy))

# c) Logistic Regression Model
# Fit the logistic regression model
logit_model <- glm(
  Cancer ~ clump.thickness + Uniformity.of.cell.size + Uniformity.of.cell.shape +
    marginal.Adhesion + Single.Epithelial.cell.size + Bare.nucleoli +
    Bland.chromatin + Normal.nucleoli + mitoses,
  data = trainData,
  family = binomial(link = "logit")
)

# Summarize the model
summary(logit_model)

# Make predictions on the test data (as probabilities)
predicted_probs <- predict(logit_model, newdata = testData, type = "response")

# Convert probabilities to class labels (e.g., threshold = 0.5)
predicted_classes <- ifelse(predicted_probs > 0.5, "1", "0")

# Confusion matrix
confusion_matrix <- table(Predicted = predicted_classes, Actual = testData$Cancer)
print(confusion_matrix)
 
# Calculate accuracy
lr_accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
accuracy <- append(accuracy, paste("Logistic Regressuion Accuracy =", lr_accuracy))

# d) 1: QDA Model
# Train the QDA model
qda_model <- qda(
  Cancer ~ clump.thickness + Uniformity.of.cell.size + Uniformity.of.cell.shape +
    marginal.Adhesion + Single.Epithelial.cell.size + Bare.nucleoli +
    Bland.chromatin + Normal.nucleoli + mitoses,
  data = trainData
)

# Summarize the QDA model
print(qda_model)


# Make predictions on the test data
qda_predictions <- predict(qda_model, newdata = testData)

# Extract the predicted classes
qda_predicted_classes <- qda_predictions$class

# Confusion matrix and accuracy
qda_confusion_matrix <- table(Predicted = qda_predicted_classes, Actual = testData$Cancer)
print(qda_confusion_matrix)
 
qda_accuracy <- sum(diag(qda_confusion_matrix)) / sum(qda_confusion_matrix)
accuracy <- append(accuracy, paste("QDA Accuracy =", qda_accuracy))

# d) 2: LDA Model
# Train the LDA model
lda_model <- lda(
  Cancer ~ clump.thickness + Uniformity.of.cell.size + Uniformity.of.cell.shape +
    marginal.Adhesion + Single.Epithelial.cell.size + Bare.nucleoli +
    Bland.chromatin + Normal.nucleoli + mitoses,
  data = trainData
)

# Make predictions on the test data
lda_predictions <- predict(lda_model, newdata = testData)

# Extract the predicted classes
lda_predicted_classes <- lda_predictions$class

# Confusion matrix and accuracy
lda_confusion_matrix <- table(Predicted = lda_predicted_classes, Actual = testData$Cancer)
print(lda_confusion_matrix)
 
lda_accuracy <- sum(diag(lda_confusion_matrix)) / sum(lda_confusion_matrix)
accuracy <- append(accuracy, paste("LDA Accuracy =", lda_accuracy))

# e) SVM Model
# Train the SVM model with a radial kernel (default)
svm_model <- svm(
  Cancer ~ clump.thickness + Uniformity.of.cell.size + Uniformity.of.cell.shape +
    marginal.Adhesion + Single.Epithelial.cell.size + Bare.nucleoli +
    Bland.chromatin + Normal.nucleoli + mitoses,
  data = trainData,
  kernel = "radial",  # Choose kernel type: "linear", "radial", "polynomial", or "sigmoid"
  cost = 1,           # Regularization parameter
  gamma = 0.1         # Kernel coefficient for radial/polynomial kernels
)

# Summarize the model
summary(svm_model)

# Make predictions on the test data
svm_predictions <- predict(svm_model, newdata = testData)

# Confusion matrix
confusion_matrix <- table(Predicted = svm_predictions, Actual = testData$Cancer)
print(confusion_matrix)
 
# Calculate accuracy
svm_accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
accuracy <- append(accuracy, paste("SVM Accuracy =", svm_accuracy))

accuracy
```
"KNN Accuracy = 0.970731707317073"                 
"Tree Accuracy = 0.941463414634146"                
"Logistic Regressuion Accuracy = 0.970731707317073"
"QDA Accuracy = 0.951219512195122"                 
"LDA Accuracy = 0.946341463414634"                 
"SVM Accuracy = 0.980487804878049" 
SVM is our optimal method as it has the highest accuracy.

2.	(35 points) For the red wine data (winequality-red.csv) on Brightspace first analyze the data using principal component analysis (PCA).  Let quality be the response and the rest of the variables be predictors. The number of principle components used should include at least 80% of the variance.  Then use these new transformed (projected) predictors to perform the following machine learning techniques: 
a)	Linear Regression 
b)	Decision trees, random forests and boosting 
c)	Neural Networks (use two hidden layers each with 4 nodes). Then perform each of the three machine learning techniques without first performing PCA.  Calculate the MSE or RMSE for each model using the white vine data (winequality-white.csv) with PCA and without PCA, compare the results for all three model.  Are the results with PCA or without PCA better for each model?  Finally, for the three machine learning techniques above with PCA, which one performs the best? For all the analysis please explain and provide evidence (mean squared error or root mean squared error for the three models) for your choices. Also, you will need to normalize all the features/predictors by a zscore (for both the training set and test set) and make the output of quality be from 0 to 1. 
```{r}
setwd("C:/Users/Marko/WIT/MATH4050/Final")
library(rpart)
library(randomForest)
library(gbm)
library(neuralnet)

# Load the data
red_wine <- read.csv("winequality-red.csv")
white_wine <- read.csv("winequality-white.csv")

# Normalize predictors using Z-score normalization
normalize <- function(x) {
  (x - mean(x)) / sd(x)
}

# Apply normalization to predictors in red_wine
red_wine_norm <- as.data.frame(lapply(red_wine[,-ncol(red_wine)], normalize))  # exclude quality column
red_wine_norm$quality <- red_wine$quality / max(red_wine$quality)  # normalize quality between 0 and 1

# Apply normalization to predictors in white_wine
white_wine_norm <- as.data.frame(lapply(white_wine[,-ncol(white_wine)], normalize))  # exclude quality column
white_wine_norm$quality <- white_wine$quality / max(white_wine$quality)  # normalize quality between 0 and 1

# PCA on the normalized red wine data
pca <- prcomp(red_wine_norm[,-ncol(red_wine_norm)], center = TRUE, scale. = TRUE)

# Get the proportion of variance explained by each principal component
explained_variance <- summary(pca)$importance[2,]

# Find the number of components that explain at least 80% of the variance
cumulative_variance <- cumsum(explained_variance)
num_components <- which(cumulative_variance >= 0.80)[1]

# Project the data onto the selected components
red_wine_pca <- data.frame(pca$x[, 1:num_components], quality = red_wine_norm$quality)

# Set seed for reproducibility
set.seed(123)

# Split into training and test sets (80% training, 20% testing)
train_indices <- sample(1:nrow(red_wine_norm), size = 0.8 * nrow(red_wine_norm))
train_data <- red_wine_norm[train_indices, ]
test_data <- red_wine_norm[-train_indices, ]

train_data_pca <- red_wine_pca[train_indices, ]
test_data_pca <- red_wine_pca[-train_indices, ]

# A) Linear Regression
# Linear Regression on original data (without PCA)
lm_model <- lm(quality ~ ., data = train_data)
lm_pred <- predict(lm_model, newdata = test_data)
lm_mse <- mean((lm_pred - test_data$quality)^2)
lm_rmse <- sqrt(lm_mse)

# Linear Regression on PCA-transformed data
lm_model_pca <- lm(quality ~ ., data = train_data_pca)
lm_pred_pca <- predict(lm_model_pca, newdata = test_data_pca)
lm_mse_pca <- mean((lm_pred_pca - test_data_pca$quality)^2)
lm_rmse_pca <- sqrt(lm_mse_pca)

# B) Decision Trees, random forests, and boosting

# Decision Tree
tree_model <- rpart(quality ~ ., data = train_data)
tree_pred <- predict(tree_model, newdata = test_data)
tree_mse <- mean((tree_pred - test_data$quality)^2)
tree_rmse <- sqrt(tree_mse)

# Random Forest
rf_model <- randomForest(quality ~ ., data = train_data)
rf_pred <- predict(rf_model, newdata = test_data)
rf_mse <- mean((rf_pred - test_data$quality)^2)
rf_rmse <- sqrt(rf_mse)

# Boosting
boost_model <- gbm(quality ~ ., data = train_data, distribution = "gaussian", n.trees = 100, interaction.depth = 3)
boost_pred <- predict(boost_model, newdata = test_data, n.trees = 100)
boost_mse <- mean((boost_pred - test_data$quality)^2)
boost_rmse <- sqrt(boost_mse)

# Repeat the same for PCA-transformed data (red_wine_pca)
tree_model_pca <- rpart(quality ~ ., data = train_data_pca)
tree_pred_pca <- predict(tree_model_pca, newdata = test_data_pca)
tree_mse_pca <- mean((tree_pred_pca - test_data_pca$quality)^2)
tree_rmse_pca <- sqrt(tree_mse_pca)

rf_model_pca <- randomForest(quality ~ ., data = train_data_pca)
rf_pred_pca <- predict(rf_model_pca, newdata = test_data_pca)
rf_mse_pca <- mean((rf_pred_pca - test_data_pca$quality)^2)
rf_rmse_pca <- sqrt(rf_mse_pca)

boost_model_pca <- gbm(quality ~ ., data = train_data_pca, distribution = "gaussian", n.trees = 100, interaction.depth = 3)
boost_pred_pca <- predict(boost_model_pca, newdata = test_data_pca, n.trees = 100)
boost_mse_pca <- mean((boost_pred_pca - test_data_pca$quality)^2)
boost_rmse_pca <- sqrt(boost_mse_pca)
# C) Neural Networks

# Neural Network with 2 hidden layers and 4 nodes each
nn_model <- neuralnet(quality ~ ., data = train_data, hidden = c(4, 4), linear.output = TRUE)
nn_pred <- predict(nn_model, newdata = test_data)
nn_mse <- mean((nn_pred - test_data$quality)^2)
nn_rmse <- sqrt(nn_mse)

# Neural Network with PCA-transformed data
nn_model_pca <- neuralnet(quality ~ ., data = train_data_pca, hidden = c(4, 4), linear.output = TRUE)
nn_pred_pca <- predict(nn_model_pca, newdata = test_data_pca)
nn_mse_pca <- mean((nn_pred_pca - test_data_pca$quality)^2)
nn_rmse_pca <- sqrt(nn_mse_pca)

# Create a data frame to compare RMSE values for each model
results <- data.frame(
  Model = c("Linear Regression (No PCA)", "Linear Regression (PCA)", "Decision Tree (No PCA)", 
            "Decision Tree (PCA)", "Random Forest (No PCA)", "Random Forest (PCA)",
            "Boosting (No PCA)", "Boosting (PCA)", "Neural Network (No PCA)", "Neural Network (PCA)"),
  RMSE = c(lm_rmse, lm_rmse_pca, tree_rmse, tree_rmse_pca, rf_rmse, rf_rmse_pca,
           boost_rmse, boost_rmse_pca, nn_rmse, nn_rmse_pca)
)

# Display results
print(results)
```
Based on the results above, models using Principal Component Analysis have differing RMSE values ranging from .068 - .081 which is favorable as compared to no PCA .063 - .091 since the range is shorter. Therefore, models with PCA are better. Furthermore, the model that seems to have performed the best at predicting the test data was Random Forest (No PCA) because it had the smallest RMSE. While it might be less flexible, it still had the least error

3.	(5 points) In this problem, you will perform K-means clustering manually, with K = 2, on a small example with n = 6	observations and p = 2	features.  The observations are as follows. 
 
Obs. 	X1 	X2 
1 	1 	4 
2 	1 	3 
3 	0 	4 
4 	5 	1 
5 	6 	2 
6 	4 	0 
 
a)	Plot the observations 
 
b)	Randomly assign cluster labels to each observation.  You can use the sample() command in R to do this.  Report the cluster labels for each observation 
print(clusters)
[1] 1 1 1 2 1 2
c)	Compute the centroid for each cluster 
     [,1] [,2]
[1,]  2.0 3.25
[2,]  4.5 0.50
d)	Assign each observation to the centroid to which it is closest, in terms of the Euclidean distance.  Report the cluster labels for each observation. 
print(clusters)
[1] 1 1 1 2 2 2
e)	Repeat (c) and (d) until the answer obtained stop changing (or each data points group or class does not change) 
print(clusters)
[1] 1 1 1 2 2 2
f)	In your plot from (a), color the observations according to the cluster labels obtained.   
 
 
If we perform K-means clustering and assign different labels to each observation will we always get the same result or the same clustering? 
No, K-means clustering does not always give the same result when starting with different initial cluster labels because the algorithm's outcome depends on the random initialization.

Note: The write up for this question should include a plot of the observations according to the cluster labels (classes) obtained initially and a final plot of the observations according to the cluster labels (classes) obtained after the algorithm has converged (class does not change for each data point). 

```{r}
X1 <- c(1, 1, 0, 5, 6, 4)
X2 <- c(4, 3, 4, 1, 2, 0)

plot(X1, X2, pch = 19, col = "black", xlab = "X1", ylab = "X2", main = "Observations")
text(X1, X2, labels = 1:6, pos = 3)
# Randomly assign cluster labels
set.seed(123) # For reproducibility
clusters <- sample(1:2, 6, replace = TRUE)
print(clusters)
# Function to compute centroids
compute_centroids <- function(X1, X2, clusters, K) {
  centroids <- matrix(NA, nrow = K, ncol = 2)
  for (k in 1:K) {
    centroids[k, ] <- c(mean(X1[clusters == k]), mean(X2[clusters == k]))
  }
  return(centroids)
}

# Compute centroids
K <- 2
centroids <- compute_centroids(X1, X2, clusters, K)
print(centroids)
# Function to assign observations to the nearest centroid
assign_clusters <- function(X1, X2, centroids) {
  distances <- sapply(1:nrow(centroids), function(k) {
    (X1 - centroids[k, 1])^2 + (X2 - centroids[k, 2])^2
  })
  new_clusters <- max.col(-distances) # Assign to closest centroid
  return(new_clusters)
}

# Assign observations
clusters <- assign_clusters(X1, X2, centroids)
print(clusters)
# Iterate until convergence
repeat {
  centroids <- compute_centroids(X1, X2, clusters, K)
  new_clusters <- assign_clusters(X1, X2, centroids)
  if (all(new_clusters == clusters)) break
  clusters <- new_clusters
}

# Final clusters
print(clusters)
# Plot with final cluster labels
plot(X1, X2, pch = 19, col = clusters, xlab = "X1", ylab = "X2", main = "Final Clusters")
text(X1, X2, labels = 1:6, pos = 3)
points(centroids[, 1], centroids[, 2], pch = 4, col = "blue", cex = 2, lwd = 2) # Centroids
```
![Question 4.](C:\Users\Marko\WIT\MATH4050\Final\Question4.png)
 
![Question 5.](C:\Users\Marko\WIT\MATH4050\Final\Question5.png)
```{r}
setwd("C:/Users/Marko/WIT/MATH4050/Final")
library(tibble)

# Load the data
train_data <- read.csv("ohms_train.csv")
test_data <- read.csv("ohms_test.csv")

# Gradient Descent Function
gradient_descent <- function(x, y, alpha = 0.01, theta_0_init = 0.04, theta_1_init = 2.0, tol = 1e-6) {
  m <- length(y)
  theta_0 <- theta_0_init
  theta_1 <- theta_1_init
  prev_theta_0 <- 0
  prev_theta_1 <- 0
  
  iter <- 0
  while (sqrt((theta_0 - prev_theta_0)^2 + (theta_1 - prev_theta_1)^2) > tol) {
    iter <- iter + 1
    prev_theta_0 <- theta_0
    prev_theta_1 <- theta_1
    
    # Compute gradients
    gradient_0 <- sum((theta_0 + theta_1 * x - y)) / m
    gradient_1 <- sum((theta_0 + theta_1 * x - y) * x) / m
    
    # Update parameters
    theta_0 <- theta_0 - alpha * gradient_0
    theta_1 <- theta_1 - alpha * gradient_1
  }
  
  return(c(theta_0, theta_1, iter))
}

# Extract inputs and outputs from training data
x_train <- train_data$I
y_train <- train_data$V

# Perform gradient descent
result <- gradient_descent(x_train, y_train)

theta_0_gd <- result[1]
theta_1_gd <- result[2]
iterations <- result[3]

# Compare with lm() function
model <- lm(V ~ I, data = train_data)
theta_0_lm <- coef(model)[1]
theta_1_lm <- coef(model)[2]

# Calculate Mean Squared Error (MSE) on test set
x_test <- test_data$I
y_test <- test_data$V

mse_gd <- mean((theta_0_gd + theta_1_gd * x_test - y_test)^2)
mse_lm <- mean((theta_0_lm + theta_1_lm * x_test - y_test)^2)

# Display results
results_table <- tibble(
  Method = c("Gradient Descent", "lm()"),
  Intercept = c(theta_0_gd, theta_0_lm),
  Slope = c(theta_1_gd, theta_1_lm),
  Difference_Intercept = c(abs(theta_0_gd - theta_0_lm)),
  Difference_Slope = c(abs(theta_1_gd - theta_1_lm)),
  MSE = c(mse_gd, mse_lm)
)



print(results_table)
 

# Final Results
iterations # 6766
mse_gd # 0.1235103
mse_lm # 0.1236589
```

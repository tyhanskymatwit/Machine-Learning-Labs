#KNN
numbahs <- data.frame(x1 = c(1, 1, 1, 2, 2, 2), x2 = c(1, 1, 2, 3, 2, 3), class = c(1, 1, 1, 1, 2, 2))
x1_new <- 1.75
x2_new <- 1.5
distances <- sqrt((numbahs$x1 - x1_new)^2 + (numbahs$x2 - x2_new)^2)
nearest_neighbors <- numbahs[order(distances)[1:3], ]
class_new <- names(which.max(table(nearest_neighbors$class)))
print(class_new)
#K-Fold
library(tidyverse) 
library(caret)
set.seed(125)
numbahs$class <- factor(numbahs$class)
train_control <- trainControl(method = "cv", number = 6)
model <- train(class ~ x1 + x2, data = numbahs, method = "knn", trControl = train_control)
new_data <- data.frame(x1 = x1_new, x2 = x2_new)
predictions <- predict(model, newdata = new_data)
predictions
factor(numbahs$class)
conf_matrix <- confusionMatrix(factor(predictions), factor(numbahs$class))
conf_matrix_table <- conf_matrix$table
mcc <- ((conf_matrix_table[1,1] * conf_matrix_table[2,2]) - (conf_matrix_table[1,2] * conf_matrix_table[2,1])) /
  sqrt((conf_matrix_table[1,1] + conf_matrix_table[1,2]) * 
         (conf_matrix_table[1,1] + conf_matrix_table[2,1]) * 
         (conf_matrix_table[2,2] + conf_matrix[1,2]) * 
         (conf_matrix_table[2,2] + conf_matrix_table[2,1]))
mcc
#KNN
library(readr)
setwd("C:/Users/Marko/WIT/MATH4050")
datac <- read_csv("C:/Users/Marko/WIT/MATH4050/Baseball_2021_season.csv")
nt <- sample(1:length(datac),0.80*nrow(datac),replace=F)
dataT <- datac[nt,]
dataTe <- datac[-nt,]
RS_new <- 700
RA_new <- 0
distances <- sqrt((datac$RS - RS_new)^2 + (datac$RA - RA_new)^2)
nearest_neighbors <- datac[order(distances)[1:length(datac)], ]
class_new <- names(which.max(table(nearest_neighbors$'W-L%')))
class_new
#K-Fold
set.seed(125)
datac$WL <- datac$`W-L%`
datac$WL <- ifelse(datac$WL > 0.50, "Above 0.50", "Below 0.50")
train_control <- trainControl(method = "cv", number = 30)
model <- train(WL ~ RS + RA, data = datac, method = "knn", trControl = train_control)
new_data <- data.frame(x1 = x1_new, x2 = x2_new)
predictions <- predict(model, newdata = new_data)
predictions
factor(numbahs$class)
conf_matrix <- confusionMatrix(factor(predictions), factor(numbahs$class))
conf_matrix_table <- conf_matrix$table
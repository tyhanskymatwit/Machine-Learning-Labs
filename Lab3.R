#A
setwd("C:/Users/Marko/WIT/MATH4050")
library(readr)
datac <- read.csv(file = "Baseball_team_2021_hit.csv", head = TRUE, sep = ",")
nt <- sample(1:length(datac),0.60*nrow(datac),replace=F)
dataT <- datac[nt,]
dataTe <- datac[-nt,]
R <- datac$R
RT <- dataT$R
RTe <- dataTe$R
X2BT <- dataT$X2B
X2BTe <- dataTe$X2B
X3BT <- dataT$X3B
X3BTe <- dataTe$X3B
BatAgeT <- dataT$BatAge
BatAgeTe <- dataTe$BatAge
HRT <- dataT$HR
HRTe <- dataTe$HR
#These data sets are all the combinations of training and test data.
HO <- lm(RT ~ X2BT, data = dataT) 
HA <- lm(RT ~ X3BT, data = dataT) 
HE <- lm(RT ~ BatAgeT, data = dataT)
HI <- lm(RT ~ HRT, data = dataT)
#These are the lm models for all the variables we'd like tested
summary(HO)#P-Value: 0.000481
summary(HA)#P-Value: 0.77
summary(HE)#P-Value: 0.448
summary(HI)
shabadomamama <- lm(RT ~ X2BT + X3BT + BatAgeT + HRT, data = dataT)
summary(shabadomamama) #Lowest P-Value: 0.00189 HRT
#Alpha = .05
#According to the training data, HR and X2B are the only relevant independent variables we use since their P-Values are below alpha
HOHA <- lm(RT ~ X2BT + X3BT, data = dataT)
HOHE <- lm(RT ~ X2BT + BatAgeT, data = dataT)
HOHI <- lm(RT ~ X2BT + HRT, data = dataT)
HAHE <- lm(RT ~ X3BT + BatAgeT, data = dataT)
HAHI <- lm(RT ~ X3BT + HRT, data = dataT)
HEHI <- lm(RT ~ BatAgeT + HRT, data = dataT)
HEHIHO <- lm(RT ~ X2BT + BatAgeT + HRT, data = dataT)
HIHOHA <- lm(RT ~ X2BT + X3BT + HRT, data = dataT)
HOHAHE <- lm(RT ~ X2BT + X3BT + BatAgeT, data = dataT)
HAHEHI <- lm(RT ~ X3BT + BatAgeT + HRT, data = dataT)

#B
RSquare <- function(A,P,n,RT,dataT){
  MeanR <- sum(dataT$R)/length(dataT$R)
  B <- predict(A) #The list of all predicted values of the function we need
  names(B) <- NULL
  Barg <- c()
  for (i in 1:length(RT)){
    Barg <- append(Barg, (RT[i]-B[i])^2)
  }
  Baba <- sum(Barg) #This gives us the summation of Yi - Pred Yi
  Barg <- c()
  for (i in 1:length(RT)){
    Barg <- append(Barg, (RT[i]-MeanR)^2)
  }
  Booey <- sum(Barg) #This gives us the summation of Yi - Mean Y
  RSpongebobSquarepants <- 1 - (((n-1)/(n-P)*(Baba/Booey)))
  RSpongebobSquarepants
  #This is my stupid name for Rsquared because I can't take anything seriously
}
#I spent so much time on this function this dear god
RSquare(HO,1,length(RT),RT,dataTe) #X2BT
RSquare(HA,1,length(RT),RT,dataTe) #X3BT
RSquare(HE,1,length(RT),RT,dataTe) #BatAge
RSquare(HI,1,length(RT),RT,dataTe) #HR
#The best model appears to be the one that includes BatAge alone. The best pair of predictors is X2B and BatAge.
#Possible Combinations X2B, X3B, BatAge, X2B + X3B, X2B + BatAge, X3B + BatAge, X2B + X3B + BatAge

#C
RSquare(HOHA,2,length(RT),RT,dataTe) #X2BT, X3BT
RSquare(HOHE,2,length(RT),RT,dataTe) #X2BT, BatAge
RSquare(HOHI,2,length(RT),RT,dataTe) #X2BT, HR
RSquare(HAHE,2,length(RT),RT,dataTe) #X3BT, BatAge
RSquare(HAHI,2,length(RT),RT,dataTe) #X3BT, HR
RSquare(HEHI,2,length(RT),RT,dataTe) #BatAge, HR
#We could compare the R Squared values when there are more predictors in question.
#However, we should bear in mind that when some values are compared but vary in P-Value (Say P1 = .00001 & P2 = .52), we should tread carefully and keep in mind that even if the Adj R^2 is fairly high, it might not be all we need to take into consideration
names(dataTe)

#D
HOe <- lm(RTe ~ X2BTe, data = dataTe) 
HAe <- lm(RTe ~ X3BTe, data = dataTe) 
HEe <- lm(RTe ~ BatAgeTe, data = dataTe)
HIe <- lm(RTe ~ HRTe, data = dataTe)
HOHAe <- lm(RTe ~ X2BTe + X3BTe, data = dataTe)
HOHEe <- lm(RTe ~ X2BTe + BatAgeTe, data = dataTe)
HOHIe <- lm(RTe ~ X2BTe + HRTe, data = dataTe)
HAHEe <- lm(RTe ~ X3BTe + BatAgeTe, data = dataTe)
HAHIe <- lm(RTe ~ X3BTe + HRTe, data = dataTe)
HEHIe <- lm(RTe ~ BatAgeTe + HRTe, data = dataTe)
HEHIHOe <- lm(RTe ~ X2BTe + BatAgeTe + HRTe, data = dataTe)
HIHOHAe <- lm(RTe ~ X2BTe + X3BTe + HRTe, data = dataTe)
HOHAHEe <- lm(RTe ~ X2BTe + X3BTe + BatAgeTe, data = dataTe)
HAHEHIe <- lm(RTe ~ X3BTe + BatAgeTe + HRTe, data = dataTe)
MSE <- function(A,B){
  ah <- c()
  B <- predict(B) #The list of all predicted values of the function we need
  names(B) <- NULL
  for (i in length(A)){
    ah <- append(ah, (A[i] - B[i])^2)
  }
  That <- (1/length(A)) * sum(ah)
  That
}
MSE(RTe, HOe)
MSE(RTe, HEe)
MSE(RTe, HIe)
MSE(RTe, HAe)
MSE(RTe, HOHAe)
MSE(RTe, HOHEe)
MSE(RTe, HOHIe)
MSE(RTe, HAHEe)
MSE(RTe, HAHIe)
MSE(RTe, HEHIe)
MSE(RTe, HEHIHOe)
MSE(RTe, HIHOHAe)
MSE(RTe, HOHAHEe)
MSE(RTe, HAHEHIe)
#The results here with the smallest MSE would be X2BTe + HRTe
#This shows us that these models are similar between the testing and training data
RMSE <- function(A,B,C){
  ah <- c()
  B <- predict(B) #The list of all predicted values of the function we need
  names(B) <- NULL
  for (i in length(A)){
    ah <- append(ah, (A[i] - B[i])^2)
  }
  That <- sqrt((sum(ah))/(length(A)-C))
  That
}
RMSE(RTe, HOe, 1)
RMSE(RTe, HEe, 1)
RMSE(RTe, HIe, 1)
RMSE(RTe, HAe, 1)
RMSE(RTe, HOHAe, 2)
RMSE(RTe, HOHEe, 2)
RMSE(RTe, HOHIe, 2)
RMSE(RTe, HAHEe, 2)
RMSE(RTe, HAHIe, 2)
RMSE(RTe, HEHIe, 2)
RMSE(RTe, HEHIHOe, 3)
RMSE(RTe, HIHOHAe, 3)
RMSE(RTe, HOHAHEe, 3)
RMSE(RTe, HAHEHIe, 3)
#The results here are consistent with our best predictors for the last model X2BTe + HRTe
#This shows that the initial values that we thought best to use for our model (X2BTe + HRTe) are consistently useful considering their lower RMSE & MSE values, P-values, and Adjusted R Squared values
#D
datad <- read.csv(file = "Bat_swing.csv", head = TRUE, sep = ",")
names(datad)
toto <- lm(distance ~ angle, data = datad)
summary(toto)
#According to a significance level of .01, we reject the hypothesis that angle and distance are correlated
quasytoto <- lm(distance ~ poly(angle, 2), data = datad)
summary(quasytoto)
#According to a significance level of .01, we reject the hypothesis that angle and distance are correlated
summary(toto)$r.squared
summary(quasytoto)$r.squared
#I didn't realise that I could just get the RSquared value from the summary so...
#Based on these R^2 values we can see that the quadratic regression is much better for data prediction than the linear regression
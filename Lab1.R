setwd("C:/Users/Marko/WIT/MATH4050")
library(readr)
Baseball_2021_season <- read_csv("Baseball_2021_season.csv")
View(Baseball_2021_season)
RS <- Baseball_2021_season$RS
RA <- Baseball_2021_season$RA
a <- 2
PartA <- (RS^a/(RA^a + RS^a))
PartA
partb <- function(b,RS,RA,Baseball_2021_season){
  Wins <- (RS^b/(RA^b + RS^b))
  MSE <- 1/length(Baseball_2021_season$RA) * sum((Wins - (Baseball_2021_season$`W-L%`)^2))
  MSE
}
b <- .1
partb(b,RS,RA,Baseball_2021_season)
b <- 100
partb(b,RS,RA,Baseball_2021_season)
"The higher the value of b, the smaller the mean squared error"
"B(i). The equation is squared to make sure the value is positive if the difference is negative"
"B(ii). THe DE and the RMSE are compared because they do not mean the same thing, even though it may seem that the square root would cancel the square in the RMSE."
Xi <- log(RA/RS)
Yi <- log(Baseball_2021_season$`W-L%`)
c <- 1
SSE <- sum((c*Xi - Yi)^2)
SSE
"We use the sum squared and not the absolute value because minimizing sums of squares is much easier than minimizing sums of absolute values"
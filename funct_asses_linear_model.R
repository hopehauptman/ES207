#Generic functions that calculate the MAE, RMSE and R2

#y is for the model-fitted predicted values and x is for the observed actual values.

#Function for mean absolute error
MAE = function(x, y){
  mean(abs(x - y))
}

#Function for root mean square error
RMSE = function(x, y){
  sqrt(mean((x - y)^2))
}

#function for R^2
R2 <- function(x,y) {
  cor(x, y)^2
  }

#This is a function to calculate all of the above
summary_stat_mod <- function(x , y)  {
  ss1 <- mean(abs(x - y))
  ss2 <- sqrt(mean((x - y)^2))
  ss3 <- cor(x, y)^2
  return(c(MAE=ss1, RMSE=ss2, R2=ss3))
}

summary_stat_mod(data)
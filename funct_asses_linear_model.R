#Generic functions that calculate the MAE, RMSE, R2, and Pbias
#df is the dataframe containing the training data
#original is the observational dependent variable
#modeleva is the model evaluated 

library(dplyr)
library(rlang) #packages for passing column names as function arguments
library(modelr)

model.test <- function(df, original, modeleva){    
  stopifnot(is.data.frame(df))
  original <- enquo(original)
  modeltest <- df %>% 
  spread_predictions(pred =  modeleva) %>% 
  mutate(d =!!original-pred, y=!!original)
  mae = mean(abs(modeltest$d))
  rmse = sqrt(mean((modeltest$d)^2))
  R2 = 1-(sum((modeltest$d)^2)/sum((modeltest$y-mean(modeltest$y))^2))
  Pbias = 100 * sum(-(modeltest$d))/sum(modeltest$y)
  cat(" MAE:", mae, "\n", "RMSE:", rmse, "\n", "R-squared:", R2, "\n", "Pbias:", Pbias)
}

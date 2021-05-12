model.test <- function(df, original, modeleva){    
  stopifnot(is.data.frame(df))
  original <- enquo(original)
  modeltest <- df %>% 
    spread_predictions(pred =  modeleva) %>% 
    mutate(d =!!original-pred, y=!!original)
  mae = mean(abs(modeltest$d))
  rmse = sqrt(mean((modeltest$d)^2))
  R2 = 1-(sum((modeltest$d)^2)/sum((modeltest$y-mean(modeltest$y))^2))
  if (sum(modeltest$y) != 0) {
    Pbias = 100 * sum(-(modeltest$d))/sum(modeltest$y)
  } else {
    
    stop("sum of observation data is zero, it is not possible to compute 'pbias'")  #adding this error message when sum of observation data is zero
  } 
  
  cat(" MAE:", mae, "\n", "RMSE:", rmse, "\n", "R-squared:", R2, "\n", "Pbias:", Pbias)
}

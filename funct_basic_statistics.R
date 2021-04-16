#The moments package has a 'skewness' function
library(moments)

#This is a function to calculate the mean, median, standard deviation, interqquartile range, and shwness from a data frame. 
summary_stat <- function(x)  {
  st1 <- mean(x, na.rm = TRUE)
  st2 <- median(x, na.rm = TRUE)
  st3 <- sd(x, na.rm = TRUE)
  st4 <- IQR(x, na.rm = TRUE)
  st5 <- skewness(x, na.rm = TRUE)
  return(c(Mean=st1, Median=st2, Standard_dev=st3, Interquartile_range=st4, Skew=st5))
}
summary_stat(data)
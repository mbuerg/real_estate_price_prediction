# outlier functions
extract_numerics <- function(df){
  #' Extract numerical columns from Data Frame
  #' 
  #' @description This function creates a subset of a data frame such that
  #' the data frame consists of non char/factors only
  #' 
  #'  
  #' @param df data frame
  #' 
  #' @usage extract_numerics(df)
  #' 
  #' @return Returns a data frame according to the input data frame but with
  #' no char/factors columns
  #' 
  #' @details This function can be helpful if you want to count the outliers
  #' of numerical columns and thus get rid if characters and factors
  #' 
  #' @examples
  #' extract_numerics(df=data)
  
  df_numeric <- df[, !(sapply(df, class) %in% c("character", "factor"))]
  return(df_numeric)
}


count_outliers <- function(x){
  #' Count outliers for each numerical columns
  #' 
  #' @description This function counts outliers for every column by calculating
  #' the upper and lower whiskers and then sum the number of values that are 
  #' higher/lower than the whiskers.
  #' 
  #'  
  #' @param x col (or row) of data frame
  #' 
  #' @usage extract_numerics(df)
  #' 
  #' @return Returns a data frame according to the input data frame but with
  #' no char/factors columns
  #' 
  #' @details This function should be used with the apply-function
  #' 
  #' @examples
  #' extract_numerics(df=data)
  
  lower_bound <- quantile(x)[1]
  higher_bound <- quantile(x)[4]
  iqr <- higher_bound - lower_bound
  whisker_low <- lower_bound - (1.5*iqr)
  whisker_high <- higher_bound + (1.5*iqr)
  
  outlier_low <- sum(x < whisker_low)
  outlier_high <- sum(x > whisker_high)
  
  
  
  return(list(outlier_low, outlier_high))
}


apply(test, 2, count_outliers)

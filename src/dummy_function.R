# create dummies function

create_dummies <- function(df
                           , trap=FALSE){
  #' Creates dummies for factors
  #' 
  #' @description This function creates Dummies for factor columns and
  #' deletes the original columns
  #' 
  #'  
  #' @param df data frame
  #' @param trap bool
  #' 
  #' @usage create_dummies(df trap)
  #' 
  #' @return Returns the input data frame without factor columns but with
  #' dummies that replace the factor columns
  #' 
  #' @details None
  #' 
  #' @examples
  #' create_dummies(df=data, trap=TRUE)
  
  # Beschränke df auf cols mit Faktoren
  df_factors <- df[, (sapply(df, class) == "factor")]
  dummies <- data.frame(matrix(ncol = 0, nrow = nrow(df)))
  
  dummy_names <- c()
  
  for(i in names(df_factors)){
    
    #alle außer das erste level
    lvls <- levels(df[, i])[-1]
    # nummerierung aller Faktorlevel
    lvl_numbers <- seq(along.with=lvls)
    
    if(trap){
      lvls <- levels(df[, i])
      lvl_numbers <- seq(along.with=lvls)
    }
    
    names(lvl_numbers) = lvls
    
    for(j in lvls){
      dummy <- as.integer(df[, i] == j)
      #names(dummy) <- paste(i, "_", j)
      dummies <- cbind(dummies, dummy)
      dummy_names <- c(dummy_names, paste(i, "_", j))
    }
    df <- df[,!names(df) == i]
  }
  
  names(dummies) <- dummy_names
  df_with_dummies <- cbind(df, dummies)
  return(df_with_dummies)
}

#test <- create_dummies(data_complete_imputated)[[1]]




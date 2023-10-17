# clean data

# check on duplicates

data_complete_imputated <- read.csv(path_data_complete_imputated)

clean_duplicates <- function(df){
  #' Deletes duplicate rows from data frame
  #' 
  #' @description This function uses duplicated()-function to get rid of
  #' rows that appear more than once in a data frame
  #' 
  #'  
  #' @param df data frame
  #' 
  #' @usage clean_duplicate(df)
  #' 
  #' @return Returns the input data frame without duplicated rows
  #' 
  #' @details None
  #' 
  #' @examples
  #' clean_duplicates(df=data)
  
  
  # assume there are no duplicates. If there are df_no_duplicates is overwritten
  # if not the original data frame is the output
  df_no_duplicates <- df
  try(
    df_no_duplicates <- df[!duplicated(df), silent=TRUE]
    )
  
  return(df_no_duplicates)
}

clean_duplicates(data_complete_imputated)

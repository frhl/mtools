#' converts a data column into onehot encoding.
#' 
#' takes a dataset and a variable and converts it to a onehot encoding.
#' 
#' @param data a data.frame
#' @param var a variable in the data.frame 
#' @export 

as.onehot <- function(data, var){
  
  stopifnot(var %in% names(data))
  
  for (level in data[[var]]){
    col = as.character(data[[var]])
    col[col != level] <- 0
    col[col == level] <- 1
    data[[paste(var,":",level, sep = "")]] <- as.factor(col)
  }
  return(data)
}

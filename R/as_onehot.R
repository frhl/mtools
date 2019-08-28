#' converts a data column into onehot encoding.
#' 
#' takes a dataset and a variable and converts it to a onehot encoding.
#' 
#' @param data a data.frame
#' @param var a variable in the data.frame 
#' @export 


as_onehot <- function(data, var){
  
  stopifnot(var %in% names(data))
  
  for (level in data[[var]]){
    col = as.character(data[[var]])
    col[col != level] <- 0
    col[col == level] <- 1
    data[[paste(var,":",level, sep = "")]] <- as.factor(col)
  }
  
  data[[var]] <- NULL
  
  return(data)
}

#' Converts to mean of zero and sd of one
#' 
#' takes a dataset and a variable and converts it into
#' having a mean of zero, and a standard deviation of one.
#' 
#' @param data a data.frame
#' @param var a variable in the data.frame 
#' @export 


as_standardized <- function(data, var){
  
  stopifnot(var %in% names(data))
  stopifnot(is.numeric(data[[var]]))
  
  x <- data[[var]]
  x <- x - mean(x)
  x <- x / sd(x)
  data[[var]] <- x
  
  return(data)
}


#' Converts data column to be ranging between one and zero
#' 
#' takes a dataset and a variable and converts it into
#' numbers ranging between one and zero.
#' 
#' @param data a data.frame
#' @param var a variable in the data.frame 
#' @export 

as_normalized <- function(data, var){
  
  stopifnot(var %in% names(data))
  stopifnot(is.numeric(data[[var]]))  
  
  x <- data[[var]]
  data[[var]] <- (x-min(x))/(max(x)-min(x))
  
  return(data)
}

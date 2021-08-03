#' Calculate a z-score for numeric data
#' 
#' @param x A data point
#' @param mu An average
#' @param s A standard deviation
#' 
#' @export
 
numeric_z <- function(x, mu, s){
  z <-  (x-mu)/s
  return(z)
}
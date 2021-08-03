#' Calculate a z-score for categorical data
#' 
#' @param p_hat A sample proportion
#' @param p A population proportion
#' @param n Sample size
#' 
#' @export

categorical_z <- function(p_hat, p, n){
  z <-  (p_hat-p)/sqrt(p*(1-p)/n)
  return(z)
}
#' Calculate a confidence interval for categorical data
#' 
#' @param p_hat A sample proportion
#' @param alpha A significance level
#' @param n Sample size
#' 
#' @import stats
#' 
#' @export

prop_ci <- function(p_hat, alpha, n){
  a <- 1-(alpha/2)
  ci <- p_hat+c(-1,1)*qnorm(a)*sqrt(p_hat*(1-p_hat)/n)
  lower_ci <- ci[1]
  upper_ci <- ci[2]
  df <- data.frame(lower_ci, upper_ci)
  return(df)
}
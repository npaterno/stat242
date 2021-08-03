#' Evaluate a linear model at a given point
#' 
#' @param model A linear model
#' @param value A value to make a prediction with
#' 
#' @export

linear_model_prediction <- function(model, value){
  pred <- model$coefficients[1]+value*model$coefficients[2]
  return(pred)
}
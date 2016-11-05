######################################################
### Fit model with test data                       ###
######################################################

test <- function(trained.model, model, dat_test){
  
  ### Input: 
  ###  - Fitted model using training data
  
  ### Output: Predicted values
  ###         Type: DOUBLE, column Vector
  
  if(model == "GBM"){
    
    ### load libraries
    library("gbm")
    
    pred <- predict(trained.model$fit, newdata=dat_test, 
                    n.trees=trained.model$gbm.perf, type="link")
  }
  return(pred)
}
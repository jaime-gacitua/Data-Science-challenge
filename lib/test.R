######################################################
### Fit model with test data                       ###
######################################################

test <- function(trained.model, model, dat_test){
  
  ### Input: 
  ###  - Fitted model using training data
  
  ### Output: Predicted values
  ###         Type: DOUBLE, column Vector
  
  if(model == "GBM"){
    library("gbm")
    
    pred <- predict(trained.model$fit, newdata=dat_test, 
                    n.trees=trained.model$gbm.perf, type="link")
    
    return(pred)
  }
  
  if(model == "RIDGE"){
    library(glmnet)
    
    cat("Entering Testing Module", "\n")
    
    dat_test <- as.matrix(dat_test)
    
    pred <- predict(trained.model$fit, newx = dat_test)
    
    cat("Done Testing Module", "\n")
    
    return(pred)
  }
  
  if(model == "LASSO"){
    library(glmnet)
    
    cat("Entering Testing Module", "\n")
    
    dat_test <- as.matrix(dat_test)
    
    pred <- predict(trained.model$fit, newx = dat_test)
    
    cat("Done Testing Module", "\n")
    
    return(pred)
  }

}
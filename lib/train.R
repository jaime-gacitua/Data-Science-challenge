#########################################################
### Training Functions ##################################
#########################################################


train <- function(dat_train, label_train, model, param){
  
  cat("## Entering training block \n")
  ### Train a Gradient Boosting Model (GBM)
  
  
  if(model == "GBM"){
    library("gbm")

    cat("Starting Generalized Boosting Model (GBM) \n")
    fitted.gbm <- gbm.fit(x=dat_train, y=label_train,
                       n.trees= param,
                       distribution="gaussian",
                       interaction.depth=2, 
                       bag.fraction = 0.6,
                       n.minobsinnode = 2,
                       verbose=FALSE)
    gbm.best <- gbm.perf(fitted.gbm, method="OOB")
    
    
    return(list(fit=fitted.gbm, gbm.perf=gbm.best))    
  }
  
  if(model == "RIDGE"){
    
    cat("Ridge Selected", param, typeof(param), "\n")
    
    library(glmnet)
    
    dat_train <- as.matrix(dat_train)
    label_train <- as.vector(label_train)
    
    fitted <- glmnet(x = dat_train, y = label_train, alpha=0, lambda = param) # Alpha = 0 means ridge
    
    cat("Done fitting model", "\n")
    
    return(list(fit=fitted))
  }
  
  if(model == "LASSO"){
    
    cat("Lasso Selected", param, typeof(param), "\n")
    
    library(glmnet)
    
    dat_train <- as.matrix(dat_train)
    label_train <- as.vector(label_train)
    
    fitted <- glmnet(x = dat_train, y = label_train, alpha=1, lambda = param) # Alpha = 1 means lasso
    
    
    cat("Done fitting model", "\n")
    
    return(list(fit=fitted))
  }

    
}


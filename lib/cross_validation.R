########################
### Cross Validation ###
########################

### Author: Yuting Ma
### Project 3
### ADS Spring 2016


cv.function <- function(X.train, y.train, model, params, K){
  
  
  # Setup K-folds
  cat("Parameters:", params, "\n")
  n <- length(y.train)
  n.fold <- floor(n/K)
  s <- sample(rep(1:K, c(rep(n.fold, K-1), n-(K-1)*n.fold)))  
  cv.error <- rep(NA, K)
  

  for (i in 1:K){
    
    # Setup training and test set for current loop
    cat("## Starting CV fold ", i, "## \n")
    train.data <- X.train[s != i,]
    train.label <- y.train[s != i]
    test.data <- X.train[s == i,]
    test.label <- y.train[s == i]
    

    # Train and Calculate predicted values
    trained.model <- train(train.data, train.label, model, params)
    pred <- test(trained.model, model, test.data)

    
    cov.pr <- cov(test.label,pred)
    var.test <- sd(test.label)
    var.pred <- sd(pred)
#    pearson <- cov.pr / var.test / var.pred 
    corr <- cor(test.label,pred)

    cat("Predictions: ", typeof(pred), ", ", length(pred), "\n")        
#    cat("Covariance: ", cov.pr, "\n")
#    cat("Var.Test: ", var.test, "\n")
#    cat("Var.Pred: ", var.pred, "\n")
#    cat("Predictions: ", pred, "\n")
#    cat("Pearson", pearson, "\n")
    r2 <- corr^2
    cat("R2: ", r2, "\n")
    
    cv.error[i] <- r2

  }			

#  cat("CV Errors: ", cv.error, "\n")
  
  cv.error <- na.omit(cv.error)
  
  return(c(mean(cv.error),sd(cv.error)))
  
}




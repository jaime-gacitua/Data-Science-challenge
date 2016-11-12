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

    # Calculate KPI
    
    n <- length(test.label)
    p <- ncol(train.data)
    
    
    test.label.avg <- mean(test.label)
    TSS <- sum((test.label-test.label.avg)^2)
    RSS <- sum((pred-test.label)^2) # Residual squared error
    RSE <- sqrt( RSS / (n - p - 1)) # Residual Standard error
    R2 <- 1 - RSS / TSS
    adjR2 <- 1 - (RSS / (n - p - 1) ) / ( TSS / (n-1) )
    
    cat("Predictions: ", typeof(pred), ", ", length(pred), "\n")        

    cat("n: ", n, "\n")
    cat("p: ", p, "\n")
    cat("Test Average: ", test.label.avg, "\n")
    cat("TSS: ", TSS, "\n")
    cat("RSS: ", RSS, "\n")
    cat("RSE", RSE, "\n")
    cat("R2: ", R2, "\n")
    cat("adjR2: ", adjR2, "\n")
    
    cv.error[i] <- R2
  }			

#  cat("CV Errors: ", cv.error, "\n")
  
  cv.error <- na.omit(cv.error)
  
  return(c(mean(cv.error),sd(cv.error)))
  
}




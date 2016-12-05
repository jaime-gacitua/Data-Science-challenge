
## Functions called by main.R

do.cv <- function(data.train, label.train, model, params, K){
  
  # Loop over each parameter set
  cv.errors <- lapply(params, function(x){
    
    cv.function(data.train, label.train, model, x, K)
  
    })
  
  # Transform results into a matrix. Allows plotting later.
  # 2 columns: (Mean, StDev)
  cv.errors.mat <- matrix(unlist(cv.errors), ncol = 2, byrow = TRUE)
  
  number.rows <- length(params)
  modelcol <- rep(model, number.rows)
  
  cv.errors.mat <- cbind(cv.errors.mat, modelcol)  
  
  return(cv.errors.mat)
}

plot.errors <- function(err_cv, params, txt){
  x.axis <- params
  plot(x.axis, err_cv[,1], xlab="Number of Trees", ylab="R2",
       main=paste0("Cross Validation R2 | ", txt), 
       type="n", ylim=c(0, 1), xaxt="n")
  axis(1, at = x.axis)
  points(x.axis, err_cv[,1], col="blue", pch=16)
  lines(x.axis, err_cv[,1], col="blue")
  arrows(x.axis, err_cv[,1]-err_cv[,2],x.axis, err_cv[,1]+err_cv[,2], 
         length=0.1, angle=90, code=3)
}

do.KLdivergence <- function(location.variety.check.yield){
  
  ## NOT READY YET
  
  df <- location.variety.check.yield
  
  #convert to probability distributions
  norm_var = aggregate(df[,6]~ df[,2],df,FUN = sum)
  norm_check = aggregate(df[,7]~ df[,2],df,FUN = sum)
  
  for (i in 1:nrow(df)){
    k1 = match(df[i,2],norm_var[,1])
    k2 = match(df[i,2],norm_check[,1])
    df[i,10] = df[i,6]/norm_var[k1,2]
    df[i,11] = df[i,7]/norm_check[k2,2]
    df[i,12] = df[i,10]*log(df[i,10]/df[i,11]) #calculate the number of KL Divergence
  }
  
  #sum over all for each variety
  result = aggregate(df[,12]~ df[,2],df,FUN = sum)
  
  
  
}



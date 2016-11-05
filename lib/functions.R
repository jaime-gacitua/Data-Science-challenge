
## Functions called by main.R

do.cv <- function(data.train, label.train, model, params, K){
  
  # Loop over each parameter set
  cv.errors <- lapply(params, function(x){
    cv.function(data.train, label.train, model, x, K)
  })
  
  # Transform results into a matrix. Allows plotting later.
  # 2 columns: (Mean, StDev)
  cv.errors.mat <- matrix(unlist(cv.errors), ncol = 2, byrow = TRUE)
  
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

#############################################################
######## FEATURES FUNCTIONS #################################


feature_base <- function(filename){
  library(readxl)

  data <- read_excel(filename)
  data <- data[,3:8]
  data <- data[,-5]
  features <- data[,-ncol(data)]
  response <- data[,ncol(data)]
  return(list(features = features, response = response))

}
#############################################
### Main execution script for experiments ###
#############################################

### Author: Yuting Ma
### Project 3
### ADS Spring 2016

### Specify directories
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(readxl)

### Get Features ----
source("./lib/feature.R")
filename <- "./data/Data_Regression_trial1.xlsx"
inputData <- feature_base(filename)

### Get functions
source("./lib/train.R")
source("./lib/test.R")
source("./lib/cross_validation.R")
source("./lib/functions.R")

#### Run experiments

### 1

## Setup
para <- c(4000, 5000, 6000, 7000)
K <- 5  # number of CV folds
obs.text <- "test0"
model <- "GBM"

## Run
GBM.results <- do.cv(inputData$features, inputData$response, model, para, K)
## Plot
plot.errors(GBM.results, para, "Generalized Boosting Model (GBM)" )
## Write JPEG plot
jpeg(filename = "./output/GBM.jpg",
     width = 640, height = 480, units = "px", pointsize = 12,
     quality = 100)
plot.errors(GBM.results, para, "Generalized Boosting Model (GBM)" )
dev.off()



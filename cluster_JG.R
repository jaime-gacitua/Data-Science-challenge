#############################################
### Main execution script for experiments ###
#############################################

### Author: 
### Project 
### 

### Specify directories
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(readr)
library(hexbin)
library(readxl)
library(dplyr)
library(tidyr)
library(plotly)


# Load Data
raw.data <- readxl::read_excel('./data/EXPERIMENT DATA.xlsx')
var.checks.data <- read_csv("./data/VAR_CHECKS.csv")

# Transform columns
raw.data$EXPERIMENT <- factor(raw.data$EXPERIMENT)
raw.data$LOCATION <- factor(raw.data$LOCATION, levels = unique(raw.data$LOCATION))
raw.data$VARIETY <- factor(raw.data$VARIETY)
raw.data$FAMILY <- factor(raw.data$FAMILY)
raw.data$CHECK <- factor(raw.data$CHECK)
raw.data$CLASS_OF <- factor(raw.data$CLASS_OF)

## Location Features ----

# Location Features: MEAN(Yield), SD(Yield)
location.features <- raw.data %>% dplyr::group_by(LOCATION) %>%
  dplyr::summarise(Yieldmean = mean(YIELD),
                   Yieldsd = sd(YIELD))


# Location Features: KL.Divergence(Yield.Variety, Yield.Check)

## Waiting for Talia

## Cluster LOCATION over the collected features ----
## Using K-Means

# Analyze the error for different K's
centers <- 50
error <- lapply(1:centers, function(x){
  clusters <- kmeans(x = location.features[,-1], centers = x, nstart = 100)
  return(clusters$tot.withinss)
})
plot(y = error, x = 1:centers, main = "LOCATION clustering using K-MEANS. Error for different Values of K")


# Select K and cluster
selected.centers <- 9
clusters <- kmeans(x = location.features[,-1], centers = selected.centers, nstart = 2000)

# Add the cluster to the dataframe
location.features.clustered <- cbind(location.features, clusters$cluster)
names(location.features.clustered)[4] <- 'cluster'


## Calculate VARIETY features stratified by LOCATION cluster ----

# Add cluster to raw data
raw.data.clustered <- inner_join(x = raw.data, 
                                 y = location.features.clustered[,c(1,ncol(location.features.clustered))],
                                 by = 'LOCATION')

# Generate VARIETY features matrix
variety.location.clustered.features <- raw.data.clustered %>% 
  group_by(VARIETY, cluster)%>%
  summarise(Yieldmean = mean(YIELD)) %>% spread(cluster, Yieldmean)
a <- 1:selected.centers
a <- paste0("m", a)     
names(variety.location.clustered.features) <- c("VARIETY", a)


# Bring the varieties with BAGSOLD to analyze

# Alternative 1
bagsold.regression.data <- read_excel(filename <- "./data/Data_Regression_trial1.xlsx")

# Alternative 2
bagsold.regression.data <- raw.data %>% dplyr::filter(BAGSOLD > 0) 
bagsold.regression.data <- unique(bagsold.regression.data[,c(4,12)])


bagsold.regression.data.expanded <- left_join(bagsold.regression.data,
                                              variety.location.clustered.features,
                                              by = "VARIETY")
numcols <- ncol(bagsold.regression.data.expanded)


# We have combinations of (VARIETY, LOCATION) where there is no data.
# Fill the NA gaps with the average
for(i in 3:numcols ){
  ## Get the average of the column
  average <- mean(as.matrix(na.omit(bagsold.regression.data.expanded[,i])))
  ## Replace all the values with the average
  bagsold.regression.data.expanded[is.na(bagsold.regression.data.expanded[,i]),i] = average
}

## Write text file with final features ----

#columns.to.output <- c(1,9:numcols,8)
write_csv(bagsold.regression.data.expanded,
          "./output/varieties_clustered_features_1.csv")







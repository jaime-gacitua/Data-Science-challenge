setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


library(readr)
library(hexbin)



data.with.bagsold <- read_csv('./data/dataframe_kostas.csv')

summary(data.with.bagsold)
col.relevant <- c('RM', 'VAR_YIELD', 'CHECK_YIELD', 'RATIO', 'BAGSOLD')

data.with.bagsold.hexcolumns <- data.with.bagsold[,col.relevant]

hex.plot <- hexplom(data.with.bagsold.hexcolumns
                    ,main = 'Density plots for 5 variables.\n Objective: Predict BAGSOLD')

hex.plot

names(data.with.bagsold)
                    
                    
str(data.with.bagsold)
summary(data.with.bagsold)

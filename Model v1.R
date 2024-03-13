# Setting Up --------------------------------------------------------------
cat("\014") # Clear console
rm(list=ls()) # Clear Environment

setwd("C:/Users/Anoushay/Desktop/ACTL4001/Cleaned Data")
# setwd("/Users/karandeshwal/Documents/R/ACTL4001")

# install.packages('tidyverse')
# install.packages('dplyr')
# install.packages('readxl')
# install.packages('openxlsx')

library(tidyverse)
library(dplyr)
library(readxl)
library(tidyr)
# library(reshape2)
# library(ggplot2)
# library(glmnet)
# library(gmodels)
# library(zoo) # reading time datatype
# library(corrplot) # correlation plots

# Read in the Data
inforce_data <- read.csv("inforce_data.csv", header = TRUE)
intervention_data <- read.csv("intervention_data.csv", header = TRUE)
eco_data <- read.csv("economy_data.csv", header = TRUE)
mortality_data <- read.csv("mortality_data.csv", header = TRUE)


# Base Mortality Table Calculation
mortality_data <- mortality_data[,-c(1)]
mortality_data$p_x <- 1- mortality_data$Mortality.Rate
mortality_data$n_p_x <- 1
  
for (i in 2:nrow(mortality_data)) {
  mortality_data$n_p_x[i] <- mortality_data$n_p_x[i-1] * mortality_data$p_x[i-1]
} 

mortality_data$nLx <- c(0, 
      sapply(2:nrow(mortality_data), function(i) mean(mortality_data$n_p_x[(i-1):i])))

mortality_data$n_e_x <- c(0, 
      sapply(2:nrow(mortality_data),function(i) sum(mortality_data$nLx[i:nrow(mortality_data)])/mortality_data$n_p_x[i-1]))
  

# Economy Table
eco_data$Rolling_20_Year <- 0


# 
# sapply(21:nrow(eco_data), function(i) cumprod(1+eco_data$X1.yr.Risk.Free.Annual.Spot.Rate[i-20:i])))
# 


for (i in 20:nrow(mortality_data)) {
  eco_data$Rolling_20_Year[i] <- (1+eco_data$Inflation[i]) *(1+ eco_data$Inflation[i-20])
}
  


# Sample dataset with 44 data values
original_data <- data.frame(
  value = c(1:44)
)

# Multiply each cell starting at row 20 by 1 plus the corresponding cell of the last 20 years
for (i in 20:nrow(original_data)) {
  original_data$value[i] <- original_data$value[i] * (1 + original_data$value[i-20])
}

# Print the modified dataset
print(original_data)

  
  
  
  
  
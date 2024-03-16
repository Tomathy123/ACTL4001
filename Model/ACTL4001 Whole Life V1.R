rm(list=ls()) # Clear Environment

setwd("/Users/arneetkalra/Desktop/UNSW Onedrive/UNI/2024/ACTL4001/Assignment/Data") #change to your own
# setwd("C:/Users/aluis/Documents/UNSW/ACTL4001") 
# setwd("C:/Users/Anoushay/Desktop/ACTL4001/Cleaned Data")
# setwd("/Users/karandeshwal/Documents/R/ACTL4001")

# install.packages('tidyverse')
# install.packages('dplyr')
# install.packages('readxl')
# install.packages('openxlsx')

library(tidyverse)
library(dplyr)
library(readxl)
library(tidyr)
library(openxlsx)


# Read in the Data (files in the drive)
inforce_data <- read.csv("inforce_data.csv", header = TRUE)
intervention_data <- read.csv("intervention_data.csv", header = TRUE)
eco_data <- read.csv("economy_data.csv", header = TRUE)
mortality_data <- read.csv("mortality_data.csv", header = TRUE)


# Base Mortality Table Calculation
#Removing the additional column
mortality_data <- mortality_data[,-c(1)]
#finding the probability of survival at each age
mortality_data$p_x <- 1- mortality_data$Mortality.Rate


average_spot_rate <- mean(eco_data[,4])
#This is used, if current year is past 2023


insurance_20_year <- function (x, issue_year) {
  kpx <- rep(0,20)
  
  kpx[1] <- mortality_data$p_x[x]
  
  for (i in 2:20) {
    kpx[i] <- prod(kpx[i-1],mortality_data$p_x[x+i-1])
  }
  
  #Find kpx values, will use formula k|q_x=kpx*q_(x+k)
  
  spot_rate_x <- rep(0,20)
  
  #This checks if current year is less than 2023, if so, use spot rate, otherwise use average
  for (i in 1:20) {
    current_year <- issue_year + i - 1
    if (length(eco_data$X1.yr.Risk.Free.Annual.Spot.Rate[eco_data$Year == current_year]) == 0) {
      spot_rate_x[i] <- average_spot_rate
    } else {
      spot_rate_x[i] <- eco_data$X1.yr.Risk.Free.Annual.Spot.Rate[eco_data$Year == current_year]
    }
  }
  
  #This should generate a vector of (1+i,(1+i)^2,...,(1+i)^20)
  effective_interest <- c(1+spot_rate_x[1])
  for (i in 2:20) {
    effective_interest[i] <- prod(effective_interest[i-1], (1 + spot_rate_x[i]))
  }
  
  #Finding the components of the sum for the final value
  # Using sum of v^(k+1)*kpx*q_(x+k)
  value <- rep(0,20)
  value[1] <- prod(mortality_data$Mortality.Rate[x], 
                   (1/(effective_interest[1])))
  
  for (i in 1:19) {
    value[i+1] <- prod(kpx[i],mortality_data$Mortality.Rate[x+i], 1/(effective_interest[i+1]))
  }
  
  final <- sum(value)
  return(final)
  
}

###IMPORTANT
#I think there is a small error somewhere as it is returning values which don't seem right
#When compared to AM92 in orange book.

insurance_20_year(40, 2000)


#Annuity

annuity_due_term <- function (x, issue_year) {
  
  kpx <- rep(0,20)
  
  kpx[1] <- mortality_data$p_x[x]
  
  for (i in 2:20) {
    kpx[i] <- prod(kpx[i-1],mortality_data$p_x[x+i-1])
  }
  
  
  spot_rate_x <- rep(0,19)
  
  #This checks if current year is less than 2023, if so, use spot rate, otherwise use average
  for (i in 1:19) {
    current_year <- issue_year + i - 1
    if (length(eco_data$X1.yr.Risk.Free.Annual.Spot.Rate[eco_data$Year == current_year]) == 0) {
      spot_rate_x[i] <- average_spot_rate
    } else {
      spot_rate_x[i] <- eco_data$X1.yr.Risk.Free.Annual.Spot.Rate[eco_data$Year == current_year]
    }
  }
  
  #This should generate a vector of (1+i,(1+i)^2,...,(1+i)^20)
  effective_interest <- c(1+spot_rate_x[1])
  for (i in 2:19) {
    effective_interest[i] <- prod(effective_interest[i-1], (1 + spot_rate_x[i]))
  }
  
  
  value <- rep(0,20)
  value[1] <- 1
  
  for (i in 1:19) {
    value[i+1] <- prod(kpx[i],1/effective_interest[i])
  }
  
  
  
  final <- sum(value)
  return(final)
  
}


50000*insurance_20_year(40,2000)

2262.102/annuity_due_term(40,2000)

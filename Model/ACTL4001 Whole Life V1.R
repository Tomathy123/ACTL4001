# Setting Up --------------------------------------------------------------
cat("\014") # Clear console
rm(list=ls()) # Clear Environment


# Directory ---------------------------------------------------------------
setwd("/Users/arneetkalra/Desktop/UNSW Onedrive/UNI/2024/ACTL4001/Assignment/Data") #change to your own
# setwd("C:/Users/aluis/Documents/UNSW/ACTL4001") 


# Packages ----------------------------------------------------------------
# install.packages('tidyverse')
# install.packages('dplyr')
# install.packages('readxl')
# install.packages('openxlsx')

library(tidyverse)
library(dplyr)
library(readxl)
library(tidyr)
library(openxlsx)


# Read in the Data (files in the drive) -----------------------------------
inforce_data <- read.csv("inforce_data.csv", header = TRUE)
intervention_data <- read.csv("intervention_data.csv", header = TRUE)
eco_data <- read.csv("economy_data.csv", header = TRUE)
mortality_data <- read.csv("mortality_data.csv", header = TRUE)


# Split the Inforce Data --------------------------------------------------
splitbypolicytype <- split(inforce_data, inforce_data$Policy.type)

t_20 <- splitbypolicytype[["T20"]]
spwl <- splitbypolicytype[["SPWL"]]


# Base Mortality Table Calculation ----------------------------------------
#Removing the additional column
mortality_data <- mortality_data[,-c(1)]
#finding the probability of survival at each age
mortality_data$p_x <- 1- mortality_data$Mortality.Rate


# Average Spot Rate -------------------------------------------------------
average_spot_rate <- mean(eco_data[,4])
#This is used, if current year is past 2023


# Whole Life Function -----------------------------------------------------
whole_life <- function(x, issue_year, face_value) {
  max_age <- max(which(!is.na(mortality_data$p_x)))
  n_years <- max_age - x
  
  # Calculate survival probabilities (kpx)
  kpx <- numeric(n_years + 1)
  kpx[1] <- 1 # Initial survival probability is 1
  for (i in 2:length(kpx)) {
    kpx[i] <- kpx[i-1] * mortality_data$p_x[x + i - 2] # Corrected to use actual survival rates
  }
  
  # Calculate spot rates for each year
  spot_rate_x <- rep(average_spot_rate, n_years) # Default to average if no data available
  for (i in 1:n_years) {
    current_year <- issue_year + i - 1
    if (current_year <= 2023) { # Assuming you have spot rate data up to 2023
      spot_rate_x[i] <- ifelse(is.na(eco_data$X1.yr.Risk.Free.Annual.Spot.Rate[eco_data$Year == current_year]), average_spot_rate, eco_data$X1.yr.Risk.Free.Annual.Spot.Rate[eco_data$Year == current_year])
    }
  }
  
  # Calculate present value factors (discount factors)
  v <- 1 / (1 + spot_rate_x) # Direct calculation of discount factors
  
  # Calculate the values (present value of expected benefits)
  value <- numeric(n_years)
  for(i in 1:n_years) {
    value[i] <- kpx[i] * mortality_data$Mortality.Rate[x+i-1] * prod(v[1:i])
  }
  
  final_value <- face_value*sum(value)
  return(final_value)
}


whole_life(54,2001, 1000000)


# Finding the Premiums for the SPWL Dataset -------------------------------
premiums <- mapply(whole_life, spwl$Issue.age, spwl$Issue.year, spwl$Face.amount)

spwl$prem_at_issue_year <- premiums


# Exporting the premiums for the SPWL -------------------------------------
write_csv(spwl, "spwl_with_premiums.csv")


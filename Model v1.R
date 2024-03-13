# Setting Up --------------------------------------------------------------
cat("\014") # Clear console
rm(list=ls()) # Clear Environment

setwd("/Users/arneetkalra/Desktop/UNSW Onedrive/UNI/2024/ACTL4001/Assignment/Data") #change to your own
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


# Read in the Data
inforce_data <- read.csv("inforce_data.csv", header = TRUE)
intervention_data <- read.csv("intervention_data.csv", header = TRUE)
eco_data <- read.csv("economy_data.csv", header = TRUE)
mortality_data <- read.csv("mortality_data.csv", header = TRUE)


# Base Mortality Table Calculation
#Removing the additional column
mortality_data <- mortality_data[,-c(1)]
#finding the probability of survival at each age
mortality_data$p_x <- 1- mortality_data$Mortality.Rate
#nPx Calculation
mortality_data$n_p_x <- 1
  
for (i in 2:nrow(mortality_data)) {
  mortality_data$n_p_x[i] <- mortality_data$n_p_x[i-1] * mortality_data$p_x[i-1]
} 

#nLx Calculation
mortality_data$nLx <- c(0, 
      sapply(2:nrow(mortality_data), function(i) mean(mortality_data$n_p_x[(i-1):i])))

#nEx Calculation
mortality_data$n_e_x <- c(0, 
      sapply(2:nrow(mortality_data),function(i) sum(mortality_data$nLx[i:nrow(mortality_data)])/mortality_data$n_p_x[i-1]))
  

# Economy Table
# Don't need this but use factors column
#converting to 1+interest rate
eco_data$factors <- 1+ eco_data$X1.yr.Risk.Free.Annual.Spot.Rate

# find the average factor for the years to use for the years after 2023
avg_factor <- mean(eco_data$factors, na.rm = TRUE)

# #Rolling 20 year rate
# eco_data$rolling_20 <- numeric(length(eco_data$factors))
# eco_data$rolling_20[1:19] <- 0 # Setting first 19 elements to 0
# 
# for(i in 20:length(eco_data$factors)) {
#   eco_data$rolling_20[i] <- prod(factors[(i-19):i])
# }

# Split up the Data
splitbypolicytype <- split(inforce_data, inforce_data$Policy.type)

t_20 <- splitbypolicytype[["T20"]]
spwl <- splitbypolicytype[["SPWL"]]

#tester
test <- t_20[c(1:5),]


# THE MODEL FOR BASE PREMIUM
# Arneet's trial down below:
calculate_premium <- function(issue_year, issue_age, face_amount, mortality_rate, interest_rate) {
  term_years <- 20
  premium_payments <- numeric(term_years) # To store premium payments for each year

  for (i in 1:term_years) {
    current_age <- issue_age + i - 1
    if (current_age > 120) break
    mortality_rate <- mortality_data$Mortality.Rate[mortality_data$Age == current_age] # Need to find and use exposure
    if (length(mortality_rate) == 0) next # Skip if no matching mortality rate
    current_year <- issue_year + i - 1
    interest_rate <- ifelse(current_year > 2023, avg_factor, eco_data$factors)

    # Calculate expected payout for the year
    expected_payout <- face_amount * mortality_rate
    # Discount to present value
    pv_expected_payout <- expected_payout / ((interest_rate)^i)
    premium_payments[i] <- pv_expected_payout
  }
  # sum of discounted premium payouts
  premium <- sum(premium_payments) / sum(1 / ((interest_rate[as.character(issue_year:(issue_year+19))])^(1:term_years))) # Need to fix this part here
  return(premium)
}

premium_for_policyholder <- calculate_premium(test$Issue.year[1], test$Issue.age[1], test$Face.amount[1], mortality_data$Mortality.Rate, eco_data$factors)
















# Exporting Mortality, Economic and Premium data into a csv
write_csv(intervention_data, "intervention_data.csv")
write_csv(eco_data, "economy_data.csv")
write_csv(inforce_data, "inforce_data.csv")
write_csv(mortality_data, "mortality_data.csv")
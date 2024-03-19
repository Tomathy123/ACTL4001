# Setting Up --------------------------------------------------------------
cat("\014") # Clear console
rm(list=ls()) # Clear Environment


# Directory ---------------------------------------------------------------
setwd("/Users/arneetkalra/Desktop/ACTL4001/Data") #change to your own
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
eco_data <- read.csv("economy_data.csv", header = TRUE)
mortality_modified <- read.csv("incentive_modmort.csv", header = TRUE)


# Split the Inforce Data --------------------------------------------------
splitbypolicytype <- split(inforce_data, inforce_data$Policy.type)

t_20 <- splitbypolicytype[["T20"]]
spwl <- splitbypolicytype[["SPWL"]]


# Base Mortality Table Calculation ----------------------------------------
#Removing the additional column
#mortality_data <- mortality_data[,-c(1)]
#finding the probability of survival at each age
#mortality_data$p_x <- 1- mortality_data$Mortality.Rate

#Renaming the Header
colnames(mortality_modified) <- c("Age","MS", "FS", "MNS", "FNS")

#Calculating the survival p_x
mortality_modified$MS.p_x <- 1 - mortality_modified$MS
mortality_modified$FS.p_x <- 1 - mortality_modified$FS
mortality_modified$MNS.p_x <- 1 - mortality_modified$MNS
mortality_modified$FNS.p_x <- 1 - mortality_modified$FNS


# Average Spot Rate -------------------------------------------------------
average_spot_rate <- mean(eco_data[,4])
#This is used, if current year is past 2023


# Whole Life Function -----------------------------------------------------
whole_life <- function(x, issue_year, face_value, gender, s_status) {
  max_age <- max(which(!is.na(mortality_modified$MS.p_x)))
  n_years <- max_age - x
  
  # Calculate survival probabilities (kpx)
  if(gender == "M" & s_status == "S") {
    MS.kpx <- numeric(n_years + 1)
    MS.kpx[1] <- 1 # Initial survival probability is 1
    for (i in 2:length(MS.kpx)) {
      MS.kpx[i] <- MS.kpx[i-1] * mortality_modified$MS.p_x[x + i - 2] # Corrected to use actual survival rates
    }
  } else if(gender == "F" & s_status == "S") {
    FS.kpx <- numeric(n_years + 1)
    FS.kpx[1] <- 1 # Initial survival probability is 1
    for (i in 2:length(FS.kpx)) {
      FS.kpx[i] <- FS.kpx[i-1] * mortality_modified$FS.p_x[x + i - 2] # Corrected to use actual survival rates
    } 
    } else if(gender == "M" & s_status == "NS"){
      MNS.kpx <- numeric(n_years + 1)
      MNS.kpx[1] <- 1 # Initial survival probability is 1
      for (i in 2:length(MNS.kpx)) {
        MNS.kpx[i] <- MNS.kpx[i-1] * mortality_modified$MNS.p_x[x + i - 2] # Corrected to use actual survival rates
      }
    }else if(gender == "F" & s_status == "NS") {
      FNS.kpx <- numeric(n_years + 1)
      FNS.kpx[1] <- 1 # Initial survival probability is 1
      for (i in 2:length(FNS.kpx)) {
        FNS.kpx[i] <- FNS.kpx[i-1] * mortality_modified$FNS.p_x[x + i - 2] # Corrected to use actual survival rates
      } 
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
  for(i in 1:n_years) { if (gender == "M" & s_status == "S") {
    value[i] <- MS.kpx[i] * mortality_modified$MS[x+i-1] * prod(v[1:i])
  } else if (gender == "F" & s_status == "S") {
    value[i] <- FS.kpx[i] * mortality_modified$FS[x+i-1] * prod(v[1:i])
  } else if (gender == "M" & s_status == "NS") {
    value[i] <- MNS.kpx[i] * mortality_modified$MNS[x+i-1] * prod(v[1:i])
  } else if (gender == "F" & s_status == "NS") {
    value[i] <- FNS.kpx[i] * mortality_modified$FNS[x+i-1] * prod(v[1:i])
  }
  }
  
  final_value <- face_value*sum(value)
  return(final_value)
}


whole_life(54,2001, 1000000, 'F', 'S')



# Finding the Premiums for the SPWL Dataset -------------------------------
premiums <- mapply(whole_life, spwl$Issue.age, spwl$Issue.year, spwl$Face.amount, spwl$Sex, spwl$Smoker.Status)

spwl$prem_at_issue_year <- premiums


# Discount/Accumulate to 2004 ---------------------------------------------
adjustment_factor <- function(issueYear, prem_wli) {
  if (issueYear > 2004) {
    # Policy issued after 2004: Calculate discount factor
    years <- 2004:(issueYear - 1)
    rates <- eco_data$X1.yr.Risk.Free.Annual.Spot.Rate[eco_data$Year %in% years]
    factor <- prod(1 / (1 + rates))
  } else if (issueYear < 2004) {
    # Policy issued before 2004: Calculate accumulation factor
    years <- issueYear:2003
    rates <- eco_data$X1.yr.Risk.Free.Annual.Spot.Rate[eco_data$Year %in% years]
    factor <- prod(1 + rates)
  } else {
    # Issue year is 2004, no adjustment needed
    factor <- 1
  }
  value <- prem_wli*factor
  return(value)
}

premiums_at2004 <- mapply(adjustment_factor, spwl$Issue.year, spwl$prem_at_issue_year)

spwl$prem_at_2004 <- premiums_at2004

# Discount/Accumulate to 2023 ---------------------------------------------
adjustment_factor2 <- function(issueYear, prem_wli) {
  if (issueYear > 2023) {
    # Policy issued after 2023: Calculate discount factor
    years <- 2023:(issueYear - 1)
    rates <- eco_data$X1.yr.Risk.Free.Annual.Spot.Rate[eco_data$Year %in% years]
    factor <- prod(1 / (1 + rates))
  } else if (issueYear < 2023) {
    # Policy issued before 2023: Calculate accumulation factor
    years <- issueYear:2023
    rates <- eco_data$X1.yr.Risk.Free.Annual.Spot.Rate[eco_data$Year %in% years]
    factor <- prod(1 + rates)
  } else {
    # Issue year is 2023, no adjustment needed
    factor <- 1
  }
  value2 <- prem_wli*factor
  return(value2)
}


premiums_at2023 <- mapply(adjustment_factor2, spwl$Issue.year, spwl$prem_at_issue_year)

spwl$prem_at_2023 <- premiums_at2023

# Exporting the premiums for the SPWL -------------------------------------
write_csv(spwl, "spwl_with_incentivepremiums.csv")


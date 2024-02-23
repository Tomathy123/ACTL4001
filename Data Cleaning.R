# Setting Up --------------------------------------------------------------
cat("\014") # Clear console
rm(list=ls()) # Clear Environment

setwd("/Users/arneetkalra/Desktop/UNSW Onedrive/UNI/2024/ACTL4001/Assignment/Data") #change to your own
# setwd("/Users/karandeshwal/Documents/R/ACTL4001")

# install.packages('tidyverse')
# install.packages('dplyr')
# install.packages('readxl')
# install.packages('openxlsx')

library(tidyverse)
library(dplyr)
library(readxl)
library(openxlsx)

inforce_data <- read.csv("2024-srcsc-superlife-inforce-dataset.csv", header = TRUE)
intervention_data <- read_excel("srcsc-2024-interventions.xlsx")
eco_data <- read_excel("srcsc-2024-lumaria-economic-data.xlsx")
mortality_data <- read_excel("srcsc-2024-lumaria-mortality-table.xlsx")

### Clean Inforce Data
# Remake the header 
names(inforce_data) <- inforce_data[3,]
# Remove the first 2 rows of catastrophe data
inforce_data <- inforce_data[-c(1:3),]

### Clean Intervention Data
# Remake the header 
names(intervention_data) <- intervention_data[7,]
# Remove the first 2 rows of catastrophe data
intervention_data <- intervention_data[-c(1:7),]

### Clean Eco Data
# Remake the header 
names(eco_data) <- eco_data[5,]
# Remove the first 2 rows of catastrophe data
eco_data <- eco_data[-c(1:5),]
# Remove columns 6-7
eco_data <- eco_data[,-c(6:7)]

### Clean Mortality Data
# Remake the header 
names(mortality_data) <- mortality_data[7,]
# Remove the first 2 rows of catastrophe data
mortality_data <- mortality_data[-c(1:7),]
# Remove columns 3-8
mortality_data <- mortality_data[,-c(3:8)]

### Converting all characters to numbers in Economics Data
eco_data$Year <- as.numeric(eco_data$Year)
eco_data$Inflation <- as.numeric(eco_data$Inflation)
eco_data$`Government of Lumaria Overnight Rate` <- as.numeric(eco_data$`Government of Lumaria Overnight Rate`)
eco_data$`1-yr Risk Free Annual Spot Rate` <- as.numeric(eco_data$`1-yr Risk Free Annual Spot Rate`)
eco_data$`10-yr Risk Free Annual Spot Rate` <- as.numeric(eco_data$`10-yr Risk Free Annual Spot Rate`)

### Converting all characters to numbers in Inforce Data
inforce_data$Issue.year <- as.numeric(inforce_data$Issue.year)
inforce_data$Issue.age <- as.numeric(inforce_data$Issue.age)
inforce_data$Face.amount <- as.numeric(inforce_data$Face.amount)
inforce_data$Region <- as.numeric(inforce_data$Region)
inforce_data$Death.indicator <- as.numeric(inforce_data$Death.indicator)
inforce_data$Year.of.Death <- as.numeric(inforce_data$Year.of.Death)
inforce_data$Lapse.Indicator <- as.numeric(inforce_data$Lapse.Indicator)
inforce_data$Year.of.Lapse <- as.numeric(inforce_data$Year.of.Lapse)

### Converting all characters to numbers in Mortality Data
mortality_data$Age <- as.numeric(mortality_data$Age)
mortality_data$`Mortality Rate` <- as.numeric(mortality_data$`Mortality Rate`)

### Exporting in an excel file
write.xlsx(eco_data, "economy_data.xlsx")
write.xlsx(inforce_data, "inforce_data.xlsx")
write.xlsx(mortality_data, "mortality_data.xlsx")
write.xlsx(intervention_data, "intervention_data.xlsx")

write_csv(intervention_data, "intervention_data.csv")
write_csv(eco_data, "economy_data.csv")
write_csv(inforce_data, "inforce_data.csv")
write_csv(mortality_data, "mortality_data.csv")



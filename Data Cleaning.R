setwd("/Users/arneetkalra/Desktop/UNSW Onedrive/UNI/2024/ACTL4001/Assignment/Data")

library(tidyverse)
library(dplyr)
library(readxl)

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




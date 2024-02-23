# Setting Up --------------------------------------------------------------
cat("\014") # Clear console
rm(list=ls()) # Clear Environment

setwd("/Users/arneetkalra/Desktop/UNSW Onedrive/UNI/2024/ACTL4001/Assignment/Data")
# setwd("/Users/karandeshwal/Documents/R/ACTL4001")

# install.packages('tidyverse')
# install.packages('dplyr')
# install.packages('readxl')
# install.packages('openxlsx')

library(tidyverse)
library(dplyr)
library(readxl)
library(tidyr)
library(reshape2)
library(ggplot2)
library(glmnet)
library(gmodels)
library(zoo) # reading time datatype
library(corrplot) # correlation plots

# Read in the Data
inforce_data <- read.csv("inforce_data.csv", header = TRUE)
intervention_data <- read.csv("intervention_data.csv", header = TRUE)
eco_data <- read.csv("economy_data.csv", header = TRUE)
mortality_data <- read.csv("mortality_data.csv", header = TRUE)

# Number of Smokers by Age and Urban/Rural
smokers <- inforce_data %>%
  group_by(Issue.age, Urban.vs.Rural) %>%
  summarise(Total_Smokers = sum(Smoker.Status=="S"))

ggplot(smokers, aes(Issue.age, Total_Smokers, fill = Urban.vs.Rural)) +
  geom_bar(stat = "identity", position = "dodge")

# Economy Data
ggplot(eco_data, aes(x = Year)) + 
  geom_line(aes(y = Inflation, color = "Inflation")) + 
  geom_line(aes(y = Government.of.Lumaria.Overnight.Rate, color = "Govt Overnight Rate")) + 
  geom_line(aes(y = X1.yr.Risk.Free.Annual.Spot.Rate, color = "1-yr Risk Free Rate")) + 
  geom_line(aes(y = X10.yr.Risk.Free.Annual.Spot.Rate, color = "10-yr Risk Free Rate")) +
  scale_color_manual(values = c("Inflation" = "orange", 
                                "Govt Overnight Rate" = "blue",
                                "1-yr Risk Free Rate" = "green",
                                "10-yr Risk Free Rate" = "red")) +
  labs(color = "Variable") +
  theme_minimal()

# Cohort Analysis - Issue Year


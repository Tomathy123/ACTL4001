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

dead_data <- inforce_data[!is.na(inforce_data$Death.indicator), ]
dead_data <- dead_data %>%
  mutate(Age.at.death = Year.of.Death - Issue.year + Issue.age)
alive_data <- inforce_data[is.na(inforce_data$Death.indicator), ]

# Number of Smokers by Age and Urban/Rural
smokers_byage <- inforce_data %>%
  group_by(Issue.age, Urban.vs.Rural) %>%
  summarise(Total_Smokers = sum(Smoker.Status=="S"))

ggplot(smokers_byage, aes(Issue.age, Total_Smokers, fill = Urban.vs.Rural)) +
  geom_bar(stat = "identity", position = "dodge")


# Cohort Smoker Analysis - Issue Year
smokers_bycohort <- inforce_data %>%
  group_by(Issue.year, Urban.vs.Rural) %>%
  summarise(Total_Smokers = sum(Smoker.Status=="S"))

ggplot(smokers_bycohort, aes(Issue.year, Total_Smokers, fill = Urban.vs.Rural)) + 
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


# Number of death by cohort
# inforce_data$Death.indicator <- as.character(inforce_data$Death.indicator)
# 
# inforce_data$Dead_new <- ifelse(inforce_data$Death.indicator == 1, "dead", "alive")
# 
# death_bycohort <- inforce_data %>%
#   group_by(Issue.age, Urban.vs.Rural) %>%
#   summarise(Total_Death = sum(Death.indicator=="dead"))



# Count of Urban and Rural 
count <- inforce_data %>%
  group_by(Issue.age) %>%
  summarise(Total_urban = sum(Urban.vs.Rural=="Urban"), Total_rural = sum(Urban.vs.Rural=="Rural"))

count2 <- inforce_data %>%
  summarise(Total_urban = sum(Urban.vs.Rural=="Urban"), Total_rural = sum(Urban.vs.Rural=="Rural"))

# Survival Curve
ggplot(mortality_data, aes(x = Age, y = 1 - Mortality.Rate)) +
  geom_line(color = "red") +
  labs(title = "Survival Curve", x = "Age", y = "Survival Rate")

# Policy amount distribution
hist(alive_data$Face.amount)

# Smoking vs non_smoking death age histogram
ggplot(dead_data, aes(x = Age.at.death, fill = Smoker.Status)) +
  geom_density(alpha = 0.5) +
  labs(title = "Death Age Distribution of Smokers and Non-Smokers",
       x = "Death Age",
       y = "Density",
       fill = "Smoker Status") +
  scale_fill_manual(values = c("blue", "red"))+
  theme_minimal()



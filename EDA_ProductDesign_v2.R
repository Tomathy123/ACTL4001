# Setting Up --------------------------------------------------------------
cat("\014") # Clear console
rm(list=ls()) # Clear Environment

# setwd("/Users/arneetkalra/Desktop/UNSW Onedrive/UNI/2024/ACTL4001/Assignment/Data")
setwd("/Users/karandeshwal/Documents/R/ACTL4001")

# install.packages('tidyverse')
# install.packages('dplyr')
# install.packages('readxl')
# install.packages('openxlsx')

library(tidyverse)
library(dplyr)
library(scales)
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

inforce_data_death <- inforce_data %>% mutate(Age.at.death = Year.of.Death - Issue.year + Issue.age)

inforce_data_death$Death.indicator[is.na(inforce_data_death$Death.indicator)] <- 0

# Splitting data by Sex
inforce_data_death_F <- inforce_data_death %>% filter(Sex == "F")
inforce_data_death_M <- inforce_data_death %>% filter(Sex == "M")

alive_data_F <- alive_data %>% filter(Sex == "F")
alive_data_M <- alive_data %>% filter(Sex == "M")

dead_data_F <- dead_data %>% filter(Sex == "F")
dead_data_M <- dead_data %>% filter(Sex == "M")


#### Product Design ####

# Smoking Cessation - show that the death rate of smokers is very high in comparison to non smokers
#                   - Could also show that most of the smokers have an underwriting class of moderate/ high

# Distribution of Death Data by age and sex

# All
ggplot(dead_data, aes(x = Age.at.death, fill = Smoker.Status)) +
  geom_density(alpha = 0.5) +
  labs(title = "Death Age Distribution of Smokers and Non-Smokers",
       x = "Death Age",
       y = "Density",
       fill = "Smoker Status") +
  scale_fill_manual(values = c("blue", "red"))+
  theme_minimal() + scale_y_continuous(labels = scales::percent_format(scale = 100))

# Female 
ggplot(dead_data_F, aes(x = Age.at.death, fill = Smoker.Status)) +
  geom_density(alpha = 0.5) +
  labs(title = "Death Age Distribution of Smokers and Non-Smokers Female",
       x = "Death Age",
       y = "Density",
       fill = "Smoker Status") +
  scale_fill_manual(values = c("blue", "red"))+
  theme_minimal() + scale_y_continuous(labels = scales::percent_format(scale = 100))

# Male
ggplot(dead_data_M, aes(x = Age.at.death, fill = Smoker.Status)) +
  geom_density(alpha = 0.5) +
  labs(title = "Death Age Distribution of Smokers and Non-Smokers Male",
       x = "Death Age",
       y = "Density",
       fill = "Smoker Status") +
  scale_fill_manual(values = c("blue", "red"))+
  theme_minimal() + scale_y_continuous(labels = scales::percent_format(scale = 100))

# Death Rates by Region

# All
death_rate_SNS <- inforce_data_death %>% group_by(Region,Smoker.Status) %>%
  summarise(death_rate = mean(Death.indicator))

ggplot(death_rate_SNS, aes(Region, death_rate, fill = Smoker.Status)) +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_y_continuous(labels = scales::percent_format(scale = 100)) + theme_minimal() + 
      labs(title = "Death Rates of Smokers and Non-Smokers by Region",
       x = "Region",
       y = "Death Rate",
       fill = "Smoker Status") + geom_text(aes(label = paste0(round(death_rate*100,0),"%")),
                                           hjust = 1,vjust = -0.3, size=3)

# Female
death_rate_SNS_F <- inforce_data_death_F %>% group_by(Region,Smoker.Status) %>%
  summarise(death_rate = mean(Death.indicator))

ggplot(death_rate_SNS_F, aes(Region, death_rate, fill = Smoker.Status)) +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_y_continuous(labels = scales::percent_format(scale = 100)) + theme_minimal() + 
  labs(title = "Death Rates of Smokers and Non-Smokers by Region Female",
       x = "Region",
       y = "Death Rate",
       fill = "Smoker Status") + geom_text(aes(label = paste0(round(death_rate*100,0),"%")),
                                           hjust = 1,vjust = -0.3, size=3)

# Male
death_rate_SNS_M <- inforce_data_death_M %>% group_by(Region,Smoker.Status) %>%
  summarise(death_rate = mean(Death.indicator))

ggplot(death_rate_SNS_M, aes(Region, death_rate, fill = Smoker.Status)) +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_y_continuous(labels = scales::percent_format(scale = 100)) + theme_minimal() + 
  labs(title = "Death Rates of Smokers and Non-Smokers by Region Male",
       x = "Region",
       y = "Death Rate",
       fill = "Smoker Status") + geom_text(aes(label = paste0(round(death_rate*100,0),"%")),
                                           hjust = 1,vjust = -0.3, size=3)

# Distribution of underwriting class by smokers and non smokers

# All
Underwriting_SNS_counts <- inforce_data_death %>% group_by(Smoker.Status) %>%
  summarise(total_count= n()) %>% ungroup()

underwriting_prop <- inforce_data_death %>% group_by(Underwriting.Class, Smoker.Status) %>%
  summarise(count =n()) %>% ungroup() %>% left_join(Underwriting_SNS_counts, by = "Smoker.Status") %>%
  mutate(proportion = count/total_count)

underwriting_prop$Underwriting.Class <- factor(underwriting_prop$Underwriting.Class,
                                               levels = c("very low risk", "low risk",
                                                          "moderate risk", "high risk"))

ggplot(underwriting_prop, aes(Underwriting.Class, proportion, fill = Smoker.Status)) +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_y_continuous(labels = scales::percent_format(scale = 100)) + theme_minimal() + 
  labs(title = "Underwriting Class Distribution by Smoking Status",
       x = "Underwriting Class",
       y = "Proportions of Smokers/ Non Smokers",
       fill = "Smoker Status") + geom_text(aes(label = paste0(round(proportion*100,0),"%")),
                                              vjust = -0.3, size=3)

# Female

Underwriting_SNS_counts_F <- inforce_data_death_F %>% group_by(Smoker.Status) %>%
  summarise(total_count= n()) %>% ungroup()

underwriting_prop_F <- inforce_data_death_F %>% group_by(Underwriting.Class, Smoker.Status) %>%
  summarise(count =n()) %>% ungroup() %>% left_join(Underwriting_SNS_counts_F, by = "Smoker.Status") %>%
  mutate(proportion = count/total_count)

underwriting_prop_F$Underwriting.Class <- factor(underwriting_prop_F$Underwriting.Class,
                                               levels = c("very low risk", "low risk",
                                                          "moderate risk", "high risk"))

ggplot(underwriting_prop_F, aes(Underwriting.Class, proportion, fill = Smoker.Status)) +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_y_continuous(labels = scales::percent_format(scale = 100)) + theme_minimal() + 
  labs(title = "Underwriting Class Distribution by Smoking Status Female",
       x = "Underwriting Class",
       y = "Proportions of Smokers/ Non Smokers",
       fill = "Smoker Status") + geom_text(aes(label = paste0(round(proportion*100,0),"%")),
                                           vjust = -0.3, size=3)

# Male
Underwriting_SNS_counts_M <- inforce_data_death_M %>% group_by(Smoker.Status) %>%
  summarise(total_count= n()) %>% ungroup()

underwriting_prop_M <- inforce_data_death_M %>% group_by(Underwriting.Class, Smoker.Status) %>%
  summarise(count =n()) %>% ungroup() %>% left_join(Underwriting_SNS_counts_M, by = "Smoker.Status") %>%
  mutate(proportion = count/total_count)

underwriting_prop_M$Underwriting.Class <- factor(underwriting_prop_M$Underwriting.Class,
                                                 levels = c("very low risk", "low risk",
                                                            "moderate risk", "high risk"))

ggplot(underwriting_prop_M, aes(Underwriting.Class, proportion, fill = Smoker.Status)) +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_y_continuous(labels = scales::percent_format(scale = 100)) + theme_minimal() + 
  labs(title = "Underwriting Class Distribution by Smoking Status Male",
       x = "Underwriting Class",
       y = "Proportions of Smokers/ Non Smokers",
       fill = "Smoker Status") + geom_text(aes(label = paste0(round(proportion*100,0),"%")),
                                           vjust = -0.3, size=3)


# Active Ageing - show the distribution by issue and that most participants are 35-55 (i.e. securing their future)

# distribution by issue age

# All
ggplot(data = inforce_data_death, aes(x = Issue.age)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +  # Adjust binwidth as needed
  labs(title = "Distribution of Issue Age",
       x = "Issue Age",
       y = "Frequency") + theme_minimal()

# Female
ggplot(data = inforce_data_death_F, aes(x = Issue.age)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +  # Adjust binwidth as needed
  labs(title = "Distribution of Issue Age Female",
       x = "Issue Age",
       y = "Frequency")

# Male
ggplot(data = inforce_data_death_M, aes(x = Issue.age)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +  # Adjust binwidth as needed
  labs(title = "Distribution of Issue Age Male",
       x = "Issue Age",
       y = "Frequency")


# Causes of Death for people aged 55 and over

Death_over55 <- dead_data %>% filter(Age.at.death >= "55") %>% group_by(Cause.of.Death) %>%
  summarise(count= n()) %>% mutate(proportion = count/sum(count))

ggplot(Death_over55, aes(Cause.of.Death, proportion)) +
  geom_bar(stat = "identity", position = "dodge", fill = "skyblue", color = "black") + 
  scale_y_continuous(labels = scales::percent_format(scale = 100)) + theme_minimal() + 
  labs(title = "Causes of Death for Participants Aged 55 and over",
       x = "Cause of Death",
       y = "Proportion of Deaths")


# Healthcare - The most prevalent reasons for mortality (respiratory, cancer and another thing)

# All Smokers
Causes_Death_S <- dead_data  %>%  filter(Smoker.Status == "S") %>% group_by(Cause.of.Death) %>%
summarise(count = n()) %>% arrange(desc(count)) %>% mutate(proportion = count/sum(count))

ggplot(data = Causes_Death_S, aes(Cause.of.Death, proportion)) +
  geom_bar(stat = "identity", position = "dodge", fill = "skyblue", color = "black") +  # Adjust binwidth as needed
  labs(title = "Distribution of Causes of Death All Smokers",
       x = "Causes of Death",
       y = "Proportion of Deaths") + theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(scale = 100))

# Male Smokers
Causes_Death_MS <- dead_data_M  %>%  filter(Smoker.Status == "S") %>% group_by(Cause.of.Death) %>%
  summarise(count = n()) %>% arrange(desc(count))

ggplot(data = Causes_Death_MS, aes(Cause.of.Death, count)) +
  geom_bar(stat = "identity", position = "dodge") +  # Adjust binwidth as needed
  labs(title = "Distribution of Causes of Death Male Smokers",
       x = "Causes of Death",
       y = "Frequency") + theme_minimal()

# Female Smokers
Causes_Death_FS <- dead_data_F  %>%  filter(Smoker.Status == "S") %>% group_by(Cause.of.Death) %>%
  summarise(count = n()) %>% arrange(desc(count))

ggplot(data = Causes_Death_FS, aes(Cause.of.Death, count)) +
  geom_bar(stat = "identity", position = "dodge") +  # Adjust binwidth as needed
  labs(title = "Distribution of Causes of Death Female Smokers",
       x = "Causes of Death",
       y = "Frequency") + theme_minimal()


# All Non-Smokers
Causes_Death_NS <- dead_data  %>%  filter(Smoker.Status == "NS") %>% group_by(Cause.of.Death) %>%
  summarise(count = n()) %>% arrange(desc(count)) %>% mutate(proportion = count/sum(count))

ggplot(data = Causes_Death_NS, aes(Cause.of.Death, proportion)) +
  geom_bar(stat = "identity", position = "dodge", fill = "skyblue", color = "black") +  # Adjust binwidth as needed
  labs(title = "Distribution of Causes of Death All Non-Smokers",
       x = "Causes of Death",
       y = "Proportion of Deaths") + theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(scale = 100))

# Male Non-Smokers
Causes_Death_MNS <- dead_data_M  %>%  filter(Smoker.Status == "NS") %>% group_by(Cause.of.Death) %>%
  summarise(count = n()) %>% arrange(desc(count))

ggplot(data = Causes_Death_MNS, aes(Cause.of.Death, count)) +
  geom_bar(stat = "identity", position = "dodge") +  # Adjust binwidth as needed
  labs(title = "Distribution of Causes of Death Male Non-Smokers",
       x = "Causes of Death",
       y = "Frequency") + theme_minimal()

# Female Non-Smokers
Causes_Death_FNS <- dead_data_F  %>%  filter(Smoker.Status == "NS") %>% group_by(Cause.of.Death) %>%
  summarise(count = n()) %>% arrange(desc(count))

ggplot(data = Causes_Death_FNS, aes(Cause.of.Death, count)) +
  geom_bar(stat = "identity", position = "dodge") +  # Adjust binwidth as needed
  labs(title = "Distribution of Causes of Death Female Non-Smokers",
       x = "Causes of Death",
       y = "Frequency") + theme_minimal()

# Fitness App - Respiratory and that the 2/3 of the population is urban (assuming that they are more technologically advanced)

Rur_Urb_Region <- inforce_data_death %>% group_by(Region, Urban.vs.Rural) %>% summarise(count = n())
Rur_Urb_Region <- Rur_Urb_Region %>% mutate(proportion = count/sum(count))

ggplot(Rur_Urb_Region, aes(Region, proportion, fill = Urban.vs.Rural)) +
  geom_bar(stat = "identity", position = "dodge") + theme_minimal() + 
  scale_y_continuous(labels = scales::percent_format(scale = 100)) + theme_minimal() +
  labs(title = "Proportion of Rural vs Urban P/H by Region",
       x = "Region",
       y = "Proportion",
       fill = "Urban vs Rural") + geom_text(aes(label = paste0(round(proportion*100,0),"%")),
                                           hjust = 1,vjust = -0.3, size=3)



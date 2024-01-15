# Load necessary libraries
library(ggplot2)
library(readxl)
library(openintro)

#Read Data
SamsungAdAnalysis <- read_excel("C:/Users/tirth/Downloads/SamsungAdAnalysis.xlsx")

# Summary statistics
summary(SamsungAdAnalysis)

# Store the data in the variable data
data <- SamsungAdAnalysis
data


#Sample t-test 
t.test(data$SalesAd2 , mu = 55000, alternative = "greater")
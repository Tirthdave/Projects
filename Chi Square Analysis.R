#Load Library
library(dplyr)
library(psych)
library(readxl)
library(ggplot2)

# Read the data from the Excel file
data <- read_excel("C:/Users/tirth/OneDrive/Desktop/Durham/1204-Statistical Predictive Modeling 1/Assignment/4/carsdatabase.xlsx")

# Display the structure of the dataset
describe (data)
str(data)

# Check for missing values
summary(data)



# Perform the Chi-squared test
chi_squared_result <- chisq.test(carsdatabase$Passengers, carsdatabase$AirBags)

# Display the test results
print(chi_squared_result)

# Extract p-value from the test results
p_value <- chi_squared_result$p.value

# Display the p-value
print(p_value)

# Interpret the results
if (p_value < 0.05) {
  cat("There is a significant association between the number of passengers in a car and the type of airbags")
} else {
  cat("There is no association between the number of passengers in a car and the type of airbags.")
}


# Create a bar plot
ggplot(car_data, aes(x = Passengers, fill = AirBags)) +
  geom_bar(position = "dodge", stat = "count") +
  labs(title = "Association between Passengers and AirBags", x = "Passengers", y = "Count")

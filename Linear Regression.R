#Read Data
Expenses <- read.csv("C:/Users/tirth/OneDrive/Desktop/Durham/1204-Statistical Predictive Modeling 1/Assignment/5/Expenses.csv")
View(Expenses)

# Fit a Simple Linear Regression model
model <- lm(expenses ~ sex, data = Expenses)
LinearModel<-model

summary_stats <- summary(Expenses)
print("Summary Statistics:")
print(summary_stats)


# Print a summary of the model
summary(model)

library(ggplot2)
ggplot(data = Expenses, aes(x = sex, y = expenses)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Regression Line of Expenses by Sex")


coef_test <- coef(summary(model))
p_value <- coef_test["sexmale", "Pr(>|t|)"]

# Display the p-value
cat("P-value for sexmale:", p_value)

# Check if we reject the null hypothesis
alpha <- 0.05  # chosen significance level
if (p_value < alpha) {
  cat("Reject the null hypothesis. There is a significant relationship between sex and expenses.\n")
} else {
  cat("Fail to reject the null hypothesis. There is no significant relationship between sex and expenses.\n")
}







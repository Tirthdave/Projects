library(readxl)
library(ggplot2)
library(MASS)


data <- read_excel("C:/Users/tirth/Downloads/Stock_MLRAnalysis.xlsx")
View(Stock_MLRAnalysis)

# Display basic statistics
summary(data)

# Create a histogram of the dependent variable
ggplot(data, aes(x = stock_return_scaled)) +
  geom_histogram(
    binwidth = 50,
    fill = "blue",
    color = "black",
    alpha = 0.7,
    boundary = 0.2
  ) +
  labs(
    title = "Distribution of Scaled Stock Returns",
    x = "Stock Return (Scaled)",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "none"
  ) +
  geom_vline(
    xintercept = mean(data$stock_return_scaled),
    linetype = "dashed",
    color = "red",
    size = 1
  ) +
  annotate(
    "text",
    x = mean(data$stock_return_scaled) + 50,
    y = max(hist(data$stock_return_scaled)$counts) - 5,
    label = paste("Mean =", round(mean(data$stock_return_scaled), 2)),
    color = "red",
    size = 4
  )

# T-test for the mean of stock_return_scaled
t_test_result <- t.test(data$stock_return_scaled, mu = 300)
t_test_result

# Simple linear regression using dividend
slr <- lm(stock_return_scaled ~ dividend, data = data)
summary(slr)

# Multiple linear regression
mlr <- lm(stock_return_scaled ~ return + market_overview + dividend +
                 earnings_ranking + debt_to_equity + marketcap, data = data)
summary(mlr)
# Load necessary libraries
library(ggplot2)
library(readxl)
library(openintro)

#Read Data
CholestrolLevel <- read_excel("C:/Users/tirth/Downloads/CholestrolLevel.xlsx")
View(CholestrolLevel)

# Store the data in the variable my_data
my_data <- CholestrolLevel
my_data

# QQPlot
qqnorm(my_data$After, pch = 1, xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")
qqline(my_data$After, col = "steelblue", lwd = 2)

# Create a histogram with a normal curve
histNC <- ggplot(my_data, aes(x=After)) +
  labs(title="Histogram of Cholesterol Levels with Normal Curve",
       x="Cholesterol Level") +
  theme(plot.title = element_text(hjust=0.5)) +
  xlab('Cholesterol Level')

histNC <- histNC + geom_histogram(binwidth=0.5, colour="black",
                                  aes(y=..density.., fill=..count..)) +
  scale_fill_gradient("Count", low="light blue", high="blue") +
  stat_function(fun=dnorm,
                color="red",
                args=list(mean=mean(my_data$After),
                          sd=sd(my_data$After)))
histNC


#Shapiro-Wilk Test
shapiro.test <- shapiro.test(my_data$After)
print(shapiro.test)

# Interpret the Shapiro-Wilk test result
if (shapiro.test$p.value < 0.05) 
{
  cat("The data is not normally distributed (p < 0.05).\n")
} else 
{
  cat("The data appears to be normally distributed (p >= 0.05).\n")
}

# View Dataset
View(CholestrolLevel)

#Veiw first few lines of dataset
head(CholestrolLevel)

#View key information of dataset
str(CholestrolLevel)

#Mean
mu <- mean(CholestrolLevel$After)
print(mu)

#Standard Deviation
sd <- sd(CholestrolLevel$After)
print(sd)


#Calculate z parameters
mu0 <- 5.95 # Specify the mean
alpha <- 0.05 # Specify the significance level
sigma <- 0.95 # population standard deviation
n <- nrow(CholestrolLevel) # get the sample size
n

#Calculate z (t-test)
z<-(mu-mu0)/(sigma/sqrt(n))
z

#Calculate p-value
p_value <- 2*pnorm(abs(z),lower.tail=FALSE) # p-values (we multiple by 2 since it’s a two-side test)
p_value

# Interpret the Hypothesis test result
if (p_value < 0.05) 
{
  cat("Reject the null hypothesis\n")
} else 
{
  cat("Fail to reject the null hypothesis.\n")
}

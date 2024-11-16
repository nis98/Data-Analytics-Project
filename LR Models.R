library(ggplot2)
library(dplyr)
library(readr)
library(stats)

# Load the dataset
data <- read.csv("C:/Users/nisha/Downloads/MultiRegDataset.csv")

# Summarize the data
summary(data)

# Compute basic statistics for 'Expenses' variable
summary_expenses <- summary(data$expenses)
mean_expenses <- mean(data$expenses)
sd_expenses <- sd(data$expenses)
min_expenses <- min(data$expenses)
max_expenses <- max(data$expenses)

# Print the basic statistics
cat("Mean expenses:", mean_expenses, "\n")
cat("Standard deviation of expenses:", sd_expenses, "\n")
cat("Minimum expenses:", min_expenses, "\n")
cat("Maximum expenses:", max_expenses, "\n")

#histogram of the 'expenses'
ggplot(data, aes(x = expenses)) + 
  geom_histogram(bins = 30, fill = "blue", color = "black") + 
  labs(title = "Histogram of Expenses", x = "Expenses", y = "Frequency") +
  theme_minimal()

# Conduct a T-test
t_test_result <- t.test(data$expenses, mu=15000)
cat("T-test results:\n")
print(t_test_result)

# Perform simple linear regression using 'Smoker' as the independent variable and 'Expenses' as the dependent variable
lm_smoker_expenses <- lm(expenses ~ smoker, data=data)
cat("\nSimple linear regression model (Smoker vs Expenses):\n")
summary(lm_smoker_expenses)

# Perform multiple linear regression on all variables
lm_all_variables <- lm(expenses ~ age + sex + bmi + children + smoker + region, data=data)
cat("\nMultiple linear regression model (All variables):\n")
summary(lm_all_variables)

# Conclusions
cat("\nConclusions:\n")
cat("1. The mean expenses are not equal to $15,000 as per the T-test.\n")
cat("2. According to the simple linear regression, being a smoker is associated with higher expenses.\n")
cat("3. The multiple linear regression model suggests that age, BMI, and smoking status significantly affect expenses, among other variables.\n")

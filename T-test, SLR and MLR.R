# Load Libraries
library(dplyr)
library(ggplot2)
library(readxl)
ols_stock <- read_excel("C:/Users/nisha/Downloads/ols_stock.xlsx")
View(ols_stock)

# Dataset Review
df = ols_stock
print(head(df))
str(df)

# Initial Plot
ggplot(df, aes(x = earnings_ranking, y = stock_return_scaled)) +
  geom_point(colour = "red") + geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Original Model",
       x = "Earnings Ranking",
       y = "Stock Return Scaled")

# Correlation
cor(df$earnings_ranking, df$stock_return_scaled)

# Transformation using Sqrt
cor(sqrt(df$earnings_ranking), sqrt(df$stock_return_scaled))

# Transformation using Log (add 1 to avoid log(0))
cor(log1p(df$earnings_ranking), log1p(df$stock_return_scaled))

# Plot with Line (adjusted) - Sqrt
ggplot(df, aes(x = sqrt(earnings_ranking), y = sqrt(stock_return_scaled))) +
  geom_point(colour = "red") + geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Transformed Model - SQRT",
       x = "Earnings Ranking",
       y = "Stock Return Scaled")

# Plot with Line (adjusted) - Log
ggplot(df, aes(x = log1p(earnings_ranking), y = log1p(stock_return_scaled))) +
  geom_point(colour = "red") + geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Transformed Model - Log",
       x = "Earnings Ranking",
       y = "Stock Return Scaled")

# Linear Model - No Transformation
lmodel <- lm(stock_return_scaled ~ earnings_ranking, data = df)
summary(lmodel)

# Plotting Fitted vs. Residual and QQPlot
par(mfrow = c(1, 2))
plot(fitted(lmodel), resid(lmodel), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Fitted versus Residuals")
abline(h = 0, col = "darkorange", lwd = 2)
qqnorm(resid(lmodel), main = "Normal Q-Q Plot", col = "darkgrey")
qqline(resid(lmodel), col = "dodgerblue", lwd = 2)

# Linear Model - Sqrt Transformation
lmodel_sqrt <- lm(sqrt(stock_return_scaled) ~ sqrt(earnings_ranking), data = df)
summary(lmodel_sqrt)

# Plotting Fitted vs. Residual and QQPlot - Sqrt
par(mfrow = c(1, 2))
plot(fitted(lmodel_sqrt), resid(lmodel_sqrt), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Fitted versus Residuals")
abline(h = 0, col = "darkorange", lwd = 2)
qqnorm(resid(lmodel_sqrt), main = "Normal Q-Q Plot", col = "darkgrey")
qqline(resid(lmodel_sqrt), col = "dodgerblue", lwd = 2)

# Log Transformation
lmodel_log <- lm(log(stock_return_scaled) ~ log(earnings_ranking), data = df)
summary(lmodel_log)

# Plotting Fitted vs. Residual and QQPlot - Log
par(mfrow = c(1, 2))
plot(fitted(lmodel_log), resid(lmodel_log), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Fitted versus Residuals")
abline(h = 0, col = "darkorange", lwd = 2)
qqnorm(resid(lmodel_log), main = "Normal Q-Q Plot", col = "darkgrey")
qqline(resid(lmodel_log), col = "dodgerblue", lwd = 2)

# RMSE Comparison
# Original Model
sqrt(mean(resid(lmodel) ^ 2))

# SQRT Model
sqrt(mean((sqrt(df$stock_return_scaled) - fitted(lmodel_sqrt)) ^ 2))

# Log Model
sqrt(mean((log1p(df$stock_return_scaled) - fitted(lmodel_log)) ^ 2))

# =========================================================
# HOUSE PRICE ANALYSIS IN POLAND USING ARIMA WITH
# ECONOMIC VARIABLES + INTERVENTION ANALYSIS
# =========================================================

# ---------------------------------------------------------
# 1. Install required packages
# ---------------------------------------------------------
# These packages are used for:
# - data manipulation
# - visualization
# - time series forecasting
# - statistical testing

install.packages(c(
  "readr",
  "dplyr",
  "ggplot2",
  "forecast",
  "tseries",
  "zoo",
  "lmtest"
))

# ---------------------------------------------------------
# 2. Load libraries
# ---------------------------------------------------------

library(readr)
library(dplyr)
library(ggplot2)
library(forecast)
library(tseries)
library(zoo)
library(lmtest)

# ---------------------------------------------------------
# 3. Read dataset
# ---------------------------------------------------------
# Load CSV file containing:
# - House prices
# - GDP
# - Inflation
# - Interest rates
# - Dates

data <- read_csv("Testcsv2.csv")

# ---------------------------------------------------------
# 4. Convert Date column into proper Date format
# ---------------------------------------------------------

data$Date <- as.Date(data$Date, format = "%Y-%m-%d")

# ---------------------------------------------------------
# 5. Filter data for years 2014–2023
# ---------------------------------------------------------
# This ensures the analysis only covers the selected period.

data <- filter(
  data,
  Date >= as.Date("2014-01-01") &
    Date <= as.Date("2023-12-31")
)

# ---------------------------------------------------------
# 6. Check for missing or invalid values
# ---------------------------------------------------------
# Stops the script if:
# - NA values exist
# - Infinite/non-finite values exist

if (
  any(is.na(data$Date)) ||
  any(!is.finite(data$HousePrice))
) {
  stop("There are NA or non-finite values in the Date or HousePrice columns.")
}

# ---------------------------------------------------------
# 7. Plot house prices over time
# ---------------------------------------------------------
# Visual inspection of housing market trends.

ggplot(data, aes(x = Date, y = HousePrice)) +
  geom_line() +
  labs(
    title = "House Prices in Poland Over Time",
    x = "Date",
    y = "House Price (per square meter)"
  )

# ---------------------------------------------------------
# 8. Define intervention date
# ---------------------------------------------------------
# Intervention = beginning of war impact period.

intervention_date <- as.Date("2022-01-03")

# ---------------------------------------------------------
# 9. Handle non-positive economic values
# ---------------------------------------------------------
# Logarithms cannot be calculated for:
# - zero
# - negative values
#
# Therefore values <= 0 are replaced with 0.01.

data$GDP[data$GDP <= 0] <- 0.01
data$Inflation[data$Inflation <= 0] <- 0.01

# ---------------------------------------------------------
# 10. Log-transform economic variables
# ---------------------------------------------------------
# Log transformations:
# - stabilize variance
# - reduce skewness
# - improve model behavior

data$LogGDP <- log(data$GDP)
data$LogInflation <- log(data$Inflation)

# +1 added to avoid log(0) for interest rates
data$LogInterestRate <- log(data$InterestRate + 1)

# ---------------------------------------------------------
# 11. Validate transformed variables
# ---------------------------------------------------------

if (
  any(is.na(data$LogGDP)) ||
  any(!is.finite(data$LogGDP)) ||
  any(is.na(data$LogInflation)) ||
  any(!is.finite(data$LogInflation)) ||
  any(is.na(data$LogInterestRate)) ||
  any(!is.finite(data$LogInterestRate))
) {
  stop("There are NA or non-finite values in the economic variables.")
}

# ---------------------------------------------------------
# 12. Create intervention variable
# ---------------------------------------------------------
# Binary variable:
# 0 = before intervention
# 1 = after intervention

data$Intervention <- ifelse(
  data$Date >= intervention_date,
  1,
  0
)

# ---------------------------------------------------------
# 13. Convert house prices into time series format
# ---------------------------------------------------------
# frequency = 4 because data is quarterly.

house_price_ts <- ts(
  data$HousePrice,
  start = c(2014, 1),
  frequency = 4
)

# ---------------------------------------------------------
# 14. Test stationarity using Augmented Dickey-Fuller test
# ---------------------------------------------------------
# ARIMA models generally require stationary data.

adf_result <- adf.test(data$HousePrice)

# ---------------------------------------------------------
# 15. Difference data if non-stationary
# ---------------------------------------------------------
# If p-value > 0.05:
# - data is considered non-stationary
# - first differencing is applied

if (adf_result$p.value > 0.05) {

  # First differencing
  data$HousePrice_diff <- c(
    NA,
    diff(data$HousePrice)
  )

  # Create differenced time series
  house_price_ts <- ts(
    data$HousePrice_diff[-1],
    start = c(2014, 1),
    frequency = 4
  )

  # Remove first row from regressors
  # because differencing removes one observation
  xreg_matrix <- as.matrix(
    data[-1, c(
      "Intervention",
      "LogGDP",
      "LogInflation",
      "LogInterestRate"
    )]
  )

} else {

  # Use original series if stationary
  house_price_ts <- ts(
    data$HousePrice,
    start = c(2014, 1),
    frequency = 4
  )

  xreg_matrix <- as.matrix(
    data[, c(
      "Intervention",
      "LogGDP",
      "LogInflation",
      "LogInterestRate"
    )]
  )
}

# ---------------------------------------------------------
# 16. Print ADF test results
# ---------------------------------------------------------

print(adf_result)

# Individual outputs
adf_result$p.value
adf_result$statistic
adf_result$method

# ---------------------------------------------------------
# 17. Fit ARIMA model with external regressors
# ---------------------------------------------------------
# auto.arima automatically selects:
# - AR terms
# - differencing
# - MA terms

fit_with_econ_vars <- auto.arima(
  house_price_ts,
  xreg = xreg_matrix
)

summary(fit_with_econ_vars)

# ---------------------------------------------------------
# 18. Test coefficient significance
# ---------------------------------------------------------
# Evaluates significance of:
# - GDP
# - inflation
# - interest rates
# - intervention variable

coeftest(fit_with_econ_vars)

# ---------------------------------------------------------
# 19. Residual diagnostics
# ---------------------------------------------------------
# Residuals should ideally:
# - have mean near 0
# - have constant variance
# - resemble white noise

residuals <- residuals(fit_with_econ_vars)

# Mean and variance
mean_residuals <- mean(residuals)
variance_residuals <- var(residuals)

print(paste("Mean of residuals: ", mean_residuals))
print(paste("Variance of residuals: ", variance_residuals))

# ---------------------------------------------------------
# 20. Residual visualization
# ---------------------------------------------------------

# Histogram
hist(
  residuals,
  breaks = 30,
  main = "Histogram of ARIMA Model Residuals",
  xlab = "Residuals",
  col = "lightblue",
  border = "black"
)

# Residual time plot
plot(
  residuals,
  type = "l",
  main = "Residuals of ARIMA Model",
  xlab = "Time",
  ylab = "Residuals"
)

# ---------------------------------------------------------
# 21. Normality diagnostics
# ---------------------------------------------------------

# Shapiro-Wilk test
shapiro.test(residuals)

# QQ plot
qqnorm(residuals)
qqline(residuals)

# ---------------------------------------------------------
# 22. Autocorrelation diagnostics
# ---------------------------------------------------------

# Autocorrelation Function
acf(residuals)

# Partial Autocorrelation Function
pacf(residuals)

# Ljung-Box test for autocorrelation
Box.test(
  residuals,
  lag = 20,
  type = "Ljung-Box"
)

# ---------------------------------------------------------
# 23. Model evaluation metrics
# ---------------------------------------------------------

# Information criteria
AIC(fit_with_econ_vars)
BIC(fit_with_econ_vars)

# Forecast accuracy metrics
accuracy(fit_with_econ_vars)

# ---------------------------------------------------------
# 24. Train-test split for cross-validation
# ---------------------------------------------------------

train_size <- 0.8
n <- nrow(xreg_matrix)

train_index <- 1:round(train_size * n)
test_index <- (max(train_index) + 1):n

# Split target variable
train_data <- data$HousePrice[train_index]
test_data <- data$HousePrice[test_index]

# Split external regressors
train_xreg <- xreg_matrix[train_index, ]
test_xreg <- xreg_matrix[test_index, ]

# ---------------------------------------------------------
# 25. Train ARIMA model on training set
# ---------------------------------------------------------

fit_cv <- auto.arima(
  ts(train_data,
     start = c(2014, 1),
     frequency = 4),
  xreg = train_xreg
)

# ---------------------------------------------------------
# 26. Forecast using testing set regressors
# ---------------------------------------------------------

forecast_cv <- forecast(
  fit_cv,
  xreg = test_xreg
)

# ---------------------------------------------------------
# 27. Evaluate forecasting accuracy
# ---------------------------------------------------------

accuracy(forecast_cv, test_data)

# ---------------------------------------------------------
# 28. Rolling-origin cross-validation
# ---------------------------------------------------------
# More advanced validation approach for time series.
#
# The model is repeatedly retrained on rolling windows
# and tested on future observations.

h <- 4
rolling_window <- 20

# Store forecast errors
errors <- numeric()

for (i in seq(1, nrow(data) - rolling_window - h + 1)) {

  train_data <- data$HousePrice[
    i:(i + rolling_window - 1)
  ]

  test_data <- data$HousePrice[
    (i + rolling_window):
      (i + rolling_window + h - 1)
  ]

  train_xreg <- xreg_matrix[
    i:(i + rolling_window - 1),
  ]

  test_xreg <- xreg_matrix[
    (i + rolling_window):
      (i + rolling_window + h - 1),
  ]

  # Fit rolling ARIMA model
  fit <- auto.arima(
    ts(train_data, frequency = 4),
    xreg = train_xreg
  )

  # Generate forecasts
  forecast_cv <- forecast(
    fit,
    xreg = test_xreg,
    h = h
  )

  # Store MAPE errors
  errors <- c(
    errors,
    accuracy(forecast_cv, test_data)[, "MAPE"]
  )
}

# ---------------------------------------------------------
# 29. Calculate mean forecasting error
# ---------------------------------------------------------

mean_mape <- mean(errors)

print(mean_mape)

# =========================================================
# END OF ANALYSIS
# =========================================================
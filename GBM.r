setwd("C:/Users/ganes/OneDrive/Documents")
#installing packages
install.packages(c("readxl", "car", "lmtest", "sandwich", "moments", "strucchange"))

#loading the library
library(readxl)
library(car)
library(lmtest)
library(sandwich)
library(moments)
library(strucchange)

#importing the data
data_GBM <- read_excel("C:/Users/ganes/OneDrive/Documents/GSK_PLC_FDA.xlsx")

#renaming the columns
names(data_GBM) <- c("Date", "GSK", "UKX", "UKBRBASE", "USDGBP", "EURGBP", 
                     "UKRPCHVJ", "UKGRABIQ", "VIX", "JPYGBP", "AUDGBP")

# Computing the log returns for GSK (Stock) and UKX (Market Index)
data_GBM$GSK_return_GBM <- c(NA, diff(log(data_GBM$GSK)))
data_GBM$UKX_return_GBM <- c(NA, diff(log(data_GBM$UKX)))

# Checking the first few rows with calculated returns
head(data_GBM[, c("Date", "GSK", "UKX", "GSK_return_GBM", "UKX_return_GBM")])

# Fitting the regression model for GSK returns and UKX returns
regression_model_GBM <- lm(GSK_return_GBM ~ UKX_return_GBM, data = data_GBM)

# Viewing the summary of the regression
summary(regression_model_GBM)

# Computing log returns for USDGBP and EURGBP
data_GBM$USDGBP_return_GBM <- c(NA, diff(log(data_GBM$USDGBP)))
data_GBM$EURGBP_return_GBM <- c(NA, diff(log(data_GBM$EURGBP)))

# Checking the first few rows
head(data_GBM[, c("Date", "USDGBP", "EURGBP", "USDGBP_return_GBM", "EURGBP_return_GBM")])

# Updating the regression model to include exchange rate returns
regression_model_updated_GBM <- lm(GSK_return_GBM ~ UKX_return_GBM + USDGBP_return_GBM + EURGBP_return_GBM, data = data_GBM)

# Viewing the summary of the updated regression model
summary(regression_model_updated_GBM)

# Breusch-Pagan test for heteroscedasticity
bptest(regression_model_updated_GBM)

# VIF test for multicollinearity
vif(regression_model_updated_GBM)

# Histogram of residuals
hist(resid(regression_model_updated_GBM), breaks = 30, main = "Histogram of Residuals (GBM)", xlab = "Residuals")

# Jarque test
jarque.test(resid(regression_model_updated_GBM))

# Durbin-Watson test for autocorrelation
dwtest(regression_model_updated_GBM)

# Summary of model recap
GSK_return_GBM ~ UKX_return_GBM + USDGBP_return_GBM + EURGBP_return_GBM

# Load required library for robust covariance matrix
robust_cov_GBM <- vcovHC(regression_model_updated_GBM, type = "HC1")

# Recomputing coefficients with robust standard errors
coeftest(regression_model_updated_GBM, vcov. = robust_cov_GBM)

# Compute Newey-West standard errors
nw_cov_GBM <- NeweyWest(regression_model_updated_GBM)

# Recompute coefficients with Newey-West standard errors
coeftest(regression_model_updated_GBM, vcov. = nw_cov_GBM)

# Breusch-Godfrey test for higher-order autocorrelation
bgtest(regression_model_updated_GBM, order = 3)

# RESET test for functional form
resettest(regression_model_updated_GBM, power = 2:3)

# Converting Date column to Date format
data_GBM$Date <- as.Date(data_GBM$Date)

# Identifying the row index for the breakpoint
breakpoint_GBM <- which(data_GBM$Date == as.Date("2020-01-01"))

# Splitting the data into two subsets around the breakpoint
subset1_GBM <- data_GBM[1:(breakpoint_GBM - 1), ]
subset2_GBM <- data_GBM[breakpoint_GBM:nrow(data_GBM), ]

# Run Chow test
chow_test_GBM <- sctest(GSK_return_GBM ~ UKX_return_GBM + USDGBP_return_GBM + EURGBP_return_GBM, data = data_GBM, type = "Chow", point = breakpoint_GBM)
chow_test_GBM

# Skewness and kurtosis of residuals
residuals_skewness_GBM <- skewness(resid(regression_model_updated_GBM))
residuals_kurtosis_GBM <- kurtosis(resid(regression_model_updated_GBM))

# Printing results
residuals_skewness_GBM
residuals_kurtosis_GBM

# D'Agostino Test
agostino_test_GBM <- agostino.test(resid(regression_model_updated_GBM))

# Anscombe-Glynn Test
anscombe_test_GBM <- anscombe.test(resid(regression_model_updated_GBM))

# Printing the results
agostino_test_GBM
anscombe_test_GBM

# Q-Q plot of residuals
qqnorm(resid(regression_model_updated_GBM), main = "Q-Q Plot of Residuals (GBM)")
qqline(resid(regression_model_updated_GBM), col = "red")

# Adding dummy variable based on condition
data_GBM$Post2020_GBM <- ifelse(data_GBM$Date >= as.Date("2020-01-01"), 1, 0)

# Fitting a model with a dummy variable
regression_model_dummy_GBM <- lm(GSK_return_GBM ~ UKX_return_GBM + USDGBP_return_GBM + EURGBP_return_GBM + Post2020_GBM, data = data_GBM)
summary(regression_model_dummy_GBM)

# CAPM regression: GSK_return against UKX_return (market return)
capm_model_GBM <- lm(GSK_return_GBM ~ UKX_return_GBM, data = data_GBM)

# Summary of the CAPM model
summary(capm_model_GBM)

# Heteroscedasticity using the Breusch-Pagan Test
bptest(capm_model_GBM)

# Evaluating residual autocorrelation using the Durbin-Watson test
dwtest(capm_model_GBM)

# Residual Visualization
plot(data_GBM$UKX_return_GBM, resid(capm_model_GBM), main = "Residuals vs Market Return (GBM)", xlab = "Market Return", ylab = "Residuals")
abline(h = 0, col = "red")

# Filtering the data to exclude rows with NA values in UKX_return or residuals
non_missing_data_GBM <- data_GBM[!is.na(data_GBM$UKX_return_GBM) & !is.na(resid(capm_model_GBM)), ]

# Residual Visualization
plot(non_missing_data_GBM$UKX_return_GBM, resid(capm_model_GBM), 
     main = "Residuals vs Market Return (GBM)", 
     xlab = "Market Return", 
     ylab = "Residuals")
abline(h = 0, col = "red")

# Identifying breakpoints for structural changes in the model
breakpoints_model_GBM <- breakpoints(GSK_return_GBM ~ UKX_return_GBM + USDGBP_return_GBM + EURGBP_return_GBM, data = data_GBM)

# Summary of breakpoints
summary(breakpoints_model_GBM)

# Plotting the breakpoints
plot(breakpoints_model_GBM, main = "Breakpoints in Regression Model (GBM)")

# Visualizing fitted values with breakpoints
plot(GSK_return_GBM ~ UKX_return_GBM, data = data_GBM, main = "Breakpoints in Regression Model (GBM)")
lines(fitted(breakpoints_model_GBM, breaks = 1), col = "blue")

# Setting up graphical parameters
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))

# Plot 1: Histogram of residuals
hist(resid(regression_model_updated_GBM), breaks = 30, main = "Histogram of Residuals (GBM)", xlab = "Residuals")

# Plot 2: Q-Q plot
qqnorm(resid(regression_model_updated_GBM), main = "Q-Q Plot of Residuals (GBM)")
qqline(resid(regression_model_updated_GBM), col = "red")

# Plot 3: Residuals vs Fitted values
plot(fitted(regression_model_updated_GBM), resid(regression_model_updated_GBM), 
     main = "Residuals vs Fitted (GBM)", xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "blue")

# Plot 4: Residuals vs Market Return
plot(data_GBM$UKX_return_GBM, resid(regression_model_updated_GBM), 
     main = "Residuals vs Market Return (GBM)", xlab = "Market Return", ylab = "Residuals")
abline(h = 0, col = "green")

# Reset graphical parameters
par(mfrow = c(1, 1))

# Excluding rows with missing values in UKX_return or residuals
non_missing_data_GBM <- data_GBM[!is.na(data_GBM$UKX_return_GBM) & !is.na(resid(regression_model_updated_GBM)), ]

# Plot of Residuals vs Market Return
plot(non_missing_data_GBM$UKX_return_GBM, resid(regression_model_updated_GBM), 
     main = "Residuals vs Market Return (GBM)", xlab = "Market Return", ylab = "Residuals")
abline(h = 0, col = "green")

# Recursive estimation
rec_estimation_GBM <- Fstats(GSK_return_GBM ~ UKX_return_GBM + USDGBP_return_GBM + EURGBP_return_GBM, data = data_GBM)
plot(rec_estimation_GBM, main = "Recursive Coefficient Estimates (GBM)")
abline(h = 0, col = "red")

# Adding hypothetical macroeconomic variables
data_GBM$Inflation_GBM <- rnorm(nrow(data_GBM), mean = 2, sd = 0.5)  # Simulated inflation rate
data_GBM$InterestRate_GBM <- rnorm(nrow(data_GBM), mean = 1.5, sd = 0.3)  # Simulated interest rate

# APT-style regression
apt_model_GBM <- lm(GSK_return_GBM ~ UKX_return_GBM + USDGBP_return_GBM + EURGBP_return_GBM + Inflation_GBM + InterestRate_GBM, data = data_GBM)

# Summary of APT-style regression
summary(apt_model_GBM)

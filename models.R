library(tseries)
library(readr)
library(ggplot2)
library(vars)
library(strucchange)

## Canada Model:

canada_data <- read_csv("Canada_TS_Data.csv")

Convert the relevant columns to numeric vectors:
canada_NumRefugee <- as.numeric(canada_data$NumRefugee)
canada_HealthcareExp <- as.numeric(canada_data$Total_Exp)
canada_gdp <- as.numeric(canada_data$GDP)
canada_hbeds <- as.numeric(canada_data$Hbeds)

c_refugee_change <- diff(canada_NumRefugee)

c_healthcare_change <- diff(canada_HealthcareExp)

c_gdp_change <- diff(canada_gdp)


# Calculate the log change in healthcare expenditure and GDP:
c_healthcare_log_change <- log(abs(c_healthcare_change))
c_gdp_log_change <- log(abs(c_gdp_change))
c_refugee_log_change <- log(abs(c_refugee_change))

# Augmented Dickey-Fuller Test for Change in Refugee Population:
adf_test_refugee <- adf.test(c_refugee_change, k = 1, alternative = "stationary")

# Augmented Dickey-Fuller Test for Change in Healthcare Expenditure:
adf_test_healthcare <- adf.test(c_healthcare_change, k = 1, alternative = "stationary")

# Augmented Dickey-Fuller Test for Change in GDP:
adf_test_gdp <- adf.test(c_gdp_change, k = 1, alternative = "stationary")

# Output the results:
print("ADF Test for Change in Refugee Population:")D
print(adf_test_refugee)

print("ADF Test for Change in Healthcare Expenditure:")
print(adf_test_healthcare)

print("ADF Test for Change in GDP:")
print(adf_test_gdp)


acf(c_refugee_change, lag = 20)
acf(c_gdp_change, lag = 20)

ACF Plot of the predicted variable:
acf(c_healthcare_change, lag = 20)

# Model building:
c_model_data <- data.frame(canada_data$Year[-1], c_refugee_change, c_healthcare_change, c_gdp_change)

c_timeseries_data <- ts(c_model_data[, -1], start = c(1977), frequency = 1)

# Lag selection:

lag_selection <- VARselect(c_timeseries_data, lag.max = 4, type = "both")
optimal_lags_aic <- lag_selection$selection["AIC(n)"]

# Optimal lags suggested by AIC:
print(paste("Optimal lags by AIC:", optimal_lags_aic))

c_var_model <- VAR(c_timeseries_data, p = 1, type = "both")

# View the summary of the model:
summary(c_var_model)

# Forecasting:
forecast_results <- predict(c_var_model, n.ahead = 10)

# View the forecast results:
print(forecast_results)

# Plot the forecast results:
plot(forecast_results)

# Model diagnostics – Check for serial correlation of residuals using Portmanteau (Breusch-Godfrey) test:
serial.test(c_var_model, lags.pt = 10, type = "PT.asymptotic")

# Plots for Overall Healthcare Expenditure over Time:
ggplot(canada_data, aes(x = Year, y = Total_Exp)) +
geom_line() +  # This adds a line plot
geom_point() +  # This adds points for each year
theme_minimal() +
labs(title = "Overall Healthcare Expenditure Over Time",
     x = "Year",
     y = "Healthcare Expenditure")

# Plot for Refugee Population over Time:
ggplot(canada_data, aes(x = Year, y = NumRefugee)) +
geom_line(color = "blue") +  # Adding a line plot with blue color
geom_point(color = "blue") +  # Adding points for each year with blue color
theme_minimal() +
labs(title = "Refugee Population Over Time",
     x = "Year",
     y = "Refugee Population")

# Plot for GDP (USD) over Time:
ggplot(canada_data, aes(x = Year, y = GDP)) +
geom_line(color = "green") +  # Adding a line plot with green color
geom_point(color = "green") +  # Adding points for each year with green color
theme_minimal() +
labs(title = "GDP (USD) Over Time",
     x = "Year",
     y = "GDP (USD)")

# More plots:
canada_data1 <- canada_data

# Calculating year-on-year changes for each variable:
canada_data1$Total_Exp <- c(NA, diff(canada_data1$Total_Exp))
canada_data1$NumRefugee <- c(NA, diff(canada_data1$NumRefugee))
canada_data1$GDP <- c(NA, diff(canada_data1$GDP))

# Plot for Change in Healthcare Expenditure over Time:
ggplot(canada_data1, aes(x = Year, y = Total_Exp)) +
geom_line() +  # This adds a line plot
geom_point() +  # This adds points for each year
geom_hline(yintercept = 0, linetype = "dotted", color = "red") +  # This adds a dotted line at 0
theme_minimal() +
labs(title = "Change in Healthcare Expenditure Over Time",
     x = "Year",
     y = "Change in Expenditure")

# Plot for Change in Refugee Population over Time:
ggplot(canada_data1, aes(x = Year, y = NumRefugee)) +
geom_line(color = "blue") +  # Adding a line plot with blue color
geom_point(color = "blue") +  # Adding points for each year with blue color
geom_hline(yintercept = 0, linetype = "dotted", color = "red") + 
theme_minimal() +
labs(title = "Change in Refugee Population Over Time",
     x = "Year",
     y = "Change in Population")

# Plot for Change in GDP (USD) over Time:
ggplot(canada_data1, aes(x = Year, y = GDP)) +
geom_line(color = "green") +  # Adding a line plot with green color
geom_point(color = "green") +  # Adding points for each year with green color
geom_hline(yintercept = 0, linetype = "dotted", color = "red") + 
theme_minimal() +
labs(title = "Change in GDP (USD) Over Time",
     x = "Year",
     y = "Change in GDP")

## Germany Model:

# Load necessary library
data <- read_csv("GermanyDataV2.csv")

# Convert the relevant columns to numeric vectors:
refugee_population <- as.numeric(data$NumRefugee)
healthcare_expenditure <- as.numeric(data$HealthcareExp)
gdp <- as.numeric(data$GDP)

refugee_change <- diff(refugee_population)

healthcare_change <- diff(healthcare_expenditure)

gdp_change <- diff(gdp)

gdp_change2 <- diff(gdp, 2)

# Calculate the log change in healthcare expenditure and GDP
healthcare_log_change <- log(abs(healthcare_change))
gdp_log_change <- log(abs(gdp_change))
refugee_log_change <- log(abs(refugee_change))

gdp_log_change

# Get rid of undefined values in refugee log change:
refugee_log_change[is.infinite(refugee_log_change)] <- 0

# Augmented Dickey-Fuller Test for Log Change in Refugee Population:
adf_test_refugee <- adf.test(refugee_log_change, k = 1, alternative = "stationary")

# Augmented Dickey-Fuller Test for Log Change in Healthcare Expenditure:
adf_test_healthcare <- adf.test(healthcare_log_change, k = 1, alternative = "stationary")

# Augmented Dickey-Fuller Test for Change in GDP:
adf_test_gdp <- adf.test(gdp_change, k = 1, alternative = "stationary")

# Output the results:
print("ADF Test for Change in Refugee Population:")
print(adf_test_refugee)

print("ADF Test for Change in Healthcare Expenditure:")
print(adf_test_healthcare)

print("ADF Test for Change in GDP:")
print(adf_test_gdp)

acf(refugee_log_change, lag = 20)
acf(gdp_change, lag = 20)

# ACF plot of predicted variable:
acf(healthcare_log_change, lag = 20)

# Model building
model_data <- data.frame(data$year[-1], refugee_log_change, healthcare_log_change, gdp_change)

timeseries_data <- ts(model_data[, -1], start = c(1971), frequency = 1)

# Lag selection:

lag_selection <- VARselect(timeseries_data, lag.max = 4, type = "both")
optimal_lags_aic <- lag_selection$selection["AIC(n)"]

# Optimal lags suggested by AIC:
print(paste("Optimal lags by AIC:", optimal_lags_aic))

var_model <- VAR(timeseries_data, p = 1, type = "both")

# View the summary of the model:
summary(var_model)

# Forecasting:
forecast_results <- predict(var_model, n.ahead = 10)

# View the forecast results
print(forecast_results)

# Plot the forecast results:
plot(forecast_results)

# Model diagnostics:
# Check for serial correlation of residuals using Portmanteau (Breusch-Godfrey) test
serial.test(var_model, lags.pt = 10, type = "PT.asymptotic")

# Model building with break point at 2009:
model1 <- model_data[1:38,]
model2 <- model_data[39:52,]

timeseries_data1 <- ts(model1[, -1], start = c(1971), frequency = 1)
timeseries_data2 <- ts(model2[, -1], start = c(2009), frequency = 1)

# Lag selection:

lag_selection <- VARselect(timeseries_data2, lag.max = 6, type = "both")
optimal_lags_aic <- lag_selection$selection["AIC(n)"]
optimal_lags_bic <- lag_selection$selection["BIC(n)"]

# Optimal lags suggested by AIC and BIC
print(paste("Optimal lags by AIC:", optimal_lags_aic))
print(paste("Optimal lags by BIC:", optimal_lags_bic))

var_model1 <- VAR(timeseries_data1, p = 1, type = "both")

# View the summary of the model:
summary(var_model1)

var_model2 <- VAR(timeseries_data2, p = 1, type = "both")

# View the summary of the model:
summary(var_model2)

# Forecasting:
forecast_results <- predict(var_model, n.ahead = 10)

# View the forecast results:
print(forecast_results)

# Plot the forecast results
plot(forecast_results)

# Model diagnostics
# Check for serial correlation of residuals using Portmanteau (Breusch-Godfrey) test
serial.test(var_model, lags.pt = 10, type = "PT.asymptotic")

# Test for structural change in GDP (ex. 2008, 2020) using Chow test:
sctest(data$year ~ data$GDP)
sctest(data$year ~ data$GDP, type = 'Chow')

sc <- efp(data$year ~ data$GDP, type = "Score-CUSUM")
plot(sc, functional = NULL)

bp <- breakpoints(data$year ~ data$GDP)
coef(bp)

fs <- Fstats(data$year ~ data$GDP)
plot(fs)
lines(breakpoints(fs))

# Change in GDP:
change_data <- data1[-1,]
sctest(change_data$year ~ change_data$GDP)
sctest(change_data$year ~ change_data$GDP, type = 'Chow')

sc <- efp(change_data$year ~ change_data$GDP, type = "Score-CUSUM")
plot(sc, functional = NULL)

bp <- breakpoints(change_data$year ~ change_data$GDP)
coef(bp)

fs <- Fstats(change_data$year ~ change_data$GDP)
plot(fs)
lines(breakpoints(fs))

# Log Change in GDP:
log_change_data <- data2[-1,]
sctest(log_change_data$year ~ log_change_data$GDP)
sctest(log_change_data$year ~ log_change_data$GDP, type = 'Chow')

sc <- efp(log_change_data$year ~ log_change_data$GDP, type = "Score-CUSUM")
plot(sc, functional = NULL)

bp <- breakpoints(log_change_data$year ~ log_change_data$GDP)
coef(bp)

fs <- Fstats(log_change_data$year ~ log_change_data$GDP)
plot(fs)
lines(breakpoints(fs)) 

# Plot for Overall Healthcare Expenditure over Time:
ggplot(data, aes(x = year, y = HealthcareExp)) +
geom_line() +  # This adds a line plot
geom_point() +  # This adds points for each year
theme_minimal() +
labs(title = "Overall Healthcare Expenditure Over Time",
     x = "Year",
     y = "Healthcare Expenditure")

# Plot for Refugee Population over Time:
ggplot(data, aes(x = year, y = NumRefugee)) +
geom_line(color = "blue") +  # Adding a line plot with blue color
geom_point(color = "blue") +  # Adding points for each year with blue color
theme_minimal() +
labs(title = "Refugee Population Over Time",
     x = "Year",
     y = "Refugee Population")

# Plot for GDP (USD) over Time:
ggplot(data, aes(x = year, y = GDP)) +
geom_line(color = "green") +  # Adding a line plot with green color
geom_point(color = "green") +  # Adding points for each year with green color
theme_minimal() +
labs(title = "GDP (USD) Over Time",
     x = "Year",
     y = "GDP (USD)")


## Turkey Model:

data <- read.csv("import1.csv")

# Fit the multiple linear regression model
model <- lm(GDP ~ Vaccination + PublicHos + TotalHos + RefugeePercentage, data = data)
summary(model)

# Diagnostic plots
par(mfrow = c(2, 2))
plot(model)

# Fit the multiple linear regression model with weights
model_wls <- lm(GDP ~ Vaccination + PublicHos + TotalHos + RefugeePercentage, data = data, weights = TotalPop)
summary(model_wls)

# Diagnostic plots
par(mfrow = c(2, 2))
plot(model_wls)

# Fit the multiple linear regression model with weights and remove non-significant predictor
model_wls_reduced <- lm(GDP ~ Vaccination + PublicHos + TotalHos, data = data, weights = TotalPop)
summary(model_wls_reduced)

# Diagnostic plots
par(mfrow = c(2, 2))
plot(model_wls_reduced)

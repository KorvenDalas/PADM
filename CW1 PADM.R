# Install libraries
install.packages("readxl")
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("lmtest")
install.packages("sandwich")
install.packages("car")
install.packages("margins")
install.packages("lattice")
install.packages("caret")
install.packages("AER")

# Load necessary libraries
library(readxl)    # For reading Excel files
library(dplyr)     # For data manipulation
library(ggplot2)   # For plotting
library(gridExtra) # For arranging multiple plots
library(tidyr)     # For pivoting dataset
library(lmtest)    # For Linear Regression Model tests
library(sandwich)  # For finding robust standard errors
library(car)       # For Companion to Applied Regression
library(margins)   # For providing “marginal effects” summaries of models and prediction
library(lattice)   # For data visualisation
library(caret)     # For the classification matrix
library(AER)       # For Applied Econometrics


# Set the working directory
setwd("C:/Downloads")
getwd()

# Read in the dataset
df <- read_excel("nls80.xlsx")

# Check the structure of the data
str(df)

# The variables "brthord", "meduc" and "feduc" are class chr
df <- as.data.frame(lapply(df, as.numeric))

# Count total NA values per column
colSums(is.na(df))

# Missing values inputed as zero
df[is.na(df)] <- 0
colSums(is.na(df))

# Identify continuous variables
cont_vars <- c("wage", "hours", "iq", "kww", "educ", "exper", "tenure",
               "age", "meduc", "feduc", "lwage")

# Create a subset with only the continuous variables
df_cont <- df %>% select(all_of(cont_vars))

# Summary function
summary(df_cont)

### Visualisations ###

# Pivot data for plotting
df_cont_plot <- df_cont %>% 
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")

# Plotting data
ggplot(df_cont_plot, aes(x = value)) +
  geom_histogram(fill = "darkgoldenrod1", color = "black", bins = 10) +
  facet_wrap(~ variable, scales = "free") +
  theme_minimal() +
  labs(title = "Histograms of Continuous Variables", x = "Value", y = "Count")


### Linear Regression ###

# LR Model test without wage
model_test <- lm(lwage ~ hours + iq + kww + educ + exper + tenure +
                   + age + meduc + feduc, data = df_cont)

# Display summary test model
summary(model_test)

# Final model
model_final <- lm(lwage ~ iq + educ + tenure + hours + kww,
                  data = df_cont)

# Final model summary
summary(model_final)

# Breusch-Pagan Test for heteroskedasticity
bptest(model_final)

# Test for Multicollinearity
vif(model_final)

# DW and BG tests are for autocorrelation or time series
# not for Multicollinearity.
# Durbin-Watson Test for first-order serial correlation
dwtest(model_final)

# Breusch-Godfrey test for higher-order serial correlation
bgtest(model_final, order = 1)


### Generalised Linear Models ###

# Add university variable
# Most people should have finished university after more than 14 years of education
df$university <- ifelse(df$educ > 14, 1, 0)
df$university <- as.numeric(df$university)
str(df)

# Logit model
logit_model <- glm(university ~ sibs + age + married + black + south + urban,
                   data = df,
                   family = binomial(link = "logit"))
summary(logit_model)

# Probit model
probit_model <- glm(university ~ sibs + age + married + black + south + urban,
                    data = df,
                    family = binomial(link = "probit"))
summary(probit_model)

# Marginal effects
margins_logit <- margins(logit_model)
summary(margins_logit)

margins_probit <- margins(probit_model)
summary(margins_probit)


# Predicted probabilities
df$pred_logit <- predict(logit_model, type = "response")
df$pred_probit <- predict(probit_model, type = "response")

# Class predictions (using 0.5 threshold)
logit_pred_binary <- ifelse(df$pred_logit > 0.5, 1, 0)
probit_pred_binary <- ifelse(df$pred_probit > 0.5, 1, 0)

# Confusion Matrix - Logit Model
confusionMatrix(as.factor(logit_pred_binary), as.factor(df$university))

# Confusion Matrix - Probit Model
confusionMatrix(as.factor(probit_pred_binary), as.factor(df$university))

# Plotting the predicted probability of attending university based on "sibs"
p1 <- ggplot(df, aes(x = sibs, y = pred_logit)) +
  geom_point(alpha = 0.3, color = "black") +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(title = "Logit Model",
       x = "Sibilings", y = "Probability of attending University") +
  theme_minimal()

p2 <- ggplot(df, aes(x = sibs, y = pred_probit)) +
  geom_point(alpha = 0.3, color = "black") +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(title = "Probit Model",
       x = "Sibilings", y = "Probability of attending University") +
  theme_minimal()

grid.arrange(p1, p2, ncol = 2)


### Monte Carlo and Endogeneity on OLS estimators ###
set.seed(2025)                  # Set seed for reproducibility

beta0 <- 2                      # Coefficients
beta1 <- 0.02
beta2 <- 0.08

generate_data <- function(n) {  # Function to generate data
  exper <- rpois(n, 10)         # Generate exogenous variables "exper"
  
  v <- rnorm(n, 8, 1.5)         # Generate endogenous variables "educ"
  educ <- 5 + 0.2 * exper + v   # Correlated with "exper"

# Generate error term correlated with "exper" and "educ"
  u <- rnorm(n, 0, 1) + 0.3 * exper + 0.1 * educ
  
# Generate lwage
  lwage <- beta0 + beta1 * educ + beta2 * exper + u

# Consolidate dataframe  
  data.frame(lwage, educ, exper)}

# Function to estimate models and return coefficients
estimate_model <- function(data) {
  ols <- lm(lwage ~ educ + exper, data = data)
  coef(ols)}

n <- 1000                       # Number of entries
n_sims = 1000                   # Number of Monte Carlo simulations

# Perform Monte Carlo simulation
results <- replicate(n_sims, {
  data <- generate_data(n)
  estimate_model(data)})

# Calculate means and variances
means <- rowMeans(results)
variances <- apply(results, 1, var)

# Create results table
results_table <- data.frame(
  Estimator = "OLS",
  Coefficient = c("Intercept", "Education", "Experience"),
  True_Value = c(beta0, beta1, beta2),
  Estimate = means,
  Bias = means - c(beta0, beta1, beta2),
  Variance = variances
)

# Print results
print(results_table, digits = 3)

# Visualize distributions
par(mfrow = c(1,3))
for (i in 1:3) {
  plot_data <- data.frame(
    OLS = results[i,]
  )
  plot_title <- c("Intercept", "Education", "Experience")[i]
  boxplot(plot_data, main = plot_title, ylab = "Coefficient Estimate")
  abline(h = c(beta0, beta1, beta2)[i], col = "red", lwd = 2)
}


### 2SLS ###      # Most of the lines are the same than the previous point

set.seed(2025)

beta0 <- 2
beta1 <- 0.02
beta2 <- 0.08

generate_data_v2 <- function(n) {
  exper <- rpois(n, 10)
  
  v <- rnorm(n, 8, 1.5) 
  educ <- 5 + 0.2 * exper + v
  
  # Error term unchanged
  u <- rnorm(n, 0, 1) + 0.3 * exper + 0.1 * educ
  
  # Instrumental variable correlated to "educ"
  z <- runif(n, 0, 10) + educ * 0.5

  lwage <- beta0 + beta1 * educ + beta2 * exper + u
  
  # Consolidate dataframe  
  data.frame(lwage, educ, exper, z)}

# Function to estimate models and return coefficients
estimate_model_v2 <- function(data) {
  ols <- lm(lwage ~ educ + exper, data = data)
  tsls <- ivreg(lwage ~ educ + exper | exper + z, data = data)
  c(coef(ols), coef(tsls))}

n <- 1000                       # Number of entries
n_sims = 1000                   # Number of Monte Carlo simulations

# Simulations
results_v2 <- replicate(n_sims, {
  data <- generate_data_v2(n)
  estimate_model_v2(data)})

# Calculate means and variances
means <- rowMeans(results_v2)
variances <- apply(results_v2, 1, var)

# Create results table
results_table_v2 <- data.frame(
  Estimator = rep(c("OLS", "2SLS"), each = 3),
  Coefficient = rep(c("Intercept", "Education", "Experience"), 2),
  True_Value = rep(c(beta0, beta1, beta2), 2),
  Estimate = means,
  Bias = means - rep(c(beta0, beta1, beta2), 2),
  Variance = variances
)

# Print results
print(results_table_v2, digits = 3)

# Visualize distributions
par(mfrow = c(1,3))
for (i in 1:3) {
  plot_data <- data.frame(
    OLS = results_v2[i,],
    TSLS = results_v2[i+3,]
  )
  plot_title <- c("Intercept", "Education", "Experience")[i]
  boxplot(plot_data, main = plot_title, ylab = "Coefficient Estimate")
  abline(h = c(beta0, beta1, beta2)[i], col = "red", lwd = 2)
}







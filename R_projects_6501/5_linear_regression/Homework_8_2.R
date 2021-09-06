# Question 8.2

# Loading data
uscrime <- read.table("C:\\Users\\adri_\\Documents\\Gatech\\ISYE6501\\week 5_linear_regression\\Fall2020hw5\\data 8.2\\uscrime.txt", header = TRUE)
head(uscrime)

# Set the seed to ensure that I get the same result all the time
set.seed(1)

# Converting the data into a dataframe
crime_data <- as.data.frame(uscrime[,c('M', 'So', 'Ed', 'Po1', 'Po2',	'LF',	'M.F',	'Pop',	'NW',	'U1',	'U2',	'Wealth',	'Ineq',	'Prob',	'Time','Crime'
)] )



# Examining bivariate relationships
cor(crime_data)

# Installing the package car(Companion to applied regression)
#install.packages("car")
library(car)
scatterplotMatrix(crime_data,spread= FALSE,smoother.args=list(lty=2),main="Bivariate Analysis")


# Looking at the min and max crime rates
min(crime_data$Crime)
max(crime_data$Crime)


# Model
# Hypothesis 1: Model includes all the predictors to predict the crime rate
linear_regression_model <- lm( Crime ~ ., data = crime_data)
summary(linear_regression_model)


# Predictions
new_observations <-data.frame(M = 14.0,So = 0, Ed = 10.0, Po1 = 12.0, Po2 = 15.5,LF = 0.640, M.F = 94.0, Pop = 150, NW = 1.1, U1 = 0.120, U2 = 3.6, Wealth = 3200, Ineq = 20.1, Prob = 0.040,Time = 39.0)

preds <- predict(linear_regression_model, new_observations)
preds



# Regression diagnostics of hypothesis 1:
linear_regression_model_1 <- lm( Crime ~ ., data = crime_data)
par(mfrow = c(2,2))
plot(linear_regression_model_1)



# Hypothesis 2: Model includes only predictors that have significance in predicting the crime rate
# Model
linear_regression_model_2 <- lm( Crime ~  M + Ed + Po1 + U2 + Ineq + Prob, data = crime_data)
summary(linear_regression_model_2)

# Predictions
#new_observations <-data.frame(M = 14.0,So = 0, Ed = 10.0, Po1 = 12.0, Po2 = 15.5,LF = 0.640, M.F = 94.0, Pop = 150, NW = 1.1, U1 = 0.120, U2 = 3.6, Wealth = 3200, Ineq = 20.1, Prob = 0.040,Time = 39.0)
preds_2 <- predict(linear_regression_model_2, new_observations)
preds_2




# Regression diagnostics of hypothesis 2:
linear_regression_model_2 <- lm( Crime ~  M + Ed + Po1 + U2 + Ineq + Prob, data = crime_data)
par(mfrow = c(2,2))
plot(linear_regression_model_2)


#
AIC(linear_regression_model_1, linear_regression_model_2)
BIC(linear_regression_model_1, linear_regression_model_2)


#Installing the package DAAG(Data Analysis and Graphics Data and functions)
#install.packages("DAAG")
library(DAAG)

# cross-validation for linear regression (CVlm)
cross_validation <- cv.lm(crime_data,linear_regression_model_2,m=5) 
cross_validation


# Predictive Analytics

# Sum of square differences
SS_total <- sum((crime_data$Crime - mean(crime_data$Crime))^2)
SS_res_cross_validation <- attr(cross_validation,"ms")*nrow(crime_data) 


# Calculating R-square on cross_validation models
1 - SS_res_cross_validation/SS_total 


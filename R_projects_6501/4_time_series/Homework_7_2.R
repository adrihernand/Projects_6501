# Question 7.2

# Loading data
temperature_data <- read.table("C:\\Users\\adri_\\Documents\\Gatech\\ISYE6501\\week 4_time_series\\Fall2020hw4\\data 7.2\\temps.txt", header = TRUE)
head(temperature_data)

# Set the seed to ensure that I get the same result all the time
set.seed(1)

# Creating a vector with columns 2 to 21 
temperature_data <- as.vector(unlist(temperature_data[,2:21]))
length(temperature_data)


# Using the vector to create the time series object. STarting in 1996 with a frequency of 123 days(July 1 to OCt 31)
time_series_temperature <- ts(temperature_data,start=1996,frequency=123)
time_series_temperature


# Plotting the time series
plot(time_series_temperature, type = "o", pch = 19)

# Let's make some predictions of the time series:

# Single Exponential smoothing: does not deal with trends(beta) or cyclical variations(gamma)
Single_exponential_smooting <- HoltWinters(time_series_temperature,beta=FALSE,gamma=FALSE)
Single_exponential_smooting  # Result: baseline at the end is 63.30 and alpha is 0.83. Not much randomness in the system
plot(Single_exponential_smooting)

# Double exponential smoothing: deals with trends(beta) but not with cyclical variations(gamma)
double_exponential_smoothing <- HoltWinters(time_series_temperature,gamma=FALSE)
double_exponential_smoothing # Result: baseline at the end is 63.25 and alpha 0.84, not much randomness.Beta and b are close to zero,there is no trend.  
plot(double_exponential_smoothing)

# Triple exponential smoothing (additive seasonality). Deals with trends(beta) and cyclical variations(gamma)
triple_exponential_smoothing <- HoltWinters(time_series_temperature)
triple_exponential_smoothing # Result: baseline at the end is 71.47,alpha 0.66, there is some randomness.
                             # Result; Beta and b are close to zero,there is no trend.Gamma is 0.6248, there are cycles in the data.
                             # Result: 123 seasonal factors.
plot(triple_exponential_smoothing)


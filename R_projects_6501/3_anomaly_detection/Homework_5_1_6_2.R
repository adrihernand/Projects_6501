
# Loading data
route <-"C:\\Users\\adri_\\Documents\\Gatech\\ISYE6501\\week 3_outliers_change_detection\\Fall2020hw3\\data 5.1\\uscrime.txt"
uscrime <-read.table(route,stringsAsFactors = FALSE, header = TRUE)
head(uscrime)
crime_data <- uscrime[,"Crime"]


#install.packages("outliers")
library(outliers)


# Quick visualization
plot(crime_data)
plot(rep(0, length(crime_data)),crime_data)
plot(uscrime$Pop,crime_data)


# Check for normality of the crime data since this is an assumption of the Grubbs 

# Method 1: Shapiro Wilk test. alpha = 0.05
# Hypothesis: Data is normally distributed
shapiro.test(crime_data)  # Result: Sample deviates from normality



# Method 2: Graphical assessment using Quantile-Quantile plots
# Hypothesis: Data is normally distributed
qqnorm(crime_data)  # Result: Sample deviates from normality. Right skew


# Method 3: Graphical assessment using histograms
# Hypothesis: Data is normally distributed

library(ggplot2)
ggplot(data = uscrime,
       mapping = aes(x = crime_data)) +
  geom_histogram(bins = 9)              # Result: Sample deviates from normality. Right skew



# Method 4: Grubbs test to identify outliers. alpha = 0.05

# Hypothesis 1:  Test if both min and max value are outliers on opposite tails of the sample.
grubbs.test(crime_data, type = 11) # Result: With p-value = 1, at least one is not an outlier.

# Hypothesis 2: Test if max value is an outlier 
grubbs.test(crime_data, type = 10)  # Result: This is not an outlier.

# Hypothesis 3: Test if min value is an outlier 
grubbs.test(crime_data, type = 10, opposite = TRUE) # Result: This is not an outlier.


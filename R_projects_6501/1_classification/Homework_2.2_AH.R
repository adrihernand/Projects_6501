# Problem statement:
# 1.	Using the support vector machine function ksvm contained in the R package kernlab, find a good classifier for this data. Show the equation of your classifier, and how well it classifies the data points in the full data set.  (Don't worry about test/validation data yet; we'll cover that topic soon.)


# Load data and see it in a table format

#file.choose()
route <-"C:\\Users\\adri_\\Documents\\Gatech\\ISYE6501\\week 1_Introduction_classification\\Fall2020hw1_myhomework\\data 2.2\\credit_card_data-headers.txt"

credit_data <-read.table(route,sep="\t", header=TRUE)
head(credit_data)


# Set the seed to ensure that I get the same result always
set.seed(1)


# Install "kernlab" package for machine learning methods and activate the library
#install.packages("kernlab")
library(kernlab)


# Create Support vector machine model with data converted into matrix
ksvm_model<-ksvm(x = as.matrix(credit_data[,1:10]),
                 y = as.factor(credit_data[,11]),
                 type = "C-svc",
                 kernel= "vanilladot",
                 C = 100, scaled = TRUE)

# calculate a1.am
a <- colSums(ksvm_model@xmatrix[[1]] * ksvm_model@coef[[1]])
a
# calculate a0
a0 <- ksvm_model@b
a0


# Making predictions using our SVM classifier(applying the model to new data):
preds<- predict(ksvm_model,newdata=credit_data[,1:10])


# Testing the performance of our classifier with a confusion matrix
# First, install "Caret" and activate the library.
#install.packages("caret")
library(caret)

confusionMatrix(factor(credit_data[,11],levels = c(1,0)),preds)


# see what fraction of the model's predictions match the actual classification
sum(preds == credit_data[,11]) / nrow(credit_data)



# Testing other C values(what-if scenarios)

c_list <- list(1e-4, 1e-3, 1e-2, 0.1, 1, 10, 100, 1000)
acc_list <-list()



for (i in c_list) {
  ksvm_model<-ksvm(x = as.matrix(credit_data[,1:10]),
                   y = as.factor(credit_data[,11]),
                   type = "C-svc",
                   kernel= "vanilladot",
                   C = i, scaled = TRUE)
  preds<- predict(ksvm_model,newdata=credit_data[,1:10])            # Applying the model to new data
  acc_val <- (sum(preds == credit_data[,11]) / nrow(credit_data))   # Accuracy of the models
  acc_list <- append(acc_list, acc_val)                             # Table of results
  
}

what_if_results <- do.call(rbind, Map(data.frame, C=c_list, Accuracy=acc_list))     # Run complex loop
what_if_results


# 2.	You are welcome, but not required, to try other (nonlinear) kernels as 
# well; we're not covering them in this course, but they can sometimes be useful 
# and might provide better predictions than vanilladot.


# Trying a polynomial kernel (training the model):
ksvm_model<-ksvm(x = as.matrix(credit_data[,1:10]),
                 y = as.factor(credit_data[,11]),
                 type = "C-svc",
                 kernel= "polydot", kpar=list(degree=3),
                 C = 0.01, scaled = TRUE)

preds<- predict(ksvm_model,newdata=credit_data[,1:10])                  # Applying the model to new data          
confusionMatrix(factor(credit_data[,1],levels = c(1,0)),preds)          # Accuracy



#Trying the radial basis kernel "Gaussian"(training the model):
ksvm_model<-ksvm(x = as.matrix(credit_data[,1:10]),
                 y = as.factor(credit_data[,11]),
                 type = "C-svc",
                 kernel= "rbfdot",
                 C = 0.01, scaled = TRUE)

preds<- predict(ksvm_model,newdata=credit_data[,1:10])                 # Applying the model to new data
confusionMatrix(factor(credit_data[,11],levels = c(1,0)),preds)        # Accuracy



#Trying the Hyperbolic tangent kernel (traning the model):
ksvm_model<-ksvm(x = as.matrix(credit_data[,1:10]),           
                 y = as.factor(credit_data[,11]),
                 type = "C-svc",
                 kernel= "tanhdot",
                 C = 0.01, scaled = TRUE)

preds<- predict(ksvm_model,newdata=credit_data[,1:10])                 # Applying the model to new data
confusionMatrix(factor(credit_data[,11],levels = c(1,0)),preds)        # Accuracy


# 3.	Using the k-nearest-neighbors classification function kknn contained in 
# the R kknn package, suggest a good value of k, and show how well it 
# classifies that data points in the full data set.  Don't forget to scale the
# data (scale=TRUE in kknn).


# Clear environment
rm(list = ls())

# Loading the data again
route <-"C:\\Users\\adri_\\Documents\\Gatech\\ISYE6501\\week 1_Introduction_classification\\Fall2020hw1_myhomework\\data 2.2\\credit_card_data-headers.txt"
credit_data <- read.table(route, stringsAsFactors = FALSE, header = TRUE)
head(credit_data)

# calling the "kknn" library

library(kknn)



#Creating model and iterating 15 values of k.

accuracy <- vector("numeric", 15)

for (k in 1:15) {
  error_sum <- 0
  for (i in 1:nrow(credit_data)) {
    data.learn <- credit_data[-i, ]
    data.valid <- credit_data[i,]
    data.kknn <- kknn(R1~., data.learn, data.valid, k = k, scale = TRUE)
    data.fit <- round(fitted(data.kknn), digits = 0)
    error_sum <- error_sum + abs(data.fit - credit_data[i, 11])
  }
  accuracy[k] <- 1- error_sum/nrow(credit_data)
}
accuracy

max(accuracy)
data.frame(1:15, accuracy)
plot(1:15,accuracy)


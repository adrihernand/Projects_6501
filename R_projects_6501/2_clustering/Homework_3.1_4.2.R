
# Cross validation for KNN model(question 3.1)


# Load data and see it in a table format
route <-"C:\\Users\\adri_\\Documents\\Gatech\\ISYE6501\\week 2_validation_clustering\\Fall2020hw2\\data 3.1\\credit_card_data-headers.txt"
credit_data <-read.table(route,sep="\t", header=TRUE)
head(credit_data)


# Set the seed to ensure that I get the same result all the time
set.seed(1)



#library(kknn)

# Cross validation of knn using the leave one out method
cross_val_knn <- train.kknn(R1~., data = credit_data, kmax = 100,  scale=TRUE)
summary(cross_val_knn)

preds <- round(fitted(cross_val_knn)[[58]][1:nrow(credit_data)], digits = 0)

#library(caret)
confusionMatrix(factor(credit_data[,11],levels = c(1,0)) ,factor(preds, levels = c(1,0)))



#--------------------------------------------------------------
# Splitting data method for knn. Decision is to use 70% as training set, 15% as validation set and 15% as test set. 

rm(list = ls())

# Load data and see it in a table format
route <-"C:\\Users\\adri_\\Documents\\Gatech\\ISYE6501\\week 2_validation_clustering\\Fall2020hw2\\data 3.1\\credit_card_data-headers.txt"
credit_data <-read.table(route,sep="\t", header=TRUE)
head(credit_data)


# Set the seed to ensure that I get the same result all the time
set.seed(1)


# Randomly taking 70% to use for training 
sampling_70 = sample(nrow(credit_data), round(nrow(credit_data)*.7))
training_set = credit_data[sampling_70, ]

# Split the remaining 30% into validation and test
remaining_30 = credit_data[-sampling_70, ]
half_of_30 = sample(nrow(remaining_30), round(nrow(remaining_30)*.5))


validation_set = remaining_30[half_of_30, ]
test_set = remaining_30[-half_of_30, ]


### Create a function that trains a model with neighbors 
train.model = function(neighbors)
{
  model = kknn(R1 ~., test_set, validation_set, k = neighbors, scale = TRUE)
  
  # store the predictions
  model_fitted = as.matrix(model$fitted.values)
  
  # round the fitted values
  model_fitted_rounded = as.matrix(lapply(model_fitted[, 1], round))
  
  # merge rounded that values back into model_fitted
  model_results = data.frame(validation_set[, 'R1'], model_fitted_rounded)
  
  colnames(model_results) = c("Actual", "Predicted")
  return(model_results)
}


### Create a function that calculates the accuracy based on predictions
eval.model = function(model)
{
  results <- vector("list", nrow(model))  
  for(i in 1:nrow(model)){
    results[[i]] <- as.integer(model[i, 'Actual'] == model[i, 'Predicted'])
  }
  # calculates the percent of correct predictions
  correct = sum(data.frame(results))
  model_accuracy = correct / nrow(model)
  return(model_accuracy)
}


### Train three models using k values of 12, 15, and 20
model_k12 = train.model(12)
model_k15 = train.model(15)
model_k58 = train.model(58)

### Return accuracy for each model
eval.model(model_k12)
eval.model(model_k15)
eval.model(model_k58)

#####################################################################

# Question 4.2


######################################################### FINAL #################

# Clear environment

rm(list = ls())

# ---------------------------- Data manipulation -------------------------------------

iris_data <- read.table("C:\\Users\\adri_\\Documents\\Gatech\\ISYE6501\\week 2_validation_clustering\\Fall2020hw2\\data 4.2\\iris.txt", header = TRUE)
iris_data <- iris_data[,2:6]

# optional check to make sure the data is read correctly
head(iris_data)

set.seed(1)


#How many we have from each species (I have 50 each)
table(iris_data[,5], iris_data$Species)


# scale the data using min max normalization:
iris_data_scaled = iris_data 
for (i in 1:4) { iris_data_scaled[,i] <- (iris_data[,i]-min(iris_data[,i]))/(max(iris_data[,i])-min(iris_data[,i])) }


# Possible scenarios


# 1) Assuming all predictors(there are 4 predictors variables) when k= 3, k=4, k=5, k=6

# Scenario 1: Using sepal length,sepal width, petal length, petal width
cluster_k3 = kmeans(iris_data_scaled[,1:4], 3, iter.max = 10, nstart = 25)
cluster_k4 = kmeans(iris_data_scaled[,1:4], 4, iter.max = 10, nstart = 25)
cluster_k5 = kmeans(iris_data_scaled[,1:4], 5, iter.max = 10, nstart = 25)
cluster_k6 = kmeans(iris_data_scaled[,1:4], 6, iter.max = 10, nstart = 25)


# Comparing clusters with the species.
table(cluster_k3$cluster, iris_data$Species)
table(cluster_k4$cluster, iris_data$Species)
table(cluster_k5$cluster, iris_data$Species)
table(cluster_k6$cluster, iris_data$Species)


# 2) Assuming 3 predictors when k= 3, k=4, k=5, k=6

# Scenario 2: Using Sepal Length, Sepal Width, Petal length
cluster_k3 = kmeans(iris_data_scaled[,1:3], 3, iter.max = 10, nstart = 25)
cluster_k4 = kmeans(iris_data_scaled[,1:3], 4, iter.max = 10, nstart = 25)
cluster_k5 = kmeans(iris_data_scaled[,1:3], 5, iter.max = 10, nstart = 25)
cluster_k6 = kmeans(iris_data_scaled[,1:3], 6, iter.max = 10, nstart = 25)

#Comparing clusters with the species.
table(cluster_k3$cluster, iris_data$Species)
table(cluster_k4$cluster, iris_data$Species)
table(cluster_k5$cluster, iris_data$Species)
table(cluster_k6$cluster, iris_data$Species)


# Scenario 3: Using Sepal Length, Sepal width, Petal width
cluster_k3 = kmeans(iris_data_scaled[,c(4,1:2)], 3, iter.max = 10, nstart = 25)
cluster_k4 = kmeans(iris_data_scaled[,c(4,1:2)], 4, iter.max = 10, nstart = 25)
cluster_k5 = kmeans(iris_data_scaled[,c(4,1:2)], 5, iter.max = 10, nstart = 25)
cluster_k6 = kmeans(iris_data_scaled[,c(4,1:2)], 6, iter.max = 10, nstart = 25)

#Comparing clusters with the species.
table(cluster_k3$cluster, iris_data$Species)
table(cluster_k4$cluster, iris_data$Species)
table(cluster_k5$cluster, iris_data$Species)
table(cluster_k6$cluster, iris_data$Species)


# Scenario 4: Using Sepal Length, Petal length,Petal width
cluster_k3 = kmeans(iris_data_scaled[,c(1,3:4)], 3, iter.max = 10, nstart = 25)
cluster_k4 = kmeans(iris_data_scaled[,c(1,3:4)], 4, iter.max = 10, nstart = 25)
cluster_k5 = kmeans(iris_data_scaled[,c(1,3:4)], 5, iter.max = 10, nstart = 25)
cluster_k6 = kmeans(iris_data_scaled[,c(1,3:4)], 6, iter.max = 10, nstart = 25)

#Comparing clusters with the species.
table(cluster_k3$cluster, iris_data$Species)
table(cluster_k4$cluster, iris_data$Species)
table(cluster_k5$cluster, iris_data$Species)
table(cluster_k6$cluster, iris_data$Species)



# Scenario 5: Using Sepal Width,Petal length, Petal width
cluster_k3 = kmeans(iris_data_scaled[,2:4], 3, iter.max = 10, nstart = 20)
cluster_k4 = kmeans(iris_data_scaled[,2:4], 4, iter.max = 10, nstart = 20)
cluster_k5 = kmeans(iris_data_scaled[,2:4], 5, iter.max = 10, nstart = 20)
cluster_k6 = kmeans(iris_data_scaled[,2:4], 6, iter.max = 10, nstart = 20)

#Comparing clusters with the species.
table(cluster_k3$cluster, iris_data$Species)
table(cluster_k4$cluster, iris_data$Species)
table(cluster_k5$cluster, iris_data$Species)
table(cluster_k6$cluster, iris_data$Species)


#ggplot(iris_data_scaled, aes(Sepal.Width, Petal.Length, Petal.Width, color = cluster_k6$cluster)) + geom_point()


# 3) Assuming 2 predictors when k= 3, k=4, k=5, k=6

# Scenario 6: Using Sepal Length, Sepal  Width
cluster_k3 = kmeans(iris_data_scaled[,1:2], 3, iter.max = 10, nstart = 20)
cluster_k4 = kmeans(iris_data_scaled[,1:2], 4, iter.max = 10, nstart = 20)
cluster_k5 = kmeans(iris_data_scaled[,1:2], 5, iter.max = 10, nstart = 20)
cluster_k6 = kmeans(iris_data_scaled[,1:2], 6, iter.max = 10, nstart = 20)

#Comparing clusters with the species.
table(cluster_k3$cluster, iris_data$Species)
table(cluster_k4$cluster, iris_data$Species)
table(cluster_k5$cluster, iris_data$Species)
table(cluster_k6$cluster, iris_data$Species)


# Scenario 7: Using Petal Length, Petal Width
cluster_k3 = kmeans(iris_data_scaled[,3:4], 3, iter.max = 10, nstart = 20)
cluster_k4 = kmeans(iris_data_scaled[,3:4], 4, iter.max = 10, nstart = 20)
cluster_5 = kmeans(iris_data_scaled[,3:4], 5, iter.max = 10, nstart = 20)
cluster_k6 = kmeans(iris_data_scaled[,3:4], 6, iter.max = 10, nstart = 20)

#Comparing clusters with the species.
table(cluster_k3$cluster, iris_data$Species)
table(cluster_k4$cluster, iris_data$Species)
table(cluster_k5$cluster, iris_data$Species)
table(cluster_k6$cluster, iris_data$Species)







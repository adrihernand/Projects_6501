# Question 10.1(part a)

# Loading data
uscrime <- read.table("C:\\Users\\adri_\\Documents\\Gatech\\ISYE6501\\week 7_midterm_Advanced_regression\\Fall2020hw7\\data 10.1\\uscrime.txt", header = TRUE)
head(uscrime)

# Setting the seed to ensure that I get the same result all the time
set.seed(1)


#installing packages("rpart", "rattle" and "rpart.plot") to visualize the tree
library(rpart)
library(rattle)
library(rpart.plot)


# Creating the regression tree to predict crime
tree_model <- rpart(Crime~., data = uscrime)
summary(tree_model) # Only 3 predictors were used in the construction of this tree

# More information about how the model is structured
tree_model


# Plotting the regression tree
fancyRpartPlot(tree_model)

# Incorporating the data of question 8.2 for predictions
new_data<-data.frame(M = 14.0,So = 0,Ed = 10.0, Po1 = 12.0,Po2 = 15.5,
                 LF = 0.640, M.F = 94.0,Pop = 150,NW = 1.1,U1 = 0.120,
                 U2 = 3.6, Wealth = 3200,Ineq = 20.1,Prob = 0.04, Time = 39.0)

pred <- predict(tree_model,new_data)
pred

# Calculating the R-squared of this model
SSresiduals <- sum((predict(tree_model)-uscrime$Crime)^2)
SStotal <- sum((uscrime$Crime - mean(uscrime$Crime))^2)
R2 <- 1 - SSresiduals/SStotal
R2


######################################################################
# Question 10.1 (part b)

# Loading data
uscrime <- read.table("C:\\Users\\adri_\\Documents\\Gatech\\ISYE6501\\week 7_midterm_Advanced_regression\\Fall2020hw7\\data 10.1\\uscrime.txt", header = TRUE)
head(uscrime)

# Setting the seed to ensure that I get the same result all the time
set.seed(1)


#install.packages("randomForest")
library(randomForest)


# Creating the random forest model
random_forest_model <- randomForest(Crime~., data = uscrime, mtry = 4, importance = TRUE)
summary(random_forest_model) # Only 3 predictors were used in the construction of this tree

# More information about the model
random_forest_model


# Incorporating the data of question 8.2 for predictions
new_data<-data.frame(M = 14.0,So = 0,Ed = 10.0, Po1 = 12.0,Po2 = 15.5,
                     LF = 0.640, M.F = 94.0,Pop = 150,NW = 1.1,U1 = 0.120,
                     U2 = 3.6, Wealth = 3200,Ineq = 20.1,Prob = 0.04, Time = 39.0)

pred <- predict(random_forest_model,new_data)
pred


# Calculating R-squared of the random forest
SS_residual <- sum((predict(random_forest_model)-uscrime$Crime)^2)
SS_total <- sum((uscrime$Crime - mean(uscrime$Crime))^2)
R2 <- 1 - SS_residual/SS_total
R2


########################################################################
#Question 10.3 
#(part a)

# Loading data
german_credit<-read.table("C:\\Users\\adri_\\Documents\\Gatech\\ISYE6501\\week 7_midterm_Advanced_regression\\Fall2020hw7\\data 10.3\\germancredit.txt",sep = " ")
head(german_credit)

# Setting the seed to ensure that I get the same result all the time
set.seed(1)


# converting the values in V21 to ones and zeros
german_credit$V21[german_credit$V21==1]<-0
german_credit$V21[german_credit$V21==2]<-1


# Checking the replaced values
german_credit$V21

# Creating the logistic regression model
linear_model <- glm(V21~., family = binomial(link = "logit"), data=german_credit)
summary(linear_model)


#Predictions
pred<- predict(linear_model, data=german_credit[,-21], type="response")

#Creating a confusion matrix
confusion_matrix <- table(german_credit$V21,round(pred>0.5))
names(dimnames(confusion_matrix)) <- c("True classification", "Model Classification")
confusion_matrix


# Model quality
model_quality= (626+160)/(626+74+140+160)
model_quality


#Question 10.3 
#(part b)

#threshold = 0.4
confusion_matrix_2 <- table(german_credit$V21,round(pred>0.4))
names(dimnames(confusion_matrix_2)) <- c("True classification", "Model Classification")
confusion_matrix_2


#threshold = 0.3
confusion_matrix_2 <- table(german_credit$V21,round(pred>0.3))
names(dimnames(confusion_matrix_2)) <- c("True classification", "Model Classification")
confusion_matrix_2




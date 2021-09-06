# Question 11.1


# Loading data
uscrime <- read.table("C:/Users/adri_/Documents/Gatech/ISYE6501/week 8_variable_selection/Fall2020hw8/data 11.1/uscrime.txt", stringsAsFactors = FALSE, header = TRUE)
head(uscrime)

# Setting the seed to ensure that I get the same result all the time
set.seed(1)

# ---------------------------- Stepwise Regression -------------------------------------

# Scaling the data
scaled_data = as.data.frame(scale(uscrime[,c(1,3,4,5,6,7,8,9,10,11,12,13,14,15)]))
scaled_data <- cbind(uscrime[,2],scaled_data,uscrime[,16]) 
colnames(scaled_data)[1] <- "So"
colnames(scaled_data)[16] <- "Crime"

# Using the classification and regression package(caret)
library(caret)

# Preparing training scheme for cross validation
control <- trainControl(method = "repeatedcv", number = 5, repeats = 5)


# Training the model
model <- train(Crime ~ ., data = scaled_data, "lmStepAIC", scope = list(lower = Crime~1, upper = Crime~.), direction = "backward",trControl=control)
print(model)


# Fitting the stepwise regression model with 8 variables
model_stepwise_regression = lm(Crime ~ M.F+U1+Prob+U2+M+Ed+Ineq+Po1, data = scaled_data)
summary(model_stepwise_regression)


# Cross-validation using the leave-one-out approach
total_sum_squares <- sum((uscrime$Crime - mean(uscrime$Crime))^2)

total_sum_squared_errors <- 0

for(i in 1:nrow(scaled_data)) {
  model_stepwise_i = lm(Crime ~ M.F+U1+Prob+U2+M+Ed+Ineq+Po1, data = scaled_data[-i,])
  preds <- predict(model_stepwise_i,newdata=scaled_data[i,])
  total_sum_squared_errors <- total_sum_squared_errors + ((preds - uscrime[i,16])^2)
}

R2_stepwise_model <- 1 - total_sum_squared_errors/total_sum_squares
R2_stepwise_model

# ---------------------------- Lasso Regression -------------------------------------
#install.packages("glmnet")
library(glmnet)

# building lasso with matrix
predictors_matrix=data.matrix(scaled_data[,-16])
response_matrix=data.matrix(scaled_data$Crime)

model=cv.glmnet(x=as.matrix(scaled_data[,-16]),y=as.matrix(scaled_data$Crime),alpha=1,
                nfolds = 5,type.measure="mse",family="gaussian")

# Output the coefficients of the variables selected by lasso
coef(model, s=model$lambda.min)


# Fitting a lasso regression model with 9 variables
model_lasso_regression = lm(Crime ~So+M+Ed+Po1+M.F+NW+U2+Ineq+Prob, data = scaled_data)
summary(model_lasso_regression)


# Cross-validation using the leave-one-out approach
total_sum_squares <- sum((uscrime$Crime - mean(uscrime$Crime))^2)

total_sum_squared_errors <- 0

for(i in 1:nrow(scaled_data)) {
  model_lasso_i = lm(Crime ~ So+M+Ed+Po1+M.F+NW+U2+Ineq+Prob, data = scaled_data[-i,])
  preds <- predict(model_lasso_i,newdata=scaled_data[i,])
  total_sum_squared_errors <- total_sum_squared_errors + ((preds - uscrime[i,16])^2)
}

R2_lasso_model <- 1 - total_sum_squared_errors/total_sum_squares
R2_lasso_model


# ---------------------------- Elastic Net -------------------------------------

# Varying alpha from 0 to 1
R2=c()
for (i in 0:10) {
  model_elastic_net = cv.glmnet(x=as.matrix(scaled_data[,-16]),y=as.matrix(scaled_data$Crime),
                          alpha=i/10,nfolds = 5,type.measure="mse",family="gaussian")
  
# calculating the R-Squared values with different alphas 
R2 = cbind(R2,model_elastic_net$glmnet.fit$dev.ratio[which(model_elastic_net$glmnet.fit$lambda == model_elastic_net$lambda.min)])}
R2


# Calculating the best value of alpha for the model
alpha_model = (which.max(R2)-1)/10
alpha_model

# Building the elastic model using this alpha value.
Elastic_net=cv.glmnet(x=as.matrix(scaled_data[,-16]),y=as.matrix(scaled_data$Crime),alpha=alpha_model,
                      nfolds = 5,type.measure="mse",family="gaussian")

# Output the coefficients of the variables selected by Elastic Net
coef(Elastic_net, s=Elastic_net$lambda.min)

# Regression using The Elastic Net 12 variables that are significant
elastic_net_regression = lm(Crime ~So+M+Ed+Po1+M.F+Pop+NW+U1+U2+Wealth+Ineq+Prob, data = scaled_data)
summary(elastic_net_regression)


# Cross-validation using the leave-one-out approach
total_sum_squares <- sum((uscrime$Crime - mean(uscrime$Crime))^2)

total_sum_squared_errors <- 0

for(i in 1:nrow(scaled_data)) {
  model_lasso_i = lm(Crime ~ So+M+Ed+Po1+M.F+Pop+NW+U1+U2+Wealth+Ineq+Prob, data = scaled_data[-i,])
  preds <- predict(model_lasso_i,newdata=scaled_data[i,])
  total_sum_squared_errors <- total_sum_squared_errors + ((preds - uscrime[i,16])^2)
}

R2_elastic_net <- 1 - total_sum_squared_errors/total_sum_squares
R2_elastic_net

# Question 9.1

# Loading data
uscrime <- read.table("C:\\Users\\adri_\\Documents\\Gatech\\ISYE6501\\week 6_principal_component_analysis\\Fall2020hw6\\data 9.1\\uscrime.txt", header = TRUE)
head(uscrime)

# Set the seed to ensure that I get the same result all the time
set.seed(1)


# correlation matrix to understand relationships among variables
corr <- cor(uscrime)
round(corr, 2)



# Run PCA on matrix of scaled predictors
pca <- prcomp(uscrime[,1:15], scale. = TRUE)
summary(pca)


#Plotting PCA and deciding how many components to keep
#install.packages("ggfortify")
library("ggfortify")
autoplot(pca, data = uscrime, loadings = TRUE, loadings.colour = "red", label.size = 3, loadings.label = TRUE)
autoplot(pca, data = uscrime, x= 2, y = 3, loadings = TRUE, loadings.colour = "blue", label.size = 3, loadings.label = TRUE)
autoplot(pca, data = uscrime, x= 3, y = 4, loadings = TRUE, loadings.colour = "purple", label.size = 3, loadings.label = TRUE)
autoplot(pca, data = uscrime, x= 4, y = 5, loadings = TRUE, loadings.colour = "black", label.size = 3, loadings.label = TRUE)
screeplot(pca, type="lines",col="blue")



## Get first 4 PCs
principal_components <- pca$x[,1:4]
attributes(pca$x)
pca$x
principal_components


# Create dataframe with the 4 principal components
principal_componentes_for_crime <- cbind(principal_components, uscrime[,16]) 
as.data.frame(principal_componentes_for_crime) 

# Build the linear regression model with the first 4 principal components
linear_regression_model <- lm(V5~., data = as.data.frame(principal_componentes_for_crime)) 
summary(linear_regression_model)




################################################
## Get coefficients in terms of original data
## from PCA coefficients
################################################
# PCA Coefficients for this linear regression model

intercept <- linear_regression_model$coefficients[1]
coefs <- linear_regression_model$coefficients[2:5]
intercept
coefs

# Transform the PC coefficients into coefficients for the original variables
pca$rotation[,1:4]
coefs_scaled <- pca$rotation[,1:4] %*% coefs
t(coefs_scaled)

# from coefficients scaled to coefficients unscaled because we are reverting the process
coefs_unscaled <- coefs_scaled/sapply(uscrime[,1:15],sd)
intercept_unscaled <- intercept - sum(coefs_scaled*sapply(uscrime[,1:15],mean)/sapply(uscrime[,1:15],sd))
t(coefs_unscaled)
intercept_unscaled

# Making predictions with the linear regression and calculating the R-square with the 4 PC's
preds <- as.matrix(uscrime[,1:15]) %*% coefs_unscaled + intercept_unscaled
preds
SSE = sum((preds - uscrime[,16])^2)
SStot = sum((uscrime[,16] - mean(uscrime[,16]))^2)
1 - SSE/SStot
R2 <- 1 - SSE/SStot
R2 - (1 - R2)*4/(nrow(uscrime)-4-1)


# Cross_validation for linear regression model with 4 PC's
library("DAAG")
data_lr_pc <- as.data.frame(principal_componentes_for_crime)
linear_regression_model <- lm(V5~., data = data_lr_pc) 
cv <- cv.lm(data_lr_pc,linear_regression_model, m=5)


1 - attr(cv,"ms")*nrow(data_lr_pc)/sum((data_lr_pc$V5 - mean(data_lr_pc$V5))^2) 


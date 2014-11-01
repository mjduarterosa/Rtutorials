# # # # # # # # # # # # # # # #
# Linear regression tutorial  #
# # # # # # # # # # # # # # # #

# Import data and functions
library(MASS)
library(ISLR)

# Boston house data
fix(Boston)
names(Boston)

# Fit simple linear regression model (one variable)
lm.fit = lm(medv~lstat,data=Boston)
attach(Boston)
lm.fit = lm(medv~lstat)
summary(lm.fit)

# Access information
names(lm.fit)
coef(lm.fit)
confint(lm.fit)

# Make predictions
predict(lm.fit,data.frame(lstat=(c(5,10,15))), interval = "confidence")
predict(lm.fit,data.frame(lstat=(c(5,10,15))), interval = "prediction")

# Make a plot
plot(lstat,medv)
abline(lm.fit,lwd=3,col="red")

# Multiple plots
par(mfrow=c(2,2))
plot(lm.fit)

# Plot predictions vs residuals
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))
plot(hatvalues(lm.fit))

# Multiple regression
lm.fit=lm(medv~lstat+age,data=Boston)
summary(lm.fit)

# Multiple regression all variables
lm.fit=lm(medv~.,data=Boston)

# Print R squared
summary(lm.fit)$r.sq

# Multiple regression all variables minus one
lm.fit1=lm(medv~.-age,data=Boston)
lm.fit1=update(lm.fit, ∼.-age)

# Include interactions
summary(lm(medv~lstat*age,data=Boston))
summary(lm(medv~lstat+age+lstat:age,data=Boston))

# Include transformations
lm.fit2=lm(medv~lstat+I(lstat^2))

# Quantify difference in fit
lm.fit=lm(medv~lstat)
anova(lm.fit ,lm.fit2)
￼
# Plot results quadratic model
par(mfrow=c(2,2))
plot(lm.fit2)

# Higher order terms
lm.fit5 = lm(medv~poly(lstat,5))
summary(lm.fit5)

# Other transformations (log)
summary(lm(medv~log(rm),data=Boston))

# Dataset carseats
fix(Carseats)
names(Carseats)
lm.fit=lm(Sales~.+Income:Advertising+Price:Age,data=Carseats)
summary(lm.fit)
attach(Carseats)

# See code for dummy variables
contrasts(ShelveLoc)

# Create a function
LoadLibraries = function() {
  library(ISLR)
  library(MASS)
  print("The libraries have been loaded.")
}

# Call function
LoadLibraries()
# # # # # # # # # # # # # # #
# Ridge regression and LASSO #
# # # # # # # # # # # # # # #
library(glmnet)
library(ISLR)

# Get rid of NA
Hitters = na.omit(Hitters)

# Create model
x = model.matrix(Salary~.,Hitters)[,-1]
y = Hitters$Salary

# Ridge regression
grid = 10^seq(10,2,length=100)
ridge.mod = glmnet(x,y,alpha=0,lambda=grid)




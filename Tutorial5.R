# # # # 
# Tutorial 5 - model selection and regularisation
# # # #

library(ISLR)
fix(Hitters)
names(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))

# Omit NA values
Hitters = na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters))

# Best subset selection
library(leaps)
regfit.full = regsubsets(Salary~.,Hitters)
summary(regfit.full)

# Search for more models
regfit.full = regsubsets(Salary~.,data=Hitters,nvmax=19)
reg.summary = summary(regfit.full)
names(reg.summary)
reg.summary$rsq

# Plot results
par(mfrow=c(2,2))
plot(reg.summary$rss,xlab="Number of variables", ylab="RSS", type="l")
plot(reg.summary$adjr2,xlab="Number of variables", ylab="Adjusted RSq", type="l")

which.max(reg.summary$adjr2)
points(11,reg.summary$adjr2[11],col="red",cex=2,pch=20)

plot(reg.summary$cp,xlab="Number of variables", ylab="Cp", type="l")
which.min(reg.summary$cp)
points(10,reg.summary$cp[10],col="red",cex=2,pch=20)
which.min(reg.summary$bic)
plot(reg.summary$bic,xlab="Number of variables", ylab="BIC", type="l")
points(6,reg.summary$bic[6],col="red",cex=2,pch=20)


plot(regfit.full,scale="r2")
plot(regfit.full,scale="adjr2")
plot(regfit.full,scale="Cp")
plot(regfit.full,scale="bic")

# Best model
coef(regfit.full,6)

# Forward and Backward stepwise selection
regfit.fwd= regsubsets(Salary~.,data=Hitters,nvmax=19,method='forward')
summary(regfit.fwd)
regfit.bwd= regsubsets(Salary~.,data=Hitters,nvmax=19,method='backward')
summary(regfit.bwd)

# Use cross-validation
set.seed(1)
train = sample(c(TRUE,FALSE),nrow(Hitters),rep=TRUE)
test = (!train)

regfit.best=regsubsets(Salary~.,data=Hitters[train,], nvmax =19)

test.mat=model.matrix(Salary~.,data=Hitters[test,])

val.errors=rep(NA,19)
for (i in 1:19){
  coefi = coef(regfit.best,id=i)
  pred = test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((Hitters$Salary[test]-pred)^2)
}

which.min(val.errors)
coef(regfit.best ,10)

# Create predict function for regsubsets
predict.regsubsets = function(object ,newdata ,id ,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object ,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}

# Choose best model through cross-validation
k = 10
set.seed(1)
folds = sample(1:k,nrow(Hitters),replace=TRUE)
cv.errors = matrix(NA,k,19,dimnames=list(NULL,paste(1:19)))

for (j in 1:k){
  best.fit = regsubsets(Salary~.,data = Hitters[folds!=j,],nvmax=19)
  for (i in 1:19){
    pred = predict.regsubsets(best.fit,Hitters[folds == j,],id=i)
    cv.errors[j,i] = mean((Hitters$Salary[folds==j]-pred)^2)
  }
}

mean.cv.errors = apply(cv.errors,2,mean)
mean.cv.errors

par(mfrow=c(1,1))
plot(mean.cv.errors,type='b')

reg.best = regsubsets(Salary~.,data=Hitters,nvmax=19)
coef(reg.best,11)


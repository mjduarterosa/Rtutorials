# # # # # # # # # # # # # # #
# PCR and PLS regression    #
# # # # # # # # # # # # # # #
library(ISLR)
library(pls)

# Omit missing values
Hitters = na.omit(Hitters)

# PCR
set.seed(2)
pcr.fit = pcr(Salary~.,data=Hitters,scale=TRUE,validation="CV")
summary(pcr.fit)

# Plot results
validationplot(pcr.fit,val.type = "MSEP")

# Separate train and test
x = model.matrix(Salary~.,Hitters)[,-1]
y = Hitters$Salary
train = sample(1:nrow(x),nrow(x)/2)
test = (-train)
y.test = y[test]

set.seed(1)
pcr.fit = pcr(Salary~.,data=Hitters,subset=train,scale = TRUE,validation= "CV")
validationplot(pcr.fit,val.type="MSEP")

pcr.pred = predict(pcr.fit,x[test,],ncomp=7)
mean((pcr.pred-y.test)^2)

pcr.fit = pcr(y~x,scale=TRUE,ncomp=7)
summary(pcr.fit)

# Partial least squares
set.seed(1)
pls.fit = plsr(Salary~.,data=Hitters,subset=train,scale=TRUE,validation="CV")
summary(pls.fit)
validationplot(pls.fit,val.type="MSEP")
pls.pred = predict(pls.fit,x[test,],ncomp=2)
mean((pls.pred-y.test)^2)

pls.fit = plsr(Salary~.,data=Hitters,scale=TRUE,ncomp=2)
summary(pls.fit)



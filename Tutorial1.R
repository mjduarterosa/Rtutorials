# Load Auto dataset
#Babofias
Auto = read.table("Auto.data",header = T, na.strings = "?")

# Omit NA from data
Auto = na.omit(Auto)

# Use attach to create variables
attach(Auto)

# Plot data
plot(cylinders, mpg)

# Plot cylinders as categorical
ylinders = as.factor(cylinders)
plot(cylinders, mpg, col="red", varwidth=T, xlab="cylinders", ylab="MPG")

# Histogram
hist(mpg, col=2, breaks=15)

# Identify objects in plot
plot(horsepower ,mpg)
identify(horsepower,mpg,name)

# Scatterplots
pairs(Auto)

# Summary of variables
summary(Auto)

# Save command history
savehistory()
loadhistory()

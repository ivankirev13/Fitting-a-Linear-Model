# read in the data
df <- read.table("data01738166.txt", sep=",", header=TRUE)

# extract the columns of data to separate vectors
weight <- df$weight
cost <- df$cost 

# plot a scatterplot of the data
plot(x=weight, y=cost, type='p', xlab="Weight", ylab="Cost")


#fitting a linear model
z <- log(cost)
model <- lm(z ~ weight)

# extract parameter coefficients from model object
beta0hat <- model$coefficients[1]
beta1hat <- model$coefficients[2]

#plot
plot(weight, z)
abline(a = beta0hat, b=beta1hat, col="blue", lwd=2)

#extract the residuals from the linear model
residuals <- model$residuals

#plot the residuals agains the weight
plot(weight, residuals, xlab="Weight", ylab="Residual value")

#fit a second linear model
t <- (weight)^2
model_2 <- lm(z ~ t)

#extract parameter coefficients from the second linear model
beta0hat_2 <- model_2$coefficients[1]
beta1hat_2 <- model_2$coefficients[2]

#plot
plot (t, z)
abline (a = beta0hat_2, b=beta1hat_2, col="blue", lwd=2)

#extract the residuals from the second linear model
residuals_2 <- model_2$residuals

#plot the residuals agains t
plot(t, residuals_2, xlab="t", ylab="Residual value")

#create a boxplot
boxplot(model_2$residuals, horizontal=TRUE, xlab="Residual values")

# the outlier is the 10th value, so remove this value to define the inliers
x <- df$weight[-10]
y <- df$cost[-10]
m <- log(y)
n <- x^2

#fit the model with the adjusted data
model_3 <- lm(m ~ n)
residuals_3 <- model_3$residuals

#plot
plot(n, residuals_3)

# extract parameter coefficients from model_3 object
beta0hat_3 <- model_3$coefficients[1]
beta1hat_3 <- model_3$coefficients[2]

# plot the (transformed) values and the regression line
plot(n, m)
abline(a = beta0hat_3, b=beta1hat_3, col="blue", lwd=2)

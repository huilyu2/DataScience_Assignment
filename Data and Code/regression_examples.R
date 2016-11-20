# Simulation Script to explore regression diagnositcs

# Generate some mock data
n <- 10000
X1 <- rnorm(n,0,1)
X2 <- rnorm(n,2,.4)
X3 <- rnorm(n,15,5)
X4 <- rnorm(n,-12,1)
X5 <- rnorm(n,10,1)

# We will create a perfect linear relationship between our predictors
# and our response variable so we can see what a "good" linear model
# looks like
response <- 2*X1+4*X2-12*X3+X4+0*X5
data <- data.frame(X1,X2,X3,X4,X5,response)

# Fit the model
model <- lm(response~.,data=data)

# View summary statistics
summary(model)

# Generate diagnostic plots
plot(model)

##################################################################################
# Now let's noise up our data so we can see a less than perfect model
data$response <- data$response + rnorm(n,0,10)^2

# Fit the model
model <- lm(response~.,data=data)

# View summary statistics
summary(model)

# Generate diagnostic plots
plot(model)

##################################################################################
# Plot confidence interval on regression line

# Going to use ggplot because it's way prettier than base R
library(ggplot2)

# Generate data and create a quadratic trend with some noise
x <- runif(500,0,2)
y <- x^2 + rnorm(500,0,6)
data <- data.frame(x,y)

# Fit a quadratic model
model <- lm(y~x+I(x^2))
summary(model)

# Get CI bounds
lower <- predict(model,newdata=data,interval = "confidence",level=0.95)[,2]
upper <- predict(model,newdata=data,interval = "confidence",level=0.95)[,3]

# Plot and add in regression line / confidence band
ggplot(data=data) + geom_point(aes(x,y),color="orange") + geom_line(aes(x,model$fitted.values),color="darkblue") + 
  geom_ribbon(aes(x=x,ymin=lower,ymax=upper),fill="dodgerblue",alpha=0.5)

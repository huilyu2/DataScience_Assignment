# HW 3 - Due Tuesday Sept 20, 2016. Upload R file to Moodle with name: HW3_490IDS_YOURNETID.R
# Do Not remove any of the comments. These are marked by #
# The .R file will contain your code and answers to questions.

#Name: Hui Lyu

# Main topic: Using the "apply" family function

#Q1 (5 pts)
# Given a function below,
myfunc <- function(z) return(c(z,z^2, z^3%/%2))
#(1) Examine the following code, and briefly explain what it is doing.
y = 2:8
myfunc(y)
matrix(myfunc(y),ncol=3)
### Your explanation
# First generate a vector named y of seven consecutive integers from 2 to 8
# Then pass the value of y to the established function myfunc to compute corresponding return values
# Finally reformat the values into a matrix of 3 columns
# The first column represents the value of y, the second column represents y^2, the third column represents y^3%/%2.
# Each row of the matrix contains three return values based on myfunc of each element in y.

#(2) Simplify the code in (1) using one of the "apply" functions and save the result as m.
###code & result
m = t(sapply(2:8, myfunc))
m
#      [,1] [,2] [,3]
# [1,]    2    4    4
# [2,]    3    9   13
# [3,]    4   16   32
# [4,]    5   25   62
# [5,]    6   36  108
# [6,]    7   49  171
# [7,]    8   64  256

#(3) Find the row product of m.
###code & result
apply(m,1,prod)
# > apply(m,1,prod)
# [1]     32    351   2048   7750  23328  58653 131072

#(4) Find the column sum of m in two ways.
###code & result
apply(m,2,sum)
colSums(m)
# > apply(m,2,sum)
# [1]  35 203 646
# > colSums(m)
# [1]  35 203 646

#(5) Could you divide all the values by 2 in two ways?
### code & result
m/2
apply(m, 1:2, function(x) x/2)
#      [,1] [,2]  [,3]
# [1,]  1.0  2.0   2.0
# [2,]  1.5  4.5   6.5
# [3,]  2.0  8.0  16.0
# [4,]  2.5 12.5  31.0
# [5,]  3.0 18.0  54.0
# [6,]  3.5 24.5  85.5
# [7,]  4.0 32.0 128.0

#Q2 (8 pts)
#Create a list with 2 elements as follows:
l <- list(a = 1:10, b = 11:20)
#(1) What is the product of the values in each element?
lapply(l,prod)
# > lapply(l,prod)
# $a
# [1] 3628800
# $b
# [1] 670442572800

#(2) What is the (sample) variance of the values in each element?
lapply(l,var)
# > lapply(l,var)
# $a
# [1] 9.166667
# $b
# [1] 9.166667

#(3) What type of object is returned if you use lapply? sapply? Show your R code that finds these answers.
class(lapply(l,var))
# [1] "list"
class(sapply(1,var))
# [1] "numeric"

# Now create the following list:
l.2 <- list(c = c(21:30), d = c(31:40))
#(4) What is the sum of the corresponding elements of l and l.2, using one function call?
mapply(sum, l$a, l$b, l.2$c, l.2$d)
#  [1]  64  68  72  76  80  84  88  92  96 100

#(5) Take the log of each element in the list l:
sapply(l, log)

#(6) First change l and l.2 into matrixes, make each element in the list as column,
### your code here
l = matrix(unlist(l), ncol = 2)
l.2 = matrix(unlist(l.2), ncol = 2)

#Then, form a list named mylist using l,l.2 and m (from Q1) (in this order).
### your code here
mylist = list(l=l,l.2=l.2,m=m)

#Then, select the first column of each elements in mylist in one function call (hint '[' is the select operator).
### your code here
lapply(mylist, function(l) l[,1])

#Q3 (3 pts)
# Let's load our friend family data again.
load(url("http://courseweb.lis.illinois.edu/~jguo24/family.rda"))
#(1) Find the mean bmi by gender in one function call.
tapply(family$bmi,family$gender,mean)
#       m        f 
#25.73898 23.02564 

#(2) Could you get a vector of what the type of variables the dataset is made of?
sapply(family, class)
#firstName    gender       age    height    weight       bmi    overWt 
# "factor"  "factor" "integer" "numeric" "integer" "numeric" "logical" 

#(3) Could you sort the firstName in height descending order?
family$firstName[order(family$height, decreasing = TRUE)]
# [1] Joe Tom Tom Liz Jon Tim Bob Ann Dan Art Sal May Sue Zoe
# Levels: Ann Art Bob Dan Joe Jon Liz May Sal Sue Tim Tom Zoe

#Q4 (2 pts)
# There is a famous dataset in R called "iris." It should already be loaded
# in R for you. If you type in ?iris you can see some documentation. Familiarize 
# yourself with this dataset.
#(1) Find the mean petal length by species.
### code & result
tapply(iris$Petal.Length, iris$Species, mean)
#     setosa versicolor  virginica 
#      1.462      4.260      5.552 

#(2) Now obtain the sum of the first 4 variables, by species, but using only one function call.
### code & result
by(iris[, 1:4], iris$Species, colSums)
#iris$Species: setosa
#Sepal.Length  Sepal.Width Petal.Length  Petal.Width 
#       250.3        171.4         73.1         12.3 
#------------------------------------------------------------------------ 
#iris$Species: versicolor
#Sepal.Length  Sepal.Width Petal.Length  Petal.Width 
       #296.8        138.5        213.0         66.3 
#------------------------------------------------------------------------ 
#iris$Species: virginica
#Sepal.Length  Sepal.Width Petal.Length  Petal.Width 
#       329.4        148.7        277.6        101.3 

#Q5 (2 pts)
#Below are two statements, their results have different structure, 
lapply(1:4, function(x) x^3)
sapply(1:4, function(x) x^3)
# Could you change one of them to make the two statements return the same results (type of object)?
unlist(lapply(1:4, function(x) x^3))

#Q6. (5 pts) Using the family data, fit a linear regression model to predict 
# weight from height. Place your code and output (the model) below. 
lm(family$weight ~ family$height, data = family)
# output:
# Call:
# lm(formula = family$weight ~ family$height, data = family)
# Coefficients:
#   (Intercept)  family$height  
# -455.666          9.154 

# The model is: weight = -455.666 + 9.154*height


# How do you interpret this model?
# The weight has a positive correlation with height since the slope 9.154>0. As height goes up,
# the corresponding weight also goes up. This conforms to our common sense. The intercept is below 0, 
# which means the value of weight is below 0 when height equals 0. This is a sort of parallel movement
# to make it adjust to the data.

# Create a scatterplot of height vs weight. Add the linear regression line you found above.
plot(family$height, family$weight, xlab = "height", ylab = "weight", main = "plot of weight ~ height")
abline(lm(family$weight ~ family$height, data = family), col='red')

# Provide an interpretation for your plot.
# In the plot, the scattered spots are true values of 14 objects in the family dataset. The x
# axis is height, and the y axis is weight. Basically, as height goes up, his or her weight
# also goes up. The red line is the linear regression line based on the 14 objects. As can be 
# seen, the number of spots under the line is equal to the number of spots above the line. All
# the spots are basically close to the line, which means the residuals are not too large. There
# is no outlier in the plot. In general, it is a good linear model based on true observations.

summary(lm(family$weight ~ family$height, data = family))
# P-value is also very small, which means a good linear model.
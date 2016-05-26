#This is a sample problem from Elements of Statistical learning by Hastie and Tibshirani
# The data set is available in the ISLR package
library(ISLR)
data(Smarket)
names(Smarket)
# Summary statistics
summary(Smarket)
attach(Smarket)

# Correlation matrix
cor(Smarket[, -9])

#Logistic Regression

glm.fit = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Smarket, 
              family = binomial)
summary(glm.fit)

#Predict
glm.probs = predict(glm.fit, type = "response")
length(glm.probs)

#Add direction label < 0.5 Down else up
glm.pred = rep("Down", 1250)
glm.pred[glm.probs > 0.5] = "Up"
# Produce a confusion matrix
table(glm.pred, Direction)

# Compute the fraction of days for which the prediction was correct
mean(glm.pred == Direction)

# Create a vector corresponding to the observations from 2001 through 2004
train = (Year < 2005)
# Test data set. 2005
Smarket.2005 = Smarket[!train, ]
Direction.2005 = Direction[!train]
# Fit the logistic regression using only the train data
glm.fit <- glm(Direction ~ lag1 + lag2 + lag3 + lag4 + lag5 + Volume, data = Smarket, 
               family = binomial, subset = train)

# Test the model by fitting the test data set into the fitted model
glm.probs <- predict(glm.fit, Smarket.2005, type = "response")
# compute the predictions for 2005 and compare them to the actual
# movements of the market over that time period.
glm.pred = rep("Down", nrow(Smarket.2005))
glm.pred[glm.probs > 0.5] = "Up"
table(glm.pred, Direction.2005)

# Test error rate
mean(glm.pred == Direction.2005)

# Training error rate
mean(glm.pred != Direction.2005)

# Refit the model using only 2 predictors lag1 and lag2
glm.fit = glm(Direction ~ Lag1 + Lag2, data = Smarket, family = binomial, subset = train)
glm.probs = predict(glm.fit, Smarket.2005, type = "response")
glm.pred = rep("Down", nrow(Smarket.2005))
glm.pred[glm.probs > 0.5] = "Up"
table(glm.pred, Direction.2005)

# Test error rate: predicted correctly / total observations of the test
# data set
mean(glm.pred == Direction.2005)

# Predict
predict(glm.fit, newdata = data.frame(Lag1 = c(1.2, 1.5), Lag2 = c(1.1, -0.8)), 
        type = "response")

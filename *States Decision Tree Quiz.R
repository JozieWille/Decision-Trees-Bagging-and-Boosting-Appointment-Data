# State Data Quiz

# Recall that the state dataset has, for each of the fifty U.S. states, the population, per capita income, 
# illiteracy rate, murder rate, high school graduation rate, average number of frost days, area, latitude and longitude, 
#division the state belongs to, region the state belongs to, and two-letter abbreviation. This dataset comes from the U.S. Department of Commerce, Bureau of the Census.

#Load the state data into R and convert it to a data frame by running the following two commands in R:
  
data(state)
statedata = data.frame(state.x77)

# We will try to build a model for life expectancy using regression trees, and employ cross-validation to improve 
# our tree's performance.
---------------------------------------------------------------------------------------------------------------------------------------
# Q1.) Let's recreate the linear regression models we made in the previous exercise. First, 
# predict Life.Exp using all of the other variables as the independent variables 
# (Population, Income, Illiteracy, Murder, HS.Grad, Frost, Area ). Use the entire dataset to build the model.
  # What is the adjusted R-squared of the model? (Keep 4 significant digits. For example, 0.1111)
  
lm.fit = lm(Life.Exp ~ ., data = statedata)
summary(lm.fit)

# Answer = Adjusted R-squared:  0.6922 
---------------------------------------------------------------------------------------------------------------------------------------
# Q2.) Calculate the sum of squared errors (SSE) between the predicted life expectancies using this model and 
# the actual life expectancies. 
  
sum((statedata$Life.Exp - predict(lm.fit))^2)

# ANSWER 23.29714
---------------------------------------------------------------------------------------------------------------------------------------
# Q3.) Build a second linear regression model using just Population, Murder, Frost, and HS.Grad as independent variables 
  # (the best 4 variable model from the previous homework). What is the adjusted R-squared for this model?

lm.fit = lm(Life.Exp ~ Population + Murder + Frost + HS.Grad, data = statedata)
summary(lm.fit)
  
# ANSWER = Adjusted R-squared:  0.7126
---------------------------------------------------------------------------------------------------------------------------------------
# Q4.) Calculate the sum of squared errors again, using this reduced model.

sum((statedata$Life.Exp - predict(lm.fit))^2)
# ANSWER 23.30804
---------------------------------------------------------------------------------------------------------------------------------------
# Q5.) Using the tree package, let's now build a tree model to predict Life.Exp using all of the other variables
# as independent variables (Population, Income, Illiteracy, Murder, HS.Grad, Frost, Area). 
# In this problem we are not as interested in predicting life expectancies for new observations as we are 
# understanding how they relate to the other variables we have, so we'll use all of the data to build our model. 
# Which variables appear in the tree?
  
library(tree)
state.tree = tree(Life.Exp ~ ., data = statedata)

state.tree

# ANSWER: murder, area, hs.grad
---------------------------------------------------------------------------------------------------------------------------------------
# Q6.)Use the regression tree you just built to predict life expectancies (using the predict function), 
# and calculate the sum-of-squared-errors (SSE) 
#like you did for linear regression. What is the SSE?

  sum((statedata$Life.Exp - predict(state.tree))^2)

# ANSWER  23.64283
---------------------------------------------------------------------------------------------------------------------------------------
# Q7.) Set the random seed to 200. Use cv.tree function to cross-validate the tree. Pick the best tree based on cross-validation error.
#  Calculate the SSE of this tree. 
  
set.seed(200)
cv.state.tree = cv.tree(state.tree, FUN = prune.tree)

plot(cv.state.tree$size, cv.state.tree$dev)

prune.state = prune.tree(state.tree, best = 3)

sum((statedata$Life.Exp - predict(prune.state))^2)

# 32.8655


---------------------------------------------------------------------------------------------------------------------------------------
# Q8.) Given what you have learned about cross-validation, the tree will perform better if we did use it for prediction on a test set.


TRUE




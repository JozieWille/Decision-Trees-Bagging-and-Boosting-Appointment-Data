# WHY PPL VOTE GERBER
setwd("~/Downloads/5415 Advanced Data Analytics/Data Files & CSV's")
gerber = read.csv("gerber-1.csv", stringsAsFactors = T)

-------------------------------------------------------------------------------------------------------------------------
# 1.) What proportion of people in this dataset voted in this election?

mean(gerber$voting)

# ANSWER 0.3158996, 31.59%
-------------------------------------------------------------------------------------------------------------------------
# 2.) Which of the four "treatment groups" had the largest fraction of voters?

mean(gerber$voting[gerber$civicduty == 1])
# 0.3145377

mean(gerber$voting[gerber$hawthorne == 1])
# 0.3223746

mean(gerber$voting[gerber$self == 1])
# 0.3451515

mean(gerber$voting[gerber$neighbors == 1])
# 0.3779482 This has the largest fraction of voters
-------------------------------------------------------------------------------------------------------------------------
# 3.) Build a logistic regression model for voting using the four treatment group variables as the independent 
# variables (civicduty, hawthorne, self, and neighbors). Use all the data to build the model 
# (DO NOT split the data into a training set and testing set).
  
# Using a threshold of 0.3, what is the accuracy of the logistic regression model?
  
glm.fit = glm(voting ~ civicduty + hawthorne + self + neighbors,
              data = gerber, family = binomial)

pred = predict(glm.fit, type = "response") > 0.3

confusion_matrix = table(gerber$voting, pred)

(confusion_matrix[1, 1] + confusion_matrix[2, 2]) / nrow(gerber) # this is the way to get answer from confusion matrix ALWAYS

# ANSWER 0.5419578, 0.5420

# ALTERNATE WAY TO GET ANSWER FROM TABLE

# OUTPUT:
#    FALSE   TRUE
# 0: 134513 100875
# 1:  56730  51966

# true positives plus true negatives
(134513 +  51966) / nrow(gerber)

-------------------------------------------------------------------------------------------------------------------------
# 4.) Using a threshold of 0.5, what is the accuracy of the logistic regression model?

glm.fit = glm(voting ~ civicduty + hawthorne + self + neighbors,
                data = gerber, family = binomial)

pred = predict(glm.fit, type = "response") > 0.5

cm = table(gerber$voting, pred)

# OUTPUT:
#    FALSE
# 0: 235388
# 1: 108696

(235388) / nrow(gerber)
 # ANSWER = 0.6841004 accuracy

-------------------------------------------------------------------------------------------------------------------------
# 5.) Leave all the parameters at their default values. You can use the following command in R to build the tree:
  tree.gerber = tree(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)
# Plot the tree. What happens, and if relevant, why?

plot(tree.gerber)
# error because it is a single node tree

# ANSWER ON QUIZ:  No variables are used (the tree is only a root node) - none of the variables make a big enough effect to be split on.
-------------------------------------------------------------------------------------------------------------------------
# 6.) Now build the tree using the command:
  tree.gerber = tree(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, 
                     control=tree.control(nobs=nrow(gerber),mindev = 0))

# to force the complete tree to be built. Then plot the tree. 
# What do you observe about the order of the splits?

plot(tree.gerber)
text(tree.gerber)

# ANSWER: Neighbor is the first split, civic duty is the last.
-------------------------------------------------------------------------------------------------------------------------
# 7.) Using only the tree plot, determine what fraction (a number between 0 and 1) of "Civic Duty" people voted?
plot(tree.gerber)
text(tree.gerber)
# ANSWER 0.3145

-------------------------------------------------------------------------------------------------------------------------
# 8.) Let's just focus on the "Control" treatment group. Create a regression tree using just the "control" variable, then 
# create another tree with the "control" and "sex" variables, both with 
  control=tree.control(nobs=nrow(gerber),mindev= 0)
# Now, using the second tree (with control and sex), determine who is affected more by NOT being in the control group 
# (being in any of the four treatment groups):
  

tree.control = tree(voting ~ control, data = gerber, control=tree.control(nobs=nrow(gerber),mindev= 0))

tree.control.sex = tree(voting ~ control + sex, data = gerber, control=tree.control(nobs=nrow(gerber), mindev= 0))

plot(tree.control)
text(tree.control)

plot(tree.control.sex)
text(tree.control.sex)

# KEY for Men and Women
# Men = 0
# Female = 1 

# Difference from MEN in control versus men not in control group
0.3458 - 0.3028 
# ANSWER = 0.043

# Difference from WOMEN in control versus men not in control group
0.3342 - 0.2905
# ANSWER = 0.0437

# ANSWER = They are affected the same.

-------------------------------------------------------------------------------------------------------------------------
# 9.) Going back to logistic regression now, create a model using "sex" and "control". 
# Interpret the coefficient for "sex":
  
glm.fit = glm(voting ~ control + sex, data = gerber, family = binomial)

summary(glm.fit)

# COEFFICENT sex =  -0.055791 

# ANSWER Coefficient is negative, reflecting that women are less likely to vote.
  
-------------------------------------------------------------------------------------------------------------------------
# 10.) The four values in the results correspond to the four possibilities in the order they are stated above 
# ( (Man, Not Control), (Man, Control), (Woman, Not Control), (Woman, Control) ). 
# What is the absolute difference between the tree and the logistic regression for the (Woman, Control) case?

# FOLLOWING CODE IS GIVEN TO YOU IN PROBLEM QUESTION: 
Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1)) 
predict(glm.fit, newdata=Possibilities, type="response")
  
# OUTPUT
#     1         2         3         4 
# 0.3462559 0.3024455 0.3337375 0.2908065
control from women code = 0.2908065

control from women TREE = 0.2905

0.2908065 - 0.2905
# ANSWER = 0.0003065
-------------------------------------------------------------------------------------------------------------------------
# 11.)  How do you interpret the coefficient for the new variable in isolation? That is, how does it relate to the dependent variable?

# FOLLOWING CODE IS GIVEN TO YOU IN PROBLEM QUESTION: 
glm.fit2 = glm(voting ~ sex + control + sex:control, data=gerber, family=binomial)

summary(glm.fit2)

# OUTPUT 
# coefficent = sex:control -0.007259

# ANSWER = If a person is a woman and in the control group, the chance that she voted goes down.

 


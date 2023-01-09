setwd("/Users/joziewille/Downloads/5415 Advanced Data Analytics/Boulder Housing")

load("boulder-cleaned.RData")
str(boulder.clean)

# build regression tree
library(tree)
n <- nrow(boulder.clean)
train <- sample(n, n/2)
tree.boulder <- tree(LIST.PRICE ~ . - ADDRESS, data = boulder.clean, 
                     subset = train)
summary(tree.boulder)

# plot and label the tree
plot(tree.boulder)
text(tree.boulder, pretty = 0)

# validation set appraoch
pred <- predict(tree.boulder, newdata = boulder.clean[-train, ])
plot(pred, boulder.clean[-train, "LIST.PRICE"])
# add a line with intercept 0 and slope 1
abline(0, 1)
mean((pred - boulder.clean[-train, "LIST.PRICE"])^2)

# cross validation
cv.boulder <- cv.tree(tree.boulder)
plot(cv.boulder$size, cv.boulder$dev, type = "b")

# pruning tree to 3 terminal nodes
prune.boulder <- prune.tree(tree.boulder, best = 3)
plot(prune.boulder)
text(prune.boulder, pretty = 0)

# classification tree for appointment dataset 
# Dan Zhang, August 2016

rm(list = ls())
setwd("D:/Dropbox/courses/DataAnalytics/data/appointments")

# read data Please download Appointments.csv from D2L
appt <- read.csv("appointments.csv")
str(appt)


# building the tree
library(tree)
tree.appt <- tree(Status ~ . - MRN, data = appt)
tree.appt
summary(tree.appt)
# plot and label the tree
plot(tree.appt)
text(tree.appt, pretty = 0)
# predict the 'class' of each observation
tree.pred <- predict(tree.appt, type = "class")
# this reports the predicted probability of belong to each class
tree.pred <- predict(tree.appt)

# building a larger tree
tree.appt <- tree(Status ~ . - MRN, data = appt, 
                  control = tree.control(nobs = nrow(appt), mindev = 0.001))
tree.appt
summary(tree.appt)
# plot and label the tree
plot(tree.appt)
text(tree.appt, pretty = 0)
# predict the 'class' of each observation
tree.pred <- predict(tree.appt, type = "class")
# this reports the predicted probability of belong to each class
tree.pred <- predict(tree.appt)

# validation set approach
set.seed(200)
train <- sample(nrow(appt), 0.6 * nrow(appt))
appt.test <- appt[-train, ]
tree.train <- tree(Status ~ . - MRN, data = appt, subset = train, 
                   control = tree.control(nobs = length(train), mindev = 0.001))
# confusion matrix using test data
tree.pred <- predict(tree.train, newdata = appt.test, type = "class")
table(appt.test$Status, tree.pred)

# validation using a different threshold
tree.pred <- predict(tree.train, newdata = appt.test)
tree.pred <- (tree.pred[, 2] > 0.2)
table(appt.test$Status, tree.pred)

# cross validation using cv.tree
set.seed(200)
# 10-folds cross validation tree.train <-
# tree(Status~.-MRN,data=appt,split='gini')
tree.train <- tree(Status ~ . - MRN, data = appt, 
                   control = tree.control(nobs = nrow(appt), mindev = 0.005))
cv.appt <- cv.tree(tree.train, FUN = prune.tree, K = 5)
# tree size (number of terminal nodes) vs deviance (a measure of fit)
plot(cv.appt$size, cv.appt$dev)
# prune tree to 2 terminal nodes
prune.appt <- prune.tree(tree.train, best = 2)
prune.appt
plot(prune.appt)
text(prune.appt, pretty = 0)

# bagging and random forest
library(randomForest)
set.seed(300)
# bagging for appt dataset by setting mtry equal to the number of
# predictor variables; note that predict reports out of sample
# prediction so cross validation is not necessary; Can also use rfcv()
# for cross validation if required
bag.appt <- randomForest(Status ~ . - MRN, data = appt, mtry = 10)
pred.bag <- predict(bag.appt)
table(appt$Status, pred.bag)

# random forest
set.seed(300)
forest.appt <- randomForest(Status ~ . - MRN, data = appt, mtry = 5)
pred.forest <- predict(forest.appt)
table(appt$Status, pred.forest)

# boosting
library(gbm)
set.seed(300)
appt$outcome <- as.integer(appt$Status == "Cancelled")
boost.appt <- gbm(outcome ~ . - MRN - Status, data = appt, 
                  distribution = "bernoulli", 
                  n.trees = 1000)
pred.boost <- predict(boost.appt, n.trees = 1000, type = "response")
table(appt$Status, pred.boost > 0.25)


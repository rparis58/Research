## from the MLalgorithms.pdf 
#
# Regression vs Classification....


# SUPERVISED LEARNING
# (target) dependent variable is predicted from a given set of predictors (indepedent vars)
# we generate a function that maps inputs to output(s)
# training process continues until the model achieves a desired level of accuracy on the training data
#
# Linear regresion
# Decision Tree
# kNN
# Random Forest
# Logistic Regression

# UNSUPERVISED LEARNING
# There is not (target) dependent variable to be predicted!
# Typically it is used to cluster population into different groups eg. customer segmentation for a specific intervention
#
# apriori algorithm
# k-means
# Hierarchical clustering

# REINFORCEMENT LEARNING
# Machine is trained to make specific decisions - exposed to an environment where it trains itself and tries to capture the 
# best possible knowledge to make accurate business decisions.
#
# Markov Decision Process
# Q Learning


# SEMI-SUPERVISED LEARNING
# (SSL package)



## LINEAR REGRESSION
##
## Simple and Multiple dependent variables.
# cost of house, number of calls, total sales etc
# Y = a * X + b
#
#Load Train and Test datasets 
#Identify feature and response variable(s) and 
#values must be numeric

# lets try this with the "faithful" data set
# {stats} package
eruption.lm = lm(eruptions ~ waiting, data = faithful)

# check the R squared and F-stat for goodness of fit
summary(eruption.lm)
plot(eruption.lm$residuals)   # make sure this is random and no correlations!

(coeffs <- coefficients(eruption.lm))

# calc time need to wait for next eruption from the coeffs
since_last = 80  # in minutes
(ttw <- coeffs[1] + coeffs[2] * since_last) # 4.17 minutes

newdata <- data.frame(waiting=80)  # wrap in a data.frame and call the predict function
(prediction <- predict(eruption.lm, newdata, interval = "confidence"))
#   fit      lwr      upr     95% confidence for the MEAN
# 4.17622 4.104848 4.247592

# predict the 95% confidence range for the specific data point (will be wider than the mean confidence)
(prediction_2 <- predict(eruption.lm, newdata, interval = "predict"))
#  fit      lwr      upr
# 4.17622 3.196089 5.156351

# lm demo from the {stats} package
# demo(lm.glm,package = "stats")

# How about QQPlot to verify that the residuals distribution



## LOGISTIC REGRESSION - for classification
##
# y = f(x) when y is a categorical variable
# note: predictors can be continuous, categorical or a mix of both
# at simplest this is binary ie binomial logistic regression  1 | 0
# and more complex is multinomial logistic regression eg entertaining|borderline|boring

# use the glm() function

# Go with the Titanic example http://www.r-bloggers.com/how-to-perform-a-logistic-regression-in-r/

x <- cbind(x_train,y_train)
#Train the model using the training sets and check score 
logistic <- glm(y_train ~ ., data = x,family='binomia1') 
summary(logistic) 

#Predict Output 
predicted <- predict[logistic,x_test)


## DECISION TREE
##
require(rpart) 
x <- cbind(x_train,y_train)

#grow tree 
rit <- rpart(y_train ~ ., data = x,method="class") 
summary{fit) 

#Predict Output 
predicted <- predict(fit,x_test)


## SVM SUPPORT VECTOR MACHINE
##
require(e1071) 
x <- cbind(x_train,y_train) 

#Fitting model 
fit <-svm(y_train ~ ., data = x) summary(fit) 

#Predict Output 
predicted <- predict(fit,x_test)


## NAIVE BAYES
##
require(1071) 
x <- cbind(x_train,y_train) 

#Fitting model 
fit <-naiveBayes(y_train ~ ., data = x) 
summary(fit) 

#Predict Output 
predicted <- predict(Fit,x_test)


## KNN  K-NEAREST NEIGHBOUR
##
require(knn)  
x <- cbind(x_train,y_train) 

#Fitting model 
fit <-knn(y_train ~ ., data = x,k=5) summary(tit) 

#Predict Output 
predicted <- predict(fit,x_test)


## K MEANS
##
require(cluster) 
fit <- kmeans(X, 3) 

#Predict Output 
predicted <- predict(fit,x_test)


## RANDOM FOREST
##
require(randomForest)
x <- cbind(x_train,y_train) 

#Fitting model 
fit <- randomForest(Species ~ ., x,ntree=566) 
summary(fit) 

#Predict Output 
predicted <- predict(fit,x_test)


## DIMENSIONALITY REDUCTION ALGORITHMS
##
## PCA
require(stats) 
pca <- princomp(train, cor = TRUE) 
train_reduced <- predict(pca,train) 
test_reduced <- predict(pca,test)


## GRADIENT BOOSTING & ADABOOST
require(caret) x <- cbind(x_train,y_train) 

#Fitting model
fitControl <- trainControl( method = "repeatedcv", + number = 4, repeats = 4) 
fit <- train(y ~ ., data = x, method = "gbm“, + trControl = fitControl,verbose = FALSE)
predicted <- predict(fit,x_test,type= "prob")[,2]

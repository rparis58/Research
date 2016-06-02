require(randomForest)

classify_iris <- function(x, na.fn) {
  cols <- c('Sepal.Length', 'Sepal.Width','Petal.Length', 'Petal.Width')
  x[,cols] <- apply(x[,cols], 2, function(z) ifelse(is.na(z), na.fn(z), z))
  x$Sepal.LW <- x$Sepal.Length / x$Sepal.Width
  x$Petal.LW <- x$Petal.Length / x$Petal.Width
  x$SP.Length <- x$Sepal.Length / x$Petal.Length
  x$SP.Width <- x$Sepal.Width / x$Petal.Width
  randomForest(Species ~ ., x)
}

# for example add some NA's into a copy of the iris dataset
iris1 <- iris
iris1[,1:4] <- apply(iris1[,1:4],2, 
                     function(x){x[sample( length(x),10)] <- NA
                     x
                     } )

classify_iris(iris1, function(x) mean(x,na.rm=TRUE ))

## new version that allows the model to be an input parameter to give better separation of duties
# get very cool when functions are including other functions for good separation of concerns
#
classify_iris <- function(x, na.fn, model=randomForest) {
  cols <- c('Sepal.Length', 'Sepal.Width','Petal.Length', 'Petal.Width')
  x[,cols] <- apply(x[,cols], 2, function(z) ifelse(is.na(z), na.fn(z), z))
  x$Sepal.LW <- x$Sepal.Length / x$Sepal.Width
  x$Petal.LW <- x$Petal.Length / x$Petal.Width
  x$SP.Length <- x$Sepal.Length / x$Petal.Length
  x$SP.Width <- x$Sepal.Width / x$Petal.Width
  model(Species ~ ., x)
}
classify_iris(iris1, function(x) mean(x,na.rm=TRUE))  # using the model=default randomForest

# use SVM for model from the kernlab package
require(kernlab)
classify_iris(iris1, function(x) mean(x,na.rm=TRUE), ksvm)




library(lubridate)
library(reshape2)
library(plotly)
require(rpart)

library(knitr)


opts_chunk$set(echo = F, tidy = F, results = 'asis', comment = NA, cache = T)
g <- read.csv("dat.csv", stringsAsFactors = F)

ggplot(g, aes(x = Age, y = Interact)) + geom_point(size = 1.75, colour = "blue")

g$AgeGroup <- NA

##Grouping the users based on age
g$AgeGroup[g$Age < 50] <- "< 50"
g$AgeGroup[g$Age >= 50] <- ">= 50"


mosaicplot(table(g$AgeGroup, g$Interact), shade = T, xlab = "AgeGroup", ylab = "Interact", main = "Mosaic Plot")

g$Interact <- as.factor(g$Interact)
ggplot(g, aes(AgeGroup)) + geom_bar(aes(fill = Interact))



formula <- Interact ~ Age

##Creating a decision tree
tree1 <- rpart(formula, data = g, control=rpart.control(minsplit=10,cp=0))

fancyRpartPlot(tree1)

g$AgeGroup.DT <- NA

### Grouping the users based on the classification from the decision tree
g$AgeGroup.DT[g$Age < 28] <- "< 28"
g$AgeGroup.DT[g$Age >= 28 & g$Age < 44] <- ">= 28 & < 44"
g$AgeGroup.DT[g$Age >= 44] <- ">= 44"
mosaicplot(table(g$AgeGroup.DT, g$Interact), shade = T, xlab = "AgeGroup", ylab = "Interact", main = "Mosaic Plot")

ggplot(g, aes(AgeGroup.DT)) + geom_bar(aes(fill = Interact))



x <- rnorm(100) # data
f <- ash1(bin1(x,nbin=50),5) # compute ash estimate
plot( f , type="l" ) # line plot of estimate

f <- ash1(bin1(g$Age))
plot(f, type = "l")
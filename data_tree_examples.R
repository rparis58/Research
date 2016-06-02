# vignette('application', package = "data.tree")

require(data.tree)

acme <- Node$new("Acme Inc.")
accounting <- acme$AddChild("Accounting")
software <- accounting$AddChild("New Software")
standards <- accounting$AddChild("New Accounting Standards")
research <- acme$AddChild("Research")
newProductLine <- research$AddChild("New Product Line")
newLabs <- research$AddChild("New Labs")
it <- acme$AddChild("IT")
outsource <- it$AddChild("Outsource")
agile <- it$AddChild("Go agile")
goToR <- it$AddChild("Switch to R")

print(acme)


require(treemap)
data(GNI2014)
head(GNI2014)

GNI2014$pathString <- paste("world", 
                            GNI2014$continent, 
                            GNI2014$country, 
                            sep = "/")

population <- as.Node(GNI2014)
print(population, "iso3", "population", "GNI", limit = 20)


## build a tree from a YAML file definition
library(yaml)
yaml <- "
name: OS Students 2014/15
OS X:
  Yosemite:
    users: 16
  Leopard:
    users: 43
Linux:
  Debian:
    users: 27
  Ubuntu:
    users: 36
Windows:
  W7:
    users: 31
  W8:
    users: 32
  W10:
    users: 4
"

osList <- yaml.load(yaml)
osNode <- as.Node(osList)
print(osNode, "users")

# QUERY THE TREE
print(population, limit = 15)
population$isLeaf
population$height
population$count
population$totalCount
population$fields
population$fieldsAll
population$averageBranchingFactor

# TRAVERSAL
sum(population$Get("population", filterFun = isLeaf))
population$Prune(pruneFun = function(x) !x$isLeaf || x$population > 1000000)
sum(population$Get("population", filterFun = isLeaf), na.rm = TRUE)

# CONVERSION
popClone <- Clone(acme)
as.data.frame(acme)
ToDataFrameNetwork(acme)

# ADD CUSTOM FIELDS
software$cost <- 1000000
standards$cost <- 500000
newProductLine$cost <- 2000000
newLabs$cost <- 750000
outsource$cost <- 400000
agile$cost <- 250000
goToR$cost <- 50000

software$p <- 0.5
standards$p <- 0.75
newProductLine$p <- 0.25
newLabs$p <- 0.9
outsource$p <- 0.2
agile$p <- 0.05
goToR$p <- 1
print(acme, "cost", "p")


birds <- Node$new("Aves", vulgo = "Bird")
birds$AddChild("Neognathae", vulgo = "New Jaws", species = 10000)
birds$AddChild("Palaeognathae", vulgo = "Old Jaws", species = 60)
print(birds, "vulgo", "species")

birds$species <- function(self) sum(sapply(self$children, function(x) x$species))
print(birds, "species")


# PLOT
plot(acme)

SetGraphStyle(acme, rankdir = "TB")
SetEdgeStyle(acme, arrowhead = "vee", color = "grey35", penwidth = 2)
SetNodeStyle(acme, style = "filled,rounded", shape = "box", fillcolor = "GreenYellow", 
             fontname = "helvetica", tooltip = GetDefaultTooltip)
SetNodeStyle(acme$IT, fillcolor = "LightBlue", penwidth = "5px")
plot(acme)

plot(as.dendrogram(CreateRandomTree(nodes = 20)), center = TRUE)

## extracts from "Data Mining Algorithms in R" pdf

## PCA

require(graphics)
summary(pc.cr <- princomp(USArrests, cor=TRUE))
loadings(pc.cr)
plot(pc.cr)
biplot(pc.cr)


require(irlba)  # super efficient matrix operations
set.seed(1)
A <- matrix(runif(200),nrow=20)
P <- irlba(A, nv=1, center=colMeans(A))
cbind(P$v, prcomp(A)$rotation[,1])

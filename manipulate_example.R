# http://www.r-bloggers.com/manipulated-regression/
#

require(manipulate)

## First define a custom function that fits a linear regression line 
## to (x,y) points and overlays the regression line in a scatterplot.
## The plot is then 'manipulated' to change as y values change.

linregIllustrate <- function(x, y, e, h.max, h.med){
  max.x <- max(x)
  med.x <- median(x)
  max.xind <- which(x == max.x)
  med.xind <- which(x == med.x)
  
  y1 <- y     ## Modified y
  y1[max.xind] <- y1[max.xind]+h.max  ## at the end
  y1[med.xind] <- y1[med.xind]+h.med  ## at the center
  plot(x, y1, xlim=c(min(x),max(x)+5), ylim=c(min(y1),max(y1)), pch=16, 
       xlab="X", ylab="Y")
  text(x[max.xind], y1[max.xind],"I'm movable!", pos=3, offset = 0.3, cex=0.7, font=2, col="red")
  text(x[med.xind], y1[med.xind],"I'm movable too!", pos=3, offset = 0.3, cex=0.7, font=2, col="red")
  
  m <- lm(y ~ x)  ## Regression with original set of points, the black line
  abline(m, lwd=2)
  
  m1 <- lm(y1 ~ x)  ## Regression with modified y, the dashed red line
  abline(m1, col="red", lwd=2, lty=2)
}

## Now generate some x and y data 
x <- rnorm(35,10,5)
e <- rnorm(35,0,5)
y <- 3*x+5+e

## Plot and manipulate the plot!
manipulate(linregIllustrate(x, y, e, h.max, h.med), 
           h.max=slider(-100, 100, initial=0, step=10, label="Move y at the end"), 
           h.med=slider(-100, 100, initial=0, step=10, label="Move y at the center"))
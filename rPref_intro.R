require(rPref)
require(dplyr)
require(igraph)
require(ggplot2)

# Calculate Skyline 
sky1 <- psel(mtcars, high(mpg) * high(hp)) 

## PLOT BASIC SKYLINE
# Plot mpg and hp values of mtcars and highlight the skyline 
ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point(shape = 21) +   geom_point(data = sky1, size = 3)

# Consider again the preference from above 
p <- high(mpg) * high(hp) 

# Calculate the level-value w.r.t. p by using top-all 
res <- psel(mtcars, p, top = nrow(mtcars)) 

# Visualize the level values by the color of the points 
gp <- ggplot(res, aes(x = mpg, y = hp, color = factor(.level))) + 
  geom_point(size = 3) 
gp 

# Add the steps
gp + geom_step(direction = "vh")


## GROUPED SKYLINE

# Get grouped data set using dplyr
df <- group_by(mtcars, cyl)  # group by number of cylinders

# Calculate Grouped Skyline
sky2 <- psel(df, high(mpg) * high(hp))

summarise(sky2, skyline_size = n())

ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point(shape = 21) + 
  geom_point(aes(color = factor(sky2$cyl)), sky2, size = 3)

## Better-Than-Graph
# Pick a small data set and create preference / BTG 
df <- mtcars[1:8,] 
pref <- high(mpg) * low(wt) 
btg <- get_btg(df, pref) 


# Create labels for the nodes containing relevant values 
labels <- paste0(df$mpg, "\n", df$wt)

library(igraph)
plot(btg$graph, layout = btg$layout, vertex.label = labels,
     vertex.size = 25, vertex.color = "white")

## Shiny Examples
require(shiny)
runUrl("http://p-roocks.de/rpref/rpref_level_demo.zip")
runUrl("http://p-roocks.de/rpref/rpref_topk_demo.zip")
runUrl("http://p-roocks.de/rpref/rpref_score_demo.zip")
runUrl("http://p-roocks.de/rpref/rpref_separation_demo.zip")


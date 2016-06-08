require(purrr)
mtcars %>%
    split(.$cyl) %>%
    map( ~ lm(mpg ~ wt, data = .)) %>%
    map(summary) %>%
    map_dbl("r.squared")

randomgroup <- function(n, probs) {
  probs <- probs / sum(probs)
  g <- findInterval(seq(0, 1, length = n), c(0, cumsum(probs)),
                    rightmost.closed = TRUE)
  names(probs)[sample(g)]
}
partition <- function(df, n, probs) {
  replicate(n, split(df, randomgroup(nrow(df), probs)), FALSE) %>%
    zip() %>%
    as.data.frame()
}

msd <- function(x, y) sqrt(mean((x - y) ^ 2))

# Generate 100 random test-training splits
boot <- partition(mtcars, 100, c(test = 0.8, training = 0.2))
boot

boot <- boot %>% mutate(
  # Fit the models
  models = map(training, ~ lm(mpg ~ wt, data = mtcars)),
  # Make predictions on test data
  preds = map2(models, test, predict),
  diffs = map2(preds, test %>% map("mpg"), msd)
)

# Evaluate mean-squared difference between predicted and actual
mean(unlist(boot$diffs))

# Some data
nvars <- 10000
nsamples <- 500
sample_groups <- 5
MAT <- replicate(nvars, runif(n=nsamples))

# And a grouping vector:

f <- rep_len(1:sample_groups, nsamples)
f <- LETTERS[f]

# Settings
aggr_FUN  <- mean
combi_FUN <- function(x,y) "/"(x,y) 

# helper function
pasteC <- function(x,y) paste(x,y,sep=" - ")

# aggregate
system.time({
  temp2 <- aggregate(. ~ class, data = cbind.data.frame(class=f,MAT), aggr_FUN)
})

# reshape2
library(reshape2)
system.time({
  temp3 <- recast(data.frame(class=f,MAT),class ~ variable,id.var="class",aggr_FUN)
})

# purrr 
library(purrr)
system.time({
  tmp <- data.frame(class = f, MAT) %>%
    slice_rows("class") %>%
    by_slice(map, aggr_FUN)
})

tmp[,1:10]
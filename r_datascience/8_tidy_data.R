require(condvis)
data(mtcars)
m <- lm(mpg ~ wt + hp, data = mtcars)
ceplot(data = mtcars, model = m, S = "hp")
fit <- lm(mpg ~ wt + qsec, mtcars)
summary(fit)

require(broom)
tidy(fit)

augment(fit) %>% head()

library(ggplot2)
td <- tidy(fit, conf.int = TRUE)
ggplot(td, aes(estimate, term, color = term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  geom_vline()

require(dplyr)
mtcars %>% group_by(am) %>% do(tidy(lm(mpg ~ wt, .)))

library(glmnet)
set.seed(03-19-2015)

# generate data with 5 real variables and 45 null, on 100 observations
nobs <- 100
nvar <- 50
real <- 5
x <- matrix(rnorm(nobs * nvar), nobs)
beta <- c(rnorm(real, 0, 1), rep(0, nvar - real))
y <- c(t(beta) %*% t(x)) + rnorm(nvar, sd = 3)

glmnet_fit <- cv.glmnet(x,y)

tidied_cv <- tidy(glmnet_fit)
glance_cv <- glance(glmnet_fit)

ggplot(tidied_cv, aes(lambda, estimate)) + geom_line(color = "red") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2) +
  scale_x_log10() +
  geom_vline(xintercept = glance_cv$lambda.min) +
  geom_vline(xintercept = glance_cv$lambda.1se, lty = 2)

library(survival)

surv_fit <- survfit(coxph(Surv(time, status) ~ age + sex, lung))

td <- tidy(surv_fit)
ggplot(td, aes(time, estimate)) + geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2)
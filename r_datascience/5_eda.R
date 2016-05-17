setwd("~/Projects/R/Research/r_datascience")

# categorical variable
table(diamonds$cut)  # gives a count against each factor category
ggplot(data=diamonds) + geom_bar(mapping= aes(x=cut, fill=cut))

# compare the distributions of different subgroups in a bar chart
ggplot(data=diamonds) + geom_bar(mapping = aes(x=cut,y=..prop..,group = carat > 1, fill= carat > 1), position="dodge")

ggplot(data=diamonds, mapping=aes(x=carat)) + geom_histogram(binwidth=0.1)

ggplot(data=diamonds) + geom_histogram(aes(x=price,fill=cut))

ggplot(data=diamonds) + geom_freqpoly(aes(x=carat))
ggplot(data=diamonds) + geom_density(aes(x=carat))

# bring density into this
ggplot(data=diamonds) + geom_freqpoly(aes(x=price,y=..density..,color=cut))
ggplot(data=diamonds) + geom_density(aes(x=price,color=cut))
ggplot(diamonds,aes(x=carat)) + geom_density(aes(color=cut),kernel="optcosine",adjust=4)

# summarise and group_by
diamonds %>%
  summarise(mean = mean(price, na.rm=TRUE), sd=sd(price,na.rm=TRUE))

diamonds %>%
  group_by(cut) %>%
  summarise(mean = mean(price, na.rm = TRUE),
            sd = sd(price, na.rm  = TRUE))

# some economics examples
ggplot(data=economics) + geom_point(aes(x=date,y=unemploy))
ggplot(data=economics) + geom_line(aes(x=date,y=unemploy))

ggplot(data=economics[1:150, ]) + geom_step(aes(x=date,y=unemploy))

# geom_rug
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  geom_rug(mapping = aes(x = displ, y = hwy), position = "jitter")

ggplot(data = diamonds) +
  geom_point(mapping = aes(x = carat, y = price)) +
  geom_smooth(mapping = aes(x = carat, y = price), method = lm)

ggplot(data = diamonds) +
  geom_point(mapping = aes(x = carat, y = price)) +
  geom_smooth(mapping = aes(x = carat, y = price), method = lm, formula = y ~ poly(x,4))

# check correlation between carat
cor(diamonds$carat,diamonds$price) #0.92
cor(diamonds$carat, diamonds$depth)  #0.02

ggplot(data = diamonds) +
  geom_point(mapping = aes(x = carat, y = price)) +
  geom_quantile(mapping = aes(x = carat, y = price),
                quantiles = c(0.05, 0.5, 0.95), 
                formula = y ~ poly(x, 2))

ggplot(data = diamonds) +
  geom_bin2d(mapping = aes(x = carat, y = price), binwidth = c(0.1, 500)

ggplot(data = diamonds) + geom_hex(mapping = aes(x = carat, y = price), binwidth = c(0.1, 500))

ggplot(data = diamonds) +
  geom_point(mapping = aes(x = carat, y = price)) +
  geom_density2d(mapping = aes(x = carat, y = price))
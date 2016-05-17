demo <- data.frame(
  a = c("bar_1","bar_2","bar_3"),
  b = c(20,30,40)
)
ggplot(data=demo,mapping=aes(x=a,y=b)) + geom_bar(stat="identity",width = 1)

ggplot(data=diamonds,aes(x=cut,y=clarity)) + geom_count(aes(size=..prop..,group=clarity))

ggplot(data=diamonds,aes(x=cut,fill=cut)) + geom_bar(width=1.0) + coord_polar()

ggplot(data=diamonds) + geom_bar(mapping=aes(x=factor(1),fill=cut),width=1) + coord_polar(theta = "y")
       
ggplot(data=diamonds) + geom_bar(mapping = aes(x=cut, fill=cut), width = 1) + coord_polar() + facet_grid(. ~clarity)

ggplot(data=mpg) + geom_point(aes(x=displ,y=hwy)) + facet_wrap(~ class)

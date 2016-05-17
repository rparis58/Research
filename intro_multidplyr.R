#https://github.com/hadley/multidplyr/blob/master/vignettes/multidplyr.md

require(dplyr)
require(multidplyr)
require(nycflights13)

flights1 <- partition(flights, flight)
#> Initialising 7 core cluster.
flights2 <- summarise(flights1, dep_delay = mean(dep_delay, na.rm = TRUE))
flights3 <- collect(flights2)

system.time({
  flights %>% 
    partition() %>%
    summarise(mean(dep_delay, na.rm = TRUE)) %>% 
    collect()
})
# slow due to overhead at this scale.  needs more rows and more shards
#user  system elapsed 
#0.449   0.081   0.800

system.time({
  flights %>% 
    group_by() %>%
    summarise(mean(dep_delay, na.rm = TRUE))
})
#user  system elapsed 
#0.006   0.001   0.006

common_dest <- flights %>%
  count(dest) %>%
  filter(n >= 365) %>%
  semi_join(flights, .) %>% 
  mutate(yday = lubridate::yday(ISOdate(year, month, day)))
#> Joining by: "dest"
dim(common_dest)

cluster <- create_cluster(2)
cluster

set_default_cluster(cluster)
by_dest <- common_dest %>% 
  partition(dest, cluster = cluster)
by_dest

cluster_library(by_dest, "mgcv")
system.time({
  models <- by_dest %>% 
    do(mod = gam(dep_delay ~ s(yday) + s(dep_time), data = .))
})

system.time({
  models <- common_dest %>%
    group_by(dest) %>%
    do(mod = gam(dep_delay ~ s(yday) + s(dep_time), data = .))
})

# cluster <- create_cluster(4)
# cluster_assign_each(cluster, "filename",
#                     list("a.csv", "b.csv", "c.csv", "d.csv")
# )
# cluster_assign_expr(cluster, "my_data", readr::read_csv(filename))
# 
# my_data <- src_cluster(cluster) %>% tbl("my_data")



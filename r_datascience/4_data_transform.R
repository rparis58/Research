#options(scipen=999)

require(dplyr)
require(nycflights13)
flights

delays %>% 
  filter(n > 20) %>% 
  ggplot(aes(n,delay)) + geom_point()

# LAHMAN package for Baseball statistics
require(Lahman)

## create the "batters" tibble
batting <- tbl_df(Lahman::Batting)
batters <- batting %>%
  group_by(playerID) %>%
  summarise(
    ba = sum(H) / sum(AB),
    ab = sum(AB)
  )

# plot batters
batters %>%
  filter(ab > 100) %>%
  ggplot(aes(ab,ba)) + 
    geom_point() + 
    geom_smooth(se = FALSE)

## Before of averages that are not indicative!
not_cancelled %>%
  group_by(year, month, day) %>%
  summarise(
    avg_delay1 = mean(arr_delay),
    avg_delay2 = mean(arr_delay[arr_delay > 0])
  )

## Std Dev, InterQuartile Range, median absolute deviation
# why is distance to some destinations more variable than others?
not_cancelled %>% 
  group_by(dest) %>%
  summarise(distance_sd = sd(distance)) %>%
  arrange(desc(distance_sd))

not_cancelled %>%
  group_by(year,month,day) %>%
  summarise(
    first = min(dep_time),
    last = max(dep_time)
  )

not_cancelled %>%
  group_by(year,month,day) %>%
  mutate(r = rank(desc(dep_time))) %>%
  filter(r %in% c(1,n())) %>%
  select(year,month,day, dep_time, dep_delay, r)


# Which destinations have the most carriers?
not_cancelled %>% 
  group_by(dest) %>% 
  summarise(carriers = n_distinct(carrier)) %>% 
  arrange(desc(carriers))

# using the count() function
not_cancelled %>% count(dest) %>% arrange(desc(n))

#how many flights left before 5am?  - indicate delayed flight from previous day
not_cancelled %>%
  group_by(year,month,day) %>%
  summarise(n_early = sum(dep_time < 500))

# what proportion of flights are delayed by more than an hour
not_cancelled %>%
  group_by(year,month,day) %>%
  summarise(hour_perc = mean(arr_delay > 60, na.rm = TRUE))

# Grouping by multiple variables
daily <- group_by(flights, year, month, day)
(per_day <- summarise(daily, flights = n())) %>%
  ggplot(aes(x=flights)) + geom_histogram(binwidth = 10) +geom_smooth(inherit.aes = TRUE)

# Ungrouping
daily %>%
  ungroup() %>%
  summarise(flights = n())
 
# Grouped mutates (and filters)
flights %>%
  group_by(year, month,day) %>%
  filter(rank(arr_delay) < 10)

# introduce "group_by" structure into the data set
(popular_dests <- flights %>%
  group_by(dest) %>%
  filter( n() > 365 ))

# ...then add prop_delay 
popular_dests %>%
  filter(arr_delay > 0) %>%
  mutate(prop_delay = arr_delay / sum(arr_delay) * 100) %>%
  select(1:9,prop_delay)
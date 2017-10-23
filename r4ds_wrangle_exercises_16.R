library(tidyverse)

library(lubridate)
library(nycflights13)

#################
#16.2.4
#################

#1
ymd(c("2010-10-10", "bananas"))
# Above gives an NA and a warning message saying that it failed to parse. 

#2
# The tzone argument of today() will return the date for a specific timezone. It's important because different time zones have different dates.

#3
d1 <- "January 1, 2010"
d2 <- "2015-Mar-07"
d3 <- "06-Jun-2017"
d4 <- c("August 19 (2015)", "July 1 (2015)")
d5 <- "12/30/14" # Dec 30, 2014

mdy(d1)
ymd(d2)
dmy(d3)
mdy(d4)
mdy(d5)


#################
#16.3.4
#################

#1

# Stolen from the r4ds book
make_datetime_100 <- function(year, month, day, time) {
  make_datetime(year, month, day, time %/% 100, time %% 100)
}

flights_dt <- flights %>% 
  filter(!is.na(dep_time), !is.na(arr_time)) %>% 
  mutate(
    dep_time = make_datetime_100(year, month, day, dep_time),
    arr_time = make_datetime_100(year, month, day, arr_time),
    sched_dep_time = make_datetime_100(year, month, day, sched_dep_time),
    sched_arr_time = make_datetime_100(year, month, day, sched_arr_time)
  ) %>% 
  select(origin, dest, ends_with("delay"), ends_with("time"))


# Now my part. Looking at this by month. 
mutate(flights_dt, time = hour(dep_time) * 100 + minute(dep_time),
       mon = as.factor(month(dep_time))) %>%
  ggplot(aes(x = time, group = mon, color = mon)) +
  geom_freqpoly(binwidth = 100)
# I know this ins't normalized to the number of days in a month, so Feb is a little artificially low
# However, it doesn't look like the time of day of flights changes much by month

#2

# dep_time should equal sched_dep_time + dep_delay, right?
mutate(flights_dt, my_dep_time = sched_dep_time + (dep_delay * 60),
               error = my_dep_time - dep_time) %>%
  select(my_dep_time, dep_time, sched_dep_time, dep_delay, error) %>%
  ggplot(aes(x = error)) + geom_bar(bins = 10)

# Looks like the vast majority are accurate, but there are some inconsistencies.

#3

View(mutate(flights_dt, error = arr_time - dep_time - air_time))

# Looks like departure and arrival times are in the local time zone of the aiport. That will cause an increase in the error metric above

#4

# I'll use sched_dep_time because that is most relevent when scheduling a flight
mutate(flights_dt, sched_dep_time_hour = hour(sched_dep_time)) %>%
  group_by(sched_dep_time_hour) %>% 
  summarise(avg_delay = mean(dep_delay)) %>%
  ggplot(aes(x = sched_dep_time_hour, y = avg_delay)) +
  geom_smooth()

# Delays definitely get worse later in the day

#5
mutate(flights_dt, DoW = wday(sched_dep_time)) %>%
  group_by(DoW) %>% 
  summarise(avg_dep_delay = mean(dep_delay),
            avg_arr_delay = mean(arr_delay, na.rm = TRUE)) 

# Fly on Saturdays to reduce delays

#6

ggplot(diamonds, aes(x = carat)) + 
  geom_density()

# Both favor nice numbers

#7

mutate(flights_dt, early = dep_delay < 0,
       minute = minute(sched_dep_time) %% 10) %>%
  group_by(minute) %>%
  summarise(early = mean(early)) %>%
  ggplot(aes(x = minute, y = early)) +
  geom_point()

# Confirmed http://i.dailymail.co.uk/i/pix/2016/03/18/15/324D202500000578-3498922-image-a-33_1458315465874.jpg


#################
#16.4.5
#################

#1

# There is no transition of months into seconds because of the variable number of days in a month


#2

# overnight is either TRUE or FALSE, aka 1 or 0.
# days(1) or days(0) will return either 1 day or nothing

#3

ymd(20150101) + months(0:11)
floor_date(today(), unit = 'year') + months(0:11)

#4

as.duration(today() - ymd(19911011))

#5

today() %--% (today() + years(1)) / months(1)
# ^Put '%' around the division
today() %--% (today() + years(1)) %/% months(1)
  
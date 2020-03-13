# This script tests if behavior counts differ between day and night.
# Kathryn Busby
# 11-27-2019
# mkbusby@email.arizona.edu

# Load libraries.

library(tidyverse)
library(lubridate)

# Bring in the data.

data <- read.csv(file="[Insert your path to Behavior Census Totals.csv here",
                 header = TRUE,
                 sep = ",")

# Clean up data: convert year/month/date/time to date.time. Retain day.night column.
# Select date.time, day.night, and counts for each behavior.
# In this case, it's valid to convert NAs to zeros. They just represent the absence
# of that particular behavior in the total count for that timestamp, so omitting them will not
# exclude data.

data$time <- hm(data$time)
t.data <- data %>%
  mutate(datetime=make_datetime(year, month, date))
t.data$datetime <- paste(data$date, data$time)
t.data <- t.data %>%
  select(datetime, behavior, total.beh, proportion, day.night, abdomen.dv.count, cleaner.count, feeder.count, heater.count)

# Split the data frame into two variables that can be used to index to day and
# night for comparison.

day <- t.data %>%
  filter(day.night == "day")

night <- t.data %>%
  filter(day.night == "night")

# Visualize the distribution using histograms.

hist(day$abdomen.dv.count)
hist(night$abdomen.dv.count)
hist(day$cleaner.count)
hist(night$cleaner.count)
hist(day$feeder.count)
hist(night$feeder.count)
hist(day$heater.count)
hist(night$heater.count)

# Use Kolmogorov-Smirnov tests to compare between each behavior except for feeders.
# For feeders there was not a large enough sample size to compare counts, and they were all at night.
# Use the absolute number of each behavior. Do 3 tests of absolute counts.

ks.test(x=day$total.beh[which(t.data$behavior=="abdomen dv")],
           y=night$total.beh[which(t.data$behavior=="abdomen dv")])
ks.test(x=day$total.beh[which(t.data$behavior=="cleaner")],
           y=night$total.beh[which(t.data$behavior=="cleaner")])
ks.test(x=day$total.beh[which(t.data$behavior=="heater")],
           y=night$total.beh[which(t.data$behavior=="heater")])

# Compare between each behavior except for feeders.
# Use the proportion of each behavior instead of a total.
# Do 3 tests of proportions.

ks.test(x=day$proportion[which(t.data$behavior=="abdomen dv")],
           y=night$proportion[which(t.data$behavior=="abdomen dv")])
ks.test(x=day$proportion[which(t.data$behavior=="cleaner")],
           y=night$proportion[which(t.data$behavior=="cleaner")])
ks.test(x=day$proportion[which(t.data$behavior=="heater")],
           y=night$proportion[which(t.data$behavior=="heater")])

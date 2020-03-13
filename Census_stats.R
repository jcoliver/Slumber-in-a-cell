# This script tests for differences between behavior counts in a census.
# Kathryn Busby
# 11-27-2019
# mkbusby@email.arizona.edu

# Load libraries.

library(tidyverse)
library(lubridate)

# Bring in the data.

data <- read.csv(file="[Insert your path to Census2.csv here]",
                 header = TRUE,
                 sep = ",")

# Clean up data: convert year/month/date/time to date.time.
# Select date.time, behavior, and counts for each behavior.
# In this case, it's valid to convert NAs to zeros. They just represent the absence
# of that particular behavior in the total count for that timestamp, so omitting them will not
# exclude data.

data$time <- hm(data$time)
t.data <- data %>%
  mutate(datetime=make_datetime(year, month, date))
t.data$datetime <- paste(data$date, data$time)
t.data <- t.data %>%
  select(datetime, behavior, total.beh, proportion, abdomen.dv.count, cleaner.count, feeder.count, heater.count)

# Visualize the distribution using histograms.

hist(t.data$abdomen.dv.count)
hist(t.data$cleaner.count)
hist(t.data$feeder.count)
hist(t.data$heater.count)

# Calculate mean percentages of bees inside cells performing each behavior over the course of all censuses.

# Mean percent sleeping
mean(t.data$proportion[which(t.data$behavior == "abdomen dv")]) * 100
# Mean percent cleaning
mean(t.data$proportion[which(t.data$behavior == "cleaner")]) * 100
# Mean percent feeding
mean(t.data$proportion[which(t.data$behavior == "feeder")]) * 100
# Mean percent heating
mean(t.data$proportion[which(t.data$behavior == "heater")]) * 100

# Use Kruskal-Wallis tests to compare between behaviors.
# Use the absolute number of each behavior.

kruskal.test(total.beh ~ behavior, data=data)

# Use Kruskal-Wallis tests to compare between behaviors.
# Use the proportion of each behavior instead of a total.

kruskal.test(proportion ~ behavior, data=t.data)

# MWW on each pairwise combo.

wilcox.test(t.data$proportion[which(t.data$behavior == "abdomen dv")],
            t.data$proportion[which(t.data$behavior == "cleaner")])
wilcox.test(t.data$proportion[which(t.data$behavior == "abdomen dv")],
            t.data$proportion[which(t.data$behavior == "feeder")])
wilcox.test(t.data$proportion[which(t.data$behavior == "abdomen dv")],
            t.data$proportion[which(t.data$behavior == "heater")])
wilcox.test(t.data$proportion[which(t.data$behavior == "feeder")],
            t.data$proportion[which(t.data$behavior == "cleaner")])
wilcox.test(t.data$proportion[which(t.data$behavior == "heater")],
            t.data$proportion[which(t.data$behavior == "cleaner")])
wilcox.test(t.data$proportion[which(t.data$behavior == "heater")],
            t.data$proportion[which(t.data$behavior == "feeder")])

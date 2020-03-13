# Tests for temperature differences between behaviors, and to account for effect of duration in cell on temperature.
# Kathryn Busby
# 11-19-2019
# mkbusby@email.arizona.edu

# Bring in data and packages:

library(ggplot2)
library(dplyr)
library(tidyverse)

mydata <- read.csv(file = "[Insert path to Temperatures.csv here]",
                   header = TRUE,
                   sep = ",")

mydata <- mydata[1:19, c(1:8,17)]

# Make column names a bit neater, and exclude a few unnecessary ones.

tidydf <- mydata %>%
  select(Bee, Behavior, Mean.Tbody, Mean.Tsurr, DeltaT, DurinCell)

# Dig out the individual behaviors now as separate objects.

dv.df <- tidydf %>%
  filter(Behavior == "abdomen dv")
cleaner.df <- tidydf %>%
  filter(Behavior == "cleaner")
feeder.df <- tidydf %>%
  filter(Behavior == "feeder")
heater.df <- tidydf %>%
  filter(Behavior == "heater")

# Sample sizes are all small here. That means it's appropriate to use a non-
# parametric test. wilcox.test ought to work here.

# Start by looking to see if any of the behavior temps are different from the
# surroundings.

wilcox.test(dv.df$Mean.Tbody, dv.df$Mean.Tsurr, data=dv.df) #NS
wilcox.test(cleaner.df$Mean.Tbody, cleaner.df$Mean.Tsurr, data=cleaner.df) #NS
wilcox.test(feeder.df$Mean.Tbody, feeder.df$Mean.Tsurr, data=feeder.df) #NS
wilcox.test(heater.df$Mean.Tbody, heater.df$Mean.Tsurr, data=heater.df) #Significant

# What about differences between behaviors?
# This would be a time to use Kruskal-Wallis.

kruskal.test(Mean.Tbody ~ Behavior, data=tidydf)

# MWW on each pairwise combo.

wilcox.test(dv.df$Mean.Tbody, cleaner.df$Mean.Tbody)
wilcox.test(dv.df$Mean.Tbody, feeder.df$Mean.Tbody)
wilcox.test(dv.df$Mean.Tbody, heater.df$Mean.Tbody)
wilcox.test(feeder.df$Mean.Tbody, cleaner.df$Mean.Tbody)
wilcox.test(heater.df$Mean.Tbody, cleaner.df$Mean.Tbody)
wilcox.test(heater.df$Mean.Tbody, feeder.df$Mean.Tbody)

# Does the amount of time spent doing a behavior predict temperature differences from surroundings?
# Do a linear regression to see.

plot(x = tidydf$DurinCell, y = tidydf$DeltaT, abline(lm(formula = DeltaT ~ DurinCell, data = tidydf)))

lm.dur.T <- lm(formula = DeltaT ~ DurinCell, data = tidydf)
summary(lm.dur.T)

# What's the range of durations of behaviors?

range(as.numeric(tidydf$DurinCell))

# Just to verify summary stats:

# Mean temperature differences of dv: -0.014
mean(dv.df$DeltaT)
# +/- standard deviation: .211
sd(dv.df$DeltaT)
# n=5
length(dv.df$DeltaT)

# Mean temperature difference of feeders: .555
mean(feeder.df$DeltaT)
# +/- standard deviation .0636
sd(feeder.df$DeltaT)
# n=2
length(feeder.df$DeltaT)

# Mean temperature difference of cleaners: -0.131
mean(cleaner.df$DeltaT)
# +/- standard deviation .14
sd(cleaner.df$DeltaT)
# n=3
length(cleaner.df$DeltaT)

# Mean temperature difference of heaters: 2.611
mean(heater.df$DeltaT)
# +/- standard deviation: 1.1386
sd(heater.df$DeltaT)
# n=9
length(heater.df$DeltaT)

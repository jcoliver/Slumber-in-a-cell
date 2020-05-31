# The following code includes all scripts executed in the analysis and
# visualization of Slumber in a cell.
# Some scripts generate output files, while others display results inline.
# In each script, the corresponding data file is indicated.
# Kathryn Busby
# 03-15-2020
# mkbusby@email.arizona.edu

#===============================================================================
#===============================================================================

# Contents:
# Perform basic summary statistics on event separations, both less than and
# greater than 10s.
# Test for differences between behavior counts in a census.
# Test if behavior counts differ between day and night.
# Figure 5: Make a stacked boxplot showing the proportions of behaviors at any
# given time.
# Figure 6: Make a plot of all bee gaster ventilation event times in seconds.
# Figure 7a: Make histograms of event separations.
# Figure 7b (bottom): Make boxplots of the separations between gaster pumps,
# sorted by behavior.
# Figure 7b (top): Make boxplots of the separations between gaster pumps, sorted
# by behavior.
# Figure 8: Make a plot showing the difference between thorax and surrounding
# temperature.
# Look for differences in pulse separations between behaviors.
# Conduct a linear mixed effects regression to predict pulse separations.
# Test for temperature differences between behaviors, and for effect of duration
# in cell on temperature.

#===============================================================================
#===============================================================================

# This script performs basic summary statistics on event separations, both less
# than and greater than 10s.
# Kathryn Busby
# 02/12/2020
# mkbusby@email.arizona.edu

# Bring in Inside Cells data:

mydata <- read.csv(file = "[Insert path to Vent_times.csv here]",
                   header = TRUE,
                   sep = ",")

# There will be two dataframes after the next few lines. mydata will contain all
# rows of relevant columns.
# newdata will omit lines containing NAs. This can be applied as appropriate for
# each question, but user should be aware that if NAs are omitted, there will be
# a lot of missing bout interims because of the filter applied to cull out
# isolated clicks.

# Note: Don't re-execute the following lines without bringing in the file afresh.
# If you do, you will eliminate the wrong columns, or it may just not execute
# the line.
# Also note: column LBB does NOT select everything greater than 10 seconds.
# Rather, it eliminates isolated
# clicks. In other words, user must apply an additional filter to
# look between bouts.

mydata <- mydata[,c(1,2,5,6,8)]
newdata <- na.omit(mydata)
colnames(newdata) <- c("Behavior", "Bee", "EvTimeby3", "EvSepby3", "LBB")
colnames(mydata) <- c("Behavior", "Bee", "EvTimeby3", "EvSepby3", "LBB")

summary(newdata$EvSepby3)
summary(newdata)

mean(newdata$LBB)

# Having the overall mean, it would be good to index to each behavior and get
# stats like mean, med., mode.
# First index by behavior.
# For looking within bouts, we don't want to exclude NAs, so reference mydata
# instead of newdata.
# The next lines create objects that index to each behavior within the data
# frame, so we can easily do stats on just that behavior.
# The last line will index to all behaviors EXCEPT dv.

heaters <- mydata$EvSepby3 [which(mydata$Behavior == "heater")]
feeders <- mydata$EvSepby3 [which(mydata$Behavior == "feeder")]
cleaners <- mydata$EvSepby3 [which(mydata$Behavior == "cleaner")]
dv <- mydata$EvSepby3 [which(mydata$Behavior == "abdomen dv")]
non.dv <- mydata$EvSepby3 [-which(mydata$Behavior == "abdomen dv")]

# The following set references LBB to look between bouts, and should exclude
# NAs. So reference newdata.

heaters.LBB <- newdata$LBB[which(newdata$Behavior == "heater")]
feeders.LBB <- newdata$LBB[which(newdata$Behavior == "feeder")]
cleaners.LBB <- newdata$LBB[which(newdata$Behavior == "cleaner")]
dv.LBB <- newdata$LBB[which(newdata$Behavior == "abdomen dv")]
non.dv.LBB <- newdata$LBB[-which(newdata$Behavior == "abdomen dv")]

# Now get stats for between bouts in each group. There's now a filter to select
# only those within bouts
# (less than 1 second). We need mean, median, mode, sd, se, IQR.
# For this next set, because we are looking within bouts, we do NOT want to
# exclude NAs.

mean(heaters[which(heaters<=1000)])
mean(feeders[which(feeders<=1000)])
mean(cleaners[which(cleaners<=1000)])
mean(dv[which(dv<=1000)])
mean(non.dv[which(non.dv<=1000)])

# Means between bouts:
mean(heaters.LBB[which(heaters.LBB>=10000)])
mean(feeders.LBB[which(feeders.LBB>=10000)])
mean(cleaners.LBB[which(cleaners.LBB>=10000)])
mean(dv.LBB[which(dv.LBB>=10000)])
mean(non.dv.LBB[which(non.dv.LBB>=10000)])

# Medians within bouts:
median(heaters[which(heaters<=1000)])
median(feeders[which(feeders<=1000)])
median(cleaners[which(cleaners<=1000)])
median(dv[which(dv<=1000)])
median(non.dv[which(non.dv<=1000)])

# Medians between bouts:
median(heaters.LBB[which(heaters.LBB>=10000)])
median(feeders.LBB[which(feeders.LBB>=10000)])
median(cleaners.LBB[which(cleaners.LBB>=10000)])
median(dv.LBB[which(dv.LBB>=10000)])
median(non.dv.LBB[which(non.dv.LBB>=10000)])

# SDs between bouts:
sd.heat <- sd(heaters.LBB[which(heaters.LBB>=10000)])
sd.heat
sd.feed <- sd(feeders.LBB[which(feeders.LBB>=10000)])
sd.feed
sd.clean <- sd(cleaners.LBB[which(cleaners.LBB>=10000)])
sd.clean
sd.dv <- sd(dv.LBB[which(dv.LBB>=10000)])
sd.dv
sd.non.dv <- sd(non.dv.LBB[which(non.dv.LBB>=10000)])
sd.non.dv

# SDs within bouts:
sd.heat <- sd(heaters[which(heaters<=1000)])
sd.heat
sd.feed <- sd(feeders[which(feeders<=1000)])
sd.feed
sd.clean <- sd(cleaners[which(cleaners<=1000)])
sd.clean
sd.dv <- sd(dv[which(dv<=1000)])
sd.dv
sd.non.dv <- sd(non.dv[which(non.dv<=1000)])
sd.non.dv

# The next 4 lines give us SE within bouts.

sd.heat/sqrt(length(heaters[which(heaters<=1000)]))
sd.feed/sqrt(length(feeders[which(feeders<=1000)]))
sd.clean/sqrt(length(cleaners[which(cleaners<=1000)]))
sd.dv/sqrt(length(dv[which(dv<=1000)]))
sd.non.dv/sqrt(length(non.dv[which(non.dv<=1000)]))

# Here's IQR, but not yet filtered for within or between bouts.
IQR(heaters.LBB)
IQR(feeders.LBB)
IQR(cleaners.LBB)
IQR(dv.LBB[which(dv.LBB>=10000)])

# How about just a summary of each thing. This will get max and min, which can
# then find overall range:

# Summary between bouts:
summary(heaters.LBB[which(heaters.LBB>=10000)])
summary(feeders.LBB[which(feeders.LBB>=10000)])
summary(cleaners.LBB[which(cleaners.LBB>=10000)])
summary(dv.LBB[which(dv.LBB>=10000)])
summary(non.dv.LBB[which(non.dv.LBB>=10000)])

# Sample sizes for each:
length(heaters.LBB[which(heaters.LBB>=10000)])
length(feeders.LBB[which(feeders.LBB>=10000)])
length(cleaners.LBB[which(cleaners.LBB>=10000)])
length(dv.LBB[which(dv.LBB>=10000)])
length(non.dv.LBB[which(non.dv.LBB>=10000)])

# Summary within bouts:
summary(heaters[which(heaters<=1000)])
summary(feeders[which(feeders<=1000)])
summary(cleaners[which(cleaners<=1000)])
summary(dv[which(dv<=1000)])
summary(non.dv[which(non.dv<=1000)])

# Sample sizes for within bouts:
length(heaters[which(heaters<=1000)])
length(feeders[which(feeders<=1000)])
length(cleaners[which(cleaners<=1000)])
length(dv[which(dv<=1000)])
length(non.dv[which(non.dv<=1000)])

# Now we need the same stats, but for individual bees to see if there's a bee
# effect. We want both within and between bout stats for each bee.
# Here we index to the individual bee. There are 30.

bee.list <- unique(mydata$Bee)
mean.list <- data.frame(Bee=NA,Behavior=NA,Ev.Sep.Mean=NA,Ev.Sep.n=NA,LBB.Mean=NA,LBB.n=NA)
i <- 1

for (i in 1:length(bee.list)) {
  Bee <- as.character(bee.list[i])
  Behavior <- as.character(unique(newdata$Behavior[which(newdata$Bee == bee.list[i])]))
  Ev.Sep <- mean(na.omit(mydata$EvSepby3[which((mydata$Bee == bee.list[i]) & (mydata$EvSepby3 <= 1000))]))
  Ev.Sep.n <- length(mydata$Bee[which((mydata$Bee==bee.list[i]) & (mydata$EvSepby3 <= 1000))])
  LBB <- mean(newdata$LBB[which((newdata$Bee == bee.list[i]) & (newdata$LBB >= 10000))])
  LBB.n <- length(newdata$Bee[which((newdata$Bee==bee.list[i]) & (newdata$LBB >= 10000))])
  new.line <- c(Bee, Behavior, Ev.Sep, Ev.Sep.n, LBB, LBB.n)
  mean.list <- rbind(new.line, mean.list)
  i <- i + 1
}

mean.list <- mean.list[order(mean.list$Behavior),]

# These are stats performed on the means of inter-pulse durations. Moms stands
# for mean of means.

Ev.Sep.moms <- mean(as.numeric(mean.list$Ev.Sep.Mean[1:30]))
Ev.Sep.moms.sd <- sd(mean.list$Ev.Sep.Mean[1:30])
Ev.Sep.mean.n <- mean(as.numeric(mean.list$Ev.Sep.n[1:30]))
Ev.Sep.n.sum <- sum(as.numeric(mean.list$Ev.Sep.n[1:30]))

# These are stats performed on the means of inter-bout durations

LBB.moms <- mean(na.omit(as.numeric(mean.list$LBB.Mean[1:30])))
LBB.moms.sd <- sd(na.omit(as.numeric(mean.list$LBB.Mean[1:30])))
LBB.mean.n <- mean(as.numeric(mean.list$LBB.n[1:30]))
LBB.n.sum <- sum(as.numeric(mean.list$LBB.n[1:30]))

# These are stats performed on the means of inter-pulse durations of dv bees only

Ev.Sep.dv.moms <- mean(as.numeric(mean.list$Ev.Sep.Mean[which(mean.list$Behavior=="abdomen dv")]))
Ev.Sep.dv.moms.sd <- sd(mean.list$Ev.Sep.Mean[which(mean.list$Behavior=="abdomen dv")])
Ev.Sep.dv.n.mean <- mean(as.numeric(mean.list$Ev.Sep.n[which(mean.list$Behavior=="abdomen dv")]))
Ev.Sep.dv.n.sum <- sum(as.numeric(mean.list$Ev.Sep.n[which(mean.list$Behavior=="abdomen dv")]))

# These are stats performed on the means of inter-pulse durations of NON-dv bees only

Ev.Sep.non.dv.moms <- mean(as.numeric(mean.list$Ev.Sep.Mean[which(mean.list$Behavior!="abdomen dv")]))
Ev.Sep.non.dv.moms.sd <- sd(mean.list$Ev.Sep.Mean[which(mean.list$Behavior!="abdomen dv")])
Ev.Sep.non.dv.n.mean <- mean(as.numeric(mean.list$Ev.Sep.n[which(mean.list$Behavior!="abdomen dv")]))
Ev.Sep.non.dv.n.sum <- sum(as.numeric(mean.list$Ev.Sep.n[which(mean.list$Behavior!="abdomen dv")]))

# These are stats performed on the means of inter-bout durations of dv bees only

LBB.dv.moms <- mean(na.omit(as.numeric(mean.list$LBB.Mean[which(mean.list$Behavior=="abdomen dv")])))
LBB.dv.moms.sd <- sd(na.omit(as.numeric(mean.list$LBB.Mean[which(mean.list$Behavior=="abdomen dv")])))
LBB.dv.n.mean <- mean(as.numeric(mean.list$LBB.n[which(mean.list$Behavior=="abdomen dv")]))
LBB.dv.n.sum <- sum(as.numeric(mean.list$LBB.n[which(mean.list$Behavior=="abdomen dv")]))

# These are stats performed on the means of inter-bout durations of NON-dv bees only

LBB.non.dv.moms <- mean(na.omit(as.numeric(mean.list$LBB.Mean[which(mean.list$Behavior!="abdomen dv")])))
LBB.non.dv.moms.sd <- sd(na.omit(as.numeric(mean.list$LBB.Mean[which(mean.list$Behavior!="abdomen dv")])))
LBB.non.dv.n.mean <- mean(as.numeric(mean.list$LBB.n[which(mean.list$Behavior!="abdomen dv")]))
LBB.non.dv.n.sum <- sum(as.numeric(mean.list$LBB.n[which(mean.list$Behavior!="abdomen dv")]))

# To finish everything up nicely, let's print off the results in a tidy, descriptive output file.

sink(file = "[Insert desired output path for a text file here]")

cat("The following data frame contains the means of event separations within and between bouts, and corresponding sample sizes (n).\n
    For Ev.Sep column, we are looking only at <=1000 ms, and for LBB we are only look at >=10000 ms:\n")
print(mean.list)

cat("\nNow we look at means of means (moms) and sd of mean values:\n
    The first one is the mean of inter-pulse durations in all bees.\n")
print(Ev.Sep.moms)

cat("Next is sd of inter-pulse durations in all bees:\n")
print(Ev.Sep.moms.sd)

cat("Now mean sample size for inter-pulse durations in all bees:\n")
print(Ev.Sep.mean.n)

cat("Now the sum of all the inter-pulse sample sizes, for the total sample size per bee:\n")
print(Ev.Sep.n.sum)

cat("Now we have the mean of inter-bout durations in all bees.\n")
print(LBB.moms)

cat("Next is sd of inter-bout durations in all bees:\n")
print(LBB.moms.sd)

cat("Now mean sample size for inter-bout durations in all bees:\n")
print(LBB.mean.n)

cat("Now the sum of all the inter-bout sample sizes, for the total sample size per bee:\n")
print(LBB.n.sum)

cat("Here is the mean of inter-pulse durations in just dv bees.\n")
print(Ev.Sep.dv.moms)

cat("Next is sd of inter-pulse durations in just dv bees:\n")
print(Ev.Sep.dv.moms.sd)

cat("Now mean sample size for inter-pulse durations in just dv bees:\n")
print(Ev.Sep.dv.n.mean)

cat("Now the sum of all the inter-pulse sample sizes, for the total sample size per dv bee:\n")
print(Ev.Sep.dv.n.sum)

cat("Now we have the mean of inter-bout durations in just dv bees.\n")
print(LBB.dv.moms)

cat("Next is sd of inter-bout durations in just dv bees:\n")
print(LBB.dv.moms.sd)

cat("Now mean sample size for inter-bout durations in just dv bees:\n")
print(LBB.dv.n.mean)

cat("Now the sum of all the inter-bout sample sizes, for the total sample size per dv bee:\n")
print(LBB.dv.n.sum)

cat("Here is the mean of inter-pulse durations in just non-dv bees.\n")
print(Ev.Sep.non.dv.moms)

cat("Next is sd of inter-pulse durations in just non-dv bees:\n")
print(Ev.Sep.non.dv.moms.sd)

cat("Now mean sample size for inter-pulse durations in just non-dv bees:\n")
print(Ev.Sep.non.dv.n.mean)

cat("Now the sum of all the inter-pulse sample sizes, for the total sample size per non-dv bee:\n")
print(Ev.Sep.non.dv.n.sum)

cat("Now we have the mean of inter-bout durations in just non-dv bees.\n")
print(LBB.non.dv.moms)

cat("Next is sd of inter-bout durations in just non-dv bees:\n")
print(LBB.non.dv.moms.sd)

cat("Now mean sample size for inter-bout durations in just non-dv bees:\n")
print(LBB.non.dv.n.mean)

cat("Now the sum of all the inter-bout sample sizes, for the total sample size per non-dv bee:\n")
print(LBB.non.dv.n.sum)

sink()

#===============================================================================
#===============================================================================

# This script tests for differences between behavior counts in a census.
# Kathryn Busby
# 11-27-2019
# mkbusby@email.arizona.edu

# Load libraries.

library(tidyverse)
library(lubridate)

# Bring in the data.

data <- read.csv(file="../Data/Census2.csv",
                 header = TRUE,
                 sep = ",")

# Clean up data: convert year/month/date/time to date.time.
# Select date.time, behavior, and counts for each behavior.
# In this case, it's valid to convert NAs to zeros. They just represent
# the absence of that particular behavior in the total count for that
# timestamp, so omitting them will not exclude data.

data$time <- hm(data$time)
t.data <- data %>%
  mutate(datetime=make_datetime(year, month, date))
t.data$datetime <- paste(data$date, data$time)
t.data <- t.data %>%
  select(datetime, behavior, total.beh, proportion, abdomen.dv.count,
         cleaner.count, feeder.count, heater.count)

# Visualize the distribution using histograms.

hist(t.data$abdomen.dv.count)
hist(t.data$cleaner.count)
hist(t.data$feeder.count)
hist(t.data$heater.count)

# Calculate mean percentages of bees inside cells performing each behavior over
# the course of all censuses.

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

# We need to do a correction for the above multiple comparisons:

p.vector <- c(2.507e-15, 1.118e-12, .0028, 2.2e-16, 2.2e-16, 5.356e-07)
p.adjust(p.vector, method="holm")

# All differences are still significant.

#===============================================================================
#===============================================================================

# This script tests if behavior counts differ between day and night.
# Kathryn Busby
# 11-27-2019
# mkbusby@email.arizona.edu

# Load libraries.

library(tidyverse)
library(lubridate)

# Bring in the data.

data <- read.csv(file="../Data/Census2.csv",
                 header = TRUE,
                 sep = ",")

# Clean up data: convert year/month/date/time to date.time.
# Retain day.night column.
# Select date.time, day.night, and counts for each behavior.
# In this case, it's valid to convert NAs to zeros.
# They just represent the absence of that particular behavior in the total
# count for that timestamp, so omitting them will not exclude data.

data$time <- hm(data$time)
t.data <- data %>%
  mutate(datetime=make_datetime(year, month, date))
t.data$datetime <- paste(data$date, data$time)
t.data <- t.data %>%
  select(datetime, behavior, total.beh, proportion,
         day.night, abdomen.dv.count, cleaner.count,
         feeder.count, heater.count)

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

# Use Kolmogorov-Smirnov tests to compare between each behavior except for
# feeders.
# For feeders there was not a large enough sample size to compare counts,
# and they were all at night.
# Use the absolute number of each behavior. Do 3 tests of absolute counts.

ks.test(x=as.numeric(day$total.beh[which(day$behavior=="abdomen dv")]),
        y=as.numeric(night$total.beh[which(night$behavior=="abdomen dv")]))
ks.test(x=as.numeric(day$total.beh[which(day$behavior=="cleaner")]),
        y=as.numeric(night$total.beh[which(night$behavior=="cleaner")]))
ks.test(x=as.numeric(day$total.beh[which(day$behavior=="heater")]),
        y=as.numeric(night$total.beh[which(night$behavior=="heater")]))

# Compare between each behavior except for feeders.
# Use the proportion of each behavior instead of a total.
# Do 3 tests of proportions.

ks.test(x=day$proportion[which(day$behavior=="abdomen dv")],
        y=night$proportion[which(night$behavior=="abdomen dv")])
ks.test(x=day$proportion[which(day$behavior=="cleaner")],
        y=night$proportion[which(night$behavior=="cleaner")])
ks.test(x=day$proportion[which(day$behavior=="heater")],
        y=night$proportion[which(night$behavior=="heater")])

# Correct for multiple tests not needed since no p-values are low enough.

#===============================================================================
#===============================================================================

# This script makes a stacked boxplot showing the proportions of behaviors at
# any given time.
# Figure 5.
# Kathryn Busby and Dave Reineke
# 2/26/2020
# mkbusby@email.arizona.edu

# Import libraries.

library(tidyverse)
library(ggthemes)
library(extrafont)
library(plyr)
library(scales)
library(reshape2)

# Import data file.

data <- read.csv(file="../Data/Census.csv",
                 header = TRUE,
                 sep = ",")

# To reorder the plot so that it uses whole datetimes instead of just hours
# and minutes, we need to create a date-time object and use it to sort.
# The Lubridate package is tidyverse adjacent and accomplishes this.

data$time2 <- data$time

data$time <- hm(data$time2)

# drop the seconds
data$time <- str_remove_all(data$time, " 0S")

# manually add in 0H where missing
data[93:96,"time"] <- "00H 10M"
data[97:100,"time"] <- "00H 23M"
data[169:172,"time"] <- "00H 19M"
data[173:176,"time"] <- "00H 52M"

# manually add zeros where needed
data[9:12,"time"] <- "14H 01M"
data[21:24,"time"] <- "15H 09M"
data[29:32,"time"] <- "16H 04M"
data[45:48,"time"] <- "17H 02M"
data[61:64,"time"] <- "20H 05M"
data[101:104,"time"] <- "01H 57M"
data[105:108,"time"] <- "02H 05M"
data[109:112,"time"] <- "02H 07M"
data[113:116,"time"] <- "04H 15M"
data[117:120,"time"] <- "04H 31M"
data[121:124,"time"] <- "05H 02M"
data[133:136,"time"] <- "19H 04M"
data[177:180,"time"] <- "01H 26M"
data[185:188,"time"] <- "11H 08M"

t.data <- data %>%
  mutate(datetime=make_datetime(year, month, date))

t.data$datetime <- paste(data$date, data$time)

# order times
t.data$order.var  <- factor(seq(1:196), labels=t.data$time2)

# Use melt to get data into long format for stacking behavior categories.

melt.data <- t.data[,-c(6:8,14)]
melt.data <- melt(melt.data, id=c("year", "month", "date", "day.night",
                                  "time", "datetime","order.var"))
melt.data$datetime <- as.factor(melt.data$datetime)

# Lay down the bones of the plot:

barplot <- ggplot() + geom_bar(aes(y = melt.data$value,
                                   x = melt.data$order.var,
                                   fill=melt.data$variable),
                               data = melt.data,
                               stat="identity") +

# Customize colors: c("black", "cadetblue3", "darkgoldenrod1", "firebrick1")
# Customize labels and title
# Customize theme (to get rid of default gray grid)
  scale_fill_manual(name="Behavior",
                            values=c("black", "white","cadetblue3",
                                     "darkgoldenrod1", "firebrick1"),
                            labels=c("sleeping", " ", "cleaning",
                                     "feeding", "heating")) +
  labs(x="Timestamp", y="Number of Occurrences of Behavior") +
  ggtitle("Behaviors Occurring Across Time") +
  scale_y_continuous(breaks=c(1:17)) +
  theme(plot.margin=unit(c(0.5,0,0.5,1),"cm"),
        panel.background = element_blank(),
        plot.title = element_text(vjust=2, size=35),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(text = element_text(size=27),
        axis.line = element_line(colour = 'black', size = 1.1))

barplot

# Save the output plot!

ggsave(plot=barplot,
       filename=paste0("../Output/",Sys.Date(),"_Fig5_Census.png"),
       height=18, width=24, dpi=300, units="in")

#===============================================================================
#===============================================================================

# This script makes a plot of all bee gaster ventilation event times in seconds.
# Figure 6.
# Kathryn Busby
# February 26, 2020
# mkbusby@email.arizona.edu

# Packages: Uncomment the following line if you need to install the package.

# install.packages("ggplot2")

# Bring in file and clean up:

library(ggplot2)
mydata <- read.csv(file = "../Data/Vent_times.csv",
                   header = TRUE,
                   sep = ",")

# Make a version of the original data that only contains pertinent columns.

newdata <- mydata[,c(1,2,5)]

# We want the times to be presented in seconds, not milliseconds.

newdata$EvTimeby3 <- newdata$EvTimeby3/1000

# To sort the bees by behavior, then duration, we start by finding the max
# duration for each unique ID.
# Make a new column and populate it with the max event time for that bee.

unique.ID <- unique(newdata$Bee)
max <- as.numeric()
max.dur <- as.numeric()
i <- 1
newdata$max <- c(1:6897)

for (i in 1:length(unique.ID)) {
  max <- max(newdata$EvTimeby3[which(newdata$Bee == unique.ID[i])])
  newdata$max[which(newdata$Bee == unique.ID[i])] <-
    rep(max, length(newdata$Bee[which(newdata$Bee == unique.ID[i])]))
  i <- i + 1
}

# Now sort the times by behavior, then by duration.

newdata$Bee <- factor(x=newdata$Bee,
                      levels = unique(newdata$Bee[rev(order(newdata$Behavior,
                                                            newdata$max))]))

# Make a plot showing each individual event as a "|".
# Color by behavior. abdomen dv = "black", cleaner = "cadetblue3",
# feeder = "darkgoldenrod1", heater = "firebrick3"
# Set limits so we only look at behaviors for 150 seconds.
# This will allow patterns to be visible in the shorter duration behaviors,
# as well as provide a representative selection of longer behaviors.
# Note: The next lines will generate a warning about rows being removed.
# This is referring to those lines removed when we set the axis limits to
# 150 seconds, so is not a problem.

plot.this.behav <- ggplot(data = newdata, mapping =
                            aes(x = newdata$EvTimeby3, y = newdata$Bee,
                                color = newdata$Behavior)) +
  geom_point(pch="|", cex = 8) +
  labs(x = "Pulses (s)", y = "Bee", title = "All Bee Pulse Times") +
  theme(legend.title = element_blank()) +
  scale_color_manual(values=c("black", "cadetblue3", "darkgoldenrod1",
                              "firebrick3", "violet"),
                     name="Behavior") +
  scale_x_continuous(breaks=(seq(0,length(newdata$EvTimeby3),by=10)),
                     limits=(c(0,150)), expand=c(0,0)) +
  
  # Let's get rid of the gray grid background.
  
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  
  # The x-axis text is kind of small. We control that in the following line.
  
  theme(text = element_text(size=30))

plot.this.behav

# Save the plot in a new file.

ggsave(plot=plot.this.behav,
       filename=paste0("../Output/",Sys.Date(),"_Fig6_Pulse Times.png"),
       height=18, width=24, units="in", dpi=300)

#===============================================================================
#===============================================================================


# This script makes histograms of event separations.
# Figure 7a.
# Kathryn Busby
# 02/12/2020
# mkbusby@email.arizona.edu

# Install packages, load libraries.
# install.packages("ggplot2")

library (ggplot2)
library(tidyverse)

# Bring in data:

mydata <- read.csv(file = "../Data/Vent_times.csv",
                   header = TRUE,
                   sep = ",")

# There will be two dataframes after the next few lines. mydata will contain all
# rows of relevant columns.
# newdata will omit lines containing NAs. This can be applied as appropriate for
# each question, but user should be aware that if NAs are omitted, there will be
# a lot of missing bout interims because of the filter applied to cull out
# isolated clicks.

# Note: Don't re-execute the following lines without bringing in the file
# afresh. If you do, you will eliminate the wrong columns, or it may just not
# execute the line.
# Also note: column LBB does NOT select everything greater than 10 seconds.
# Rather, it eliminates isolated clicks. In other words, user must apply an
# additional filter to look between bouts.

newdata <- mydata[,c(1,2,5,6,8)]

# Now we need to put the time units into seconds.

newdata$EvSep <- newdata$EvSepby3/1000

# Need to get behaviors in wide format instead of long.

newdata <- newdata %>%
  pivot_wider(names_from=Behavior, values_from=EvSep)

# Each of the following lines makes a histogram of one behavior for within
# pulses.

hist.plot <- ggplot(data=newdata, aes(x=EvSep)) +
  geom_histogram(bins=200,color="black", fill="black") +
  facet_grid(newdata$Behavior, scales="free") +
  xlim(0,100) +
  theme_classic() +
  theme(text=element_text(size=30))

hist.plot

ggsave(plot=hist.plot,
       filename=paste0("../Output/",Sys.Date(),"_Fig7a_Histograms.png"),
       height=48, width=24, units="in", dpi=300)

#===============================================================================
#===============================================================================

# This script makes boxplots of the separations between gaster pumps, sorted by
# behavior.
# Figure 7b (bottom half).
# This script only makes the bottom half of the figure (displaying those event
# separations less than 10s).
# Execute the script for Figure 7b_Event Sep Boxplot Over 10s.R for the other
# half.
# Kathryn Busby
# 03/12/2020
# mkbusby@email.arizona.edu

# Load libraries.

library(reshape2)
library(ggplot2)

# Bring in Inside Cells data:

newdata <- read.csv(file = "../Data/Vent_times.csv",
                    header = TRUE,
                    sep = ",")

# For the purposes of this plot, let's only use newdata where event separations
# are less than 10 seconds:

shortdata <- newdata[which (newdata$EvSepby3 < 10000),]
shortdata$EvSepby3 <- shortdata$EvSepby3/1000

# Make a vector of our colors.

beh.colors <- c("black", "cadetblue3", "darkgoldenrod1", "firebrick3")

# Now build the plot.

ev.sep.bottom <- ggplot(shortdata, aes(x=shortdata$Behavior,
                                    y=shortdata$EvSepby3)) +
  geom_point(aes(fill=shortdata$Behavior), size=3, shape=21, colour="gray20",
             position=position_jitter(width=0.3, height=0.1)) +
  scale_fill_manual(values=beh.colors) +
  geom_boxplot(outlier.colour=NA, fill=NA, colour="gray20", size=1) +
  labs(x = "Behavior", y = "Pulse Separations (s)",
       title="All Bee Pulse Separations") +
  guides(fill=FALSE) + scale_y_continuous(name="Pulse Separations (s)",
                                          breaks=c(0:10),
                                          limits=c(0, 10)) +
  theme_classic() +
  theme(text = element_text(size=30),
        axis.line = element_line(colour = 'black', size = 1.1))

ev.sep.bottom

ggsave(plot=ev.sep.bottom,
       filename=paste0("../Output/",Sys.Date(),"_Fig7B_Bottom.png"),
       height=18, width=24, dpi=300, units="in")

#===============================================================================
#===============================================================================

# This script makes boxplots of the separations between gaster pumps, sorted by
# behavior.
# Figure 7b (top half).
# This script only makes the top half of the figure (displaying those event
# separations over 10s).
# Execute the script for Figure 7b_Event Sep Boxplot Less than 10s.R for the
# other half.
# Kathryn Busby
# 03/12/2020
# mkbusby@email.arizona.edu

# Load libraries.

library(reshape)
library(ggplot2)

# Bring in Inside Cells data:

newdata <- read.csv(file = "../Data/Vent_times.csv",
                    header = TRUE,
                    sep = ",")

# For the purposes of this plot, let's only use newdata where event separations
# are over 10s (or 10000 ms).

longdata <- newdata[which (newdata$LBB >= 10000),]
longdata$LBB <- longdata$LBB/1000

# Now build the plot.
# Side note: Each time you reexecute this plot, the horizontal jitter will make
# it look a bit different.
# This does not affect the vertical distribution, which is what we're really
# looking at.
# The warnings are because we're not displaying the topmost points.
# If you're interested in the tails, check out the histograms of each behavior's
# event separations.

ev.sep.top <- ggplot(longdata, aes(x=longdata$Behavior, y=longdata$LBB)) +
  geom_point(aes(fill=longdata$Behavior), size=3, shape=21,
             colour="gray20",
             position=position_jitter(width=0.3, height=0.1)) +
  geom_boxplot(outlier.colour=NA, fill=NA, colour="grey20", size=1) +
  labs(x = "Behavior", title="Bout Separations Over 10s") +
  guides(fill=FALSE) + scale_y_continuous(name="Pulse Separations (s)",
                                          breaks=c(5,10,15,20,25,30,35,40),
                                          limits=c(10, 40)) +
  scale_fill_manual(values=c("black",
                             "cadetblue3",
                             "darkgoldenrod1",
                             "firebrick3")) +
  theme_classic() +
  theme(text = element_text(size=30),
        axis.line = element_line(colour = 'black', size = 1.1))

ev.sep.top

ggsave(plot=ev.sep.top,
       filename=paste0("../Output/",Sys.Date(),"_Fig7B_Top.png"),
       height=18, width=24, units="in", dpi=300)

#===============================================================================
#===============================================================================

# This script makes a plot showing the difference between thorax temperature and
# surrounding temperature for bees across all behaviors.
# Figure 8.
# Kathryn Busby
# February 26, 2020
# mkbusby@email.arizona.edu

# Load libraries.

library(ggplot2)
library(dplyr)

mydata <- read.csv(file = "../Data/Temperatures.csv",
                   header = TRUE,
                   sep = ",")

# Clean up mydata so it only includes relevant columns.

mydata <- mydata[,1:8]

# For the plot, we're going to want to superimpose horizontal lines that will
# indicate where each mean is.
# To build the bones for the horizontal lines, we here make a data frame of
# behavior means.
# Then we have to add in the start and stop points for the x values of the
# lines.
# mean.beh makes a tibble of mydata behaviors and a mean temperature for each
# behavior. 

mean.beh <- mydata %>%
  group_by (Behavior) %>%
  summarize (mean_temp = mean(Mean.Tbody)-35)

# Now we need to define the start and end points for each horizontal line, so
# we can tell ggplot where to truncate them later.
# Make a vector that counts the number of data points in each behavior category,
# then add it as a new column to mean.beh.

x <- table(mydata$Behavior)

mean.beh$x <- x

mean.beh$x.start <- c(0, x[1], (x[1]+x[2]), (x[1]+x[2]+x[3]))
mean.beh$x.end <- c(x[1], (x[1]+x[2]), (x[1]+x[2]+x[3]), (x[1]+x[2]+x[3]+x[4]))

# Okay let's do this entire thing a second time in order to generate a second
# line.

mean.diff.beh <- mydata %>%
  group_by (Behavior) %>%
  summarize (mean_diff_temp = mean(DeltaT))

x2 <- table(mydata$Behavior)

mean.diff.beh$x <- x2
mean.diff.beh$x.start <- c(0, x[1], (x[1]+x[2]), (x[1]+x[2]+x[3]))
mean.diff.beh$x.end <- c(x[1], (x[1]+x[2]), (x[1]+x[2]+x[3]),
                         (x[1]+x[2]+x[3]+x[4]))

# Make a vector to give the second y-axis the appropriate breaks:

y1.breaks <- c("-3","-2","-1","0","1","2","3","4","5","6")
y2.breaks <- c("30","31","32","33","34","35","36","37","38","39","40","41")

# Make a color vector:

colors <- c("black", "cadetblue3", "darkgoldenrod1", "firebrick3")

# First, get the data ordered by behavior so it'll plot that way:

mydata$Bee <- factor(x=mydata$Bee,
                     levels = unique(mydata$Bee[order(mydata$Behavior,
                                                      mydata$Mean.Tbody)]))

# We want a plot with individual bees on the x-axis and delta temperature on the
# y-axis.
# We want the bees grouped by behavior.
# The colors should match the pattern we've been using for behaviors:
# abdomen dv = "black", cleaner = "cadetblue3", feeder = "darkgoldenrod1",
# heater = "firebrick3"

temp.plot <- ggplot(data=mydata) +
  geom_point(mapping=aes(x=reorder(mydata$Bee,mydata$Behavior),
                                                     y=mydata$DeltaT,
                         color=mydata$Behavior),
             cex = 12, show.legend=TRUE) +
  geom_point(aes(x=reorder(mydata$Bee, mydata$Behavior),
                 y=(mydata$Mean.Tbody-35),
                 color=mydata$Behavior),
             pch=3, cex=12, show.legend=TRUE) +
  scale_color_manual(values=colors, name="Behavior") +
  scale_y_continuous(sec.axis = sec_axis(~.+35,
                                         name = "+ Absolute Body Temperature (C)",
                                         breaks=c(30:42))) +
  
  labs(x = "Bee, Ordered by Behavior",
       y = "o Thorax Temperature Difference from Surroundings (Degrees Celsius)",
       title = "Bee Delta Thorax Temperatures") +
  
  # Let's get rid of the gray grid background and pretty up the axis labels:
  
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  
  # Add horizontal lines for the means of each behavior.
  
  geom_segment(data=mean.beh,
               mapping=aes(x=x.start,
                           xend=x.end,y=mean_temp,
                           yend=mean_temp,group=Behavior,
                           color=Behavior), size=1.2, linetype="dashed") +
  
  # Do it again, make a second line for the mean difference.
  
  geom_segment(data=mean.diff.beh,
               mapping=aes(x=x.start,
                           xend=x.end,
                           y=mean_diff_temp,
                           yend=mean_diff_temp,
                           group=Behavior,
                           color=Behavior), size=1.2) +
  
  # Control axis text in the following line.
  
  theme(text = element_text(size=36))

temp.plot

# Save the plot! :)

ggsave(plot=temp.plot,
       filename=paste0("../Output/",Sys.Date(),"_Fig8_Temps.png"),
       height=18, width=24, units="in", dpi=300)

#===============================================================================
#===============================================================================

# This script looks for differences in pulse separations between behaviors.
# 03-13-2020
# Kathryn Busby
# mkbusby@email.arizona.edu

# Load libraries.
library(tidyverse)

# Bring in data file.

mydata <- read.csv(file = "[Insert your path to Vent_times.csv here]",
                   header = TRUE,
                   sep = ",")

# Eliminate those NAs that occurred where there wasn't an event separation, at
# the beginning of each behavior.

shortdata <- mydata %>%
  select(Behavior, EvSepby3)

shortdata <- na.omit(shortdata)

# I want to compare between groups of event separations.
# This should be looking within bouts. So first index to <1000.

pulses <- shortdata %>%
  filter(EvSepby3 < 1000)

# Kruskal-Wallis tests for pulse separations between behaviors.
# These data are all right skewed, so not normally distributed.

kruskal.test(EvSepby3 ~ Behavior, data=pulses)

# Next we need to test for differences between specific pairs of behaviors.
# Create variables to index to them individually.

dv.pulse <- pulses %>%
  filter(Behavior == "abdomen dv") %>%
  filter(EvSepby3 < 1000)

cleaner.pulse <- shortdata %>%
  filter(Behavior == "cleaner") %>%
  filter(EvSepby3 < 1000)

feeder.pulse <- shortdata %>%
  filter(Behavior == "feeder") %>%
  filter(EvSepby3 < 1000)

heater.pulse <- shortdata %>%
  filter(Behavior == "heater") %>%
  filter(EvSepby3 < 1000)

# Now we need to do the non-parametric pairwise comparisons that correspond
# to the KW test, which would
# be Wilcoxon-Mann-Whitney tests of all the following pairs:

# cleaner-abdomen dv
wilcox.test(dv.pulse$EvSepby3, cleaner.pulse$EvSepby3)
# feeder-abdomen dv  
wilcox.test(dv.pulse$EvSepby3, feeder.pulse$EvSepby3)
# heater-abdomen dv   
wilcox.test(dv.pulse$EvSepby3, heater.pulse$EvSepby3)
# feeder-cleaner     
wilcox.test(feeder.pulse$EvSepby3, cleaner.pulse$EvSepby3)
# heater-cleaner 
wilcox.test(heater.pulse$EvSepby3, cleaner.pulse$EvSepby3)
# heater-feeder  
wilcox.test(heater.pulse$EvSepby3, feeder.pulse$EvSepby3)

#===============================================================================
#===============================================================================

# This script conducts a linear mixed effects regression.
# Kathryn Busby and Dave Reineke
# 10/30/2019
# mkbusby@email.arizona.edu

# Install packages. Load libraries.

# install.packages("lmerTest")
# install.packages("emmeans")
library(lme4)
library(lmerTest)
library(emmeans)
library(ggplot2)
library(dplyr)
library(tidyverse)

# Load data file. 

mydata <- read.csv(file = "[Insert path to Vent_times.csv here]",
                   header = TRUE,
                   sep = ",")

# Select the relevant columns only.

tidydf <- mydata %>%
  select(Behavior, Bee, EvSepby3) %>%
  filter(EvSepby3<1000)

# Do the test.

lmerEvSep <- lmer(EvSepby3 ~ Behavior + (1|Bee), tidydf)
summary(lmerEvSep)

glmEvSep <- glm(EvSepby3~Behavior+Bee, data=tidydf)
summary(glmEvSep)

# Code from this point on is from David Reineke and then edited by Kathryn Busby:

# anova with ranked data (see R-Boggers)
# https://www.r-bloggers.com/beware-the-friedman-test/ 

mod7r <- lmer(EvSepby3~Behavior + (1|Bee), data=tidydf)

# hypothesis tests
anova(mod7r) # F tests fixed effects
ranova(mod7r) # test random effects

# multiple comparisons
emmeans <- emmeans(mod7r, pairwise~Behavior, adjust="Tukey",
                   lmer.df="satterthwaite")

# check residuals for normality
plot(mod7r, main="Plot mod7r to check residuals for normality")
hist(residuals(mod7r))
boxplot(residuals(mod7r), main="Boxplot of mod7r residuals")
qqnorm(residuals(mod7r))
qqline(residuals(mod7r))

# only do this if residuals are not normal
# model using ranked data

tidydf$EvSepby3.r <- rank(tidydf$EvSepby3)
mod7r.r <- lmer(EvSepby3.r~Behavior + (1|Bee), data=tidydf)

plot(mod7r.r, main="Plot mod7r to check residuals for normality")
hist(residuals(mod7r.r))
boxplot(residuals(mod7r.r), main="Boxplot of mod7r residuals")
qqnorm(residuals(mod7r.r))
qqline(residuals(mod7r.r))

# hypothesis tests now on newly transformed model (mod7r.r)
anova(mod7r.r) # F tests fixed effects
ranova(mod7r.r) # test random effects

# multiple comparisons
emmeans.r <- emmeans(mod7r.r, pairwise~Behavior, adjust="Tukey",
                     lmer.df="satterthwaite", lmerTest.limit = 6121)

# other transformations can be tried as well (log, etc.)

# Let's save results in a text file.

sink(file="[Insert desired output file path here.]")

cat("Our data are not normally distributed, so we've applied a rank transformation.\n
    The following analyses are performed after a rank transformation:\n\n")

cat("The following describes our linear mixed model fit:\n\n")
print(mod7r.r)

cat("This shows differences between fixed effects:\n\n")
print(anova(mod7r.r))

cat("This shows differences between random effects:\n\n")
print(ranova(mod7r.r))

cat("multiple comparisons:\n\n")
print(emmeans.r)

sink()

#===============================================================================
#===============================================================================

# Tests for temperature differences between behaviors, and to account for effect
# of duration in cell on temperature.
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
wilcox.test(heater.df$Mean.Tbody, heater.df$Mean.Tsurr, data=heater.df) #Sig.

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

# Does the amount of time spent doing a behavior predict temperature differences
# from surroundings?
# Do a linear regression to see.

plot(x = tidydf$DurinCell, y = tidydf$DeltaT,
     abline(lm(formula = DeltaT ~ DurinCell, data = tidydf)))

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

#===============================================================================
#===============================================================================

# Here we adjust for multiple testing problems using the Holm Method.
# Kathryn Busby
# May 24, 2020
# mkbusby@email.arizona.edu

# In the temperature difference script we detected 7 p-values, all in a family
# of related tests. We need to account for the increased probability of Type 1
# error due to repeated tests.

# Function p.adjust() takes a vector of p-values and uses the specified method
# to appropriately reduce the p-values.
# Here I plug in the p-values obtained in the scripts above. This is done
# manually so scripts can be executed independently. If this method were used
# on new data, user would need to manually edit these p-values.


# The first family of tests was when we looked for differences between the 
# Tdiff of specific behaviors, paired. Without this correction, the only
# significant difference was between heaters and dv.
# Here are the p-values from each pair.

pw.heaters.dv <- .001261
pw.heaters.cleaners <- .036 
pw.heaters.feeders <- .036
pw.dv.cleaners <- .25
pw.dv.feeders <- 1
pw.cleaners.feeders <- .8

# Make a vector of the values above:

pw.vector <- c(pw.heaters.dv, pw.heaters.cleaners, pw.heaters.feeders,
               pw.dv.cleaners, pw.dv.feeders, pw.cleaners.feeders)

# Now make the adjustment:

p.adjust(pw.vector, method="holm")

# Looks like now the only significant difference is between heaters and dv.

# Next adjustment is for the students' responses to blind behavior assessment.

pb.corr.dv <- .00001
pb.corr.cleaners <- .00001
pb.corr.feeders <- .00001
pb.corr.heaters <- .00001

# Make a vector of the values above:

pb.corr.vector <- c(pb.corr.dv, pb.corr.cleaners, pb.corr.feeders,
                    pb.corr.heaters)

# Now make the adjustment:

p.adjust(pb.corr.vector, method="holm")

# All p-values are still different after this correction.
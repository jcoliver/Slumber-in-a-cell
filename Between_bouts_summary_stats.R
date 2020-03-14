# This script performs basic summary statistics on event separations, both less than and greater than 10s.
# Kathryn Busby
# 02/12/2020
# mkbusby@email.arizona.edu

# Bring in Inside Cells data:

mydata <- read.csv(file = "[Insert path to Vent_times.csv here]",
                   header = TRUE,
                   sep = ",")

# There will be two dataframes after the next few lines. mydata will contain all rows of relevant columns.
# newdata will omit lines containing NAs. This can be applied as appropriate for each question, but user
# should be aware that if NAs are omitted, there will be a lot of missing bout interims because of the 
# filter applied to cull out isolated clicks.

# Note: Don't re-execute the following lines without bringing in the file afresh. If you do, you will 
# eliminate the wrong columns, or it may just not execute the line.
# Also note: column LBB does NOT select everything greater than 10 seconds. Rather, it eliminates isolated
# clicks. In other words, user must apply an additional filter to look between bouts.

mydata <- mydata[,c(1,2,5,6,8)]
newdata <- na.omit(mydata)
colnames(newdata) <- c("Behavior", "Bee", "EvTimeby3", "EvSepby3", "LBB")
colnames(mydata) <- c("Behavior", "Bee", "EvTimeby3", "EvSepby3", "LBB")

summary(newdata$EvSepby3)
summary(newdata)

mean(newdata$LBB)

# Having the overall mean, it would be good to index to each behavior and get stats like mean, med., mode.
# First index by behavior.
# For looking within bouts, we don't want to exclude NAs, so reference mydata instead of newdata.
# The next lines create objects that index to each behavior within the data frame, so we can easily do
# stats on just that behavior.
# The last line will index to all behaviors EXCEPT dv.

heaters <- mydata$EvSepby3 [which(mydata$Behavior == "heater")]
feeders <- mydata$EvSepby3 [which(mydata$Behavior == "feeder")]
cleaners <- mydata$EvSepby3 [which(mydata$Behavior == "cleaner")]
dv <- mydata$EvSepby3 [which(mydata$Behavior == "abdomen dv")]
non.dv <- mydata$EvSepby3 [-which(mydata$Behavior == "abdomen dv")]

# The following set references LBB to look between bouts, and should exclude NAs. So reference newdata.

heaters.LBB <- newdata$LBB[which(newdata$Behavior == "heater")]
feeders.LBB <- newdata$LBB[which(newdata$Behavior == "feeder")]
cleaners.LBB <- newdata$LBB[which(newdata$Behavior == "cleaner")]
dv.LBB <- newdata$LBB[which(newdata$Behavior == "abdomen dv")]
non.dv.LBB <- newdata$LBB[-which(newdata$Behavior == "abdomen dv")]

# Now get stats for between bouts in each group. There's now a filter to select only those within bouts
# (less than 1 second). We need mean, median, mode, sd, se, IQR.
# For this next set, because we are looking within bouts, we do NOT want to exclude NAs.

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

# How about just a summary of each thing. This will get max and min, which can then find overall range:

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

# Now we need the same stats, but for individual bees to see if there's a bee effect.
# We want both within and between bout stats for each bee.
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

# These are stats performed on the means of inter-pulse durations. Moms stands for mean of means.

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


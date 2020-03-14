# This script makes histograms of event separations, used in Figure 7a.
# Kathryn Busby
# 02/12/2020
# mkbusby@email.arizona.edu

# Bring in Inside Cells data:

mydata <- read.csv(file = "[Insert your path to Vent_times.csv here]",
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

mydata <- mydata[,c(1,2,5,6,9)]
newdata <- na.omit(mydata)
colnames(newdata) <- c("Behavior", "Bee", "EvTimeby3", "EvSepby3", "LBB")
colnames(mydata) <- c("Behavior", "Bee", "EvTimeby3", "EvSepby3", "LBB")

# To plot event separations in histograms, first index to separate behaviors.
# For looking within bouts, we don't want to exclude NAs, so reference mydata instead of newdata.
# The next lines create objects that index to each behavior within the data frame, so we can easily do
# stats on just that behavior.
# The last line will index to all behaviors EXCEPT dv.

heaters <- mydata$EvSepby3 [which(newdata$Behavior == "heater")]
feeders <- mydata$EvSepby3 [which(newdata$Behavior == "feeder")]
cleaners <- mydata$EvSepby3 [which(newdata$Behavior == "cleaner")]
dv <- newdata$EvSepby3 [which(newdata$Behavior == "abdomen dv")]

# Each of the following lines makes a histogram of one behavior.

heater.hist <- hist(heaters, xlab="heater event separations", xlim=c(0,100000), ylim=c(0, 10), breaks=150)
cleaner.hist <- hist(cleaners, xlab="cleaner event separations", xlim=c(0,100000), ylim=c(0, 10), breaks=150)
feeder.hist <- hist(feeders, xlab="feeder event separations", xlim=c(0,100000), ylim=c(0, 10), breaks=150)
dv.hist <- hist(dv, xlab="sleeper event separations", xlim=c(0,100000), ylim=c(0, 10), breaks=150)


# This script makes a plot of all bee gaster ventilation event times in seconds.
# Kathryn Busby
# February 26, 2020
# mkbusby@email.arizona.edu

# Packages: Uncomment the following line if you need to install the package.

# install.packages("ggplot2")

# Bring in file and clean up:

library(ggplot2)
mydata <- read.csv(file = "[Insert path to All Event Times.csv data file here]",
                   header = TRUE,
                   sep = ",")

# Make a version of the original data that only contains pertinent columns.

newdata <- mydata[,c(1,2,5)]
  
# We want the times to be presented in seconds, not milliseconds.

newdata$EvTimeby3 <- newdata$EvTime..3/1000

# To sort the bees by behavior, then duration, we start by finding the max duration for each unique ID.
# Make a new column and populate it with the max event time for that bee.

unique.ID <- unique(newdata$Bee)
max <- as.numeric()
max.dur <- as.numeric()
i <- 1
newdata$max <- c(1:6897)

for (i in 1:length(unique.ID)) {
  max <- max(newdata$EvTime..3[which(newdata$Bee == unique.ID[i])])
  newdata$max[which(newdata$Bee == unique.ID[i])] <- rep(max, length(newdata$Bee[which(newdata$Bee == unique.ID[i])]))
  i <- i + 1
}

# Now sort the times by behavior, then by duration.

newdata$Bee <- factor(x=newdata$Bee, levels = unique(newdata$Bee[rev(order(newdata$Behavior,newdata$max))]))

# Make a plot showing each individual event as a "|".
# Color by behavior. abdomen dv = "black", cleaner = "cadetblue3", feeder = "darkgoldenrod1", heater = "firebrick3"
# Set limits so we only look at behaviors for 150 seconds. This will allow patterns to be visible
# in the shorter duration behaviors, as well as provide a representative selection of longer behaviors.
# Note: The next lines will generate a warning about rows being removed. This is referring to those
# lines removed when we set the axis limits to 150 seconds, so is not a problem.

plot.this.behav <- ggplot(data = newdata, mapping =
                            aes(x = newdata$EvTimeby3, y = newdata$Bee, color = newdata$Behavior)) +
  geom_point(pch="|", cex = 4) +
  labs(x = "Event Times (sec)", y = "Bee", title = "All Bee Event Times") +
  scale_color_manual(values=c("black", "cadetblue3", "darkgoldenrod1", "firebrick3", "violet"),
                                                                           name="Behavior") +
  scale_x_continuous(breaks=(seq(0,length(newdata$EvTimeby3),by=10)), limits=(c(0,150)), expand=c(0,0)) +
  
  # Let's get rid of the gray grid background.
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  
  # The x-axis text is kind of small. We control that in the following line.
  
  theme(axis.text.x = element_text(size=18), axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12))

# Save the plot in a new file.

ggsave(filename="[Insert path to desired output file here]", height=9, width=12, units="in")


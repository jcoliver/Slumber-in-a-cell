# This script makes a plot showing the difference between thorax temperature and surrounding
# temperature for bees across all behaviors.
# Kathryn Busby
# February 26, 2020
# mkbusby@email.arizona.edu

# Install packages: Uncomment this if you need to install the two packages listed below.

# install.packages("ggplot2")
# install.packages("dplyr")

# Load libraries.

library(ggplot2)
library(dplyr)

mydata <- read.csv(file = "[Insert your file path to Temperatures to Show Only Relevant Bees.csv here]",
                   header = TRUE,
                   sep = ",")

# Clean up mydata so it only includes relevant columns.

mydata <- mydata[,1:8]

# For the plot, we're going to want to superimpose horizontal lines that will indicate where each mean is.
# To build the bones for the horizontal lines, we here make a data frame of behavior means.
# Then we have to add in the start and stop points for the x values of the lines.
# mean.beh makes a tibble of mydata behaviors and a mean temperature for each behavior. 

mean.beh <- mydata %>%
  group_by (Behavior) %>%
  summarize (mean_temp = mean(Mean.Tbody)-35)

# Now we need to define the start and end points for each horizontal line, so we can tell ggplot where to
# truncate them later.
# Make a vector that counts the number of data points in each behavior category,
# then add it as a new column to mean.beh.

x <- table(mydata$Behavior)

mean.beh$x <- x
             
mean.beh$x.start <- c(0, x[1], (x[1]+x[2]), (x[1]+x[2]+x[3]))
mean.beh$x.end <- c(x[1], (x[1]+x[2]), (x[1]+x[2]+x[3]), (x[1]+x[2]+x[3]+x[4]))

# Okay let's do this entire thing a second time in order to generate a second line.

mean.diff.beh <- mydata %>%
  group_by (Behavior) %>%
  summarize (mean_diff_temp = mean(DeltaT))

x2 <- table(mydata$Behavior)

mean.diff.beh$x <- x2
mean.diff.beh$x.start <- c(0, x[1], (x[1]+x[2]), (x[1]+x[2]+x[3]))
mean.diff.beh$x.end <- c(x[1], (x[1]+x[2]), (x[1]+x[2]+x[3]), (x[1]+x[2]+x[3]+x[4]))
  
# Make a vector to give the second y-axis the appropriate breaks:

y1.breaks <- c("-3","-2","-1","0","1","2","3","4","5","6")
y2.breaks <- c("30","31","32","33","34","35","36","37","38","39","40","41")

# Make a color vector:

colors <- c("black", "cadetblue3", "darkgoldenrod1", "firebrick3")

# First, get the data ordered by behavior so it'll plot that way:

mydata$Bee <- factor(x=mydata$Bee, levels = unique(mydata$Bee[order(mydata$Behavior, mydata$Mean.Tbody)]))

# We want a plot with individual bees on the x-axis and delta temperature on the y-axis.
# We want the bees grouped by behavior.
# The colors should match the pattern we've been using for behaviors:
# abdomen dv = "black", cleaner = "cadetblue3", feeder = "darkgoldenrod1", heater = "firebrick3"

plot <- ggplot(data=mydata) + geom_point(mapping=aes(x=reorder(mydata$Bee, mydata$Behavior),
                                                     y=mydata$DeltaT, color=mydata$Behavior), cex = 4, show.legend=TRUE) +
  geom_point(aes(x=reorder(mydata$Bee, mydata$Behavior), y=(mydata$Mean.Tbody-35),
                 color=mydata$Behavior), pch=3, cex=4, show.legend=TRUE)


plot + scale_color_manual(values=colors,
                          name="Behavior") +

  scale_y_continuous(sec.axis = sec_axis(~.+35, name = "+ Absolute Body Temperature (C)", breaks=c(30:42))) +
  
  labs(x = "Bee, Ordered by Behavior",
       y = "o Thorax Temperature Difference from Surroundings (Degrees Celsius)",
       title = "Bee Delta Thorax Temperatures") +

# Let's get rid of the gray grid background and pretty up the axis labels:
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      panel.background = element_blank(), axis.line = element_line(colour = "black"),
      axis.text.x = element_text(angle = 45, hjust = 1)) +
  
# Add horizontal lines for the means of each behavior.

  geom_segment(data=mean.beh, mapping=aes(x=x.start, xend=x.end,y=mean_temp, yend=mean_temp,group=Behavior, color=Behavior), linetype="dashed") +

# Do it again, make a second line for the mean difference.
  
  geom_segment(data=mean.diff.beh, mapping=aes(x=x.start, xend=x.end,y=mean_diff_temp, yend=mean_diff_temp,group=Behavior, color=Behavior), hjust=1) +
  
# Control axis text in the following line.
  
  theme(axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=18),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12))

# Save the plot! :)

ggsave(filename="[Insert your file path to desired output file here]", height=9, width=12, units="in")

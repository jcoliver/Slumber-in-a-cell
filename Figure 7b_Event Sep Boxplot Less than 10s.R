# This script makes boxplots of the separations between gaster pumps, sorted by behavior.
# This script only makes the bottom half of the figure (displaying those event separations less than 10s).
# Execute the script for Figure 7b_Event Sep Boxplot Over 10s.R for the other half.
# Kathryn Busby
# 03/12/2020
# mkbusby@email.arizona.edu

# Load libraries.

library(reshape2)
library(ggplot2)

# Bring in Inside Cells data:

newdata <- read.csv(file = "[Insert path to Behavior Census Totals.csv here.]",
                   header = TRUE,
                   sep = ",")

# For the purposes of this plot, let's only use newdata where event separations are less than 10 seconds:

shortdata <- newdata[which (newdata$EvSepby3 < 10000),]

# Make a vector of our colors.

beh.colors <- c("black", "cadetblue3", "darkgoldenrod1", "firebrick3")

# Now build the plot.

ev.sep.box <- ggplot(shortdata, aes(x=shortdata$Behavior, y=shortdata$EvSepby3)) +
  geom_point(aes(fill=shortdata$Behavior), size=1, shape=21, colour="gray20",
             position=position_jitter(width=0.3, height=0.1)) +
  scale_fill_manual(values=beh.colors) +
  geom_boxplot(outlier.colour=NA, fill=NA, colour="gray20") +
  labs(x = "Behavior", y = "Event Separations (ms)", title="All Bee Event Separations") +
  guides(fill=FALSE) + scale_y_continuous(name="Event Separations (ms)",
                                                     breaks=c(0, 5000, 10000),
                                                     limits=c(0, 10000)) +
  theme_classic()

ggsave(filename="[Insert path to desired output file here]", height=9, width=12, units="in")


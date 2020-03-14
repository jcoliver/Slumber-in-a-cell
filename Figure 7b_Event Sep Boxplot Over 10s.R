# This script makes boxplots of the separations between gaster pumps, sorted by behavior.
# This script only makes the top half of the figure (displaying those event separations over 10s).
# Execute the script for Figure 7b_Event Sep Boxplot Less than 10s.R for the other half.
# Kathryn Busby
# 03/12/2020
# mkbusby@email.arizona.edu

# Load libraries.

library(reshape)
library(ggplot2)

# Bring in Inside Cells data:

newdata <- read.csv(file = "[Insert path to Vent_times.csv here.]",
                                     header = TRUE,
                                     sep = ",")

# For the purposes of this plot, let's only use newdata where event separations are over 10s (or 10000 ms).

longdata <- newdata[which (newdata$LBB >= 10000),]

# Now build the plot.
# Side note: Each time you reexecute this plot, the horizontal jitter will make it look a bit different.
# This does not affect the vertical distribution, which is what we're really looking at.
# The warnings are because we're not displaying the topmost points.
# If you're interested in the tails, check out the histograms of each behavior's event separations.

ev.sep.box <- ggplot(longdata, aes(x=longdata$Behavior, y=longdata$LBB)) +
  geom_point(aes(fill=longdata$Behavior), size=1.5, shape=21, colour="gray20",
             position=position_jitter(width=0.3, height=0.1)) +
  geom_boxplot(outlier.colour=NA, fill=NA, colour="grey20") +
  labs(x = "Behavior", y = "Event Separations (ms)", title="Bout Separations Over 10s") +
  guides(fill=FALSE) + scale_y_continuous(name="Event Separations (ms)",
                                                     breaks=c(10000, 20000, 30000, 40000),
                                                     limits=c(10000, 40000)) +
  scale_fill_manual(values=c("black", "cadetblue3", "darkgoldenrod1", "firebrick3")) +
  theme_classic()

ggsave(filename="[Insert path to desired output file here.]", height=9, width=12, units="in")


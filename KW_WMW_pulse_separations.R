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

# Eliminate those NAs that occurred where there wasn't an event separation, at the beginning of each behavior.

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

# Now we need to do the non-parametric pairwise comparisons that correspond to the KW test, which would
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


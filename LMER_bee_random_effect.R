# This script conducts a linear mixed effects regression..
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
emmeans <- emmeans(mod7r, pairwise~Behavior, adjust="Tukey", lmer.df="satterthwaite")

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
emmeans.r <- emmeans(mod7r.r, pairwise~Behavior, adjust="Tukey", lmer.df="satterthwaite", lmerTest.limit = 6121)

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





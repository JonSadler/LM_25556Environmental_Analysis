#•••••••••••••••••••••••••••••••••••••••••••
##  CLASS EXERCISES                        •
#•••••••••••••••••••••••••••••••••••••••••••
# Do the following for all three data files:
# Author Jon Sadler
# 1. load in the data and examine it's structure (including experimental balance)
# 2. Posit your hypotheses (null and alternative)
# 3. play with some pictures
# 4. draw a contingency table to figure out how the data are structured
# 5. Select an appropriate ANOVA model
# 6. Validate the model
# 7. Briefly interpret the results - in your script file!!!!

#Updated March 10th 2022
# EXERCISE 1
# -----------------------------------------------------------------------------------------
# Filename: Hoglouse.csv - a file of water louse distribution along a rivers in Devon
# response - hoglouse numbers
# explanatory variable - Upper, Mid and lower sites (i.e.  longitudinal profile)
# see Gardner's book....on the reading lists
# ----------------------------------------------------------------------------------------
Hog <- read.csv(file.choose())
Hog
str(Hog)
View(Hog)
glimpse(Hog)

# Make a picture
boxplot(Hog$Count ~ Hog$ Site, col = "blue", ylab = "Number") # Not symetrical so not normally distributed

hist(Hog$Count, main = "", col = "red", xlab = "Count") # Doesn't look promising for normality
qqnorm(Hog$Count); qqline(Hog$Count)
shapiro.test(Hog$Count)

# What about heterogeneity of variances....?
require(car)
leveneTest(Hog$Count, Hog$Site)

Upper <- subset(Hog, Site == "Upper")
Mid <- subset(Hog, Site == "Mid")
Lower <- subset(Hog, Site == "Lower")
var(Upper$Count)
var(Mid$Count)
var(Lower$Count)

# Conclude not normally distributed with homogeneity of variance marginal = needs a non-parametric test

kruskal.test(Hog$Count ~ Hog$Site)

# Interpretion
# difference between the means, especially in the mid regions of the river.

# EXERCISES 2
# -----------------------------------------------------------------------------------------
# Filename: Medley.csv
# Medley and Clements (1998) investigated the impact of zinc contamination (and other
# heavy metals) on the diversity of diatom species in the USA Rocky Mountains (from
# Box 8.1 of Quinn and Keough (2002))
# File contents:
# DIATOM - number of different species of diatoms on the a known area of rocks in streams (continous variable)
# ZINC - mpm of zinc in the water column (background, low, medium, high) (factor - explanatory variable)
# ----------------------------------------------------------------------------------------
Algal <- read.csv(file.choose())
Algal
str(Algal)
#note ZINC is a character and need to be a factor
Algal$ZINC <-as.factor(Algal$ZINC)
# check it is now a factor
library(tidyverse)
glimpse(Algal)
View(Algal)

# Make some pictures
boxplot(DIVERSITY ~ ZINC, data = Algal, col = "blue", ylab = "Diversity")
qqnorm(Algal$DIVERSITY); qqline(Algal$DIVERSITY)
shapiro.test(Algal$DIVERSITY)

# Variances.....?
leveneTest(Algal$DIVERSITY, Algal$ZINC)

# One-way ANOVA is appropriate do we see a difference?
summary(aov(DIVERSITY ~ ZINC, data = Algal)) # Note we can nest the summary command in the Anova call
# or put into an object to use later
Model <- aov(DIVERSITY ~ ZINC, data = Algal)

autoplot(Model) # from ggfortify library
plot(Model)

# Interpretation:
# ZINC levels impact the diversity of the diatom communities
# All other factors (e.g. flow, nutrient status, pH) being stable
# Extra bonus mark if you used a Tukey posthoc test to examine the difference between the means
TukeyHSD(Model)


# EXERCISE 3
# -----------------------------------------------------------------------------------------
# Filename: Quinn.csv (data from By G. P. Quinn and M. J. Keough, 2002 - "Experimental Design and Data Analysis for Biologists" )
# File contents:
# DENSITY - Limpet density treatment (L1 = 8 individuals per 225 cm2 enclosure, L2 = 15, L3 = 30 and L4 = 45) (factor - explanatory variable)
# SEASON - Season of the year (Spring or summer) (factor - explanatory variable)
# EGGS - egg production by limpets (continous response variable)
# ---------------------------------------------------------------------------------------
# we're going to use a two-anova to analyse this....
Limpet <- read.csv(file.choose())
# Plot the lines and points one plot to compare them
names(Limpet)
str(Limpet)
# change SEASON and DENSITY to factors
Limpet$DENSITY <- as.factor(Limpet$DENSITY)
Limpet$SEASON <- as.factor(Limpet$SEASON)

plot(EGGS ~ DENSITY, data = Limpet, pch = 19,cex = 1.5,
     ylab = list("Eggs Produced", cex = 1.2))

plot(EGGS ~ SEASON, data = Limpet, pch = 19,cex = 1.5,
     ylab = list("Eggs Produced", cex = 1.2))
# both indicate likely differences for each factor

# Check assumptions
# normality

qqnorm(Limpet$EGGS); qqline(Limpet$EGGS)
shapiro.test(Limpet$EGGS)
# Data are normally distributed....

# check for homogeneity of variance
leveneTest(EGGS~DENSITY, data = Limpet)
leveneTest(EGGS~SEASON, data = Limpet)
# all fine....to do it graphically and account for both factors we need to run the model!

#plot interaction
interaction.plot(Limpet$DENSITY, Limpet$SEASON, Limpet$EGGS, fun = mean)


# Run ANOVA with full interactions
Limpet_aov <- aov(EGGS ~ SEASON * DENSITY, data = Limpet)

# Look at the output
summary(Limpet_aov)
# Results indicate that SEASON and DENSITY impact egg production. 
# There is no interaction between the two explanatory factors. 

# Confirm all diagnostics
op <- par(mfrow = c(2, 2)) # set graphics device toplot four graphs in a 2 x 2 matrix
plot(Limpet_aov)
par(op)
# Or 

library(ggfortify)
autoplot(Limpet_aov)

# No visible issues! Looks excellent.

# EXERCISE 4 [ PLEASE USE THIS AS YOUR FORMATIVE EXERCISE]
# -----------------------------------------------------------------------------------------
# Filename: fish_pred.csv (data from Doncaster and Davey, 2002, p. 50)
# fish predation experiment - using enclosures of two species of fish to assess their impact on predation on chironomids
# File contents:
# Density - density of chironomids left activty fish predation exercise (response variable - continuous)
# Loach - Presence of a loach in the enclosure (factor - 0 = absent, 1 = present)
# Bullhead - Presence of a bullhead in the enclosure (factor - 0 = absent, 1 = present)
# ----------------------------------------------------------------------------------------



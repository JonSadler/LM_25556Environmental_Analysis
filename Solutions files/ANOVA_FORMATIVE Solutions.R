# MSc / MSci STUDENT EXERCISE 3
# This is a formative exercise due for submission. See canvas pages for details on the assessment
# -----------------------------------------------------------------------------------------
# Filename: fish_pred.csv (data from Doncaster and Davey, 2002, p. 50)
# fish predation experiment - using enclosures of two species of fish to assess their impact on predation on chironomids
# File contents:
# Density - density of chironomids left activty fish predation exercise (response variable - continuous)
# Loach - Presence of a loach in the enclosure (factor - 0 = absent, 1 = present)
# Bullhead - Presence of a bullhead in the enclosure (factor - 0 = absent, 1 = present)
# ----------------------------------------------------------------------------------------

pred <- read.csv(file.choose())
str(pred)
head(pred)
pred

# Check for balance

table(pred$Bullhead, pred$Loach)
# Fine so we can go with 2 way ANOVA (as we have one response and two FACTORS)

# Draw some pictures
# Boxplots
par(mfrow = c(1, 2)) # 1 row , 2 columns for the images
boxplot(Density ~ Bullhead, col = "lightblue", data = pred, xlab = "Bullhead")
boxplot(Density ~ Loach, col = "blue", data = pred, xlab = "Loach")

# Variances look okay and the pattern indicates that Loach are good predators and reduce midge density but Bullheads don't.

# Check assumptions
# normality

qqnorm(pred$Density); qqline(pred$Density)
shapiro.test(pred$Density)
# Data are normally distributed....

# Homogeneity of variance
# Check for residual spreads indicating heterogeneity of variance
library(alr3)    # New package do some reading....
resplot(lm(Density~Bullhead+Loach, pred))

require(car)
leveneTest(pred$Density ~ pred$Bullhead * pred$Loach)
# No issues visible

#plot interaction

interaction.plot(pred$Bullhead, pred$Loach, pred$Density, col=c(2,3), xlab = "Predators", ylab = "Midge Density", trace.label = "")
# Indicates no interaction but it is sensible to include the interaction in the plot anyway

# Run ANOVA
pred_aov <- aov(Density ~ Bullhead * Loach, data = pred)

# Look at the output
summary(pred_aov)
# Results indicate that Loach reduces midge density but bullheads don't. There is no interaction between the two explanatories (or the fish), so the addition of bullheads to the cages did not result in greater impact on midge density.

# Confirm all diagnostics
par(mfrow = c(2, 2)) # set graphics device toplot four graphs in a 2 x 2 matrix
plot(pred_aov)
# Slight wedge in res v fit plot and dip in Scale - location.....these are not worrying patterns (but you already know this as you checked the assumptions earlier)

# ••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••
# Do the following for all three data files:
# EXERCISES for Doctoral Researchers

# 1. load in the data and examine it's structure (including experimental balance)
# 2. Posit your hypotheses (null and alternative)
# 3. play with some pictures
# 4. draw a contingency table to figure out how the data are structured
# 5. Select an appropriate ANOVA model
# 6. Validate the model
# 7. Briefly interpret the results - in your script file!!!!

# DR STUDENT EXERCISE 1
# -----------------------------------------------------------------------------------------
# Filename: Medley.csv
# Medley and Clements (1998) investigated the impact of zinc contamination (and other
# heavy metals) on the diversity of diatom species in the USA Rocky Mountains (from
# Box 8.1 of Quinn and Keough (2002))
# File contents:
# DIATOM - number of different species of diatoms on the a known area of rocks in streams (continous variable)
# ZINC - Season of the year (Spring or summer) (factor - explanatory variable)
# ----------------------------------------------------------------------------------------

# DR STUDENTS EXERCISE 2
# -----------------------------------------------------------------------------------------
# Filename: Urchin.csv (data from By G. P. Quinn and M. J. Keough, 2002 - "Experimental Design and Data Analysis for Biologists" )
# File contents:
# DENSITY - urchin density treatment (L1 = 8 individals per 225 cm2 enclosure, L2 = 15, L3 = 30 and L4 = 45) (factor - explanatory variable)
# SEASON - Season of the year (Spring or summer) (factor - explanatory variable)
# EGGS - egg production by limpets (continous response variable)
# ----------------------------------------------------------------------------------------


# load data file
urchin <- read.csv(file.choose())
str(urchin)
names(urchin)

# attach data file
attach(urchin)

# I assume you have done the usual visualisations and the like!!!!

# examine treatment effect....
TreatEffect <- aov(ALGAE ~ TREAT + Error(TREAT : PATCH))
summary(TreatEffect)

# Examine patch effect by comparing the nested models
PatchEffect <- anova(aov(ALGAE ~ TREAT), aov(ALGAE ~ TREAT / PATCH), test = "F")

# Look at the results
PatchEffect

# So we can see that urchin removal does not impact filamentous algae growth
# But algal growth is different between the 16 patches randomly selected for the experiment

# DR STUDENTS EXERCISE 4
# -----------------------------------------------------------------------------------------
# Filename: urchin.csv (data from By G. P. Quinn and M. J. Keough, 2002 - "Experimental Design and Data Analysis for Biologists" )

# File contents:
# TREAT - urchin density treatment (con=original density, t0.66=66% original density, t0.33=33% original density, rem=all urchins removed) (factor variable - explanatory)
# PATCH - random nesting factor (treatment replicated within four patches) - there are 5 quadrat per patch (factor - explanatory)
# ALGAE - percentage cover of filimentous algae (Response - continuous)
# ----------------------------------------------------------------------------------------



#Prepare the data frame
 aovdata <- read.table("Worked 3.txt", header = T)
 attach(aovdata)

 # Create factors for the explanatory variables
 Shore <- factor(Shore) ; Recruit <- factor(Recruit) ; Trtmnt <- factor(Trtmnt)

#Commands for factorial analysis
 model3_3i <- aov(Density ~ Recruit*Trtmnt + Error(Recruit:Shore + Recruit:Shore:Trtmnt))
 summary(model3_3i)
 anova(aov(Density ~ Recruit/Shore + Recruit*Trtmnt), aov(Density ~ Recruit/Shore*Trtmnt), test = "F") # test Trtmnt:Recruit/Shore (and calculation of F-value for Recruit:Shore by hand from residual error)



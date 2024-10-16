# WEEK 8 solution files
# Let's conclude with an example of multiple logistic regression
# Data are derived from Bolger et al. (1997) study of habitat fragmentation on rodent pops
# response = presence/absence of rodents
# explanatories - area of canyon fragment, % cover of shrub, distance to nearest canyon

# Import the data / check the structure etc
# Call it bolger please.....
# Do some plotting!!!!
# Investigate collinearity with a scatterplotmatrix
# and some VIFs
# Now estimate the dispersion parameter
# Confirm log odds ratio linearity
# Check for influential sample points
# Run the model and interpret it...
# Check the log odds ratio and interpret that..
# Do some model selection
# Refit the best model and generate the regression equation
# Create the plot to summarise the relationship - use the predict function

# Import the data
bolger <- read.csv(file.choose())
str(bolger)
head(bolger)

# Need CAR package
library(car)

# Investigate collinearity with a scattermatrixlibrary(car)
scatterplotMatrix(~RODENTSP + DISTX + AGE + PERSHRUB, data=bolger,diag = list(method = "boxplot"))
# No issues suggested

# and some VIFs - note we ar not suing the full interactive model
#This because the interactions lead to huge VIFs. Have a look.

bolger.glm <- glm(RODENTSP ~ DISTX + AGE + PERSHRUB, family = binomial, data = bolger)
vif(bolger.glm)
# All below 3 so fine

# Now estimate the dispersion parameter
bolger.glm$deviance/bolger.glm$df.resid
# No issue here either.....

# Confirm log odds ratio linearity
crPlots(bolger.glm)
# No significantly worrying problems - age not great. NOTE the possible outliers in DISTX and AGE

# Check for influential sample points
influence.measures(bolger.glm) # nothing too worrying here...but 19?

# graphically plot the Cooks distances
plot(bolger.glm, which = 4) # 19 is high but not 1. 
# I'd  probably remove it but let's plough on because the author of this exercise didn't...

# Look at the outcomes of the model. You've already run this.
summary(bolger.glm)
# So the probability of rodent occurrence increases with % shrub cover but not age 
# since isolation or distance to nearest canyon fragment

# Check the log odds ratio and interpret that..
## odds ratios and 95% CI
exp(coef(bolger.glm))
# the chances of rodent presences increases slightly (10%)
# for every 1% increase in shrub cover. NOTE this is because 1.10 > 1.00

# Do some model selection
# We have an AIC because we used a log link function so MuMIn should work
library(MuMIn)
options(na.action=na.fail) # set options in Base R concerning missing values
summary(model.avg(dredge(bolger.glm), fit = TRUE, subset = TRUE))

# Best model only includes percentage shrub cover. Notice relative variable variance
options(na.action = "na.omit") # reset base R options

# Refit the best model 
bolger.glm <- glm(RODENTSP ~ PERSHRUB, family = binomial, data = bolger)
summary(bolger.glm)

# Create the plot to summarise the relationship - use the predict function
# Calculate predicted values based on fitted model
xs<-seq(0,100,l=1000)
bolger.predict <- with(bolger, (predict(bolger.glm, type="response", se=T, newdata=data.frame(DISTX=mean(DISTX), AGE=mean(AGE), PERSHRUB=xs))))
# The mean argument sets means for the non-significant terms as constant
# The response argument plots on the original scale as the it is a log link function
# Produce base plot
plot(RODENTSP~PERSHRUB, data=bolger, xlab=NA, ylab=NA, axes = FALSE, pch=16)
# Plot fitted model and 95% CI bands
points(bolger.predict$fit~xs, type="l", col="gray")
lines(bolger.predict$fit+bolger.predict$se.fit ~ xs, col="gray", type="l", lty=2)
lines(bolger.predict$fit-bolger.predict$se.fit ~ xs, col="gray", type="l", lty=2)
# Axes titles
mtext("Native rodent presence/absence", 2, line=3)
axis(2,las=2)
mtext("Percentage shrub cover",1, line=3)
axis(1)
box(bty="l")

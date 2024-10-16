# **************************************
# PART FOUR: CLASS EXERCISES           *
# **************************************
#
# 1.  Use the ozone.data.csv datafile:
# 		(a) use the datafile called ozone.data.csv;
#		(b) explore the data to look at its structure, normality, linearity, heterogeneity;
#		(c) create coplots to look for interactions between the variables;
# 		(d) create a multiplicative linear model to illustrate the what factors influences ozone levels in the city;
#		(e) Use model averaging to select the most parsimonious model;
#		(f) validate the 'best' model;
#		(g) create the predictive model and generate the regression equation.

#ozone.data.csv DATAFILE CONTENTS:
# Based on Daily readings of the following air quality values for May 1, 1973 (a Tuesday)
# to September 30, 1973 in New York.
# 
# Response - ozone: Mean ozone in parts per billion from 1300 to 1500 hours at Roosevelt Island
# Explanatory - rad: Solar radiation in Langleys in the frequency band 4000–7700 Angstroms from 0800-1200 hours at Central Park
# Explanatory - wind: Average wind speed in miles per hour at 0700 and 1000 hours at LaGuardia Airport
# Explanatory - temp: Maximum daily temperature in degrees Fahrenheit at La Guardia Airport

# Load file (file is in Dropbox; Teaching material; Courses...)
Ozone <- read.csv(file.choose(), header = TRUE)


# look at data structure
str(Ozone)
head(Ozone)

# Draw pictures to explore it....
# Boxplots - normality
op <- par(mfrow = c(2,2))
boxplot(Ozone$ozone, ylab="Ozone")
boxplot(Ozone$rad, ylab = "Solar Radiation")
boxplot(Ozone$wind, ylab = "Wind")
boxplot(Ozone$temp, ylab = "Temperature")
par(op)
# Generally okay; wind and ozone have a few extreme values but it should be fine

# relationships between response and explanatories
library(car)
scatterplotMatrix(~ozone + rad + wind + temp, data = Ozone, diagonal = list(method = "qqplot")) # use qqplot not boxplot
# Mixture of humped and linear x/Y relationships. 
# Ozone ~ rad looks quadratic....ozone and rad slightly off normal. 
# Might consider log10 transform if models are a mess.
# QQplots indicate that O2 and rad are a problem

# generate first full model using linear function. The full model uses all the interactions.

M1 <- lm(ozone ~ rad * wind * temp, data = Ozone)

# look at summary
summary(M1)
# Nothing appears to be significant. 
# need to simplify this a little....but first we need to check for inflationary values
# Note that any exponent will lead to inflation....

vif(lm(ozone ~ rad * wind * temp, data = Ozone))
# but interaction terms are huge so remove the 3-way and largest 2-ways ones.

vif(lm(ozone ~ rad + wind + temp+ wind:temp, data = Ozone))
# Chuck out wind as it is huge leaving the interaction with wind and temp.
# Our understanding how wind impacts temperature and ozone production suggests this is sensible.

vif(lm(ozone ~ rad + temp + wind:temp, data = Ozone))
# no issues

M2 <- lm(ozone ~ rad + temp + wind:temp, data = Ozone)

#validate
op <- par(mfrow = c(2,2))
plot(M2)
par(op)
# It's a mess.....heterogeneous residuals and not especially normally distributed

library(MuMIn)
# set base R to allow na
options(na.action=na.fail) # set options in Base R concerning missing values

M3 <- model.avg(dredge(M2, rank = "AICc")) # code introduces model.avg(), get.models and dredge functions
options(na.action = "na.omit") # reset base R options
summary(M3)
# best models > 2 AICc includes ALL variables, so it is no different from the one about...
# As a result the residuals are still a mess...

# AT THIS POINT WE MIGHT REFLECT ON HOW WE MODEL THESE DATA AND ADD IN SOME QUADRATICS AND MAYBE USE
# GENERALISED LINEAR APPROACHES, SUCH AS POISSON REGRESSION. BUT WE'LL COME ONTO THAT IN THE NEXT FEW WEEKS
# we go back to a linear model and transform the response 

M4 <- lm(log10(ozone) ~ rad + temp + wind:temp, data = Ozone)

op <- par(mfrow = c(2,2))
plot(M4)
par(op)
# looks pretty good...

# plot residuals against explanatory variables....

op <- par(mfrow = c(2,2)) # We cannot use the exponents here!
plot(M4$resid ~ Ozone$rad) 	# Looks okay
plot(M4$resid ~ Ozone$temp)	# Looks okay
plot(M4$resid ~ Ozone$wind)	# Looks okay
par(op)

# We could now do some plotting / prediction.....but I'll leave that code up to you

# NOTE: you'll get a very similar outcome if you model without the interaction. And no one would complain!
M5 <- lm(log10(ozone) ~ rad + temp + wind, data = Ozone)
op <- par(mfrow = c(2,2))
plot(M5)
par(op)

# lowest AIC makes some sense:
AIC(M4,M5)
# M4 with interaction has the lowest AIC (it's negative!)
# plus the residual plots for M4 are slightly better

*********************************
  # PART FOUR: CLASS EXERCISES      *
  # *********************************
  
  # PART ONE : POLYNOMIAL REGRESSION (you're combining model selection and polynomial regression here!)
  # 	1. Use the mytilus.csv datafile (see Logan 2010 pp. 244-248;for data and analysis) :
  #		(a) explore the data to look at its structure, normality; 
  #		(b) Assess its linearity (using the Car package - 'you've done this before');
  #		(c) add in polynomial terms for the distance variable up to the 5th order (quintic model);
  #		(d) validate the model. NOTE: 5 polynomials may overfit the model. Most biologists would have kittens over its use! What is the 		correct polynomial fit? That is compare the models.....
  #		(e) create the predictive model and generate the regression equation.
  # Data description: the data are Lap94 allele frequency data from mussels at increasing distances from Southport harbour
# Response is LAP
# Explanatory is DIST
#  IMPORTANT - fit a transformation to the response from the outset using an arcsin transformation  - arc(sqrt(LAP)) 
# the transformed code is Y in your regression equation.....

# load libraries
library(tidyverse)
library(mgcv) # for GAMs
library(car)
library(ggfortify)
library(MuMIn)

###################################################
### 
###################################################
# use file.choose here....
mytilus <- read.csv(file.choose())


###################################################
### confirm lack of linear fit....
###################################################
# using car package

scatterplot(asin(sqrt(LAP))*180/pi ~DIST, data=mytilus)

# check residuals

plot(lm(asin(sqrt(LAP))*180/pi ~DIST, data=mytilus), which=1)

###################################################
### Fit 5th order polynomial
###################################################
mytilus.lm5<- lm(asin(sqrt(LAP))*180/pi ~ DIST + I(DIST^2) +I(DIST^3) + I(DIST^4) + I(DIST^5), mytilus)

###################################################
### Check diagnostic - no clear wedge; might be okay
###################################################
plot(mytilus.lm5, which=1)
summary(mytilus.lm5)
###################################################
### Examine contribution of powers to model. anything beyond cubic (3rd order) adds nothing..
###################################################
anova(mytilus.lm5)

###################################################
### respecify models and check whether quadratic is better
###################################################
mytilus.lm1<- lm(asin(sqrt(LAP))*180/pi ~DIST, mytilus)
mytilus.lm2<- lm(asin(sqrt(LAP))*180/pi ~DIST+I(DIST^2), mytilus)
anova(mytilus.lm2, mytilus.lm1)
mytilus.lm3<- lm(asin(sqrt(LAP))*180/pi ~DIST+I(DIST^2)+I(DIST^3), mytilus)
anova(mytilus.lm3, mytilus.lm2)

AIC(mytilus.lm1,mytilus.lm2,mytilus.lm3)
###################################################
### add in terms and check for improvement 
### cubic fits data better than qradratic (p=0.0183); but quadratic is not better than linear (p=0.087)
###################################################

###################################################
### check model coefficients for plotting
########################
summary(mytilus.lm3)

###################################################
### plot graph
###################################################
plot(asin(sqrt(LAP))*180/pi ~ DIST, data=mytilus,pch=16, axes=F, xlab="", ylab="")
axis(1, cex.axis=.8)
mtext(text=expression(paste("Miles east of Southport, Connecticut")), side=1, line=3)
axis(2, las=1)
mtext(text=expression(paste("Arcsin ",sqrt(paste("freq. of allele ", italic("Lap"))^{94}))), side=2, line=3)
x<-seq(0,80,l=1000)
points(x,predict(mytilus.lm3, data.frame(DIST=x)), type="l")
box(bty="l")
# Add in se if you wish or CIs....

# plotting using ggplot
ggplot(mytilus, aes(x = DIST , y = asin(sqrt(LAP))*180/pi)) + geom_point() + geom_smooth() + 
  labs(x = "Miles east of Southport, Connecticut", 
       y = expression(paste("Arcsin ",sqrt(paste("freq. of allele ", italic("Lap"))^{94}))))+
  theme_bw()

# PART TWO :NON-LINEAR REGRESSION
# I have no exercise for you here......! My view is that GAMs are the best way to deal with data of this nature
# unless you are planning to do some physical modelling where the maths really matters.

# PART THREE: GENERALISED ADDITIVE MODELLING (GAMS)
#		1. Datafile = old faithful. It is in the base system already as a dataframe called "faithful".  
#		(a) Explore the data (use the car package scatterplot)
#		(b) Generate a linear model - you've already done this once!
# 		(c) Generate a GAM
#		(d) Compare the two models using AIC

scatterplot(eruptions ~ waiting, data = faithful)
# doesn't look especially linear but we'll compare a linear model and GAM
# No massive normality issues

# we use a glm with a guassian error structure because we'll run a gam too
M1 <- glm(eruptions ~ waiting, family = gaussian, data = faithful)

# look at it
summary(M1)

# validate
op <- par(mfrow = c(2, 2)) 
plot(M1)
par(op)

# Although there are clear blocks no evidence of increasing spread with fit
# But do we get a better model using a gam?

# uses mcgv package
M2 <- gam(eruptions ~ s(waiting), family = gaussian, data = faithful)
summary(M2)
# look at the smoother....
plot(M2) # indicates nonlinearity

# validate
gam.check(M2) # pretty good 

# compare models using AIC
AIC(M1,M2)

# glm is better!! Not what we expected. Missing values between blocks are causing this..
# create a plot of these.....and use predict....up to you here as it is not really required on the exercise..but it is good practice. 

#		2. Use the vegetation.csv data to create a multiple factor GAM 
# Use everything but disregard Time and Transect
#		(a) Explore the data
#		(b) Look for correlations between the variables to check for covariability
#		(c) Create a GAM
#		(d) Do some model selection (using the same approach as a linear model)
#		(e) Validate the best model

# Load the data
Veg <- read.csv(file.choose())
# Look at it

glimpse(Veg) 
head(Veg)
# Experiment to look at grassland richness over time in Yellowstone National Park
# The study Skink et al. 2007 identified a range of important variables and we are going to use those
# 
# Response variable = SR or species richness of plants per site
# Explanatory - Rock content (ROCK)
# Explanatory - baresoil (BARESOIL)
# Explanatory - Litter (LITTER)
# Explanatory - ppt in Autumn (FallPrec)
# Explanatory - Max Spring temperature (SprTmax)
# Explanatory - Year of transect (Time)
# Explanatory - Transect number (Transect)

# Use everything but disregard Time and Transect; 
# IMPORTANT note - we'll revisit this as it really should be modelled in a different way

# - if you need it for a scatterMatrixplot in car
# Check for covariation
 scatterplotMatrix(~R + ROCK + BARESOIL + LITTER + FallPrec + 
                    SprTmax, data = Veg, diag = list(method = "boxplot"))
# Normality looks okay (less so in the histogram) but some wiggly relationships so we opt to use a GAM	

# Check for covariability ahead of model creation
vif(lm(R ~ ROCK + BARESOIL + LITTER + FallPrec + 
         SprTmax, data = Veg))
# All below 3 so we are good to go - or would be if it were a linear model. That is not especially appropriate so we'll use pairplots to look for correlations over 0.5.

# Pairplots for correlations
# little function to take correlation coefficients and print them on the top panel
# Function source: Zuur et al. (2009) - Mixed Models
panel.cor <- function(x, y, digits=1, prefix="", cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r1=cor(x,y,use="pairwise.complete.obs")
  r <- abs(cor(x, y,use="pairwise.complete.obs"))
  txt <- format(c(r1, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex <- 0.9/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex * r)
}
# function end
# call pairplot 
pairs(~R + ROCK + BARESOIL + LITTER + FallPrec + 
        SprTmax, data = Veg, panel = panel.smooth, upper.panel = panel.cor)
# pairs is in base R
# panel.smooth adds a loess smoother to the plot
# diag.panel = panel.hist - adds histrgram to the diagnol 
# upper.panel calls output from correlations and sits them in the top panel
# No massive worries in terms of +ve correlations - strong -ve correlations. might not be an issue.....


# Run initial full model
M1 <- gam(R ~ s(ROCK, bs = "cs") + s(BARESOIL, bs = "cs") + s(LITTER, bs = "cs") + s(FallPrec, bs = "cs") + s(SprTmax, bs = "cs"), data = Veg)
# note we using a General additive not generalised additive model so there is no family option
# new bit of code , bs = "cs", this adds regression spline shrinkage. This allows smoother to have 0 df. Then we can remove them from the model

# Check results
summary(M1)
# not all smoother terms are significant

# plot smoothers
op <- par(mfrow = c(2, 3)) 
plot(M1, residuals=TRUE) # extra code plots the residuals against the smoother.
par(op)

op <- par(mfrow = c(2, 2)) 
gam.check(M1)
par(op)
# complex response of SR to some covariates. These are never easy to interpret ecologically!

# Find best fitting model using model averaging via MuMIn
# set base R to allow na
options(na.action=na.fail) # set options in Base R concerning missing values

# run model
M2 <- model.avg(dredge(M1, rank = "AICc")) # code introduces model.avg(), get.models and dredge functions
summary(M2)
# dredge selects BARESOIL, LITTER, ROCK, SprTmax as the best model (drop in AIC to full model is 4.48); throws out FallPrec

# Rerun model with reduced terms (covariables 1, 3,4,5) # excludes FallPrec
M3 <- gam(R ~ s(ROCK, bs = "cs") + s(BARESOIL, bs = "cs") + s(LITTER, bs = "cs") + s(SprTmax, bs = "cs"), data = Veg)

# validate the model
par(mfrow=c(2,2))  
gam.check(M3)  
# These look pretty good. Slight indication of heterogeneity in plot 2. 
# Normality is marginal too but I can live with it
# k values are good s(SprTmax) is a little under 1 but p value is not significant

#Check residuals against explanatories (all look pretty good):
op <- par(mfrow = c(2,2)) # 
plot(M3$resid ~ Veg$ROCK) 	# Looks okay
plot(M3$resid ~ Veg$BARESOIL)	# Looks okay
plot(M3$resid ~ Veg$LITTER)	# Looks okay
plot(M3$resid ~ Veg$SprTmax)	# Looks okay
par(op)

# Does not make much sense to predict from a multivariate model but you may need to display individual variables.
# If so the plot them using predict on their normal Y axes as we have done in the past.
# You can get partials for GAMs by using the plot command 

# Plot the new smoother. Notice they do change....from M1 above.
op <- par(mfrow = c(2, 2)) 
plot(M3, residuals=TRUE) # extra code plots the residuals against the smoother.
par(op)

# You can check the effect size of each variable by partialing the deviance....but that is not for here!

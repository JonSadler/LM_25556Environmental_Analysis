# WEEK 5 Solutions

# EXERCISE 1
# look at the data in the base file
str(faithful)
faithful

# plot some pictures
require(car)
scatterplot(eruptions ~ waiting, data = faithful)	

# Looks decent in terms of the boxplots - indicates normality and homogeneity of variance
# Maybe an issue with linearity of the response to explanatory but we'll have a look

# Specify and examine linear model
summary(lm(eruptions ~ waiting, data = faithful))
anova(lm(eruptions ~ waiting, data = faithful))

# write the model to an object
faithful.lm <- lm(eruptions ~ waiting, data = faithful)
# look at plots for validation
op <- par(mfrow = c(2, 2)) 
plot(faithful.lm)
par(op)

# QQ-plot indicates normality
# residuals v fitted suggests limited heterogeneity that is a result of two clumps of points
# Scale v location indicates homogeneity
# Leverage plot is fine

# plot residuals against explanatory
plot(faithful.lm$resid ~ faithful$waiting,
	xlab = "Waiting time (mins)",
	ylab = "Model residuals")
# Not brilliant but we'll go with it for the time being	
	
# Decision will run with a linear model
# We'll revisit this in a week or so to run another regression technique

# plot model and add R-square etc
plot(eruptions ~ waiting, data = faithful,
	xlab = "Waiting time between eruptions (mins)",
	ylab = "Eruption duration (mins)",
	pch = 21, col = "grey", bg = "grey") # We've left the axes on
# add the regression line from the model (faithful.lm) using abline.....
abline(faithful.lm, col="black")
# add the equation
text(99,2, "eruptions = 0.0756waiting + -1.874", pos = 2, cex=0.65)
# add r-square value
text(99,1.75, expression(paste(R^2 == 0.8108)), pos = 2, cex = 0.6) # add in text using the expression/paste functions
# create a sequence of 1000 number spanning the range of humidities (min to max)
x <- seq(min(faithful$waiting), max(faithful$waiting), l=1000)  # notice this is an 'l' = length. NOT a 1!!!!!!!
#for each value of x, calculate the upper and lower 95% confidence
y<-predict(faithful.lm, data.frame(waiting=x), interval="c")
#plot the upper and lower 95% confidence limits
matlines(x,y, lty=3, col="black") # This function add the CIs, lty = line type (dashed)

# EXERCISE 2
# load data file
Mussel <- read.csv(file.choose())

# Look at it
str(Mussel)
head(Mussel)

scatterplot(INDIV ~ AREA, data = Mussel)
# This indicates that the data are not normaly disributed (especially AREA)
# The abundance data don't look too good either. Huge peak in the middle
# outliers in both!!!!

# Let's fit a linear model nonetheless
mussel.lm <- lm(SPECIES ~ AREA, data = Mussel)

# look at it
summary(mussel.lm)

# Now check assumption by using R's inbuilt model validation plot defaults
# set graphics parameters because we want all the plots on one graphic
op <- par(mfrow = c(2, 2))  # this gives us a 2 x 2 panel with four images and allocates it to object 
# Plot the diagnostics
plot(mussel.lm)
par(op)    # turns graphics device back to default of one plot per page!

# Residuals v Fitted indicate a problem. It's wedge shaped and humped!
# qqplot is a bit dodgy but might be okay
# Scale-Location plot indicates a wedge
# Cook distance / leverage looks okay - but there are some large values in area

# FINAL VALIDATION TASK - residuals against explanatory variable
plot(mussel.lm$resid ~ Mussel$AREA,
	xlab = "Mussel bed Area", 
	ylab = "Residuals")
# This indicates a few large values and a slight wedge due to numerous small patches
# Conclusion we reject the model

# So what do we do?
# We can linearise the explanatory variables by transforming them and re-run the model
# I am not a fan of this - we'll look at other approaches next week!
# and repeat the validation

# Let's fit a linear model nonetheless
mussel.lm1 <- lm(SPECIES ~ log10(AREA), data = Mussel)

# look at it
summary(mussel.lm1)

# Now check assumption by using R's inbuilt model validation plot defaults
# set graphics parameters because we want all the plots on one graphic
op <- par(mfrow = c(2, 2))  # this gives us a 2 x 2 panel with four images and allocates it to object 
# Plot the diagnostics
plot(mussel.lm1)
par(op)    # turns graphics device back to default of one plot per page!

# Residuals v Fitted are better but there is still a bit of wedge at higher fitted values
# qqplot is a better
# Scale-Location plot indicates a slight wedge with higher values
# Cook distance / leverage looks improved

# FINAL VALIDATION TASK - residuals against explanatory variable
plot(mussel.lm1$resid ~ log10(Mussel$AREA),
	xlab = "Mussel bed Area", 
	ylab = "Residuals")
# This indicates a few large values and a slight wedge due to numerous small patches
# Conclusion we reject the model

# Try log10 on the response - not unusual with abundance data
# Let's fit a linear model nonetheless
mussel.lm2 <- lm(log10(INDIV) ~ log10(AREA), data = Mussel)

# look at it
summary(mussel.lm2)

# Now check assumption by using R's inbuilt model validation plot defaults
# set graphics parameters because we want all the plots on one graphic
op <- par(mfrow = c(2, 2))  # this gives us a 2 x 2 panel with four images and allocates it to object 
# Plot the diagnostics
plot(mussel.lm2)
par(op)    # turns graphics device back to default of one plot per page!

# Residuals v Fitted are much better 
# qqplot is okay - slight issue on lower values
# Scale-Location plot is fine
# Cook distance / leverage looks okay

# FINAL VALIDATION TASK - residuals against explanatory variable
plot(mussel.lm2$resid ~ log10(Mussel$AREA),
	xlab = "Log Mussel bed Area", 
	ylab = "Residuals")
# This much improved
# Conclusion we accept the model

#Plot the graph add equations etc
plot(log10(INDIV) ~ log10(AREA), data = Mussel,
	xlab = "Log Mussel clump area (mm2)",
	ylab = "Log Number of Individuals",
	pch = 21, col = "grey", bg = "grey") # We've left the axes on
# add the regression line from the model (faithful.lm) using abline.....
abline(mussel.lm2, col="black")
# add the equation
text(4.0, 1.4, expression(paste(Log[10], "INDIV = 0.835", log[10], "AREA - 0.576", pos = 2)), cex = .7)
# add r-square value
text(4.0, 1.3, expression(paste(R^2 == 0.852)), cex = .7) # add in text using the expression/paste functions
# create a sequence of 1000 number spanning the range of humidities (min to max)
x <- seq(min(Mussel$AREA), max(Mussel$AREA), l=1000)  # notice this is an 'l' = length. NOT a 1!!!!!!!
#for each value of x, calculate the upper and lower 95% confidence
y<-predict(mussel.lm2, data.frame(AREA=x), interval="c")
#plot the upper and lower 95% confidence limits
matlines(log10(x),y, lty=3, col="black") # This function add the CIs, lty = line type (dashed) 
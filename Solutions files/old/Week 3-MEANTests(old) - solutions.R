# Week 3 : Means tests and intro to linear models
# SOLUTIONS FOR CLASS EXERCISES

# EXERCISE 1:
# (i) plotting a qqplot (use help ?qqplot)
qqnorm(ozone$O3); qqline(ozone$O3) # We use the full response data not the subset

# (ii) using the shapiro-wilks test (use the help system ?shapio.test)
shapiro.test(ozone$O3)

# (iii) calculating the garden variance
var(GardA$O3)
var(GardB$O3)

# (iv) test for heterogeneity of variances using levenes test (You need to load the 'car' library first; then select ?leveneTest)
library(car) # You'll need to install this

leveneTest(O3 ~ Garden, data = ozone)

# CLASS EXERCISE 2: 
# (i) Check for normality, histogram, QQplot, Shapiro-Wilks test
qqnorm(A.test$H); qqline(A.test$H)
shapiro.test(A.test$H)  

# (ii) Check for heterogeneity of variances using a levene test. NOTE there are graphical means of doing this....more next week
leveneTest(A.test$H, A.test$Sam)

# DATA ANALYSIS EXERCISES

# A. Beeeater bird abundances across habitats

Beeeater <- matrix(c(86, 3, 11, 34, 45, 56), 3, 2, byrow = TRUE, dimnames = list(c("Woodland", "Grassland", "Field"), c("Female", "Male")))

Beeeater # Lists the data
str(Beeeater) # Looks at it's structure

# It's a contingency table so we are looking at Chi Square and nothing else

# Draw a picture 
barplot(Beeeater, beside = TRUE, col = c("Powderblue", "Blue", "lightgreen"), 
	ylim = c(0,90), legend=TRUE)

chisq.test(Beeeater)

# Interpretion 
# Males and female Beeeaters differ in their use of habitat

# B: Hoglouse distribution

Hog <- read.csv(file.choose())
Hog
str(Hog)

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
# Big difference between the means, especially in the mid regions of the river.

# C: Algal growth experiment
Algal <- read.csv(file.choose())
Algal
str(Algal)

# Make some pictures
boxplot(Biomass ~ Substrate, data = Algal, col = "blue", ylab = "Weight (mgs)")
qqnorm(Algal$Biomass); qqline(Algal$Biomass)
shapiro.test(Algal$Biomass)

# Variances.....?
leveneTest(Algal$Biomass, Algal$Substrate)

# One-way ANOVA is appropriate do we see a difference? 
summary(aov(Biomass ~ Substrate, data = Algal)) # Note we can nest the summary command in the Anova call

# Interpretation:
# Substrate roughness influences algal growth. Rougher surfaces lead to enhanced growth
# All other factors (e.g. flow, nutrient status, pH) being stable
# Extra bonus mark if you used a Tukey posthoc test to examine the difference between the means
# Week three solutions: data exploration and visualisation
# updated Feb 16th 2021

#*****************************************************************************
# EXERCISE 1: barplots                                                       * 
# Use the cityrain.csv dataset (derived from R Graphics Cookbook)            *
#*****************************************************************************
# 1. Create a barplot comparing the four cities monthly rainfall (HINT: use par() function to sit one over the other;
# think about the X axis!!!)
# 2. Modify the labels and create a title
# 3. Set the bar for each month to a different colour
# 4. Save the output as a PDF file

# This is trickier than it looks. First, you need to figure out what format base R requires your data in for barcharts
# And also what ggplot uses (if you are trying that): ggplot needs long data (row variable) format
# R will order factors alphabetically so you when you plot the months they will start at April.
# So you also need to find out how to preserve the original order of the factors
# R may or may not list every other label on the X axis so we might also need to fix that.
# And lastly you need to know the graphic parameter response to sit one plot on the other (or use a stacked graph!)
# So here are some base R and ggplot examples of how you might deal with it.....

# Base R graphics
rain <- read.csv(file.choose(), header=T)
head(rain)
str(rain)

# Knowing that R is going to order cities by their names it is worth forcing it to order by month (using the order command)
# make V1 an ordered factor
rain$Month <- factor(rain$Month, order = TRUE) # overwrites month with a new factor in the dataframe which is ordered to preserve the mon format

# Barplot() in base R needs the data vectorised, so you need the data in wide format and call
# a colum with thr $ operator or look for a column number using [,n].

# We need labels on the x axis - it won't print them by default, so we have to create them and allocate them using names.arg()
# Note the number of labels must tally with the number of bars!!!!
# Coerce R to print all X labels. Not a good idea to play with defaults so shink the font size
# e.g. 
barplot(rain$NewYork, names.arg=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov", "Dec"),
        xlab = "Month", 
        ylab = "PPT",
        main = "New York",
        cex.names=0.7, # shrink font size so all months are listed (if you wish)
        cex.lab=0.8, # cex.lab=0.8 (scale labels a little larger than months),
        cex.axis=0.8) # cex.axis=0.8 (same for axis labels)  
# or 
barplot(rain[,3], names.arg=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov", "Dec"),
        xlab = "Month", 
        ylab = "PPT",
        main = "New York",
        cex.names=0.7, # shrink font size so all months are listed (if you wish)
        cex.lab=0.8, # cex.lab=0.8 (scale labels a little larger than months),
        cex.axis=0.8) # cex.axis=0.8 (same for axis labels)  
# Find max rainfall from 4 cities to scale the Y axis so they are displayed in the same scale
max(rain[, 2:5]) # 216.4 so scale Y axes to 220 on all graphs

# create graphic window to receive four plots (want them sat on top of each other 1 x 4)
op <- par(mfrow = c(2, 2))

# plug in all the plots....
# remember to change the names and title names
# New York
barplot(rain$NewYork, names.arg=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov", "Dec"),
        xlab = "Month", 
        ylab = "PPT",
        main = "New York",
        cex.names=0.7, # shrink font size so all months are listed (if you wish)
        cex.lab=0.8, # cex.lab=0.8 (scale labels a little larger than months),
        cex.axis=0.8, # cex.axis=0.8 (same for axis labels)  
        ylim = (c(0, 220)))
barplot(rain$Tokyo, names.arg=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov", "Dec"),
        xlab = "Month", 
        ylab = "PPT",
        main = "Tokyo",
        cex.names=0.7, # shrink font size so all months are listed (if you wish)
        cex.lab=0.8, # cex.lab=0.8 (scale labels a little larger than months),
        cex.axis=0.8, # cex.axis=0.8 (same for axis labels)  
        ylim = (c(0, 220)))
# London
barplot(rain$London, names.arg=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov", "Dec"),
        xlab = "Month", 
        ylab = "PPT",
        main = "London",
        cex.names=0.7, # shrink font size so all months are listed (if you wish)
        cex.lab=0.8, # cex.lab=0.8 (scale labels a little larger than months),
        cex.axis=0.8, # cex.axis=0.8 (same for axis labels)  
        ylim = (c(0, 220)))
# Berlin
barplot(rain$Berlin, names.arg=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov", "Dec"),
        xlab = "Month", 
        ylab = "PPT",
        main = "Berlin",
        cex.names=1, # shrink font size so all months are listed (if you wish)
        cex.lab=0.8, # cex.lab=0.8 (scale labels a little larger than months),
        cex.axis=0.8, # cex.axis=0.8 (same for axis labels)  
        ylim = (c(0, 220)))
 # clear graphics window and parameters so next graph will display full frame
dev.off()
# NOTE: as always in R. You get to the same end point in many ways.
# So had the data been format in row variable format, you could have created the four
# vectors using the subset commands
# Also check out the barchart() function in the Lattice package, where you can use the data command
# If you are feeling brave think about how you might automate this using a loop....

# Or alternatively use ggplot graphics
# Data are in the wrong format for ggplot we need to transpose to row variable format
require(tidyr)
rain_long <- rain %>%
  gather(City, Ppt, c(Tokyo,NewYork,London,Berlin))
rain_long # look at the new datafile

# A lot easier.....!
require(ggplot2)
P <- ggplot(rain_long, aes(Month, Ppt)) + geom_bar(stat = "identity") + 
  facet_wrap(~City)
P # Display the plot

# note you still have the ordering issue. The best fix for this is make sure that your month
# column is a date class variable. We'll come back to that as the class progresses.

# It will look cluttered but ggplot does fit the labels to X axis...zoom it to see.
# Note: you need the stat = "identify" element in the geom_bar() because the default is a 
# count and you want to use the values as listed.
# Notice facet_wrap in ggplot scales the y axes to the same size....and add city labels for free!
# Clear to beautify further you need to play with the normal parameters. See ggplot help.

#*****************************************************************************
# EXERCISE 2: boxplots                                                       * 
# Use the Deer.txt dataset                                                   *
#*****************************************************************************
# Create boxplots to illustrate the difference between:
# 1. Animal size v gender
# 2. Change the default colour scheme, add labels to the axes
# 3. Save it as jpeg file

Deer <- read.table(file="Deer.txt", header = TRUE) # read table as it is tab delineated text file
# The file needs to be in your working directory!
str(Deer) # check structure and look for issues.....

# notice Sex is not a factor it is showing as an integer....we need to change it
Deer$Sex <- as.factor(Deer$Sex)

# Would also help to recode it to a factor for the X axis label
Deer$FSex[Deer$Sex=="1"] <- "Male"
Deer$FSex[Deer$Sex=="2"] <- "Female"

# Recall from the workshop that the size variable is (LCT) - which is based on length
boxplot(LCT ~ FSex, 
        xlab = "Gender",
        ylab = "Length (cm)",
        col = "powderblue",
        data = Deer)
# You can save this manually by clicking in "Export" in the panel on the bottom right.

# Or hard code it.....
jpeg(file=DeerBox) # You set the size in pixels etc have a look at help
boxplot(LCT ~ FSex, 
        xlab = "Gender",
        ylab = "Length (cm)",
        col = "powderblue",
        data = Deer) 
dev.off() # Sets device back to the screen....otherwise next graphic will be invisible!

# ggplot file code 
DeerBox <- ggplot(Deer[!is.na(Deer$FSex),], aes(x=FSex, y=LCT)) + 
  geom_boxplot() +  
  xlab("Gender") + # add x label
  ylab("Length (cm)")  #add y label
#ggplot will plot the NAs unless you tell it not too
# In this instance that is all the deer where gender wasn't specified
# Deer[!is.na(Deer$FSex),] tells it to ignore the NA rows.

DeerBox # look at it.

#Save it to a jpeg....
ggsave(file="DeerBox-ggplot.jpg") # note ggplot selects the format from the filename suffix.

#*****************************************************************************
# EXERCISE 3: Scatterplots                                                   * 
# Use the Vegetation.csv dataset                                            *
#*****************************************************************************
# 1. Plot a graph showing Species Richness (R) against Exposed (BARESOIL)
# 2. Add a linear trend line
# 3. Save it as a Tiff file

Veg <- read.csv(file="Vegetation.csv", header= TRUE)
str(Veg) # Look at it.....

# Create plot
plot(x = Veg$BARESOIL, y = Veg$R,
     xlab = "Exposed soil",
     ylab = "Species richness", main = "Soil litter",
     xlim = c(0, 45), ylim = c(4, 19), pch = 19)
M0 <- lm(Veg$R ~ Veg$BARESOIL, data = Veg) # Notice that we have the dependent (response) variable first
abline(M0)

# ggplot code....
ggplot(Veg, aes(x=BARESOIL, y=R)) +
  geom_point(size=3, colour = "grey60") +
  geom_smooth(method=lm) +
  labs(x = "% Soil Litter") +
  labs(y = "Species richness")
# it automatically adds CIS onto the plot

#*****************************************************************************
# EXERCISE 4: coplots                                                        * 
# Use the Vegetation.csv dataset                                            *
#*****************************************************************************
# 1. Plot species richness versus any covariate you like conditional on transect
# 2. Change the colour to powderblue
# 3. Add a regression using the panel function
# 4. Now swap the abline for a loess smoother 

coplot(R ~ LITTER | as.factor(Transect), pch = 19,
       xlab = "% Soil litter", 
       ylab = "Richness", 
       col = "powderblue", 
       data = Veg)

# add regression line using a function
panel.lm = function(x, y, ...) {
  tmp <- lm(y~x,na.action=na.omit) 
  abline(tmp, lwd = 1)
  points(x,y, ...)}
# create plot....
coplot(R ~ LITTER | as.factor(Transect), pch = 19,
       xlab = "% Soil litter", 
       ylab = "Richness", 
       panel=panel.lm,
       col = "powderblue", 
       data = Veg)

# with loess smoother using panel.smooth function....
# create plot....
coplot(R ~ LITTER | as.factor(Transect), pch = 19,
       xlab = "% Soli litter", 
       ylab = "Richness", 
       panel=panel.smooth,
       col = "powderblue", 
       data = Veg)

#*****************************************************************************
# EXERCISE 5: facetting                                                      * 
# Use the deep sea research data (ISIT.txt)                                  *
#*****************************************************************************
# 1. Use the Lattice package to plot depth v bioluminscence for each station. Read the help file to see how do this.
# 2. Use ggplot to do the same thing. 

library(lattice)
ISIT<-read.table("ISIT.txt",header=TRUE)

# basic lattice XY plot without lines
xyplot(Sources~SampleDepth|factor(Station),data=ISIT,
       xlab="Sample Depth",ylab="Sources")

# with panel function to add lines.....
xyplot(Sources~SampleDepth|factor(Station),data=ISIT,
       xlab="Sample Depth",ylab="Sources",
       panel = function(x, y) {
         panel.grid(h=-1, v= 2)
         I1<-order(x)
         llines(x[I1], y[I1],col=1)})

# in ggplot2 ......

require(ggplot2)
ggplot(ISIT, aes(SampleDepth, Sources)) +
  geom_line(color = "black") +
  facet_wrap(~Station) +
  labs(x = "Depth(m)") +
  labs(y = "Bioluminesence")
# Remember to click on "zoom" to look at it
# You'll have noticed that ggplot generates better looking and organised graphics with much
# less code and removing the need for function writing!
# With practice you can hone plots in it to look amazing....worth a bit of effort.
# Nonetheless  - it is important to be able to code base R graphics too!
# Looking at the data you an immediately see that some stations have very few data points (4 and 5)
# While others show different patterns of Biolum with depth
# This impacts how the data can be analysed - so we need to know!
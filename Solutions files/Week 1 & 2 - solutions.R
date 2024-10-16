# WEEK ONE SOLUTIONS

# ****************************************************
# EXERCISE 1 - using the c function to create vectors*
# ****************************************************
#   Farm    Month   Year    Sex   LengthClass   LengthCT    Ecervi    Tb
#   MO      11      00      1     1             75          0         0         
#   MO      07      00      2     1             85          0         0  
#   MO      07      01      2     1             91.6        0         1
#   MO      NA      NA      2     1             95          NA        NA
#   LN      09      03      1     1             NA          0         0
#   SE      09      03      2     1             105.1       0         0
#   QM      11      02      2     1             106         0         0

# These data are a small part of a data file used in Zuur et al. 2009 (see reading)
# Tb is the occurrence of Tuberculosis in cattle on some farms;
# The variables Farm, Month, Year, Sex, LengthClass are self-explanatory
# Ecervi is the occurrence of a cattle parasite

# Using the c function, create a variable that contains the length variable that contains the lengths of seven animals
LengthCT <- c(75, 85, 91.6, 95, NA, 105.1, 106)

# Also create a variable that contains the Tb values
Tb <- c(0, 0, 1, NA, 0, 0, 0)

# What's the average length, sd of the length of the cattle on the farms?
Av.length <-mean(LengthCT, na.rm = TRUE)  # You need the na.rm = TRUE because there is a missing value in the vector
Sd.length <-sd(LengthCT, na.rm = TRUE)

# Display the calculations
Av.length
Sd.length

#*****************************************************************************
# CLASS EXERCISE 2: Using BirdData2 find the following using the [] functions* 
#*****************************************************************************
# 1. data from the last row and last column
BirdData2[8,4]

# 2. rows 3-4 in column called "Head"
BirdData2[3 : 4, 3]
    
# 3. All the data from columns 2 and 3 (you need to use 2: 3 to link the columns)
BirdData2[,2 : 3]
    
# 4. All the weights and write them to a variable Y
Y <- BirdData2[,4]
Y

# 5. All the columns excluding the first and third "Wingcrd" and "Wt"; use the 'c' function to do this
X <- BirdData2[, c(-1, -3)]
X

# ************************************************   
# CLASS EXERCISE 3: Creating and using dataframes*
#**************************************************
# 1. Use the data on deer parasites and Tb (EXERCISE 1) to create a dataframe
# 2. Square root transform the length variable
# 3. Add it to the dataframe as a new variable 
# NOTE: To do this you need to use 'c' to create all the variables first!!!

# Create the variables
Farm <- c("MO", "MO", "MO", "MO", "LN", "SE", "QM")   # Note strings need "" marks
Month <- c(11, 07, 07, NA, 09, 09, 11)
Year <- c(00, 00, 01, NA, 03, 03, 02)
Sex <- c(1, 2, 2, 2, 1, 2, 2)
LClass <- c(1, 1, 1, 1, 1, 1, 1)
LengthCT <- c(75, 85, 91.6, 95, NA, 105.1, 106)
Ecervi <- c(0, 0, 0, NA, 0, 0, 0)
Tb <- c(0, 0, 1, NA, 0, 0, 0)

# Creating the dataframe. You could have created the SQLenCT new variable first then the dataframe (see below)
SQLenCT <- sqrt(LengthCT)           # Note that it ignored the NA (functions vary!)
Deer <-data.frame(Farm, Month, Year, Sex, LClass, LengthCT, Ecervi, Tb, SQLenCT)
Deer
#  BUT I chose to do in one command.
Deer <-data.frame(Farm, Month, Year, Sex, LClass, LengthCT, Ecervi, Tb, SQLenCT = sqrt(LengthCT))
Deer

#*************************
# WEEK 2 Solutions
#*************************

# **********************************************************   
# CLASS EXERCISE 1: Importing and manipulating data frames *
# **********************************************************
# 1. Use the deep sea research data (ISIT.txt). Load it in using read.table. The file contains bioluminscence data on            organisms from various depths and locations in the North Sea.

DeepSea <- read.table(file = "ISIT.txt", header = TRUE)
names(DeepSea)              # You need to know the data structure
str(DeepSea)
# NOTE I have called the imported file DeepSea. If you have called it something else this won't work!!!
# Same is true of all the objects; substitute your object names in for mine!

# 2. Extract the data from station 1. How many observations are there from this station?
unique(DeepSea$Station)    # checking how many stations (there are 19 in total)
DeepSea.S1 <- DeepSea[DeepSea$Station == 1, ]
dim(DeepSea.S1)            # List there are 38 samples / observations

require(dplyr)
DeepSea.S1 <- filter(DeepSea, Station ==1)
dim(DeepSea.S1)  

# 3. What are minimum, maximum, median and mean sampled depth of stations 2 and 3
DeepSea.S2 <- data.frame(DeepSea[DeepSea$Station == 2, ])   # Selecting the Station 2 data. Could have done in one block....
MinS2 <- min(DeepSea.S2$RelativeDepth, na.rm = TRUE)
MaxS2 <- max(DeepSea.S2$RelativeDepth, na.rm = TRUE)
MedS2 <- median(DeepSea.S2$RelativeDepth, na.rm = TRUE)
MeanS2 <- mean(DeepSea.S2$RelativeDepth, na.rm = TRUE)
S2Depth <- cbind(MinS2, MaxS2, MedS2, MeanS2)
print(S2Depth)

DeepSea.S3 <- data.frame(DeepSea[DeepSea$Station == 3, ])  # Selecting Station 3 data
MinS3 <- min(DeepSea.S3$RelativeDepth, na.rm = TRUE)
MaxS3 <- max(DeepSea.S3$RelativeDepth, na.rm = TRUE)
MedS3 <- median(DeepSea.S3$RelativeDepth, na.rm = TRUE)
MeanS3 <- mean(DeepSea.S3$RelativeDepth, na.rm = TRUE)
S3Depth <- cbind(MinS3, MaxS3, MedS3, MeanS3)
print(S3Depth)

#Quicker to do it all in dplyr!!!!
DeepSea %>% filter(Station == 1) %>% summarise(Min=min(RelativeDepth), max=max(RelativeDepth), median=median(RelativeDepth), Mean=mean(RelativeDepth))
DeepSea %>% filter(Station == 2) %>% summarise(Min=min(RelativeDepth), max=max(RelativeDepth), median=median(RelativeDepth), Mean=mean(RelativeDepth))

# or 
DeepSea %>% group_by(Station) %>% summarise(Min=min(RelativeDepth), max=max(RelativeDepth), median=median(RelativeDepth), Mean=mean(RelativeDepth))
# for the whole list of stations descriptors

# 4. Identify stations with fewer observations than 20. Create a data frame omitting them.
table(DeepSea$Station)         # Check number of observations using 'table' function
# This returns two rows. The first is the station number and the second the number of 
DeepSea1 <- data.frame(DeepSea[DeepSea$Station !=4 & DeepSea$Station !=5, ])   # create new data frame exclud. stations 4 and 5
unique(DeepSea1$Station)          # Check they've gone!

# 5. Extract the data for 2002 and sort it by increasing depth values
unique(DeepSea$Year)                      # How many years?
Y2000 <- DeepSea[DeepSea$Year == 2002, ]  # Selecting the data
Y2000                                     # Looking to see if it's ok
ord1 <- order(Y2000$RelativeDepth)        # Ordering by depth
Y2000Ord <- Y2000[ord1, ]
Y2000Ord

# You can combine it into one line of script (R is flexible!!!!)
Y2000b <- Y2000[order(Y2000$RelativeDepth), ]
Y2000b

# OR in dplyr
DeepSea %>% filter(Year == 2002) %>% arrange(RelativeDepth)

# 6. Show the data that were measured at depths greater than 2000m in April (all years)

D2000 <- DeepSea[DeepSea$Month == 4 & DeepSea$RelativeDepth >2000, ]
D2000


# In dplyr 
DeepSea1 <- DeepSea %>% filter(Month == 4) %>% filter(RelativeDepth >2000)
DeepSea1

# 7. Check out the merge() function that allows you to merge datasets with a common ID. 
# JUST HAVE LOOK AT TH|IS FUNCTION USING THE HELP FACILITY

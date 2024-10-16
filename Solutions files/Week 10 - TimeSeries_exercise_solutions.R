
#Question solution

#Q1:Which command do you use to check if the directory is ok?
getwd()

#Q2. Can you read a file directly instead of opening the folder? What's the command?
#yes e.g. 
ouelleTemp <- read.csv("ouelle_temperature.csv", header=TRUE,sep=',')

#Q3: what command could you use to quickly look at the beginning and end of you file to do a quick check?
head("filename")
tail("filename")

#Q4. Do you get an error? Is the package installed? How do you install it if not?
#ibrary() can only access the package if installed already using e.g.:
install.packages("imputeTS")

#Q5. How do you know if it worked? Do you still have missing values?
#There is many option but you could try this again to find out:
is.na(ouelleTemp)
sum(is.na(ouelleTemp))

#Q6. How would you look at other time step. e.g. montlhy fluctuation?
#simply adjust the unit of time you want to look at, e.g.: 

MonthlyTempFluct <- aggregate(ouelleTemp$temp_main_stem, 
                            by = list(month(ouelleTemp$dateTime)), 
                            mean)



#Exercice solution

##########################################################################################################
# exercise 1: calculate the daily mean, maximum and minimum temperatures in of the 'temp_tributary' series  

startDate <- min(yday(ouelleTemp$dateTime)) 
endDate <- max(yday(ouelleTemp$dateTime)) 
dailyMeanTrib <- numeric() 
n <- 1 

for (i in startDate:endDate){
  rowIdx <- which(yday(ouelleTemp$dateTime) == i)
  dailyMeanTrib[n] <- mean(ouelleTemp$temp_tributary[rowIdx])
  n<-n+1
}	
rm(startDate,endDate,i,n,rowIdx)

#or:

dailyMeanTrib <- tapply(ouelleTemp$temp_tributary, yday(ouelleTemp$dateTime), mean) 

#Repeat using max and min instead of mean, see help(max) or help(min) for more info

################################################################################################
#exercice 2. Plot the daily mean temperature difference between the tributary and the main stem

#Calculte the difference (see script for the main stem daily mean)

TempDiff<-(dailyMean-dailyMeanTrib)

#now plot the difference... wait how to you get the dates for each values?

dailyTemp2 <- data.frame(wholeDays = unique(floor_date(ouelleTemp$dateTime, unit="days")), 
                         dailyMean, dailyMeanTrib, TempDiff)
View(dailyTemp2)


plot(dailyTemp2$wholeDays, dailyTemp2$TempDiff, type = "l",
     ylab = "Temperature (?C)", xlab = "Date", 
     xaxt = "n", col = "grey50")
axis.POSIXct(1, at = seq(min(ouelleTemp$dateTime), 
                         max(ouelleTemp$dateTime), 
                         by = "week"), 
             format = "%y/%m/%d")


#look also at line 409 in script for alternative way to look at fluctuation


##############################################################################################
#exercice 3. Calculate the number of days on which temperatures in the tributary exceeds 18 ?C
#Simply apply the same method that you used for the main stem:

thresholdExceededTrib <- ouelleTemp$dateTime[ouelleTemp$temp_tributary >= 18]	
thresholdExceededTrib <- unique(floor_date(thresholdExceededTrib, unit = "days"))
length(thresholdExceededTrib)

#you should get 44 days exceeding 18C


#######################################################################################################################
# exercise 4 Plot the trend in mean annual maximum daily temperatures (column 'MAX_TEMPERATURE' in climate_station.csv) 
#and calculate the annual increase Compare.  

annualmax <- aggregate(dt$MAX_TEMPERATURE, list(year(dt$LOCAL_DATE)), max)
names(annualmax) <- c("year","max")
plot(annualmax)
lm(annualmax$max ~ annualmax$year)
abline(lm(annualmax$max ~ annualmax$year), lwd = 3, lty = 2)



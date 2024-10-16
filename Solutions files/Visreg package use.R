# Examples of using visreg package using ozone.data 
??visreg

ozone <- read.csv(file.choose())
data("airquality")
names(airquality)
install.packages("visreg")
library(visreg)
library(mgcv)
M1 <- gam(Ozone ~ Solar.R + Wind + Temp, data=airquality)
visreg(M1)




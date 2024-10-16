#Datasets for REM
library(tidyverse)
#Fish data

doubs.spe <- read.csv ('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/DoubsSpe.csv', row.names = 1)
doubs.env <- read.csv ('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/DoubsEnv.csv', row.names = 1)
doubs.spa <- read.csv ('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/DoubsSpa.csv', row.names = 1)

write.csv(doubs.spe, "species",row.names=FALSE) 
write.csv(doubs.env, "env_data") 
write.csv(doubs.spa,"spatial_coords") 



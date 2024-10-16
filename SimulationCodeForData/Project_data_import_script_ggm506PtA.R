install.packages("readxl") # package needed to load in from an xlsx worksheet
library(readxl) # read command
library(tidyverse)
# Put them all in your working directory otherwise this code won't work!

# The code is:
yourdataname <- read_excel("yourselecteddataset.xlsx", sheet = 2) 

# where yourdataname is the name you give to the data on import
# "yourselecteddataset.xlsx" is the dataset excel spreadsheet name [with extension]
# sheet = 2 is the datasheet number within the spreadsheet [they are number 1-n left to right!]

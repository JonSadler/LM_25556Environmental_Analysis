
set.seed(123)
#simulated decomposition dataset
# fine mass loss
reps<-factor(c(rep(seq(1,8),7)))
d0<-rnorm(8,mean=0.31,sd=0.03)
d2<-rnorm(8,mean=0.36,sd=0.04)
d5<-rnorm(8,mean=0.42,sd=0.05)
d7<-rnorm(8,mean=0.44,sd=0.06)
d10<-rnorm(8,mean=0.49,sd=0.06)
d15<-rnorm(8,mean=0.57,sd=0.07)
d25<-rnorm(8,mean=0.65,sd=0.06)

f_loss<-c(d0,d2,d5,d7,d10,d15,d25)
depth<- rep(c("0","2","5","7","10","15","25"),each=8)
AllFData<-data.frame(reps,f_loss,depth)
#AllFData$id <- 1:nrow(AllFData)

boxplot(f_loss~depth, data = AllFData)
fineaov <- aov(f_loss~depth, data = AllFData)
summary(fineaov)
autoplot(fineaov)               

model.tables(fineaov,"means")
# write sheet

# design has 28 channels, 7 depths [one per channel] and 4 fine, 4 coarse bags per channel
# coarse mass loss

# add in fish to channels and simulate their impact on coarse loss
# Simulate coarse mass loss data
reps <- factor(c(rep(seq(1, 8), 7)))  # Four replicate channels for each depth, repeated 7 times
d0 <- rnorm(8, mean = 0.55, sd = 0.08)
d2 <- rnorm(8, mean = 0.55, sd = 0.07)
d5 <- rnorm(8, mean = 0.57, sd = 0.05)
d7 <- rnorm(8, mean = 0.59, sd = 0.08)
d10 <- rnorm(8, mean = 0.60, sd = 0.1)
d15 <- rnorm(8, mean = 0.62, sd = 0.09)
d25 <- rnorm(8, mean = 0.65, sd = 0.09)

# Create 'fish' variable: 2 replicate depths with fish present and 2 with fish absent
fish_present <- c(rep(1, 4), rep(0, 4))
fish <- rep(fish_present, times = 7)

# Simulate larger values of c_loss where fish are absent
c_loss <- c(d0, d2, d5, d7, d10, d15, d25)
c_loss[fish == 0] <- c_loss[fish == 0] + 0.1  # Add 0.1 to c_loss where fish are absent

# Create the dataframe including 'fish' variable
AllCData <- data.frame(
  reps = reps,
  c_loss = c_loss,
  depth = factor(rep(c("0", "2", "5", "7", "10", "15", "25"), each = 8)),
  fish = factor(fish)
)

# Check the structure of the dataset
str(AllCData)

# Boxplot of coarse mass loss by depth and fish presence
boxplot(c_loss ~ fish, data = AllCData)

# combine the datafiles. Add in the channels.
mass_loss <- cbind(AllFData,AllCData)
#mass_loss <- merge(AllFData,AllCData,by="id")

#randomise the channel order and create new variable

#delete columns we don't need
mass_loss <- mass_loss[,c(-4,-6)]

#ANOVA

fine <- aov(f_loss~depth+fish,data=mass_loss)
summary(fine)
autoplot(fine)

coarse <- aov(c_loss~depth+fish,data=mass_loss)
summary(coarse)
autoplot(coarse)

# write the file

# write datafile
write.csv(mass_loss,"Leaf_bag2024.csv",row.names = FALSE) 

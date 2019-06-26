#script for organizing hemlock core data

#set working directory
setwd("~/Desktop/hemlockdata")

#read in data
datT1 <- read.csv ("datT1.csv")
datT2 <- read.csv ("datT2.csv")
datT3 <- read.csv ("datT3.csv")

#create matrix
meanring <- matrix(rep(NA, dim(datT1)[1]*(dim(datT1)[2]-1)),
                   nrow = dim(datT1)[1])

#calculate means across 3 datasets
for(i in 1:(ncol(datT1)-1)){
  meanring[,i] <- (datT1[,i+1] + datT2[,i+1] + datT3[,i+1])/3
}

#add column names
colnames(meanring) <- colnames(datT1[2:15])

#final data frame w/ years
cores <- data.frame(year=datT1$year,meanring)
head(cores)

#installing dplr
install.packages("dplR")
library(dplR)

#make cores2 dataframe with years as row names and ascending years
row.names(cores) <- cores$year
cores2 <- cores[,-1]
cores2 <- cores2[order(row.names(cores2)),]

####detrending series####

#try series.detrend with all methods for each core
#look at results, determine best method (ModNegExp or Mean)
detrend.series(y = cores2$T_0A, make.plot = TRUE, verbose = TRUE) #modnegexp
detrend.series(y = cores2$T_0B, make.plot = TRUE, verbose = TRUE) #mean
detrend.series(y = cores2$T_2A, make.plot = TRUE, verbose = TRUE) #mean
detrend.series(y = cores2$T_2B, make.plot = TRUE, verbose = TRUE) #modnegexp
detrend.series(y = cores2$T_3A, make.plot = TRUE, verbose = TRUE) #modnegexp
detrend.series(y = cores2$T_3B, make.plot = TRUE, verbose = TRUE) #modnegexp
detrend.series(y = cores2$T_4A, make.plot = TRUE, verbose = TRUE) #mean
detrend.series(y = cores2$T_4B, make.plot = TRUE, verbose = TRUE) #modnegexp
detrend.series(y = cores2$T_5A, make.plot = TRUE, verbose = TRUE) #modnegexp
detrend.series(y = cores2$T_5B, make.plot = TRUE, verbose = TRUE) #modnegexp
detrend.series(y = cores2$T_6A, make.plot = TRUE, verbose = TRUE) #modnegexp
detrend.series(y = cores2$T_6B, make.plot = TRUE, verbose = TRUE) #modnegexp
detrend.series(y = cores2$T_deadA, make.plot = TRUE, verbose = TRUE) #mean
detrend.series(y = cores2$T_deadB, make.plot = TRUE, verbose = TRUE) #modnegexp

#create method vector 
methodD <- c("ModNegExp", "Mean", "Mean", "ModNegExp", "ModNegExp", "ModNegExp", 
            "Mean", "ModNegExp", "ModNegExp", "ModNegExp", "ModNegExp", 
            "ModNegExp", "Mean", "ModNegExp")

#create coresD (Detrended) matrix
coresD <- matrix(rep(NA, dim(cores2)[1]*dim(cores2)[2]), 
                 ncol = dim(cores2)[2])

#use for loop to detrend each series according to method vector
for(i in 1:dim(cores2)[2]){
  coresD[,i] <- detrend.series(y = cores2[,i],
                               method = paste(methodD[i]))
}

#add in column names and years (row names)
colnames(coresD) <- colnames(cores2)
row.names(coresD) <- rownames(cores2)
coresD <- as.data.frame(coresD)

####lining up years####

plot(rownames(coresD), coresD$T_5B, type = "l", col = "purple")
lines(rownames(coresD), coresD$T_5B, type = "l", col = "blue")




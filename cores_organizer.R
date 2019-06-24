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


#script for organizing hemlock core data

#set working directory
setwd("~/Desktop/hemlockdata")

#read in data
datT1 <- read.csv ("datT1.csv")
datT2 <- read.csv ("datT2.csv")
datT3 <- read.csv ("datT3.csv")

#create 15x73 matrix
meanring <- matrix(NA, dim(datT1[1]), ncol = dim(datT1[1]),
                   nrow = dim(datT1[2]))




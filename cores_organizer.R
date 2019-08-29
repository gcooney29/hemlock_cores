#script for organizing hemlock core data

#set working directory
setwd("~/Desktop/hemlockdata")

#read in data
datT1 <- read.csv ("datT1.csv")
datT2 <- read.csv ("datT2.csv")
datT3 <- read.csv ("datT3.csv")

#packages
library(plyr)
library(dplR)

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

#make cores2 dataframe with years as row names and ascending years
row.names(cores) <- cores$year
cores2 <- cores[,-1]
cores2 <- cores2[order(row.names(cores2)),]

####detrending series####

#try series.detrend with all methods for each core
#look at results
detrend.series(y = cores2$T_0A, make.plot = TRUE, verbose = TRUE) 
detrend.series(y = cores2$T_0B, make.plot = TRUE, verbose = TRUE) 
detrend.series(y = cores2$T_2A, make.plot = TRUE, verbose = TRUE) 
detrend.series(y = cores2$T_2B, make.plot = TRUE, verbose = TRUE)
detrend.series(y = cores2$T_3A, make.plot = TRUE, verbose = TRUE) 
detrend.series(y = cores2$T_3B, make.plot = TRUE, verbose = TRUE) 
detrend.series(y = cores2$T_4A, make.plot = TRUE, verbose = TRUE)
detrend.series(y = cores2$T_4B, make.plot = TRUE, verbose = TRUE) 
detrend.series(y = cores2$T_5A, make.plot = TRUE, verbose = TRUE)
detrend.series(y = cores2$T_5B, make.plot = TRUE, verbose = TRUE)
detrend.series(y = cores2$T_6A, make.plot = TRUE, verbose = TRUE) 
detrend.series(y = cores2$T_6B, make.plot = TRUE, verbose = TRUE) 
detrend.series(y = cores2$T_deadA, make.plot = TRUE, verbose = TRUE) 
detrend.series(y = cores2$T_deadB, make.plot = TRUE, verbose = TRUE) 

#detrend using splines
coresDspline <- matrix(rep(NA, dim(cores2)[1]*dim(cores2)[2]), 
                 ncol = dim(cores2)[2])

for(i in 1:dim(cores2)[2]){
  coresDspline[,i] <- detrend.series(y = cores2[,i],
                               method = "Spline")
}

colnames(coresDspline) <- colnames(cores2)
row.names(coresDspline) <- rownames(cores2)
coresDspline <- as.data.frame(coresDspline)
  
####lining up years####

##shifting series and accounting for false rings, etc##

year <- as.numeric(row.names(coresDspline))

#missing 5 rings at the end
D0A <- data.frame(year = seq(1984, 2017),
                  T_0A = c(coresDspline[year>=1989, 1],
                            rep(NA,5)))

#counted 2018 as part of 2017; remove extra year
D0B <- data.frame(year = seq(1946, 2017),
                  T_0B = c(coresDspline[year>=1945&year<=2016, 2]))
D0B$T_0B <- ifelse(D0B$year >= 1977& D0B$year<= 1982, NA, D0B$T_0B)

#missing rings at the end, adjust for false ring in 1993 
D2A<- data.frame (year = seq(1981, 2017),
                      T_2A = c(coresDspline[year<=1995&year>=1984, "T_2A"],
                                coresDspline[year==1996, "T_2A"] +
                                  coresDspline[year==1997, "T_2A"],
                                coresDspline[year>=1998&year<=2017, "T_2A"],
                                rep(NA, 4)))

#counted 2018 as part of 2017; remove extra year
D2B <- data.frame(year = seq(1996, 2017),
                  T_2B = c(coresDspline[year>=1995&year<=2016, 4]))

#adjust for false ring in 1983
D3A <- data.frame(year = seq(1956, 2017),
                  T_3A = c(coresDspline[year<=1981&year>=1955, "T_3A"],
                            coresDspline[year==1982, "T_3A"] +
                              coresDspline[year==1983, "T_3A"],
                            coresDspline[year>=1984, "T_3A"]))

#adjust for missing year
D3B <- data.frame(year = seq(1967, 2017),
                 T_3B = c(coresDspline[year<=1974&year>=1969, "T_3B"],
                           NA, coresDspline[year>=1975&year<=2017, "T_3B"],
                           NA))

#2 missing rings at the end
D4A <- data.frame(year = seq(1986, 2017),
                  T_4A = c(coresDspline[year>=1988, 7],
                            rep(NA,2)))

#2 missing rings at the end
D4B <- data.frame(year = seq(1988, 2017),
                  T_4B = c(coresDspline[year>=1990, 8],
                            rep(NA,2)))
#no shift
D5A <- data.frame (year = year, T_5A = coresDspline[, "T_5A"])

#adjust for 2 false rings
D5B <- data.frame (year = seq(1971, 2017),
                   T_5B = c(coresDspline[year<=1978&year>=1973, "T_5B"],
                             coresDspline[year==1979, "T_5B"] +
                               coresDspline[year==1980, "T_5B"],
                             coresDspline[year>=1981&year<=1997, "T_5B"],
                             coresDspline[year==1998, "T_5B"] +
                               coresDspline[year==1999, "T_5B"],
                             coresDspline[year>=2000, "T_5B"],
                             rep(NA, 4)))

#adjust for false ring in 1982
D6A <- data.frame(year = seq(1956, 2017),
                  T_6A = c(coresDspline[year<=1980&year>=1955, 11],
                            coresDspline[year==1981, 11] +
                              coresDspline[year==1982, 11],
                            coresDspline[year>=1983, 11]))

#adjust for missing ring
D6B <- data.frame(year = seq(1964, 2017),
                         T_6B = c(coresDspline[year<=2012&year>=1966, "T_6B"],
                                   NA, coresDspline[year>=2013&year<=2017, "T_6B"],
                                   NA))

#create data frame of means, standard deviation, and number of samples of all samples
allsample <- list(D0A, D0B, D2A, D2B, D3A, D3B, D4A, D4B, D5A, D5B, D6A, D6B)

sampleDF <- join_all(allsample, by = "year", type = "full")
sampleDF <- sampleDF[order(sampleDF$year),]

na.lengths <- function(x){length(na.omit(x))}

allmeans <- data.frame(year = year, width = 
                         apply(sampleDF[,2:13], 1, "mean", na.rm = TRUE), 
                       width.sd = apply(sampleDF[,2:13], 1, "sd", na.rm = TRUE),
                       width.n = apply(sampleDF[,2:13], 1, "na.lengths"))

rm(list=setdiff(ls(), c("allmeans")))

plot(allmeans$year, allmeans$width, type = "l", col = "gold")

points(D0A$year, D0A$T_0A, type = "l", col = "orange")
plot(D0B$year, D0B$T_0B, type = "l")
points(D2A$year, D2A$T_2A, type = "l")
points(D2B$year, D2B$T_2B, type = "l", col = "darkgreen")
points(D3A$year, D3A$T_3A, type = "l")
points(D3B$year, D3B$T_3B, type = "l", col = "blue")
points(D4A$year, D4A$T_4A, type = "l")
points(D4B$year, D4B$T_4B, type = "l", col = "red")
points(D5A$year, D5A$T_5A, type = "l")
points(D5B$year, D5B$T_5B, type = "l", col = "pink")
points(D6A$year, D6A$T_6A, type = "l", col = "turquoise")
points(D6B$year, D6B$T_6B, type = "l", col = "purple")


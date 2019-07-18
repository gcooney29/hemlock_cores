####plotting script####

#source
#source("gracecooney/Desktop/Colgate/github/hemlock_cores/cores_organizer.R")

#create plot of mean
cores_means <- as.data.frame(rowMeans(coresC[13:75,], na.rm = TRUE))
colnames(cores_means) <- c("mean")
plot(row.names(cores_means), cores_means$mean, type = "l")

#create plot of live trees
alive_means <- as.data.frame(rowMeans(coresC[11:73, 1:12], na.rm = TRUE))
colnames(alive_means) <- c("mean")
plot(row.names(alive_means), alive_means$mean, type = "l")

#create plot of dead trees
dead_means <- as.data.frame(rowMeans(coresC[11:73, 13:14], na.rm = TRUE))
colnames(dead_means) <- c("mean")
plot(row.names(dead_means), dead_means$mean, type = "l")

#dead vs alive
plot(row.names(dead_means), dead_means$mean, type = "l", col = "red")
points(row.names(alive_means), alive_means$mean, type = "l", col = "blue")

#read in NOAA tennessee hemlock data
rainbow <- read.csv ("rainbow.csv") 
henwallow <- read.csv ("henwallow.csv")
henwallow2 <- read.csv ("henwallow2.csv")
savagegulf <- read.csv ("savagegulf.csv")


#compare data 
plot(rainbow$year, rainbow$trsgi, type = "l", col = "blue")
points(henwallow$year, henwallow$trsgi, type = "l", col = "red")


points(as.numeric(row.names(coresDspline))-6, coresDspline$T_0A, type = "l", col = "orange")
plot(as.numeric(row.names(coresDspline))+1, coresDspline$T_0B, type = "l", col = "purple")
points(as.numeric(row.names(coresDspline))-3, coresDspline$T_2A, type = "l", col = "grey")
points(as.numeric(row.names(coresDspline))+2, coresDspline$T_2B, type = "l", col = "violet")
points(as.numeric(row.names(coresDspline)), coresDspline$T_3A, type = "l", col = "peru")
points(as.numeric(row.names(coresDspline))-1, coresDspline$T_3B, type = "l", col = "pink")
points(as.numeric(row.names(coresDspline))-2, coresDspline$T_4A, type = "l", col = "tan")
points(as.numeric(row.names(coresDspline))-2, coresDspline$T_4B, type = "l", col = "brown")
points(as.numeric(row.names(coresDspline))-1, coresDspline$T_5A, type = "l", col = "turquoise")
points(as.numeric(row.names(coresDspline)), coresDspline$T_5B, type = "l", col = "gold")
plot(as.numeric(row.names(coresDspline)), coresDspline$T_6A, type = "l", col = "darkgreen")
points(as.numeric(row.names(coresDspline))-2, coresDspline$T_6B, type = "l", col = "green")

#points(as.numeric(row.names(cores2)), cores2$T_deadB, type = "l", col = "darkblue")
#points(as.numeric(row.names(cores2))-4, cores2$T_deadA, type = "l", col = "lightblue")




points(as.numeric(row.names(alive_means))-1, alive_means$mean, type = "l")
coresD2 <- coresD[row.names(coresD)>=1955 & row.names(coresD)<=1995,]

#corr.rwl.seg(coresD2[,13:14], seg.length = 6, master = henwallow$trsgi, bin.floor = 1965)
pdf("skeletonplot.pdf", width =11, height = 8)
for(i in 1:14){
  a <- skel.plot(cores2[,i], as.numeric(row.names(cores2)), 
            sname = colnames(cores2)[i], dat.out = TRUE) 
  
}
skel.plot(henwallow$trsgi, henwallow$year)
skel.plot(rainbow$trsgi, rainbow$year)
dev.off()
?skel.plot
str(a)

div1 <- read.csv("div1.txt")
datev1 <- as.Date(div1$YearMonth, "%Y%m")

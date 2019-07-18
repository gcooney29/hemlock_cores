####plotting script####

#source
#source("gracecooney/Desktop/Colgate/github/hemlock_cores/cores_organizer.R")

#read in NOAA tennessee hemlock data
rainbow <- read.csv ("rainbow.csv") 
henwallow <- read.csv ("henwallow.csv")
henwallow2 <- read.csv ("henwallow2.csv")
savagegulf <- read.csv ("savagegulf.csv")


#compare data 
plot(rainbow$year, rainbow$trsgi, type = "l", col = "blue")
points(henwallow$year, henwallow$trsgi, type = "l", col = "red")


points(as.numeric(row.names(coresDspline)), coresDspline$T_0A, type = "l", col = "orange")
points(as.numeric(row.names(coresDspline)), coresDspline$T_0B, type = "l", col = "purple")
points(as.numeric(row.names(coresDspline)), coresDspline$T_2A, type = "l", col = "grey")
points(as.numeric(row.names(coresDspline)), coresDspline$T_2B, type = "l", col = "violet")
points(as.numeric(row.names(coresDspline)), coresDspline$T_3A, type = "l", col = "peru")
points(as.numeric(row.names(coresDspline)), coresDspline$T_3B, type = "l", col = "pink")
points(as.numeric(row.names(coresDspline)), coresDspline$T_4A, type = "l", col = "tan")
points(as.numeric(row.names(coresDspline)), coresDspline$T_4B, type = "l", col = "brown")
points(as.numeric(row.names(coresDspline)), coresDspline$T_5A, type = "l", col = "turquoise")
points(as.numeric(row.names(coresDspline)), coresDspline$T_5B, type = "l", col = "gold")
points(as.numeric(row.names(coresDspline)), coresDspline$T_6A, type = "l", col = "darkgreen")
points(as.numeric(row.names(coresDspline)), coresDspline$T_6B, type = "l", col = "green")

#create skeleton plots in order to line up years

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



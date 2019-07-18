
source("~/Desktop/Colgate/github/hemlock_cores/cores_organizer.R")

div1 <- read.csv("div1.txt")
div1$year <- as.numeric(substr(as.character(div1$YearMonth), 1,4))
div1$month <- as.numeric(substr(as.character(div1$YearMonth), 5,6))

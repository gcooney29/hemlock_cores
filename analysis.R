
source("~/Desktop/Colgate/github/hemlock_cores/cores_organizer.R")

div1 <- read.csv("div1.txt")
div1$year <- as.numeric(substr(as.character(div1$YearMonth), 1,4))
div1$month <- as.numeric(substr(as.character(div1$YearMonth), 5,6))

summer <- div1[div1$month >=6& div1$month <= 8, ]

sum_c <- aggregate(summer$TAVG, by = list(summer$year), FUN = "mean")
colnames(sum_c) <- c("year", "s_TAVG")
sum_cp <- aggregate(summer$PCP, by = list(summer$year), FUN = "sum")
sum_c$s_PCP <- sum_cp$x

sum_p <- data.frame(year = sum_c$year, 
                    s_TAVGp = c(NA, sum_c$TAVG[1:nrow(sum_c)-1]),
                    s_PCPp = c(NA, sum_c$PCP[1:nrow(sum_c)-1]))

allmeans2 <- join(allmeans, sum_c, by = "year", type = "left")

model <- lm(allmeans2$width ~ sum_p$s_TAVGp)
summary(model)

shapiro.test(residuals(model))
hist(residuals(model))

plot(sum_p$s_TAVGp, allmeans2$width, pch = 19)
plot(na.omit(allmeans2$s_PCP), residuals(model))
abline(h = 0)

plot(allmeans2$year, allmeans2$width, type = "l")
plot(sum_p$year, sum_p$s_PCPp, type = "l")

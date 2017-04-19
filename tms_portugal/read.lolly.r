#TG 21.10.2014 file to analyse weightsd and signal data of TMS probe to estimate transferfunction
#modified: 03.07.2015 TG
#modified: 17.07.2015 TG
#modified: 21.09.2015 TG

# clear workspace
rm(list=ls()); gc()


#path <- "d:/Lolly/data/portugal_Jose"
#path <- "d:/sensors/Lolly/data/portugal_Jose"
path <- "~/boxup/whk_echse/tms_portugal"

setwd(path)
FILES <- dir(path = path, pattern = "^da.*.csv$")
#compose files
for (i in 1:length(FILES))
{
  D <- read.table(FILES[i], header = F, sep = ";")
  if (dim(D)[1] > 1)
  {
    D[,2] <- as.POSIXct(D[,2], format="%Y.%m.%d %H:%M", tz = "GMT")
    
    if (i == 1)
    {
      data <- D
    } else
    {
      X <- which(data[length(data[,2]),2]  < D[,2])
      data <- rbind(data, D[X,])
    }
    
  }
}
str(data)
dim(data)
names(data) <- c("Nr", "date", "X", "T1", "T2", "T3", "SM", "Y", "Z")

plot(data$date, data$T1, type="l", xlab="time", ylab="Temperature [?C]", main = "TMS data")
lines(data$date, data$T2, col = "red", lty = 3)
lines(data$date, data$T3, col = "pink", lty = 4)

plot(data$date, data$SM, type="l", xlab="time", ylab="SM [TMS]", main = "TMS data")

X <- which(as.POSIXct("29.04.2015 15:00", format="%d.%m.%Y %H:%M", tz = "GMT") <= data$date)[1]
Y <- which(as.POSIXct("19.09.2015 11:00", format="%d.%m.%Y %H:%M", tz = "GMT") <= data$date)
Y <- Y[1]
plot(data$date[X:Y], data$T1[X:Y], type="l", xlab="time", ylab="Temperature [?C]", main = "TMS data", ylim = (c(min(data[,4:6]), max(data[,4:6]))))
lines(data$date[X:Y], data$T2[X:Y], col = "firebrick3", lty = 3)
lines(data$date[X:Y], data$T3[X:Y], col = "lightskyblue2", lty = 4)

data$T1[data$T1 < -7] <- NA
data$T2[data$T2 < -7 | data$T2 > 30] <- NA
data$T3[data$T3 < -7 | data$T3 > 30] <- NA


plot(data$date[X:Y], data$T1[X:Y], type="l", xlab="time", ylab="Temperature [?C]", main = "TMS data", ylim = (c(min(data[,4:6], na.rm = TRUE), max(data[,4:6], na.rm = TRUE))))
lines(data$date[X:Y], data$T2[X:Y], col = "firebrick3", lty = 3)
lines(data$date[X:Y], data$T3[X:Y], col = "lightskyblue2", lty = 4)


plot(data$date[X:Y], data$SM[X:Y], type="l", xlab="time", ylab="SM [TMS]", main = "TMS data")

gmt <- -2*3600 #remove local time and summer time
WEIGHT <- read.table("weights.txt", skip = 14, sep  = "\t")
names(WEIGHT) <- c("date", "weight")
WEIGHT$date <- as.POSIXct(WEIGHT$date, format = "%d.%m.%Y %H:%M", tz = "GMT")+ gmt
#flower pot coaster, bottle, probe TMS 93131942
dev <- 40.8+46.89+122.35
#soil not abolute dry value not reliable
S <- 826.25 - dev + 100 +40.8

#Volume of bottle
#dyameter in cm
dym <- 7.5
DEPTH <- 11.5 #in cm
VOL <- 3.14159265359 * (dym/2)^2 * DEPTH #in qcm

#weight of soil and water in g
WEIGHT$SOIL_water <- WEIGHT$weight - dev
#dry soil in g -10 g bonus
SOIL <- WEIGHT$SOIL_water[length(WEIGHT$SOIL_water)] -10
#bulk density in g/qcm
Ds <- SOIL/VOL
#weight water
WEIGHT$WATER <- WEIGHT$SOIL_water- SOIL
#starting volume of water
WATER <- WEIGHT$WATER[1]
#gravimetric soil moisture g/g
WEIGHT$gSM <- WEIGHT$WATER / SOIL

PORO <- WATER / SOIL # g/g
PORO2 <- 1- Ds/2.648 #m?/m? with density of silicia
PORO3 <- PORO2 / Ds # g/g
#volumetric soil moisture m?/m?
WEIGHT$SM <- WEIGHT$gSM * Ds

plot(WEIGHT$date, WEIGHT$weight, xlab = "Time", ylab = "Weight of sample [g]")
plot(WEIGHT$date, WEIGHT$SOIL_water, xlab = "Time", ylab = "Weight of soil and water in sample [g]")
plot(WEIGHT$date, WEIGHT$WATER, xlab = "Time", ylab = "Weight of water in the sample [g]")
plot(WEIGHT$date, WEIGHT$SM*100, xlab = "Time", ylab = "Soil Moisture [%]")

WEIGHT$SOIL_water[length(WEIGHT$SOIL_water)]

#extract data points of logger at which weight was measured
x <- c()
for (i in 1:length(WEIGHT$date))
{
  x[i] <- which((as.numeric(data$date - WEIGHT$date[i]))^2 == min(as.numeric(data$date - WEIGHT$date[i])^2))
}

plot(data$SM[x], WEIGHT$weight, ylab = "Weight sample [g]", xlab = "TMS signal [-]")
plot(data$SM[x], WEIGHT$SM, ylab = "vol. Soil Moisture [m?/m?]", xlab = "TMS signal [-]")
#TMS standard polynom with standard parametrisation
data$SM2 <- (0.000000000009*data$SM^4-0.0000000382*data$SM^3+0.0001*data$SM^2-0.1168*data$SM+42.407)
plot(data$SM2[x], WEIGHT$SM*100, ylab = "Weight sample [g]", xlab = "TMS SM [m?/m?]")

#linear model
mod <- lm(WEIGHT$SM ~ data$SM[x])
mod$coefficients
summary(mod)
upper <- data$SM[x] * (mod$coefficients[2] + coef(summary(mod))[, "Std. Error"][2]) + mod$coefficients[1] + coef(summary(mod))[, "Std. Error"][1]
lower <- data$SM[x] * (mod$coefficients[2] - coef(summary(mod))[, "Std. Error"][2]) + mod$coefficients[1] - coef(summary(mod))[, "Std. Error"][1]

plot(data$SM[x], WEIGHT$SM)
lines(data$SM[x], data$SM[x] *mod$coefficients[2] + mod$coefficients[1], col = "tomato2")
lines(data$SM[x], data$SM[x] * (mod$coefficients[2] + coef(summary(mod))[, "Std. Error"][2]) + mod$coefficients[1] + coef(summary(mod))[, "Std. Error"][1], col = "dodgerblue2")
lines(data$SM[x], data$SM[x] * (mod$coefficients[2] - coef(summary(mod))[, "Std. Error"][2]) + mod$coefficients[1] - coef(summary(mod))[, "Std. Error"][1], col = "dodgerblue2")
r <- 250/256
g <-83/256
b <- 62/256
polygon(c(data$SM[x], rev(data$SM[x])), c(lower, rev(upper)),  col=rgb(r,g,b,0.3), border=NA)

#polynom model
TMS <- data$SM[x]
SM <- WEIGHT$SM
nlmod <- nls(SM ~ a * TMS^2 + b * TMS +c, trace = TRUE, start = list(a = 1e-8, b = 1E-4, c = -1E-3))
summary(nlmod)
PARA <- summary(nlmod)

PARA$parameters[,"Estimate"][1]
upper2 <- data$SM[x]^2 * (PARA$parameters[,"Estimate"][1] + coef(summary(nlmod))[, "Std. Error"][1]) + data$SM[x] * (PARA$parameters[,"Estimate"][2] + coef(summary(nlmod))[, "Std. Error"][2]) + (PARA$parameters[,"Estimate"][3] + coef(summary(nlmod))[, "Std. Error"][3])
lower2 <- data$SM[x]^2 * (PARA$parameters[,"Estimate"][1] - coef(summary(nlmod))[, "Std. Error"][1]) + data$SM[x] * (PARA$parameters[,"Estimate"][2] - coef(summary(nlmod))[, "Std. Error"][2]) + (PARA$parameters[,"Estimate"][3] - coef(summary(nlmod))[, "Std. Error"][3])


lines(data$SM[x], data$SM[x]^2 *PARA$parameters[,"Estimate"][1] + data$SM[x] * PARA$parameters[,"Estimate"][2] + PARA$parameters[,"Estimate"][3], col = "darkolivegreen2", lwd = 2, lty = 3)
polygon(c(data$SM[x], rev(data$SM[x])), c(lower2, rev(upper2)),  col=rgb(r,g,b,0.3), border=NA)

# polynom with same amounts of degrees of freedom like TMS has provided
nlmod2 <- nls(SM ~ a * TMS^4 + b * TMS^3 + c * TMS^2 + d * TMS + e, trace = TRUE, start = list(a = -1e-14, b = 1E-10, c = -1E-7, d = 1E-4, e = -1E-1))
summary(nlmod2)
PARA2 <- summary(nlmod2)

PARA2$parameters[,"Estimate"][1]
upper3 <- data$SM[x]^4 * (PARA2$parameters[,"Estimate"][1] + coef(summary(nlmod2))[, "Std. Error"][1]) + data$SM[x]^3 * (PARA2$parameters[,"Estimate"][2] + coef(summary(nlmod2))[, "Std. Error"][2]) + data$SM[x]^2 * (PARA2$parameters[,"Estimate"][3] + coef(summary(nlmod2))[, "Std. Error"][3]) + data$SM[x] * (PARA2$parameters[,"Estimate"][4] + coef(summary(nlmod2))[, "Std. Error"][4]) + (PARA2$parameters[,"Estimate"][5] + coef(summary(nlmod2))[, "Std. Error"][5])
lower3 <- data$SM[x]^4 * (PARA2$parameters[,"Estimate"][1] - coef(summary(nlmod2))[, "Std. Error"][1]) + data$SM[x]^3 * (PARA2$parameters[,"Estimate"][2] - coef(summary(nlmod2))[, "Std. Error"][2]) + data$SM[x]^2 * (PARA2$parameters[,"Estimate"][3] - coef(summary(nlmod2))[, "Std. Error"][3]) + data$SM[x] * (PARA2$parameters[,"Estimate"][4] - coef(summary(nlmod2))[, "Std. Error"][4]) + (PARA2$parameters[,"Estimate"][5] - coef(summary(nlmod2))[, "Std. Error"][5])

lines(data$SM[x], data$SM[x]^4 *PARA2$parameters[,"Estimate"][1] + data$SM[x]^3 * PARA2$parameters[,"Estimate"][2] + data$SM[x]^2 * PARA2$parameters[,"Estimate"][3] + data$SM[x] * PARA2$parameters[,"Estimate"][4] + PARA2$parameters[,"Estimate"][5], col = "blue", lwd = 2, lty = 4)
polygon(c(data$SM[x], rev(data$SM[x])), c(lower3, rev(upper3)),  col=rgb(r,g,b,0.3), border=NA)

plot(data$SM[x], WEIGHT$SM, ylim = c(-1,1))
lines(data$SM[x], data$SM[x]^4 *PARA2$parameters[,"Estimate"][1] + data$SM[x]^3 * PARA2$parameters[,"Estimate"][2] + data$SM[x]^2 * PARA2$parameters[,"Estimate"][3] + data$SM[x] * PARA2$parameters[,"Estimate"][4] + PARA2$parameters[,"Estimate"][5], col = "blue", lwd = 2, lty = 4)
polygon(c(data$SM[x], rev(data$SM[x])), c(lower3, rev(upper3)),  col=rgb(r,g,b,0.3), border=NA)

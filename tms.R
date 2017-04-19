################################################################################
# Authors: Jose M. Delgado, Julius Eberhard
# Last Edit: 2016-10-20
# Project: ECHSE Evapotranspiration
# Program: tms
# Aim: Analyse Weights and Signal Data of TMS Probe to Estimate
#      Transfer Function
# Original File: ~/boxup/whk_echse/tms_portugal/read.lolly.r
################################################################################

rm(list=ls())
gc()

# PROGRAM PARAMETERS -----------------------------------------------------------

# Should R make plots for exploratory data analysis?
plots <- F  # logical

# SET AND LOAD -----------------------------------------------------------------

path <- "~/boxup/whk_echse/tms_portugal"
setwd(path)

# collect all ascii data files
files <- dir(path=path, pattern="^da.*.csv$")

# read observed soil weights
weight <- read.table("weights.txt", skip=14, sep="\t")
names(weight) <- c("date", "weight")

# read field observations of TMS probe
obs.HS <- read.table("field_data/data_93131961_8.csv", sep=";")
obs.NSA <- read.table("field_data/data_93131959_8.csv", sep=";")

# PREPROCESSING ----------------------------------------------------------------

# create data lists for field stations
HS <- list(obs=obs.HS, sm.out.lin=NA, sm.out.lin.obs=NA, sm.out.poly2=NA,
           sm.out.poly3=NA, sm.out.poly4=NA)
NSA <- list(obs=obs.NSA, sm.out.lin=NA, sm.out.lin.obs=NA, sm.out.poly2=NA,
            sm.out.poly3=NA, sm.out.poly4=NA)

# compose (lab) data files
D <- list()
for (i in 1:length(files)) {
  D[[i]] <- read.table(files[i], sep=";")
  if (dim(D[[i]])[1] > 1) {
    D[[i]][, 2] <- as.POSIXct(D[[i]][, 2], format="%Y.%m.%d %H:%M", tz="GMT")
    if (i == 1) {
      data <- D[[i]]
    } else {
      # select and add rows in current file that include later periods
      X <- which(data[nrow(data), 2] < D[[i]][, 2])
      data <- rbind(data, D[[i]][X, ])
    }
  }
}
names(data) <- c("Nr", "date", "X", "T1", "T2", "T3", "sm", "Y", "Z")

if (plots) {
  # plot all TMS data
  plot(D[[1]][, 2], D[[1]][, 7], type="l", col=1,
       xlim=c(as.POSIXct("2015-04-01"), as.POSIXct("2015-09-20")),
       ylim=c(0, 5000))
  lines(D[[2]][, 2], D[[2]][, 7], col=2)
  lines(D[[3]][, 2], D[[3]][, 7], col=3)
  lines(D[[4]][, 2], D[[4]][, 7], col=4)
  lines(D[[5]][, 2], D[[5]][, 7], col=5)
  lines(D[[6]][, 2], D[[6]][, 7], col=6)
  lines(D[[7]][, 2], D[[7]][, 7], col=7)
}

# prepare field observations

HS$obs[, 2] <- as.POSIXct(HS$obs[, 2], format="%d.%m.%Y %H:%M", tz="GMT")
NSA$obs[, 2] <- as.POSIXct(NSA$obs[, 2], format="%d.%m.%Y %H:%M", tz="GMT")
names(HS$obs)[c(2, 7)] <- c("date", "tms")
names(NSA$obs)[c(2, 7)] <- c("date", "tms")

if (plots) {
  # plot raw temperatures
  plot(data$date, data$T1, type="l", xlab="Time", ylab="Temperature (°C)",
       main="TMS data")
  lines(data$date, data$T2, col="red", lty=3)
  lines(data$date, data$T3, col="pink", lty=4)
  # plot raw capacitance
  plot(data$date, data$sm, type="l", xlab="Time", ylab="SM [TMS]",
       main="TMS data")
}

# select time interval
from <- which(as.POSIXct("2015-04-29 15:00", tz="GMT") <= data$date)[1]
to <- which(as.POSIXct("2015-09-19 11:00", tz="GMT") <= data$date)[1]

if (plots) {
  # plot temperatures for selected interval
  plot(data$date[from:to], data$T1[from:to], type="l", xlab="time",
       ylab="Temperature (degC)", main="TMS data",
       ylim=(c(min(data[, 4:6]), max(data[, 4:6]))))
  lines(data$date[from:to], data$T2[from:to], col="firebrick3", lty=3)
  lines(data$date[from:to], data$T3[from:to], col="lightskyblue2", lty=4)
}

# set unreasonable values to NA: temp. should be between 10 and 30 degC
data$T1[data$T1 < 10] <- NA
data$T2[data$T2 < 10 | data$T2 > 30] <- NA
data$T3[data$T3 < 10 | data$T3 > 30] <- NA

if (plots) {
  # plot temperatures for selected interval without unreasonable values
  plot(data$date[from:to], data$T1[from:to], type="l", xlab="time",
       ylab="Temperature (degC)", main="TMS data",
       ylim=(c(min(data[, 4:6], na.rm=T), max(data[, 4:6], na.rm=T))))
  lines(data$date[from:to], data$T2[from:to], col="firebrick3", lty=3)
  lines(data$date[from:to], data$T3[from:to], col="lightskyblue2", lty=4)
  # plot capacitance for selected interval
  plot(data$date[from:to], data$sm[from:to], type="l", xlab="time",
       ylab="TMS Signal", main="TMS data")
}

# remove local time and summer time
gmt <- -2 * 3600
weight$date <- as.POSIXct(weight$date, format="%d.%m.%Y %H:%M", tz="GMT") + gmt

# weight of devices: flower pot coaster + bottle + probe TMS 93131942
pot.coaster <- 40.8  # g
bottle <- 46.89  # g
probe <- 122.35  # g, TMS 93131942
dev <- pot.coaster + bottle + probe  # g

# volume of bottle
d <- 7.5  # cm, diameter
height <- 11.5  # cm, height
vol <- 3.14159265359 * (d / 2)^2 * height  # (cm3 = mL)

# weight of soil and water (g)
weight$soil_water <- weight$weight - dev

# dry soil in g
# J.M.D. had a -10 g "bonus" here, unnecessary according to Thomas G.
soil <- tail(weight$soil_water, 1)

# bulk density (g.cm-3)
bd <- soil / vol

# weight of water (g)
weight$water <- weight$soil_water - soil

# starting weight of water (g), needed for porosity
water <- weight$water[1]

# gravimetric soil moisture (g.g-1)
weight$gsm <- weight$water / soil

# porosity, different approaches
poro <- water / vol  # (mL.mL-1)
poro2 <- 1 - bd / 2.648  # (g.g-1) with density of SiO2

# volumetric soil moisture m3.m-3
weight$sm <- weight$gsm * bd  # mL.mL-1

if (plots) {
  # plot sample weights (total)
  plot(weight$date, weight$weight, xlab="Time", ylab="Weight of sample (g)")
  # plot sample weights (soil + water)
  plot(weight$date, weight$soil_water, xlab="Time",
       ylab="Weight of soil and water in sample (g)")
  # plot sample weights (water)
  plot(weight$date, weight$water, xlab="Time",
       ylab="Weight of water in the sample (g)")
  # plot volumetric soil moisture content
  plot(weight$date, weight$sm * 100, xlab="Time",
       ylab="Vol. Soil Moisture Content (%)")
}

# extract data points of logger at which weight was measured
x <- c()
for (i in 1:length(weight$date)) {
  x[i] <- which.min(as.numeric(data$date - weight$date[i])^2)
}

# Next section is a little bit obscure.
# TMS standard polynomial with standard parametrisation
data$sm2 <- .000000000009 * data$sm^4 - .0000000382 * data$sm^3 +
            .0001 * data$sm^2 - .1168 * data$sm + 42.407

if (plots)
  # Plot what exactly?
  plot(data$sm2[x], weight$sm * 100, xlab="TMS SM (m3 m-3)",
       ylab="Weight sample (g)")

# MODEL CALIBRATION ------------------------------------------------------------

sm <- weight$sm
tms <- data$sm[x]

# color values: transparent red
r <- 250 / 256
g <- 83 / 256
b <- 62 / 256
al <- .3  # alpha (opacity)

# UppErr function: calculates upper standard error for every observation
UppErr <- function(x,       # x values
                   mod.par  # parameters of nls model, coef(summary(nls()))
                   ) {
  rowSums(sapply(1:nrow(mod.par),
                 function(i)
                   x ^ (nrow(mod.par) - i) * (mod.par[i, 1] + mod.par[i, 2])))
}

# LowErr function: calculates the lower standard error for every observation
LowErr <- function(x,       # x values
                   mod.par  # parameters of nls model, coef(summary(nls)))
                   ) {
  rowSums(sapply(1:nrow(mod.par),
                 function(i)
                   x ^ (nrow(mod.par) - i) * (mod.par[i, 1] - mod.par[i, 2])))
}

# LINEAR MODEL

lmod <- lm(sm ~ tms)
para1 <- coef(summary(lmod))
upp1 <- tms * (para1[2, 1] + para1[2, 2]) + para1[1, 1] + para1[1, 2]
low1 <- tms * (para1[2, 1] - para1[2, 2]) + para1[1, 1] - para1[1, 2]
# plot data & linear model
plot(data$sm[x], weight$sm, main="linear", xlab="TMS Signal",
     ylab="Vol. Soil Moisture Content [-]")
abline(lm(sm ~ tms), col="tomato2")
polygon(c(tms, rev(tms)), c(low1, rev(upp1)), col=rgb(r, g, b, al), border=NA)

# POLYNOMIAL MODEL (2ND ORDER)

nlmod2 <- nls(sm ~ a * tms ^ 2 + b * tms + c,
              trace=T, start=list(a=1e-8, b=1e-4, c=-1e-3))
para2 <- coef(summary(nlmod2))
upp2 <- UppErr(tms, para2)
low2 <- LowErr(tms, para2)
# plot data & 2nd order model
plot(tms, weight$sm, main="polynomial 2", xlab="TMS Signal", 
     ylab="Vol. Soil Moisture Content [-]")
lines(tms, predict(nlmod2), col="red")
polygon(c(tms, rev(tms)), c(low2, rev(upp2)), col=rgb(r, g, b, 0.3), border=NA)

# POLYNOMIAL MODEL (3RD ORDER)

nlmod3 <- nls(sm ~ a3 * tms ^ 3 + b3 * tms ^ 2 + c3 * tms + d3,
              trace=T, start=list(a3=-1e-12, b3=1e-8, c3=1e-4, d3=-1e-3))
para3 <- coef(summary(nlmod3))
upp3 <- UppErr(tms, para3)
low3 <- LowErr(tms, para3)
# plot data & 3rd order model
plot(tms, weight$sm, main="polynomial 3", xlab="TMS Signal", 
     ylab="Vol. Soil Moisture Content [-]")
lines(tms, predict(nlmod3), col="blue")
polygon(c(tms, rev(tms)), c(low3, rev(upp3)), col=rgb(r, g, b, 0.3), border=NA)

# POLYNOMIAL MODEL (4TH ORDER)

nlmod4 <- nls(sm ~ a * tms^4 + b * tms^3 + c * tms^2 + d * tms + e, 
              trace=T, start=list(a=-1e-14, b=1e-10, c=-1e-7, d=1e-4, e=-1e-1))
para4 <- coef(summary(nlmod4))
upp4 <- UppErr(tms, para4)
low4 <- LowErr(tms, para4)
# plot data & 4th order model
plot(tms, weight$sm, main="polynomial 4", xlab="TMS Signal",
     ylab="Vol. Soil Moisture Content [-]")
lines(tms, predict(nlmod4), col="red")
polygon(c(tms, rev(tms)), c(low4, rev(upp4)), col=rgb(r, g, b, 0.3), border=NA)

# ARTIFICIAL LINEAR MODEL

# assume linear behavior of TMS signal; force minimum and maximum s.m. value
wc_res <- .049  # bit lower than predicted residual s.m.
wc_sat <- .388  # bit higher than predicted saturated s.m.

# scaling factor
HS$a <- with(HS, (wc_sat - wc_res) / (range(obs$tms)[2] - range(obs$tms)[1]))
NSA$a <- with(NSA, (wc_sat - wc_res) / (range(obs$tms)[2] - range(obs$tms)[1]))
# intersect
HS$b <- wc_res
NSA$b <- wc_res

# MODEL APPLICATION ------------------------------------------------------------

HS$sm.out.lin.obs <- with(HS, a * obs$tms + b)
NSA$sm.out.lin.obs <- with(NSA, a * obs$tms + b)
HS$sm.out.lin <- with(HS, predict(lmod, newdata=obs))
NSA$sm.out.lin <- with(NSA, predict(lmod, newdata=obs))
HS$sm.out.poly2 <- with(HS, predict(nlmod2, newdata=obs))
NSA$sm.out.poly2 <- with(NSA, predict(nlmod2, newdata=obs))
HS$sm.out.poly3 <- with(HS, predict(nlmod3, newdata=obs))
NSA$sm.out.poly3 <- with(NSA, predict(nlmod3, newdata=obs))
HS$sm.out.poly4 <- with(HS, predict(nlmod4, newdata=obs))
NSA$sm.out.poly4 <- with(NSA, predict(nlmod4, newdata=obs))

# plot different results

r <- 62 / 256
g <- 83 / 256
b <- 250 / 256
al <- .1  # opacity

# Hauptstation
pdf("plot_tms_predict_HS.pdf")
# set outer margins
par(oma=c(0, 0, 3, 0))
# 4 x 1 layout
layout(matrix(1:4, nrow=4), heights=c(.23, .23, .23, .31))
# plot linear models
par(mar=c(0, 5, 4, 0))
plot(NA, xaxt="n", main="linear", ylab="Vol. Soil Moist. (-)",
     xlim=with(HS, c(min(obs$date), max(obs$date))),
     ylim=with(HS, c(min(sm.out.lin), max(sm.out.lin))))
polygon(x=c(as.POSIXct("2013-12-20 16:30:00"),
            rep(as.POSIXct("2014-08-04 18:00:00"), 2),
            as.POSIXct("2013-12-20 16:30:00")),
        y=c(-1, -1, 1, 1), col=rgb(r, g, b, al), border=NA)
with(HS, lines(obs$date, sm.out.lin))
with(HS, lines(obs$date, sm.out.lin.obs, col="red2"))
legend("topright", c("linear map", "regression"), lty=c(1, 1),
       col=c("red2", "1"))
with(HS, axis(1, at=pretty(range(obs$date)),
              labels=rep("", length(pretty(range(obs$date))))))
abline(h=0, lty=3)
# plot polynomial 2
plot(NA, xaxt="n", main="2nd-degree polynomial",
     ylab="Vol. Soil Moist. (-)",
     xlim=with(HS, c(min(obs$date), max(obs$date))),
     ylim=with(HS, c(min(sm.out.poly2), max(sm.out.poly2))))
polygon(x=c(as.POSIXct("2013-12-20 16:30:00"),
            rep(as.POSIXct("2014-08-04 18:00:00"), 2),
            as.POSIXct("2013-12-20 16:30:00")),
        y=c(-1, -1, 1, 1), col=rgb(r, g, b, al), border=NA)
with(HS, lines(obs$date, sm.out.poly2))
with(HS, axis(1, at=pretty(range(obs$date)),
              labels=rep("", length(pretty(range(obs$date))))))
abline(h=0, lty=3)
# plot polynomial 3
plot(NA, xaxt="n", main="3rd-degree polynomial",
     ylab="Vol. Soil Moist. (-)",
     xlim=with(HS, c(min(obs$date), max(obs$date))),
     ylim=with(HS, c(min(sm.out.poly3), max(sm.out.poly3))))
polygon(x=c(as.POSIXct("2013-12-20 16:30:00"),
            rep(as.POSIXct("2014-08-04 18:00:00"), 2),
            as.POSIXct("2013-12-20 16:30:00")),
        y=c(-1, -1, 1, 1), col=rgb(r, g, b, al), border=NA)
with(HS, lines(obs$date, sm.out.poly3))
with(HS, axis(1, at=pretty(range(obs$date)),
              labels=rep("", length(pretty(range(obs$date))))))
abline(h=0, lty=3)
# plot polynomial 4
par(mar=c(4, 5, 4, 0))
plot(NA, xaxt="n", main="4th-degree polynomial", xlab="Date",
     ylab="Vol. Soil Moist. (-)",
     xlim=with(HS, c(min(obs$date), max(obs$date))),
     ylim=with(HS, c(min(sm.out.poly4), max(sm.out.poly4))))
polygon(x=c(as.POSIXct("2013-12-20 16:30:00"),
            rep(as.POSIXct("2014-08-04 18:00:00"), 2),
            as.POSIXct("2013-12-20 16:30:00")),
        y=c(-1, -1, 1, 1), col=rgb(r, g, b, al), border=NA)
with(HS, lines(obs$date, sm.out.poly4))
with(HS, axis(1, at=pretty(range(obs$date)), labels=pretty(range(obs$date))))
abline(h=0, lty=3)
# give common title
mtext("Portugal, Hauptstation", outer=T)
dev.off()

system("evince ~/boxup/whk_echse/tms_portugal/plot_tms_predict_HS.pdf &")

# Nebenstation A
# TODO(2016-10-20): check data period for polygon!
pdf("plot_tms_predict_NSA.pdf")
# set outer margins
par(oma=c(0, 0, 3, 0))
# 4 x 1 layout
layout(matrix(1:4, nrow=4), heights=c(.23, .23, .23, .31))
# plot linear models
par(mar=c(0, 5, 4, 0))
plot(NA, xaxt="n", main="linear", ylab="Vol. Soil Moist. (-)",
     xlim=with(NSA, c(min(obs$date), max(obs$date))),
     ylim=with(NSA, c(min(sm.out.lin), max(sm.out.lin))))
polygon(x=c(as.POSIXct("2013-12-20 16:30:00"),
            rep(as.POSIXct("2014-08-04 18:00:00"), 2),
            as.POSIXct("2013-12-20 16:30:00")),
        y=c(-1, -1, 1, 1), col=rgb(r, g, b, al), border=NA)
with(NSA, lines(obs$date, sm.out.lin))
with(NSA, lines(obs$date, sm.out.lin.obs, col="red2"))
legend("topright", c("linear map", "regression"), lty=c(1, 1),
       col=c("red2", "1"))
with(NSA, axis(1, at=pretty(range(obs$date)),
               labels=rep("", length(pretty(range(obs$date))))))
abline(h=0, lty=3)
# plot polynomial 2
plot(NA, xaxt="n", main="2nd-degree polynomial",
     ylab="Vol. Soil Moist. (-)",
     xlim=with(NSA, c(min(obs$date), max(obs$date))),
     ylim=with(NSA, c(min(sm.out.poly2), max(sm.out.poly2))))
polygon(x=c(as.POSIXct("2013-12-20 16:30:00"),
            rep(as.POSIXct("2014-08-04 18:00:00"), 2),
            as.POSIXct("2013-12-20 16:30:00")),
        y=c(-1, -1, 1, 1), col=rgb(r, g, b, al), border=NA)
with(NSA, lines(obs$date, sm.out.poly2))
with(NSA, axis(1, at=pretty(range(obs$date)),
               labels=rep("", length(pretty(range(obs$date))))))
abline(h=0, lty=3)
# plot polynomial 3
plot(NA, xaxt="n", main="3rd-degree polynomial",
     ylab="Vol. Soil Moist. (-)",
     xlim=with(NSA, c(min(obs$date), max(obs$date))),
     ylim=with(NSA, c(min(sm.out.poly3), max(sm.out.poly3))))
polygon(x=c(as.POSIXct("2013-12-20 16:30:00"),
            rep(as.POSIXct("2014-08-04 18:00:00"), 2),
            as.POSIXct("2013-12-20 16:30:00")),
        y=c(-1, -1, 1, 1), col=rgb(r, g, b, al), border=NA)
with(NSA, lines(obs$date, sm.out.poly3))
with(NSA, axis(1, at=pretty(range(obs$date)),
               labels=rep("", length(pretty(range(obs$date))))))
abline(h=0, lty=3)
# plot polynomial 4
par(mar=c(4, 5, 4, 0))
plot(NA, xaxt="n", main="4th-degree polynomial", xlab="Date",
     ylab="Vol. Soil Moist. (-)",
     xlim=with(NSA, c(min(obs$date), max(obs$date))),
     ylim=with(NSA, c(min(sm.out.poly4), max(sm.out.poly4))))
polygon(x=c(as.POSIXct("2013-12-20 16:30:00"),
            rep(as.POSIXct("2014-08-04 18:00:00"), 2),
            as.POSIXct("2013-12-20 16:30:00")),
        y=c(-1, -1, 1, 1), col=rgb(r, g, b, al), border=NA)
with(NSA, lines(obs$date, sm.out.poly4))
with(NSA, axis(1, at=pretty(range(obs$date)), labels=pretty(range(obs$date))))
abline(h=0, lty=3)
# give common title
mtext("Portugal, Nebenstation A", outer=T)
dev.off()

system("evince ~/boxup/whk_echse/tms_portugal/plot_tms_predict_NSA.pdf &")

# Linear model would be best for reasons of simplicity;
# however, linear regression (including other regressions) leads to negative
# values or exceeds the theoretically possible saturated water content.
# Possibility 1: Map field obs. linearly to possible range (sm.out.lin.obs).
# Possibility 2:
# Choose either polynomial 3 or 4; 2 leads to negative values and overestimates
# soil moisture (PTF after Woesten estimate 38.7 % saturated water content);
# 4 has some negative values, too -> set to 0 if used for model.

# OUTPUT -----------------------------------------------------------------------

write.table(with(HS, data.frame(date=obs$date, sm=sm.out.lin.obs)),
            "../data/portugal/soil_moisture_HS.txt", sep="\t", row.names=F)
write.table(with(NSA, data.frame(date=obs$date, sm=sm.out.lin.obs)),
            "../data/portugal/soil_moisture_NSA.txt", sep="\t", row.names=F)

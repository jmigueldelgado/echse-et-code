################################################################################
# Author: Julius Eberhard
# Last Edit: 2017-06-04
# Project: ECHSE Evapotranspiration
# Program: echse_portugal
# Aim: Data Preprocessing and Main Executing Script for ET in Portugal
################################################################################

rm(list=ls())
assign("last.warning", NULL, envir=baseenv())

# set time zone to UTC
Sys.setenv(TZ="UTC")

# PROGRAM PARAMETERS -----------------------------------------------------------

output <- "evap"  # [evap, glorad, gloradmax, rad_net, radex, soilheat]
field.station <- "HS"  # [HS, NSA]
et.choice <- "eta"  # potential or actual evapotranspiration [etp, eta]
et.choice[2] <- 13  # model [1=Makk, 11=PM, 12=FAO, 13=SW]
wc.new <- T  # logical: Shall the newly calculated soil moisture data be used?
emismeth <- "Brunt"  # [Brunt, Idso, both]; method for calculating net emissivity

# model period: start & end of model period, note the format!
if (output %in% c("gloradmax", "rad_net", "radex")) {
  tstart <- "2014-04-29 22:00:00"
  tend <- "2014-07-01 05:00:00"
} else if (field.station == "HS") {
# HS
  #tstart <- "2014-04-29 21:00:00"; tend <- "2014-05-04 13:00:00"
  tstart <- "2014-05-14 12:00:00"; tend <- "2014-05-20 19:00:00"
  #tstart <- "2014-05-22 15:00:00"; tend <- "2014-05-25 21:00:00"
  #tstart <- "2014-06-17 12:00:00"; tend <- "2014-06-21 17:00:00"
  #tstart <- "2014-06-26 01:00:00"; tend <- "2014-07-01 07:00:00"
} else {
# NSA
  tstart <- "2014-04-29 21:00:00"; tend <- "2014-05-04 13:00:00"
  #tstart <- "2014-05-14 12:00:00"; tend <- "2014-05-20 19:00:00"
  #tstart <- "2014-05-22 15:00:00"; tend <- "2014-05-25 21:00:00"
  #tstart <- "2014-06-12 12:00:00"; tend <- "2014-06-14 03:00:00"
  #tstart <- "2014-06-17 12:00:00"; tend <- "2014-06-21 17:00:00"
  #tstart <- "2014-06-26 01:00:00"; tend <- "2014-07-01 07:00:00"
  #tstart <- "2014-07-15 12:00:00"; tend <- "2014-07-21 07:00:00"
  #tstart <- "2014-07-23 23:00:00"; tend <- "2014-08-04 18:00:00"
}

# Shall the model period strictly exclude times with missing data?
no.na <- TRUE

# For rad_net & radex, we need many data, so no.na is unchecked.
if (output %in% c("gloradmax", "rad_net", "radex"))
  no.na <- FALSE

# declare availability of input data
A <- list(alb=1,
          apress=1,
          cano_height=1,
          cloud=0,
          doy=1,
          glorad=1,
          glorad_max=0,
          hour=1,
          lai=1,
          rad_long=1,
          rad_net=1,
          rad_net_soil=0,
          radex=0,
          rhum=1,
          soilheat=1,
          sundur=0,
          temp_max=0,
          temp_min=0,
          temper=1,
          totalheat=0,
          utc_add=1,
          wc_vol_root=1,
          wc_vol_top=1,
          wind=1)

# locations
locs <- list(field.station,  # alb
             "Met",  # apress
             field.station,  # cano_height
             "any",  # cloud, not actually used
             "any",  # doy
             field.station,  # glorad
             "any",  # glorad_max
             "any",  # hour
             field.station,  # lai
             field.station,  # rad_long
             field.station,  # rad_net
             "any",  # rad_net_soil
             "any",  # radex
             "Met",  # rhum
             field.station,  # soilheat
             "Met",  # sundur
             "any",  # temp_max
             "any",  # temp_min
             field.station,  # temper
             "any",  # totalheat
             "any",  # utc_add
             field.station,  # wc_vol_root
             field.station,  # wc_vol_top
             field.station)  # wind

# SET AND LOAD -----------------------------------------------------------------

# set working directory
if (system("hostname", intern=T) == "raspberrypi") {
  setwd("/home/pi/whk_echse")
} else {
  setwd("/home/julius/boxup/whk_echse")
}

# load scripts
source("~/R/functions/jufun.R")  # CheckPack(), ptf functions
source("echseCtrl.R")  # control files
source("echseInput.R")  # data input files
source("echseParEst.R")  # parameter estimation
source("echsePost.R")  # postprocessing
source("echsePre.R")  # preprocessing

# load packages
library(MASS)  # write.matrix()
library(RAtmosphere)  # suncalc() in echseParEst() for f_day, f_night
library(TTR)  # runMean() in echsePost()
library(xtable)  # latex tables
library(xts)  # time series handling

# load data
# meteo data
HS.full <- get(load("data/portugal/meteo_HS.Rdata"))
NSA.full <- get(load("data/portugal/meteo_NSA.Rdata"))
meteo <- na.omit(read.csv("data/portugal/meteo_ok.csv", header=TRUE))
radl.down <- readRDS("data/portugal/Ldown")  # field station
radl.up <- readRDS("data/portugal/Lup")      # ditto
rads.down <- readRDS("data/portugal/Kdown")  # ditto
rads.up <- readRDS("data/portugal/Kup")      # ditto
index(radl.down) <- index(radl.down) + 3600
index(radl.up) <- index(radl.up) + 3600
index(rads.down) <- index(rads.down) + 3600
index(rads.up) <- index(rads.up) + 3600
# soil data at field stations
soildata <- read.table("data/portugal/soildata.dat", header=T)
# water content at field stations
wc.HS <- read.table("data/portugal/soil_moisture_HS.txt", header=T, sep="\t")
wc.NSA <- read.table("data/portugal/soil_moisture_NSA.txt", header=T, sep="\t")

# PREPROCESSING ----------------------------------------------------------------

engine <- paste(output, "portugal", sep="_")  # engine name: output_location
years <- min(meteo$year, format(index(HS.full), "%Y"),
             format(index(NSA.full), "%Y")):
         max(meteo$year, format(index(HS.full), "%Y"),
             format(index(NSA.full), "%Y"))

# time step in seconds
if (output == "evap") {
  dt <- 3600
} else if (output == "glorad") {
  dt <- 86400
  A$sundur <- 1
} else if (output == "gloradmax") {
  dt <- 3600
} else if (output == "rad_net") {
  dt <- 3600
} else if (output == "radex") {
  dt <- 3600
} else if (output == "soilheat") {
  dt <- 3600
}

# data & variable & parameter selection
selection <- echseSelect(output)

# structures
HS.list <- NULL
NSA.list <- NULL
meteo.list <- NULL
HS.names <- c(expression(Net~Radiation~(W~m^{-2})),
              expression(Temperature~(degree~C)),
              expression(Soil~heat~flux~(W~m^{-2})),
              expression(Soil~moisture~content~(-{})),
              expression(Sensible~heat~flux~(W~m^{-2})),
              expression(Latent~heat~flux~(W~m^{-2})),
              expression(Bowen~ratio~(-{})),
              expression(Evaporation~(mm~h^{-1})),
              expression(Vapor~pressure~deficit~(kPa)),
              expression(Vapor~pressure~(kPa)),
              expression(Wind~speed~(m~s^{-1})),
              expression(LW~downward~radiation~(W~m^{-2})),
              expression(LW~upward~radiation~(W~m^{-2})),
              expression(SW~downward~radiation~(W~m^{-2})),
              expression(SW~upward~radiation~(W~m^{-2})),
              expression(LW~net~radiation~(W~m^{-2})))
NSA.names <- HS.names
meteo.names <- c("PARgl (unit?)",
                 "PARdif (unit?)",
                 expression(Incoming~radiation~SW~(W~m^{-2})),
                 expression(Outgoing~radiation~LW~(W~m^{-2})),
                 expression(Temperature~(degree~C)),
                 expression(Relative~humidity~("%")),
                 expression(Rainfall~(mm~d^{-1})),
                 expression(Barometric~pressure~(hPa)))

# FIELD STATION DATA

tzone(HS.full) <- "UTC"
tzone(NSA.full) <- "UTC"

# rad data: remove duplicates, fill up missing dates, calculate hourly means
RadPrep <- function(data) {
  seq.10min <- seq(index(data)[3], tail(index(data), 1), by="10 min")
  seq.1h <- seq(index(data)[3], tail(index(data), 1), by="hours")
  xts(period.apply(merge(xts(order.by=seq.10min), data),
                   seq.int(1, length(seq.10min), by=6),
                   mean),
      order.by=seq.1h[-length(seq.1h)])
}

radl.down.hly <- RadPrep(radl.down[!duplicated(index(radl.down))])
radl.up.hly <- RadPrep(radl.up[!duplicated(index(radl.up))])
rads.down.hly <- RadPrep(rads.down[!duplicated(index(rads.down))])
rads.up.hly <- RadPrep(rads.up[!duplicated(index(rads.up))])

# merge meteo data at field stations, fill up missing dates
HS.full <- merge(xts(order.by=seq(index(HS.full)[1],
                                  tail(index(HS.full), 1), by="hours")), 
                 HS.full, radl.down.hly, radl.up.hly, rads.down.hly,
                 rads.up.hly, radl.down.hly - radl.up.hly, tzone="UTC")
NSA.full <- merge(xts(order.by=seq(index(NSA.full)[1],
                                   tail(index(NSA.full), 1), by="hours")), 
                  NSA.full, radl.down.hly, radl.up.hly, rads.down.hly,
                  rads.up.hly, radl.down.hly - radl.up.hly, tzone="UTC")
names(HS.full)[(ncol(HS.full) - 4):ncol(HS.full)] <- c("RLdown", "RLup",
                                                       "RSdown", "RSup",
                                                       "RLnet")
names(NSA.full)[(ncol(NSA.full) - 4):ncol(NSA.full)] <- c("RLdown", "RLup",
                                                          "RSdown", "RSup",
                                                          "RLnet")

# create mask of NA intervals
if (field.station == "HS") {
  na.mask <- sort(unique(do.call(c, lapply(list(HS.full[, "R"], 
                                                HS.full[, "T"], 
                                                HS.full[, "G"], 
                                                HS.full[, "theta"], 
                                                HS.full[, "u"],
                                                HS.full[, "RLdown"],
                                                HS.full[, "RLup"],
                                                HS.full[, "RSdown"],
                                                HS.full[, "RSup"]),
                                           function(x)
                                             index(HS.full)[is.na(x)]))))
} else {
  na.mask <- sort(unique(do.call(c, lapply(list(NSA.full[, "R"], 
                                                NSA.full[, "T"], 
                                                NSA.full[, "G"], 
                                                NSA.full[, "theta"], 
                                                NSA.full[, "u"],
                                                NSA.full[, "RLdown"],
                                                NSA.full[, "RLup"],
                                                NSA.full[, "RSdown"],
                                                NSA.full[, "RSup"]),
                                           function(x)
                                             index(NSA.full)[is.na(x)]))))
}

# check no.na criterion
na.present <- any(!is.na(match(as.character(seq(as.POSIXct(tstart), 
                                                as.POSIXct(tend), 
                                                by=paste(dt, "sec"))),
                               as.character(na.mask))))
if (no.na && na.present)
  stop("Model period contains missing data and no.na == TRUE.")

# split data into list elements for further processing
for (i in 1:length(HS.names)) {
  HS.list <- c(HS.list,
               list(xts(order.by=index(HS.full[format(as.Date(tend), "%Y"), ]),
                        HS.full[format(as.Date(tend), "%Y"), i])))
  NSA.list <- c(NSA.list,
                list(xts(order.by=index(NSA.full[format(as.Date(tend),
                                                        "%Y"), ]),
                         NSA.full[format(as.Date(tend), "%Y"), i])))
}

# soil moisture data
wc.HS.xts <- merge(xts(wc.HS$sm, order.by=as.POSIXct(wc.HS$date)),
                   xts(order.by=seq(as.POSIXct(wc.HS$date[1]),
                                    as.POSIXct(tail(wc.HS$date, 1)),
                                    by="15 min")))
wc.NSA.xts <- merge(xts(wc.NSA$sm, order.by=as.POSIXct(wc.NSA$date)),
                    xts(order.by=seq(as.POSIXct(wc.NSA$date[1]),
                                     as.POSIXct(tail(wc.NSA$date, 1)),
                                     by="15 min")))
# take hourly data
ep.HS <- endpoints(wc.HS.xts, on="hours") + 1
wc.HS.xts <- xts(period.apply(wc.HS.xts, ep.HS[-length(ep.HS)], mean),
                 order.by=index(wc.HS.xts)[ep.HS[-c(1, length(ep.HS))]])
ep.NSA <- endpoints(wc.NSA.xts, on="hours") + 1
wc.NSA.xts <- xts(period.apply(wc.NSA.xts, ep.NSA[-length(ep.NSA)], mean),
                  order.by=index(wc.NSA.xts)[ep.NSA[-c(1, length(ep.NSA))]])
if (!wc.new) {
  # Set soil moisture to zero where negative ...
  HS.list[[4]][HS.list[[4]] < 0] <- 0
  NSA.list[[4]][NSA.list[[4]] < 0] <- 0
} else {
  # ... or use new data instead.
  date.rg.HS <- range(index(HS.list[[4]]))
  date.rg.NSA <- range(index(NSA.list[[4]]))
  HS.list[[4]] <- wc.HS.xts[index(wc.HS.xts) >= date.rg.HS[1]
                              & index(wc.HS.xts) <= date.rg.HS[2]]
  NSA.list[[4]] <- wc.NSA.xts[index(wc.NSA.xts) >= date.rg.NSA[1]
                                & index(wc.NSA.xts) <= date.rg.NSA[2]]
}

# METEO STATION DATA

meteo$date <- NA  # date (Y-m-d)

# hhmm==0 is understood as 24:00 in file but as 0:00 here;
# therefore increase doy by 1
# i.e. "doy=320, hhmm=0" becomes "doy=321, hhmm=0"
meteo$doy[which(meteo$hhmm == 0)] <- meteo$doy[which(meteo$hhmm == 0)] + 1

# convert dates into POSIX-readable dates
for (j in 1:length(years)) {
  meteo$date[meteo$year == years[j]] <- 
    as.Date(meteo$doy[which(meteo$year == years[j])], 
            origin=paste0(years[j] - 1, "-12-31"))
}
meteo$date <- as.Date(meteo$date)
hours <- sprintf("%02d", floor(meteo$hhmm / 100))
minutes <- sprintf("%02d", meteo$hhmm - (floor(meteo$hhmm / 100) * 100))
seconds <- sprintf("%02d", rep(0, length(meteo$hhmm)))
meteo$dati <- paste0(meteo$date, " ", hours, ":", minutes, ":", seconds)

# select relevant columns, convert to xts
meteo <- meteo[, -c(1, 2, 3, 4, ncol(meteo) - 2, ncol(meteo) - 1)]
meteo.xts <- xts(meteo[2:nrow(meteo), ],
                 order.by=as.POSIXct(meteo$dati[1:(nrow(meteo) - 1)],
                                     tz="UTC"),
                 tzone="UTC")

# fill up missing dates
suppressWarnings(
  meteo.xts <- merge(xts(order.by=seq(index(meteo.xts)[1], 
                                      tail(index(meteo.xts), 1), 
                                      by="30 min"), tzone="UTC"), 
                     meteo.xts, tzone="UTC")
)

# time sequence
time.seq <- echseTimeSeq(index(meteo.xts), tstart, dt)

# split data into list elements for further processing
for (k in 1:8)
  meteo.list[[k]] <- meteo.xts[, k + 1]

# fill up missing global radiation data with meteo data
na.dates.HS.r <- index(HS.list[[1]][is.na(HS.list[[1]])])
na.dates.NSA.r <- index(NSA.list[[1]][is.na(NSA.list[[1]])])
HS.list[[1]][na.dates.HS.r] <- ifelse(length(na.dates.HS.r) != 0,
                                      meteo.list[[3]][na.dates.HS.r], NA)
NSA.list[[1]][na.dates.NSA.r] <- ifelse(length(na.dates.NSA.r) != 0,
                                        meteo.list[[3]][na.dates.NSA.r], NA)

# fill up missing temperature data with meteo data
na.dates.HS.t <- index(HS.list[[2]][is.na(HS.list[[2]])])
na.dates.NSA.t <- index(NSA.list[[2]][is.na(NSA.list[[2]])])
HS.list[[2]][na.dates.HS.t] <- ifelse(length(na.dates.HS.t) != 0,
                                      meteo.list[[5]][na.dates.HS.t], NA)
NSA.list[[2]][na.dates.NSA.t] <- ifelse(length(na.dates.NSA.t) != 0,
                                        meteo.list[[5]][na.dates.NSA.t], NA)

# derive sunshine hours according to WMO, 2003 (time for which rad > 120 W.m-2)
if (dt == 86400) {
  rad <- meteo.xts[index(meteo.xts) >= time.seq[1] & 
                   index(meteo.xts) <= tail(time.seq, 1), 3]
  # add up to multiple of 48 length
  length(rad) <- length(rad) + 48 - length(rad) %% 48
  # matrix with days (rows) and 48 half hours (columns)
  rad.mat <- matrix(rad, ncol=48, byrow=T)
  sundur.val <- c()
  days <- 1:nrow(rad.mat)
  for (i in days)
    sundur.val[i] <- length(rad.mat[i, na.omit(rad.mat[i, ]) > 120]) / 2
  sundur.xts <- xts(sundur.val,
                    order.by=seq(index(meteo.xts[time.seq])[1],
                                 tail(index(meteo.xts[time.seq]), 1),
                                 by=paste(dt, "sec")))
}

# ENGINE PARAMETERS ------------------------------------------------------------

# INDIVIDUAL PARAMETERS

# geographical parameters: Silveira, Santarem, Portugal
lat <- 39.14
lon <- 8.33
elev <- 160  # in m

# soil parameters, soil type: loamy sand (FAO)
clay <- soildata[1, "clay"]
silt <- soildata[1, "silt"]
som <- soildata[1, "som"]
soil_dens <- 1500  # bulk density, estimated from different sources
soilprop <- data.frame(clay=clay, bulkD=soil_dens / 1000, silt=silt, om=som)
wc_sat <- ptf.wosten.theta.s(clay=clay, bulkD=soil_dens / 1000, silt=silt,
                             om=som, topSoil=1)
wc_pwp <- pft.rawls(soilprop, parameters="theta", h=15000)[, "theta"]
wc_res <- pft.rawls(soilprop, parameters="theta_r", h=0)[, "theta_r"]
# field capacity for estimation of wc_etmax
wc_fc <- pft.rawls(soilprop, parameters="theta", h=316)[, "theta"]
# wc_etmax is a calibration parameter! This is only a rough estimation:
wc_etmax <- 0.8 * wc_fc  # used only for Makkink, FAO
bubble <- pft.rawls(soilprop, parameters="h_b", h=0)[, "h_b"]
pores_ind <- pft.rawls(soilprop, parameters="lambda", h=0)[, "lambda"]

# check if measured water content undercuts parameter value
if (any(HS.list[[4]] < wc_res, na.rm=TRUE))
  wc_res <- min(na.omit(HS.list[[4]]))

# plant parameters
crop_faoref <- 1
crop_makk <- .8
glo_half <- 200
par_stressHum <- .03
res_leaf_min <- 50
wstressmax <- 15849  # -> thesis
wstressmin <- 100

# collect individual parameters
paramNum <- list(bubble=bubble, crop_faoref=crop_faoref, crop_makk=crop_makk,
                 elev=elev, glo_half=glo_half, lat=lat, lon=lon,
                 par_stressHum=par_stressHum, pores_ind=pores_ind,
                 res_leaf_min=res_leaf_min, soil_dens=soil_dens,
                 wc_etmax=wc_etmax, wc_pwp=wc_pwp, wc_res=wc_res,
                 wc_sat=wc_sat, wstressmax=wstressmax, wstressmin=wstressmin)

# write parameter list to latex table
names.pN <- sapply(names(paramNum),
                   function(name)
                     paste0("\\", "verb!", name, "!"))
comm.pN <- c(paste0("PTF by \\", "citet{rawls85}"),
             "evaporation of reference crop",
             paste0("Eq. \\", "eqref{eq:cropmakk}"),
             "local elevation map", "guessed", "GIS data", "ditto",
             "guessed", paste0("PTF by \\", "citet{rawls85}"), "guessed",
             "guessed", "calibration", paste0("PTF by \\", "citet{rawls85}"),
             paste0("(PTF by \\", "citet{rawls85})"),
             paste0("PTF by \\", "citet{woesten99}"),
             "wilting point", "field capacity")
paramNum.df <- data.frame(Parameter=names.pN,
                          Value=as.numeric(paramNum),
                          Unit=c("hPa", "--", "--", "m", "W m$^{-2}$", "°",
                                 "°", "hPa$^{-1}$", "--", "s m$^{-1}$",
                                 "kg m$^{-3}$", "--", "--", "--", "--", "hPa",
                                 "hPa"),
                          Comment=comm.pN)
paramNum.tex <- xtable(paramNum.df, align=c("c", "l", "r", "l", "l"),
                       caption=paste0("Object-specific scalar parameters ",
                                      "(\\", "textsf{paramNum}), ",
                                      field.station, " Portugal"),
                       label=paste0("tab:portugal", field.station, "_paramNum"))
print.xtable(paramNum.tex,
             file=paste0("doku/portugal", field.station, "_paramNum.tex"),
             include.rownames=F, sanitize.text.function=identity,
             caption.placement="top")

# SHARED PARAMETERS

choice_gloradmax <- 1
choice_plantDispl <- 1
choice_rcs <- 1
choice_roughLen <- 2
drag_coef <- .07
eddy_decay <- 2.5
emis_a <- .34
emis_b <- -.14
ext <- .4
f_day <- .1
f_night <- .7
fcorr_a <- 1.35
fcorr_b <- -.35
h_humMeas <- 2  # ifelse(field.station=="NSA", 7.98, 2)
h_tempMeas <- 2  # ifelse(field.station=="NSA", 7.98, 2)
h_windMeas <- 2  # ifelse(field.station=="NSA", 7.98, 2)
na_val <- "-9999."
radex_a <- .25
radex_b <- .5
res_b <- 25
rough_bare <- .01
rss_a <- 37.5  # calibration...
rss_b <- -1.23  # calibration...

path.meteo <- paste0("~/uni/projects/evap_portugal/data/forcing/",
                     "meteo/05_meteofill/out/", field.station, "/")
path.proj <- paste0("~/uni/projects/")

# estimate f_day & f_night from soil heat flux and net radiation
# ... Remember to run the rad_net_* engine first!
if (output != "rad_net") {
  f.out <- echseParEst("f",
                       rnetfile=paste0(path.proj, "rad_net_portugal/run/out/",
                                       field.station, "/test1.txt"),
                       sheatfile=paste0(path.meteo, "soilheat_data.dat"),
                       lat=lat, lon=lon, plots=FALSE)
  f_day <- f.out[1]
  f_night <- f.out[2]
}

#debugonce(echseParEst)

# estimate radex_a, radex_b from global radiation and extraterr. radiation
# ... Remember to run the radex_* engine first!
if (output != "radex") {
  radex.out <- echseParEst("radex",
                           rxfile=paste0(path.proj, "radex_portugal/run/out/",
                                         field.station, "/test1.txt"),
                           rsdfile="data/portugal/Kdown",
                           r.quantile=0.05, plots=FALSE)
  radex_a <- radex.out[1]
  radex_b <- radex.out[2]
}

#debugonce(echseParEst)

# compare emissivity models (Brunt vs. Idso-Jackson)
# no estimation, functions returns suggested values from Maidment (1993)
emis.out <- echseParEst("emis",
                        rsdfile="data/portugal/Kdown",
                        rxfile=paste0(path.proj, "radex_portugal/run/out/",
                                      field.station, "/test1.txt"),
                        rldfile="data/portugal/Ldown",
                        rlufile="data/portugal/Lup",
                        tafile=paste0(path.meteo, "temper_data.dat"),
                        hrfile=paste0(path.meteo, "rhum_data.dat"),
                        field.station=field.station,
                        radex_a=radex_a,
                        radex_b=radex_b)

# estimate fcorr_a, fcorr_b
# ... Remember to run the radex_* engine first!
fcorr.out <- echseParEst("fcorr",
                         rldfile="data/portugal/Ldown",
                         rlufile="data/portugal/Lup",
                         rsdfile="data/portugal/Kdown",
                         hrfile=paste0(path.meteo, "rhum_data.dat"),
                         rxfile=paste0(path.proj, "radex_portugal/run/out/",
                                       field.station, "/test1.txt"),
                         tafile=paste0(path.meteo, "temper_data.dat"),
                         emis_a=emis_a, emis_b=emis_b, radex_a=radex_a,
                         radex_b=radex_b, emismeth=emismeth, plots=FALSE)
fcorr_a <- fcorr.out$a
fcorr_b <- fcorr.out$b

if (emismeth == "both") {
# In dubio, return Brunt coefficients because this is the favored model.
  fcorr_a <- fcorr_a[1]
  fcorr_b <- fcorr_b[1]
}

if (round(fcorr_a + fcorr_b, 3) != 1)
  stop("The sum of fcorr_a and fcorr_b must equal 1!")

# collect shared parameters
sharedParamNum <- list(choice_et=et.choice[2],
                       choice_gloradmax=choice_gloradmax,
                       choice_plantDispl=choice_plantDispl,
                       choice_rcs=choice_rcs, choice_roughLen=choice_roughLen,
                       drag_coef=drag_coef, eddy_decay=eddy_decay,
                       emis_a=emis_a, emis_b=emis_b, ext=ext, f_day=f_day,
                       f_night=f_night, fcorr_a=fcorr_a, fcorr_b=fcorr_b,
                       h_humMeas=h_humMeas, h_tempMeas=h_tempMeas,
                       h_windMeas=h_windMeas, na_val=na_val, radex_a=radex_a,
                       radex_b=radex_b, res_b=res_b, rough_bare=rough_bare,
                       rss_a=rss_a, rss_b=rss_b)

# write parameter list to latex table
names.sPN <- sapply(names(sharedParamNum),
                    function(name)
                      paste0("\\", "verb!", name, "!"))
comm.sPN <- c("calibration",
              paste0("as used by \\", "citet{shuttleworth85} from \\",
              "citet{monteith73}"),
              paste0("as used by \\", "citet{maidment93} for average ",
                     "conditions"),
              "ditto", "guessed", "estimation from soil heat data",
              "ditto", paste0("as used by \\", "citet{maidment93}"), "ditto",
              "", "", "", "estimation from radiation data", "ditto",
              paste0("as used by \\", "citet{shuttleworth85}"), "ditto", "", "")
sharedParamNum.df <- data.frame(Parameter=names.sPN[-c(1:5, 18)],
                                Value=as.numeric(sharedParamNum)[-c(1:5, 18)],
                                Unit=c("--", "--", "--", "--", "--", "--", "--",
                                       "--", "--", "m", "m", "m", "--",
                                       "--", "s m$^{-1}$", "m", "--", "--"),
                                Comment=comm.sPN)
sharedParamNum.tex <- xtable(sharedParamNum.df,
                             align=c("c", "l", "r", "l", "l"),
                             caption=paste0("Group-specific ",
                                            "scalar parameters ",
                                            "(\\", "textsf{sharedParamNum}), ",
                                            field.station, " Portugal"),
                             label=paste0("tab:portugal", field.station,
                                          "_sharedParamNum"))
print.xtable(sharedParamNum.tex,
             file=paste0("doku/portugal", field.station, "_sharedParamNum.tex"),
             include.rownames=F, sanitize.text.function=identity,
             caption.placement="top")

# EXTERNAL INPUT PARAMETERS (alb, cano_height, lai)

alb <- echseParEst("alb", rsdfile="data/portugal/Kdown",
                   rsufile="data/portugal/Kup", plots=TRUE)
cano_height <- 0.20  # ifelse(field.station=="NSA", 7.98, 0.20)
lai <- 0.778  # ifelse(field.station == "NSA", 1.397, 0.778)

inputExt.df <- data.frame(Parameter=c(paste0("\\", "verb!alb!"),
                                      paste0("\\", "verb!cano_height!"),
                                      paste0("\\", "verb!lai!")),
                          Value=c(alb, cano_height, lai),
                          Unit=c("--", "m", "--"))
inputExt.tex <- xtable(inputExt.df, align=c("c", "l", "r", "l"),
                       caption=paste0("Time-dependent parameters ",
                                      "(\\", "textsf{inputExt}), ",
                                      field.station, " Portugal"),
                       label=paste0("tab:portugal", field.station,
                                    "_inputExt"))
print.xtable(inputExt.tex,
             file=paste0("doku/portugal", field.station, "_inputExt.tex"),
             include.rownames=F, sanitize.text.function=identity,
             caption.placement="top")

# WRITE DATA INPUT FILES -------------------------------------------------------

# alb (Albedo, -, AVER)
echseInput(engine=engine, 
           variable="alb",
           stn=field.station,
           const=alb,
           t.seq=time.seq,
           directory=paste0("vegPar_time_series/", field.station, "/"))

# apress (Air Pressure, hPa, AVER)
echseInput(engine=engine, 
           variable="apress", 
           na.val=mean(as.numeric(meteo.list[[8]]), na.rm=T), 
           stn="Met", 
           column=8, 
           t.seq=time.seq,
           directory=paste0("forcing/meteo/05_meteofill/out/", field.station,
                            "/"))

# cano_height (Canopy Height, m, AVER)
echseInput(engine=engine,
           variable="cano_height",
           stn=field.station,
           const=cano_height,
           t.seq=time.seq,
           directory=paste0("vegPar_time_series/", field.station, "/"))

# doy (Day of Year, d)
doy.df <- data.frame(end_of_interval=format(time.seq, "%Y-%m-%d %H:%M:%S"),
                     as.numeric(strftime(time.seq, "%j", tz="UTC")))
names(doy.df)[2] <- "any"
write.matrix(doy.df, 
             paste0("~/uni/projects/", engine,
                    "/data/vegPar_time_series/", field.station,
                    "/doy_data.dat"), sep="\t")

# glorad (Global Radiation, W.m-2, AVER)
echseInput(engine=engine,
           variable="glorad",
           stn=field.station,
           column=14,
           t.seq=time.seq,
           directory=paste0("forcing/meteo/05_meteofill/out/", field.station,
                            "/"))

# hour (Hour of Day, h)
hour.df <- data.frame(end_of_interval=format(time.seq, "%Y-%m-%d %H:%M:%S"),
                      as.numeric(strftime(time.seq, "%H", tz="UTC")))
names(hour.df)[2] <- "any"
write.matrix(hour.df,
             paste0("~/uni/projects/", engine, 
                    "/data/vegPar_time_series/", field.station,
                    "/hour_data.dat"), sep="\t")

# lai (Leaf Area Index, -, AVER)
echseInput(engine=engine,
           variable="lai",
           stn=field.station,
           const=lai,
           t.seq=time.seq,
           directory=paste0("vegPar_time_series/", field.station, "/"))

# rad_long (Net Incoming Long-Wave Radiation, W.m-2, AVER)
echseInput(engine=engine,
           variable="rad_long",
           na.val=0,
           stn=field.station,
           column=16,
           t.seq=time.seq,
           directory=paste0("forcing/meteo/05_meteofill/out/", field.station,
                            "/"))

# rad_net (Net Incoming Radiation, W.m-2, AVER)
echseInput(engine=engine,
           variable="rad_net",
           na.val=0,  # not perfect
           stn=field.station,
           column=1,
           t.seq=time.seq,
           directory=paste0("forcing/meteo/05_meteofill/out/", field.station,
                            "/"))

# rhum (Relative Humidity, %, AVER)
echseInput(engine=engine,
           variable="rhum",
           na.val=mean(as.numeric(meteo.list[[6]]), na.rm=T), 
           stn="Met",
           column=6,
           t.seq=time.seq,
           directory=paste0("forcing/meteo/05_meteofill/out/", field.station,
                            "/"))

# soilheat (Soil Heat Flux, W.m-2, AVER)
echseInput(engine=engine,
           variable="soilheat",
           na.val=mean(get(paste0(field.station, ".list"))[[3]],
                       na.rm=T),  # not perfect
           stn=field.station,
           column=3,
           t.seq=time.seq,
           directory=paste0("forcing/meteo/05_meteofill/out/", field.station,
                            "/"))

# sundur (Sunshine Duration, h, CUMU)
if (A$sundur == 1) {
  sundur.df <- data.frame(end_of_interval=format(index(sundur.xts),
                                                 "%Y-%m-%d %H:%M:%S"),
                          sundur.xts)
  names(sundur.df)[2] <- "Met"
  write.matrix(sundur.df,
               paste0("~/uni/projects/", engine,
                      "/data/forcing/meteo/05_meteofill/out/", field.station,
                      "/sundur_data.dat"),
               sep="\t")
}

# temper (Mean Temperature over Time Interval, degC, AVER)
echseInput(engine=engine, 
           variable="temper", 
           na.val=mean(get(paste0(field.station, ".list"))[[2]],
                       na.rm=T),  # not perfect
           stn=field.station, 
           column=2, 
           t.seq=time.seq,
           directory=paste0("forcing/meteo/05_meteofill/out/", field.station,
                            "/"))

# utc_add (Deviation of Time Zone from UTC, h)
echseInput(engine=engine,
           variable="utc_add",
           stn="any",
           const=0,
           t.seq=time.seq,
           directory=paste0("vegPar_time_series/", field.station, "/"))

# wc_vol_root (Water Content in Root Zone, -, AVER)
echseInput(engine=engine,
           variable="wc_vol_root",
           na.val=wc_res,
           stn=field.station,
           column=4,
           t.seq=time.seq,
           directory=paste0("forcing/meteo/05_meteofill/out/", field.station,
                            "/"))

# wc_vol_top (Water Content in Topsoil, -, AVER)
echseInput(engine=engine,
           variable="wc_vol_top",
           na.val=wc_res,
           stn=field.station,
           column=4,
           t.seq=time.seq,
           directory=paste0("forcing/meteo/05_meteofill/out/", field.station,
                            "/"))

# wind (Wind Speed, m.s-1, AVER)
echseInput(engine=engine, 
           variable="wind", 
           na.val=mean(get(paste0(field.station, ".list"))[[11]],
                       na.rm=T),  # not perfect
           stn=field.station, 
           column=11, 
           t.seq=time.seq,
           directory=paste0("forcing/meteo/05_meteofill/out/", field.station,
                            "/"))

# calibration off.

# WRITE MODEL CONTROL FILES ----------------------------------------------------

echseCtrl_cnf(engine, tstart, tend, dt)
echseCtrl_out(engine, output, et.choice[1])
echseCtrl_loc(engine, selection$DAT, A, locs)
echseCtrl_fil(engine, field.station, selection$DAT, A, past=T)
echseCtrl_shpar(engine, sharedParamNum, selection$SHP)
echseCtrl_par(engine, paramNum, selection$PAR)
echseCtrl_ini(engine, selection$STV, selection$I)

# CALL MODEL RUN & POSTPROCESSING ----------------------------------------------

#debugonce(echsePost)
echsePost(engine, et.choice, ma.width=1, field.station=field.station, 
          comp=output, wc_etmax=wc_etmax)

# SHOW RESULTS -----------------------------------------------------------------

Sys.sleep(1.0)

dstart <- strsplit(tstart, " ")[[1]][1]
dend <- strsplit(tend, " ")[[1]][1]

if (output == "evap") {
  comp.file <- paste0("plot_et_compare_portugal_", field.station, "_", dstart,
                      "_", dend, ".pdf")
  cum.file <- paste0("plot_et_cum_portugal_", field.station, "_", dstart, "_",
                     dend, ".pdf")
} else if (output %in% c("glorad", "rad_net", "soilheat")) {
  comp.file <- paste0("plot_", output, "_compare_portugal_", field.station, "_",
                      dstart, "_", dend, ".pdf")
  cum.file <- paste0("plot_", output, "_cum_portugal_", field.station, "_",
                     dstart, "_", dend, ".pdf")
}

if (is.null(warnings())) {
  if (output == "evap") {
    system(paste0("cd ~/boxup/whk_echse; evince ", comp.file, " &"))
  } else if (output %in% c("glorad", "rad_net", "soilheat")) {
    system(paste0("cd ~/boxup/whk_echse; evince ", comp.file, " &"))
  }
}

# SAVE RESULTS -----------------------------------------------------------------

if (output == "evap") {
  if (et.choice[2] == 1) {
    method <- "makkink"
  } else if (et.choice[2] == 11) {
    method <- "penman"
  } else if (et.choice[2] == 12) {
    method <- "faoref"
  } else {
    method <- "shuttleworth"
  }
}

if (is.null(warnings())) {
  if (output == "evap") {
    # copy plot for comparison to archive directory
    system(paste0("cd ~/boxup/whk_echse; cp ", comp.file,
                  " results/evap_portugal/", et.choice[1], "_", method, "/"))
    # copy cumulative plot to archive dir
    system(paste0("cd ~/boxup/whk_echse; cp ", cum.file,
                  " results/evap_portugal/", et.choice[1], "_", method, "/"))
    # copy simulation results to archive dir
    system(paste0("cd ~/uni/projects/evap_portugal/run/out; cp test1.txt ",
                  field.station, "/test1.txt"))
  } else if (output %in% c("glorad", "rad_net", "soilheat")) {
    # copy plot for comparison to archive directory
    system(paste0("cd ~/boxup/whk_echse; cp ", comp.file, " results/", engine,
                  "/"))
    # copy cumulative plot to archive dir
    system(paste0("cd ~/boxup/whk_echse; cp ", cum.file, " results/", engine,
                  "/"))
    # copy simulation results to archive dir
    system(paste0("cd ~/uni/projects/", engine, "/run/out; cp test1.txt ",
                  field.station, "/test1.txt"))
  } else if (output %in% c("gloradmax", "radex")) {
    # copy simulation results to archive dir
    system(paste0("cd ~/uni/projects/", engine, "/run/out; cp test1.txt ",
                  field.station, "/test1.txt"))
  }
}

################################################################################
# Author: Julius Eberhard
# Last Edit: 2017-06-12
# Project: ECHSE Evapotranspiration
# Program: echse_portugal
# Aim: Data Preprocessing and Main Executing Script for ET in Portugal
#      (Herdade do Machuqueira do Grou)
################################################################################

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# This program - pre-processes data & writes them to ECHSE input files
#              - collects/calculates parameters for the ECHSE evapotranspiration
#                engines & writes them to ECHSE control files
#              - runs the models using the respective ECHSE engines
#              - creates plots and other output
#              - files the output.
# Required packages: MASS, RAtmosphere, soilwaterptf, TTR, xtable, xts
# Required scripts: echseCtrl.R, echseInput.R, echseParEst.R, echsePost.R,
#                   echsePre.R
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Note on parameter estimation:
# Some parameters are estimated using output from other ECHSE engines.
# Therefore: Before estimating f_day, f_night,   run output=="rad_net",
#            before estimating radex_a, radex_b, run output=="radex",
#            before estimating fcorr_a, fcorr_b, run output=="radex" AND
#                                                estimate radex_a, radex_b.
# Once the engines rad_net_portugal and radex_portugal have been run for both
# field stations (HS, NSA), ET can be calculated properly for both stations.
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

rm(list=ls())
assign("last.warning", NULL, envir=baseenv())

# set time zone to UTC
Sys.setenv(TZ="UTC")


# PROGRAM PARAMETERS :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# choose output [evap, glorad, gloradmax, rad_net, radex, soilheat]
output <- "glorad"
# choose field station [HS, NSA]
fs <- "HS"
# potential [etp] or actual [eta] evapotranspiration
et.choice <- "eta"
# ET model [1=Makk, 11=PM, 12=FAO, 13=SW]
et.choice[2] <- 13
# Use the newly calculated soil moisture data?
wc.new <- TRUE
# choose method for net emissivity [Brunt, Idso, both]
emismeth <- "Brunt"

# model period: start & end of model period, note the format!
if (output %in% c("gloradmax", "rad_net", "radex")) {
  tstart <- "2014-04-29 22:00:00"
  tend <- "2014-07-01 05:00:00"
} else if (fs == "HS") {
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

# For glorad, rad_net & radex, we need many data, so no.na is unchecked.
if (output %in% c("gloradmax", "rad_net", "radex"))
  no.na <- FALSE

# time step in seconds
if (output == "evap") {
  dt <- 3600
} else if (output == "glorad") {
  dt <- 86400
} else if (output == "gloradmax") {
  dt <- 3600
} else if (output == "rad_net") {
  dt <- 3600
} else if (output == "radex") {
  dt <- 3600
} else if (output == "soilheat") {
  dt <- 3600
} else {
  stop(paste0("No valid output specified, output=", output))
}

# declare availability of input data
# [0 = not given as input to ECHSE, 1 = input given to ECHSE]
A <- list(alb=1,                             # albedo
          apress=1,                          # air pressure
          cano_height=1,                     # canopy height
          cloud=0,                           # cloudiness, not used
          doy=1,                             # day of year
          glorad=1,                          # global radiation
          glorad_max=0,                      # clear-sky global radiation
          hour=1,                            # hour of day
          lai=1,                             # leaf area index
          rad_long=1,                        # net long-wave radiation
          rad_net=1,                         # net total radiation
          rad_net_soil=0,                    # net radiation at soil surface
          radex=0,                           # extraterr. radiation
          rhum=1,                            # rel. humidity
          soilheat=1,                        # soil heat flux
          sundur=ifelse(dt == 86400, 1, 0),  # sundur only given when dt = 1 day
          temp_max=0,                        # maximum air temperature within dt
          temp_min=0,                        # minimum air temperature within dt
          temper=1,                          # mean air temperature within dt
          totalheat=0,                       # total heat flux
          utc_add=1,                         # time shift from UTC
          wc_vol_root=1,                     # water content at root depth
          wc_vol_top=1,                      # water content at topsoil
          wind=1)                            # wind speed

# locations [fs, tower, any]
locs <- list(fs,  # alb
             "tower",  # apress
             fs,  # cano_height
             "any",  # cloud, not used
             "any",  # doy
             fs,  # glorad
             "any",  # glorad_max
             "any",  # hour
             fs,  # lai
             fs,  # rad_long
             fs,  # rad_net
             "any",  # rad_net_soil
             "any",  # radex
             "tower",  # rhum
             fs,  # soilheat
             "tower",  # sundur
             "any",  # temp_max
             "any",  # temp_min
             fs,  # temper
             "any",  # totalheat
             "any",  # utc_add
             fs,  # wc_vol_root
             fs,  # wc_vol_top
             fs)  # wind


# PACKAGES & DATA ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# set working directory
#if (system("hostname", intern=TRUE) == "raspberrypi") {
#  setwd("/home/pi/whk_echse")
#} else {
#  setwd("/home/julius/boxup/whk_echse")
#}

# load packages
library(MASS)  # write.matrix()
library(RAtmosphere)  # suncalc() in echseParEst() for f_day, f_night
if (system("hostname", intern=TRUE) != "raspberrypi")
  library(soilwaterptf)  # pedotransfer functions
library(TTR)  # runMean() in echsePost()
library(xtable)  # latex tables
library(xts)  # time series handling

# load data
# meteo data
HS <- get(load("data/portugal/meteo_HS.Rdata"))
NSA <- get(load("data/portugal/meteo_NSA.Rdata"))
tower <- na.omit(read.csv("data/portugal/meteo_tower_2014part.csv",
                          header=TRUE))  # eddy tower, meteo data
eddy <- get(load("data/portugal/COR_ZM.RDA"))  # eddy tower, eddy data
rld <- readRDS("data/portugal/Ldown")    # field station
rlu <- readRDS("data/portugal/Lup")      # ditto
rsd <- readRDS("data/portugal/Kdown")    # ditto
rsu <- readRDS("data/portugal/Kup")      # ditto
index(rld) <- index(rld) + 3600
index(rlu) <- index(rlu) + 3600
index(rsd) <- index(rsd) + 3600
index(rsu) <- index(rsu) + 3600
# soil data at field stations
soildata <- read.table("data/portugal/soildata.dat", header=T)
# water content at field stations
wc.HS <- read.table("data/portugal/soil_moisture_HS.txt", header=T, sep="\t")
wc.NSA <- read.table("data/portugal/soil_moisture_NSA.txt", header=T, sep="\t")


# FUNCTIONS ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# load scripts
if (system("hostname", intern=TRUE) == "raspberrypi")
  source("~/R/functions/jufun.R")  # ptf, if soilwaterptf package not available
source("echseCtrl.R")  # control files
source("echseInput.R")  # data input files
source("echseParEst.R")  # parameter estimation
source("echsePost.R")  # postprocessing
source("echsePre.R")  # preprocessing

TimeSeq <- function(data,  # list of xts objects
                    dt,  # time interval, as character (see ?seq.Date, Details)
                    at.full = NA  # start at full ... [mins, hours, days, ...]
                    ) {
  # creates time sequence of specified constant interval dt
  # covering the whole period of data

  mi <- min(index(data), na.rm=TRUE)
  ma <- max(index(data), na.rm=TRUE)

  if (!is.na(at.full)) {
    # select index of first occurence of full ... (first full hour, e.g.)
    ep <- endpoints(data, on=at.full) + 1  # endpoint selects last of ...
    mi <- index(data)[ep[2]]  # ep[1] = 1 by default, ep[2] is first full ...
  }

  return(seq(mi, ma, dt))
}

RadPrep <- function(data  # xts object (radiation component)
                    ) {
  # prepares radiation data for estimating parameters and writing input files

  # remove duplicates
  data <- data[!duplicated(index(data))]
  # fill up missing dates
  seq.10min <- TimeSeq(data, "10 min", "hours")
  data <- merge(xts(order.by=seq.10min), data)
  # return hourly means
  seq.1h <- TimeSeq(data, "1 hour", "hours")
  return(xts(period.apply(data, seq.int(1, length(seq.10min), by=6), mean),
             order.by=seq.1h[-length(seq.1h)]))
}


# DATA PREPROCESSING :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# check for correct program parameters
if (!(fs %in% c("HS", "NSA")))
  stop(paste0("No valid field station specified. fs=", fs))
if (!(et.choice[1] %in% c("etp", "eta")))
  stop(paste0("No valid ET choice specified. et.choice[1]=", et.choice[1]))
if (!(et.choice[2] %in% c(1, 11, 12, 13)))
  stop(paste0("No valid ET model specified. et.choice[2]=", et.choice[2]))
if (!(emismeth %in% c("Brunt", "Idso", "both")))
  stop(paste0("No valid emissivity model specified. emismeth=", emismeth))

# some basic variables
engine <- paste(output, "portugal", sep="_")  # engine name: output_location
if (et.choice[2] == 1) {
  etmeth <- "makkink"
} else if (et.choice[2] == 11) {
  etmeth <- "penman"
} else if (et.choice[2] == 12) {
  etmeth <- "faoref"
} else if (et.choice[2] == 13) {
  etmeth <- "shuttleworth"
}

# ECHSE data & variable & parameter selection for writing control files
selection <- echseSelect(output)

# labels for plotting input data
HS.names <- c((Net~Radiation~(W~m^{-2})),
              (Temperature~(degree~C)),
              (Soil~heat~flux~(W~m^{-2})),
              (Soil~moisture~content~(-{})),
              (Sensible~heat~flux~(W~m^{-2})),
              (Latent~heat~flux~(W~m^{-2})),
              (Bowen~ratio~(-{})),
              (Evaporation~(mm~h^{-1})),
              (Vapor~pressure~deficit~(kPa)),
              (Vapor~pressure~(kPa)),
              (Wind~speed~(m~s^{-1})),
              (LW~downward~radiation~(W~m^{-2})),
              (LW~upward~radiation~(W~m^{-2})),
              (SW~downward~radiation~(W~m^{-2})),
              (SW~upward~radiation~(W~m^{-2})),
              (LW~net~radiation~(W~m^{-2})))
NSA.names <- HS.names
tower.names <- c("PARgl (unit?)",
                 "PARdif (unit?)",
                 (Incoming~radiation~SW~(W~m^{-2})),
                 (Outgoing~radiation~LW~(W~m^{-2})),
                 (Temperature~(degree~C)),
                 (Relative~humidity~("%")),
                 (Rainfall~(mm~d^{-1})),
                 (Barometric~pressure~(hPa)))

# FIELD STATION DATA -----------------------------------------------------------

# prepare radiation data
rld.hly <- RadPrep(rld)
rlu.hly <- RadPrep(rlu)
rsd.hly <- RadPrep(rsd)
rsu.hly <- RadPrep(rsu)

# merge meteo data at field stations, fill up missing dates
HS <- merge(HS, rld.hly, rlu.hly, rsd.hly, rsu.hly, rld.hly - rlu.hly,
            tzone="UTC")
HS <- merge(xts(order.by=seq(min(index(HS)), max(index(HS)), by="hours")), HS)
NSA <- merge(NSA, rld.hly, rlu.hly, rsd.hly, rsu.hly, rld.hly - rlu.hly,
            tzone="UTC")
NSA <- merge(xts(order.by=seq(min(index(NSA)), max(index(NSA)), by="hours")),
             NSA)

# convert xts to list objects (simpler access to data later on)
HS <- as.list(HS)
NSA <- as.list(NSA)
names(HS) <- c(head(names(HS), length(HS) - 5),
               "RLdown", "RLup", "RSdown", "RSup", "RLnet")
names(NSA) <- c(head(names(NSA), length(NSA) - 5),
                "RLdown", "RLup", "RSdown", "RSup", "RLnet")

# create mask of NA intervals
if (fs == "HS") {
  na.mask <- sort(unique(do.call(c, lapply(HS[c("R", "T", "G", "theta", "u",
                                                "RLdown", "RLup", "RSdown",
                                                "RSup")],
                                           function(x)
                                             index(HS[[1]])[is.na(x)]))))
} else {
  na.mask <- sort(unique(do.call(c, lapply(NSA[c("R", "T", "G", "theta", "u",
                                                 "RLdown", "RLup", "RSdown",
                                                 "RSup")],
                                           function(x)
                                             index(NSA[[1]])[is.na(x)]))))
}

# check no.na criterion
na.present <- any(!is.na(match(as.character(seq(as.POSIXct(tstart), 
                                                as.POSIXct(tend), 
                                                by=paste(dt, "sec"))),
                               as.character(na.mask))))
if (no.na && na.present)
  stop("Model period contains missing data and no.na == TRUE.")

# soil moisture data: fill up missing dates (15 min sequence)
wc.HS <- xts(wc.HS$sm, order.by=as.POSIXct(wc.HS$date))
wc.HS <- merge(wc.HS, xts(order.by=TimeSeq(wc.HS, "15 min", "hours")))
wc.NSA <- xts(wc.NSA$sm, order.by=as.POSIXct(wc.NSA$date))
wc.NSA <- merge(wc.NSA, xts(order.by=TimeSeq(wc.NSA, "15 min", "hours")))
# take hourly data
ep.HS <- endpoints(wc.HS, on="hours") + 1
wc.HS <- xts(period.apply(wc.HS, ep.HS[-length(ep.HS)], mean),
             order.by=index(wc.HS)[ep.HS[-c(1, length(ep.HS))]])
ep.NSA <- endpoints(wc.NSA, on="hours") + 1
wc.NSA <- xts(period.apply(wc.NSA, ep.NSA[-length(ep.NSA)], mean),
              order.by=index(wc.NSA)[ep.NSA[-c(1, length(ep.NSA))]])
if (!wc.new) {
  # Set soil moisture to zero where negative ...
  HS[[4]][HS[[4]] < 0] <- 0
  NSA[[4]][NSA[[4]] < 0] <- 0
} else {
  # ... or use new data instead.
  datrg.HS <- range(index(HS[[4]]))
  datrg.NSA <- range(index(NSA[[4]]))
  HS[[4]] <- wc.HS[index(wc.HS) >= datrg.HS[1] & index(wc.HS) <= datrg.HS[2]]
  NSA[[4]] <- wc.NSA[index(wc.NSA) >= datrg.NSA[1] &
                     index(wc.NSA) <= datrg.NSA[2]]
}

# TOWER DATA: METEO ------------------------------------------------------------

tower$date <- NA  # date (Y-m-d)
class(tower$date) <- "Date"

# hhmm==0 is understood as 24:00 in file but as 0:00 here;
# therefore increase doy by 1
# i.e. "doy=320, hhmm=0" becomes "doy=321, hhmm=0"
tower$doy[which(tower$hhmm == 0)] <- tower$doy[which(tower$hhmm == 0)] + 1

# convert dates into POSIX-readable dates
years <- as.numeric(unique(c(tower$year, format(index(HS$R), "%Y"))))
for (y in years) {
  tower$date[tower$year == y] <- as.Date(tower$doy[tower$year == y],
                                         origin=paste0(y - 1, "-12-31"))
}
hours <- sprintf("%02d", floor(tower$hhmm / 100))
minutes <- sprintf("%02d", tower$hhmm - (floor(tower$hhmm / 100) * 100))
seconds <- sprintf("%02d", rep(0, length(tower$hhmm)))
tower$dati <- paste0(as.character(tower$date), " ", hours, ":", minutes, ":",
                     seconds)

# select relevant columns, convert to xts
tower <- tower[, -c(1:5, ncol(tower) - 1:3)]
tower <- xts(tower[2:nrow(tower), -ncol(tower)],
             order.by=as.POSIXct(tower$dati[2:nrow(tower)]), tzone="UTC")

# fill up missing dates
tower <- merge(xts(order.by=TimeSeq(tower, "30 min")), tower, tzone="UTC")

# simulation time sequence
timeseq <- echseTimeSeq(index(tower), tstart, dt)

# derive sunshine hours according to WMO, 2003 (time for which rad > 120 W.m-2)
if (dt == 86400) {
  rad <- tower[index(tower) >= timeseq[1] & index(tower) <= tail(timeseq, 1), 3]
  # add up to multiple of 48 length
  length(rad) <- length(rad) + 48 - length(rad) %% 48
  # matrix with days (rows) and 48 half hours (columns)
  rad.mat <- matrix(rad, ncol=48, byrow=TRUE)
  sundur <- c()
  days <- 1:nrow(rad.mat)
  for (i in days)
    sundur[i] <- length(rad.mat[i, na.omit(rad.mat[i, ]) > 120]) / 2
  sundur <- xts(sundur, order.by=TimeSeq(tower[timeseq], paste(dt, "sec")))
}

# convert xts to list object (simpler access to data later on)
tower <- as.list(tower)

# TOWER DATA: EDDY -------------------------------------------------------------

eddy <- xts(eddy[, -(1:3)], order.by=as.POSIXct(paste(eddy$date, eddy$time)))
eddy <- eddy[!duplicated(index(eddy))]
# take latent heat flux with Foken/Mauder quality flag 0, 1 (= quality ok)
eddy.le <- eddy$LE[eddy$qc_LE == 0 | eddy$qc_LE == 1]
eddy.le <- eddy.le[!duplicated(index(eddy.le))]
# fill up missing dates
eddy <- merge(xts(order.by=TimeSeq(eddy, "30 min", "hours")), eddy)
eddy.le <- merge(xts(order.by=TimeSeq(eddy, "30 min", "hours")), eddy.le)


# ENGINE PARAMETERS ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# INDIVIDUAL PARAMETERS --------------------------------------------------------

# geographical parameters: Herdade do Machuqueira do Grou, Santarem, Portugal
lat <- 39.14
lon <- 8.33
elev <- 160  # in m

# soil parameters, soil type: loamy sand (FAO)
clay <- soildata[1, "clay"]
silt <- soildata[1, "silt"]
som <- soildata[1, "som"]  # soil organic matter
soil_dens <- 1500  # bulk density, estimated from different sources
soilprop <- data.frame(clay=clay, bulkD=soil_dens / 1000, silt=silt, om=som)
wc_sat <- ptf.wosten.theta.s(clay=clay, bulkD=soil_dens / 1000, silt=silt,
                             om=som, topSoil=1)
wc_pwp <- pft.rawls(soilprop, parameters="theta", h=15000)[, "theta"]  # wilting
wc_res <- pft.rawls(soilprop, parameters="theta_r", h=0)[, "theta_r"]
# field capacity for estimation of wc_etmax
wc_fc <- pft.rawls(soilprop, parameters="theta", h=316)[, "theta"]
# wc_etmax is a calibration parameter! This is only a rough estimation:
wc_etmax <- 0.8 * wc_fc  # used only for Makkink & FAO
# van Genuchten parameters
bubble <- pft.rawls(soilprop, parameters="h_b", h=0)[, "h_b"]  # h_b
pores_ind <- pft.rawls(soilprop, parameters="lambda", h=0)[, "lambda"]  # lambda

# check if measured water content undercuts parameter value
if (any(get(fs)[["theta"]] < wc_res, na.rm=TRUE))
  wc_res <- min(get(fs)[["theta"]], na.rm=TRUE)

# plant parameters
crop_faoref <- 1
crop_makk <- 0.8
glo_half <- 200  # global radiation at half-max stomatal *conductance*
par_stressHum <- 0.03
res_leaf_min <- 50
wstressmax <- ifelse(fs == "HS", 13100, 14000)  # Gazdar, 2016
wstressmin <- 100

# collect individual parameters
paramNum <- list(bubble=bubble, crop_faoref=crop_faoref, crop_makk=crop_makk,
                 elev=elev, glo_half=glo_half, lat=lat, lon=lon,
                 par_stressHum=par_stressHum, pores_ind=pores_ind,
                 res_leaf_min=res_leaf_min, soil_dens=soil_dens,
                 wc_etmax=wc_etmax, wc_pwp=wc_pwp, wc_res=wc_res,
                 wc_sat=wc_sat, wstressmax=wstressmax, wstressmin=wstressmin)

# SHARED PARAMETERS ------------------------------------------------------------

choice_gloradmax <- 1
choice_plantDispl <- 1
choice_rcs <- 1
choice_roughLen <- 2
drag_coef <- 0.07
eddy_decay <- 2.5
emis_a <- 0.34
emis_b <- -0.14
ext <- 0.4
f_day <- 0.1
f_night <- 0.7
fcorr_a <- 1.35
fcorr_b <- -0.35
h_humMeas <- 2  # ifelse(fs=="NSA", 7.98, 2)
h_tempMeas <- 2  # ifelse(fs=="NSA", 7.98, 2)
h_windMeas <- 2  # ifelse(fs=="NSA", 7.98, 2)
na_val <- "-9999."
radex_a <- 0.25
radex_b <- 0.5
res_b <- 25
rough_bare <- 0.01
rss_a <- 37.5  # calibration...
rss_b <- -1.23  # calibration...

path.meteo <- paste0("~/uni/projects/evap_portugal/data/forcing/",
                     "meteo/05_meteofill/out/", fs, "/")
path.proj <- paste0("~/uni/projects/")

# estimate f_day & f_night from soil heat flux and net radiation
# ... Remember to run the rad_net_* engine first!
if (output != "rad_net") {
  f.out <- echseParEst("f",
                       rnetfile=paste0(path.proj, "rad_net_portugal/run/out/",
                                       fs, "/test1.txt"),
                       sheatfile=paste0(path.meteo, "soilheat_data.dat"),
                       lat=lat, lon=lon, plots=FALSE)
  f_day <- f.out[1]
  f_night <- f.out[2]
}

# estimate radex_a, radex_b from global radiation and extraterr. radiation
# ... Remember to run the radex_* engine first!
if (output != "radex") {
  radex.out <- echseParEst("radex",
                           rxfile=paste0(path.proj, "radex_portugal/run/out/",
                                         fs, "/test1.txt"),
                           rsdfile="data/portugal/Kdown",
                           r.quantile=0.05, plots=FALSE)
  radex_a <- radex.out[1]
  radex_b <- radex.out[2]
}

# compare emissivity models (Brunt vs. Idso-Jackson)
# no estimation, function returns suggested values from Maidment (1993)
emis.out <- echseParEst("emis",
                        rsdfile="data/portugal/Kdown",
                        rxfile=paste0(path.proj, "radex_portugal/run/out/",
                                      fs, "/test1.txt"),
                        rldfile="data/portugal/Ldown",
                        rlufile="data/portugal/Lup",
                        tafile=paste0(path.meteo, "temper_data.dat"),
                        hrfile=paste0(path.meteo, "rhum_data.dat"),
                        fs=fs,
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
                                       fs, "/test1.txt"),
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

# EXTERNAL INPUT PARAMETERS (alb, cano_height, lai) ----------------------------

alb <- echseParEst("alb", rsdfile="data/portugal/Kdown",
                   rsufile="data/portugal/Kup", plots=TRUE)
cano_height <- 0.20  # ifelse(fs=="NSA", 7.98, 0.20)
lai <- 0.778  # ifelse(fs == "NSA", 1.397, 0.778)


# WRITE LATEX FILES ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# The following section creates LaTeX files containing parameter tables
# to be included in the Documentation using \input{filename}.
# Set first condition to FALSE if not needed.

if (TRUE) {
# paramNum
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
                                      "(\\", "textsf{paramNum}), ", fs,
                                      " Portugal"),
                       label=paste0("tab:portugal", fs, "_paramNum"))
print.xtable(paramNum.tex,
             file=paste0("doku/portugal", fs, "_paramNum.tex"),
             include.rownames=FALSE, sanitize.text.function=identity,
             caption.placement="top")

# sharedParamNum
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
                                            fs, " Portugal"),
                             label=paste0("tab:portugal", fs,
                                          "_sharedParamNum"))
print.xtable(sharedParamNum.tex,
             file=paste0("doku/portugal", fs, "_sharedParamNum.tex"),
             include.rownames=FALSE, sanitize.text.function=identity,
             caption.placement="top")

# inputExt
inputExt.df <- data.frame(Parameter=c(paste0("\\", "verb!alb!"),
                                      paste0("\\", "verb!cano_height!"),
                                      paste0("\\", "verb!lai!")),
                          Value=c(alb, cano_height, lai),
                          Unit=c("--", "m", "--"))
inputExt.tex <- xtable(inputExt.df, align=c("c", "l", "r", "l"),
                       caption=paste0("Time-dependent parameters ",
                                      "(\\", "textsf{inputExt}), ", fs,
                                      " Portugal"),
                       label=paste0("tab:portugal", fs, "_inputExt"))
print.xtable(inputExt.tex,
             file=paste0("doku/portugal", fs, "_inputExt.tex"),
             include.rownames=FALSE, sanitize.text.function=identity,
             caption.placement="top")
}


# WRITE DATA INPUT FILES :::::::::::::::::::::::::::::::::::::::::::::::::::::::

path.veg <- paste0("~/uni/projects/", engine, "/data/vegPar_time_series/",
                   fs, "/")

# alb (Albedo, -, average)
echseInput(engine=engine, 
           variable="alb",
           stn=fs,
           const=alb,
           t.seq=timeseq,
           directory=path.veg)

# apress (Air Pressure, hPa, average)
echseInput(engine=engine, 
           variable="apress", 
           na.val=mean(as.numeric(tower.list[[8]]), na.rm=T), 
           stn="tower", 
           column=8, 
           t.seq=timeseq,
           directory=path.meteo)

# cano_height (Canopy Height, m, average)
echseInput(engine=engine,
           variable="cano_height",
           stn=fs,
           const=cano_height,
           t.seq=timeseq,
           directory=path.veg)

# doy (Day of Year, d)
doy.df <- data.frame(end_of_interval=format(timeseq, "%Y-%m-%d %H:%M:%S"),
                     as.numeric(strftime(timeseq, "%j", tz="UTC")))
names(doy.df)[2] <- "any"
write.matrix(doy.df, paste0(path.veg, "doy_data.dat"), sep="\t")

# glorad (Global Radiation, W.m-2, average)
echseInput(engine=engine,
           variable="glorad",
           stn=fs,
           column=14,
           t.seq=timeseq,
           directory=path.meteo)

# hour (Hour of Day, h)
hour.df <- data.frame(end_of_interval=format(timeseq, "%Y-%m-%d %H:%M:%S"),
                      as.numeric(strftime(timeseq, "%H", tz="UTC")))
names(hour.df)[2] <- "any"
write.matrix(hour.df, paste0(path.veg, "hour_data.dat"), sep="\t")

# lai (Leaf Area Index, -, average)
echseInput(engine=engine,
           variable="lai",
           stn=fs,
           const=lai,
           t.seq=timeseq,
           directory=path.veg)

# rad_long (Net Incoming Long-Wave Radiation, W.m-2, average)
echseInput(engine=engine,
           variable="rad_long",
           na.val=0,
           stn=fs,
           column=16,
           t.seq=timeseq,
           directory=path.meteo)

# rad_net (Net Incoming Radiation, W.m-2, average)
echseInput(engine=engine,
           variable="rad_net",
           na.val=0,  # not perfect
           stn=fs,
           column=1,
           t.seq=timeseq,
           directory=path.meteo)

# rhum (Relative Humidity, %, average)
echseInput(engine=engine,
           variable="rhum",
           na.val=mean(as.numeric(tower.list[[6]]), na.rm=T), 
           stn="tower",
           column=6,
           t.seq=timeseq,
           directory=path.meteo)

# soilheat (Soil Heat Flux, W.m-2, average)
echseInput(engine=engine,
           variable="soilheat",
           na.val=mean(get(fs)[[3]], na.rm=T),  # not perfect
           stn=fs,
           column=3,
           t.seq=timeseq,
           directory=path.meteo)

# sundur (Sunshine Duration, h, cumulative)
if (A$sundur == 1) {
  sundur.df <- data.frame(end_of_interval=format(index(sundur.xts),
                                                 "%Y-%m-%d %H:%M:%S"),
                          sundur.xts)
  names(sundur.df)[2] <- "tower"
  write.matrix(sundur.df, paste0(path.meteo, "sundur_data.dat"), sep="\t")
}

# temper (Mean Temperature over Time Interval, degC, average)
echseInput(engine=engine,
           variable="temper",
           na.val=mean(get(paste0(fs, ".list"))[[2]], na.rm=T),  # not perfect
           stn=fs,
           column=2,
           t.seq=timeseq,
           directory=path.meteo)

# utc_add (Deviation of Time Zone from UTC, h)
echseInput(engine=engine,
           variable="utc_add",
           stn="any",
           const=0,
           t.seq=timeseq,
           directory=path.veg)

# wc_vol_root (Water Content in Root Zone, -, average)
echseInput(engine=engine,
           variable="wc_vol_root",
           na.val=wc_res,
           stn=fs,
           column=4,
           t.seq=timeseq,
           directory=path.meteo)

# wc_vol_top (Water Content in Topsoil, -, average)
echseInput(engine=engine,
           variable="wc_vol_top",
           na.val=wc_res,
           stn=fs,
           column=4,
           t.seq=timeseq,
           directory=path.meteo)

# wind (Wind Speed, m.s-1, average)
echseInput(engine=engine,
           variable="wind",
           na.val=mean(get(paste0(fs, ".list"))[[11]], na.rm=T),  # not perfect
           stn=fs,
           column=11,
           t.seq=timeseq,
           directory=path.meteo)


# WRITE MODEL CONTROL FILES ::::::::::::::::::::::::::::::::::::::::::::::::::::

echseCtrl_cnf(engine, tstart, tend, dt)
echseCtrl_out(engine, output, et.choice[1])
echseCtrl_loc(engine, selection$DAT, A, locs)
echseCtrl_fil(engine, fs, selection$DAT, A, past=TRUE)
echseCtrl_shpar(engine, sharedParamNum, selection$SHP)
echseCtrl_par(engine, paramNum, selection$PAR)
echseCtrl_ini(engine, selection$STV, selection$I)


# CALL MODEL RUN & POSTPROCESSING ::::::::::::::::::::::::::::::::::::::::::::::

echsePost(engine, et.choice, ma.width=1, fs=fs, comp=output, wc_res=wc_res,
          wc_sat=wc_sat)


# SHOW RESULTS :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

Sys.sleep(1.0)

dstart <- strsplit(tstart, " ")[[1]][1]
dend <- strsplit(tend, " ")[[1]][1]

if (output %in% c("evap", "glorad", "rad_net", "soilheat")) {
  comp.file <- paste0("plot_", output, "_compare_portugal_", fs, "_",
                      dstart, "_", dend, ".pdf")
  cum.file <- paste0("plot_", output, "_cum_portugal_", fs, "_",
                     dstart, "_", dend, ".pdf")
}

# open pdf for comparison between simulation and observation
if (is.null(warnings())) {
  system(paste0("cd ~/boxup/whk_echse; evince ", comp.file, " &"))
}


# SAVE RESULTS :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

if (is.null(warnings())) {
  if (output == "evap") {
    # move plot for comparison to archive directory
    system(paste0("cd ~/boxup/whk_echse; mv ", comp.file,
                  " results/evap_portugal/", et.choice[1], "_", etmeth, "/"))
    # move cumulative plot to archive dir
    system(paste0("cd ~/boxup/whk_echse; mv ", cum.file,
                  " results/evap_portugal/", et.choice[1], "_", etmeth, "/"))
    # copy simulation results to archive dir
    system(paste0("cd ~/uni/projects/evap_portugal/run/out; cp test1.txt ",
                  fs, "/test1.txt"))
  } else if (output %in% c("glorad", "rad_net", "soilheat")) {
    # move plot for comparison to archive directory
    system(paste0("cd ~/boxup/whk_echse; mv ", comp.file, " results/", engine,
                  "/"))
    # move cumulative plot to archive dir
    system(paste0("cd ~/boxup/whk_echse; mv ", cum.file, " results/", engine,
                  "/"))
    # copy simulation results to archive dir
    system(paste0("cd ~/uni/projects/", engine, "/run/out; cp test1.txt ",
                  fs, "/test1.txt"))
  } else if (output %in% c("gloradmax", "radex")) {
    # copy simulation results to archive dir
    system(paste0("cd ~/uni/projects/", engine, "/run/out; cp test1.txt ",
                  fs, "/test1.txt"))
  }
}

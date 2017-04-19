################################################################################
# Author: Julius Eberhard
# Last Edit: 2016-10-24
# Project: ECHSE Evapotranspiration
# Program: echse_morocco
# Aim: Data Preprocessing and Main Executing Script for ET in Morocco
################################################################################

rm(list=ls())
assign("last.warning", NULL, envir=baseenv())

# set time zone to UTC
Sys.setenv(TZ="UTC")

# PROGRAM PARAMETERS ------------------------------------------------------------

output <- "evap"  # output quantity [evap, glorad, gloradmax, rad_net, radex]
field.station <- "Met"  # [Met]
et.choice <- "etp"  # Potential or actual evapotranspiration? [etp, eta]
et.choice[2] <- 13  # model for calculation [1=Makk, 11=PM, 12=FAO, 13=SW]
tstart <- "2013-06-15 00:00:00"  # start of model period, note the format!
tend <- "2013-09-20 15:00:00"  # end of model period, note the format!

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
          rad_long=0,
          rad_net=0,
          rad_net_soil=0,
          radex=0,
          rhum=1,
          soilheat=0,
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
             "Met",  # glorad
             "any",  # glorad_max
             "any",  # hour
             field.station,  # lai
             "any",  # rad_long
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
  setwd("/home/pi/ownCloud/whk_echse")
} else {
  setwd("/home/julius/boxup/whk_echse")
}

# load scripts
source("~/R/jufun.R")  # CheckPack()
source("echseCtrl.R")  # control files
source("echseInput.R")  # data input files
source("echseParEst.R")  # parameter estimation
source("echsePost.R")  # postprocessing
source("echsePre.R")  # preprocessing

# load packages
CheckPack(c("MASS", "RAtmosphere", "soilwaterptf", "TTR", "xtable", "xts"))
library(MASS)  # write.matrix()
library(soilwaterptf)
library(xtable)  # latex tables
library(xts)  # time series handling

# load data
# latent heat & actual evapotranspiration
le.eta.day <- read.csv("data/morocco/latent_heat_daily.csv")
le.30m <- read.csv("data/morocco/latent_heat_halfhourly.csv")
# meteo station: rel hum, global rad, air temp, wind
meteo <- read.csv("data/morocco/ET0.csv")
soildata <- read.table("data/morocco/soildata.dat", header=T)

# PREPROCESSING ----------------------------------------------------------------

engine <- paste(output, "morocco", sep="_")  # engine name: output_location

# time step in seconds
if (output == "evap") {
  dt <- 3600
} else if (output == "glorad") {
  dt <- 86400
  A$sundur <- 1
} else if (output == "gloradmax") {
  dt <- 86400
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
meteo.list <- NULL
meteo.names <- c(expression(Relative~humidity~("%")),
                 expression(Global~radiation~(W~m^{-2})),
                 expression(Air~temperature(degree~C)),
                 expression(Wind~(m~s^{-1})))

# LATENT HEAT & ACTUAL ET DATA

le.day.xts <- xts(le.eta.day[, 2],
                  order.by=as.POSIXct(le.eta.day[, 1], format="%m/%d/%Y"),
                  tzone="UTC")
eta.day.xts <- xts(le.eta.day[, 3],
                   order.by=as.POSIXct(le.eta.day[, 1], format="%m/%d/%Y"),
                   tzone="UTC")
le.30m.dati <- paste(as.character(le.30m[, 1]),
                     as.character(le.30m[, 2]))
le.30m.xts <- xts(le.30m[, 3],
                  order.by=as.POSIXct(le.30m.dati, format="%m/%d/%Y %H:%M:%S"),
                  tzone="UTC")

# fill up missing dates
le.30m.xts <- merge(xts(order.by=seq(index(le.30m.xts)[1], 
                                     tail(index(le.30m.xts), 1), by="hours")),
                    le.30m.xts, tzone="UTC")

# METEO STATION DATA

meteo.dati <- paste(as.character(meteo[, 1]), as.character(meteo[, 2]))
meteo.xts <- xts(meteo[, 2:ncol(meteo)],
                 order.by=as.POSIXct(meteo.dati, format="%m/%d/%Y %H:%M"),
                 tzone="UTC")

# fill up missing dates
suppressWarnings(
  meteo.xts <- merge(xts(order.by=seq(index(meteo.xts)[1],
                                      tail(index(meteo.xts), 1),
                                      by="30 min")),
                     meteo.xts, tzone="UTC")
)

# time sequence
time.seq.min <- seq(max(c(index(meteo.xts)[1], index(le.30m.xts)[1])),
                    min(c(tail(index(meteo.xts), 1),
                          tail(index(le.30m.xts), 1))),
                    by="30 min")
time.seq <- echseTimeSeq(time.seq.min, tstart, dt)

# split data into list elements for further processing
for (i in c(2, 3, 5, 6))
  meteo.list <- c(meteo.list,
                  list(xts(order.by=index(meteo.xts), meteo.xts[, i])))

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

# geographical parameters: Agafay Desert, Haouz Plain, Morocco
lat <- 31.50  # Mroos 2014, p. 29
lon <- 8.14 # source: ditto, in decimal degrees, westward +
elev <- 464  # ditto, in m

# soil parameters, soil type: ranging from sandy loam to clay loam (FAO)
# take means of all samples
clay <- soildata[, "clay"]
silt <- soildata[, "silt"]
som <- rep(0, nrow(soildata))
som[soildata[, "depth_upper"] < 30] <- 2  # estimated for semidesert conditions
soil_dens <- 1.5  # bulk density, estimated from different sources
soilprop <- data.frame(clay=clay, bulkD=soil_dens, silt=silt, om=som)
wc_sat <- mean(ptf.wosten.theta.s(clay=clay, bulkD=soil_dens, silt=silt, 
                                  om=som, topSoil=1))
wc_pwp <- mean(pft.rawls(soilprop, parameters="theta", h=15000)[, "theta"])
wc_res <- mean(pft.rawls(soilprop, parameters="theta_r", h=0)[, "theta_r"])
# field capacity for estimation of wc_etmax
wc_fc <- mean(pft.rawls(soilprop, parameters="theta", h=316)[, "theta"])
# wc_etmax is a calibration parameter! this is only a rough estimation:
wc_etmax <- .8 * wc_fc
bubble <- mean(pft.rawls(soilprop, parameters="h_b", h=0)[, "h_b"])
pores_ind <- mean(pft.rawls(soilprop, parameters="lambda", h=0)[, "lambda"])

# plant parameters
crop_faoref <- 1
crop_makk <- .8
glo_half <- 200
par_stressHum <- .03
res_leaf_min <- 50
wstressmax <- 10000
wstressmin <- 100

# collect individual parameters
paramNum <- list(bubble=bubble, crop_faoref=crop_faoref, crop_makk=crop_makk,
                 elev=elev, glo_half=glo_half, lat=lat, lon=lon,
                 par_stressHum=par_stressHum, pores_ind=pores_ind,
                 res_leaf_min=res_leaf_min, soil_dens=soil_dens,
                 wc_etmax=wc_etmax, wc_pwp=wc_pwp, wc_res=wc_res,
                 wc_sat=wc_sat, wstressmax=wstressmax, wstressmin=wstressmin)

# write parameter list to latex document
names.pN <- sapply(names(paramNum),
                   function(name)
                     paste0("\\", "verb!", name, "!"))
comm.pN <- c(paste0("PTF by \\", "citet{rawls85}"),
             "evaporation of reference crop",
             paste0("Eq. \\", "ref{eq:cropmakk}"),
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
                                      "(\\", "textsf{paramNum}), Morocco"),
                       label=paste0("tab:morocco", field.station, "_paramNum"))
print.xtable(paramNum.tex,
             file=paste0("doku/morocco", field.station, "_paramNum.tex"),
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
ext <- .5
f_day <- .2
f_night <- .7
fcorr_a <- 1.35
fcorr_b <- -0.35
h_humMeas <- 2
h_tempMeas <- 2
h_windMeas <- 2
na_val <- "-9999."
radex_a <- .25
radex_b <- .5
res_b <- 25
rough_bare <- .01
rss_a <- 37.5  # calibration...
rss_b <- -1.23  # calibration...

# derive radex parameters from global radiation and extraterr. radiation
# ... Remember to run the radex_* engine first!
if (output != "radex") {
  radex.out <- echseParEst("radex",
                           rxfile=paste0("~/uni/projects/radex_morocco",
                                         "/run/out/test1.txt"),
                           grfile=paste0("~/uni/projects/evap_morocco",
                                         "/data/forcing/meteo/05_meteofill/out/",
                                         field.station, "/glorad_data.dat"),
                           r.quantile=0.05, plots=T)
  radex_a <- radex.out[1]
  radex_b <- radex.out[2]
}

if (fcorr_a + fcorr_b != 1)
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
                                            "Morocco"),
                             label=paste0("tab:morocco", field.station,
                                          "_sharedParamNum"))
print.xtable(sharedParamNum.tex,
             file=paste0("doku/morocco", field.station, "_sharedParamNum.tex"),
             include.rownames=F, sanitize.text.function=identity,
             caption.placement="top")

# EXTERNAL INPUT PARAMETERS (alb, cano_height, lai)

alb <- 0.3
cano_height <- 1.2
lai <- 0.5

inputExt.df <- data.frame(Parameter=c(paste0("\\", "verb!alb!"),
                                      paste0("\\", "verb!cano_height!"),
                                      paste0("\\", "verb!lai!")),
                          Value=c(alb, cano_height, lai),
                          Unit=c("--", "m", "--"))
inputExt.tex <- xtable(inputExt.df, align=c("c", "l", "r", "l"),
                       caption=paste0("Time-dependent parameters ",
                                      "(\\", "textsf{inputExt}), ",
                                      field.station, " Morocco"),
                       label=paste0("tab:morocco", field.station,
                                    "_inputExt"))
print.xtable(inputExt.tex,
             file=paste0("doku/morocco", field.station, "_inputExt.tex"),
             include.rownames=F, sanitize.text.function=identity,
             caption.placement="top")

# WRITE DATA INPUT FILES -------------------------------------------------------

# alb (Albedo, -, AVER)
echseInput(engine=engine, 
           variable="alb", 
           stn="Met", 
           const=alb,
           t.seq=time.seq,
           directory=paste0("vegPar_time_series/", field.station, "/"))

# apress (Air Pressure, hPa, AVER)
# -missing-
echseInput(engine=engine, 
           variable="apress", 
           stn="Met", 
           const=1000,  # somewhat average
           t.seq=time.seq,
           directory=paste0("forcing/meteo/05_meteofill/out/", field.station,
                            "/"))

# cano_height (Canopy Height, m, AVER)
echseInput(engine=engine, 
           variable="cano_height", 
           stn="Met", 
           const=1.2, 
           t.seq=time.seq,
           directory=paste0("vegPar_time_series/", field.station, "/"))

# doy (Day of Year, d)
doy.df <- data.frame(end_of_interval=format(time.seq, "%Y-%m-%d %H:%M:%S"),
                     as.numeric(strftime(time.seq, "%j")))
names(doy.df)[2] <- "any"
write.matrix(doy.df, 
             paste0("~/uni/projects/", engine, 
                    "/data/vegPar_time_series/", field.station,
                    "/doy_data.dat"), sep="\t")

# glorad (Global Radiation, W.m-2, AVER)
echseInput(engine=engine,
           variable="glorad",
           na.val=0,
           stn="Met",
           column=2,
           t.seq=time.seq,
           directory=paste0("forcing/meteo/05_meteofill/out/", field.station,
                            "/"))

# hour (Hour of Day, h)
hour.df <- data.frame(end_of_interval=format(time.seq, "%Y-%m-%d %H:%M:%S"),
                      as.numeric(strftime(time.seq, "%H")))
names(hour.df)[2] <- "any"
write.matrix(hour.df, 
             paste0("~/uni/projects/", engine, 
                    "/data/vegPar_time_series/", field.station,
                    "/hour_data.dat"), sep="\t")

# lai (Leaf Area Index, -, AVER)
echseInput(engine=engine, 
           variable="lai", 
           stn="Met",
           const=.5,
           t.seq=time.seq,
           directory=paste0("vegPar_time_series/", field.station, "/"))

# rhum (Relative Humidity, %, AVER)
echseInput(engine=engine,
           variable="rhum", 
           na.val=mean(as.numeric(met.list[[2]]), na.rm=T), 
           stn="Met",
           column=1,
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
           na.val=mean(as.numeric(met.list[[4]]), na.rm=T),  # not perfect
           stn="Met",
           column=3,
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
# -missing-
echseInput(engine=engine, 
           variable="wc_vol_root", 
           stn="Met",
           const=.8 * wc_sat,
           t.seq=time.seq,
           directory=paste0("forcing/meteo/05_meteofill/out/", field.station,
                            "/"))

# wc_vol_top (Water Content in Topsoil, -, AVER)
# -missing-
echseInput(engine=engine, 
           variable="wc_vol_top", 
           stn="Met", 
           const=.9 * wc_sat, 
           t.seq=time.seq,
           directory=paste0("forcing/meteo/05_meteofill/out/", field.station,
                            "/"))

# wind (Wind Speed, m.s-1, AVER)
echseInput(engine=engine,
           variable="wind",
           na.val=mean(as.numeric(met.list[[6]]), na.rm=T),  # not perfect
           stn="Met",
           column=4,
           t.seq=time.seq,
           directory=paste0("forcing/meteo/05_meteofill/out/", field.station,
                            "/"))

# WRITE MODEL CONTROL FILES ----------------------------------------------------

echseCtrl_cnf(engine, tstart, tend, dt)
echseCtrl_out(engine, output, et.choice[1])
echseCtrl_loc(engine, selection$DAT, A, locs)
echseCtrl_fil(engine, field.station, selection$DAT, A)
echseCtrl_shpar(engine, sharedParamNum, selection$SHP)
echseCtrl_par(engine, paramNum, selection$PAR)
echseCtrl_ini(engine, selection$STV, selection$I)

# CALL MODEL RUN & POSTPROCESSING ---------------------------------------------

echsePost(engine, et.choice, ma.width=1, field.station=field.station, 
          comp=output, wc_etmax=wc_etmax)

# SHOW RESULTS ----------------------------------------------------------------

Sys.sleep(1.0)

dstart <- strsplit(tstart, " ")[[1]][1]
dend <- strsplit(tend, " ")[[1]][1]

if (output == "evap") {
  comp.file <- paste0("plot_et_compare_morocco_", field.station, "_", dstart,
                      "_", dend, ".pdf")
  cum.file <- paste0("plot_et_cum_morocco_", field.station, "_", dstart, "_",
                     dend, ".pdf")
} else if (output == "glorad") {
  comp.file <- paste0("plot_glorad_compare_morocco_", field.station, "_",
                      dstart, "_", dend, ".pdf")
  cum.file <- paste0("plot_glorad_cum_morocco_", field.station, "_",
                     dstart, "_", dend, ".pdf")
}

if (is.null(warnings())) {
  if (output == "evap") {
    system(paste0("cd ~/boxup/whk_echse; evince ", comp.file, " &"))
  } else if (output == "glorad") {
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
                  " results/evap_morocco/", et.choice[1], "_", method, "/"))
    # copy cumulative plot to archive dir
    system(paste0("cd ~/boxup/whk_echse; cp ", cum.file,
                  " results/evap_morocco/", et.choice[1], "_", method, "/"))
    # copy simulation results to archive dir
    system(paste0("cd ~/uni/projects/evap_morocco/run/out; cp test1.txt ",
                  field.station, "/test1.txt"))
  } else if (output == "glorad") {
    # copy plot for comparison to archive directory
    system(paste0("cd ~/boxup/whk_echse; cp ", comp.file, " results/", engine,
                  "/"))
    # copy cumulative plot to archive dir
    system(paste0("cd ~/boxup/whk_echse; cp ", cum.file, " results/", engine,
                  "/"))
    # copy simulation results to archive dir
    system(paste0("cd ~/uni/projects/", engine, "/run/out; cp test1.txt ",
                  field.station, "/test1.txt"))
  } else if (output %in% c("gloradmax", "rad_net", "radex")) {
    # copy simulation results to archive dir
    system(paste0("cd ~/uni/projects/", engine, "/run/out; cp test1.txt ",
                  field.station, "/test1.txt"))
  }
}

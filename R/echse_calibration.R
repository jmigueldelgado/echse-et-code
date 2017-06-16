################################################################################
# Author: Julius Eberhard
# Last Edit: 2016-09-30
# Project: ECHSE Evapotranspiration
# Program: echse_calibration
# Aim: Calibration of ECHSE Models for Simulating Evapotranspiration
# TODO(2017-06-16): some unfinished business here, basically from the beginning
################################################################################

rm(list=ls())


# PROGRAM PARAMETERS :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# path to main R script calling the model
CAL.main.path <- "~/boxup/whk_echse/echse_portugal.R"
# path to model output
CAL.out.path <- "~/uni/projects/evap_portugal/run/output/test1.txt"
# path to observations (which are compared to model output)
CAL.obs.path <- ""


# LOAD DATA ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

CAL.obs <- read.table(CAL.obs.path)


# PREPROCESSING ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# find starting and ending line of script
CAL.lstart <- system(paste("grep -in 'calibration on'", main.path,
                           "| sed 's/^\([0-9])\+\):.*$/\1/'"))
CAL.lend <- system(paste("grep -in 'calibration off'", main.path,
                         "| sed 's/^\([0-9])\+\):.*$/\1/'"))
# parameter ranges and sampling
CAL.drag_coef <- sort(runif(10, 0, 0))
CAL.ext <- sort(runif(10, 0, 0))
CAL.par_stressHum <- sort(runif(10, 0, 0))
CAL.res_leaf_min <- sort(runif(10, 0, 0))
CAL.rss_a <- sort(runif(10, 0, 0))
CAL.rss_b <- sort(runif(10, 0, 0))
CAL.wc_etmax <- sort(runif(10, 0, 0))
CAL.wstressmax <- sort(runif(10, 0, 0))

# MONTE CARLO LOOP -------------------------------------------------------------
# wrong ... don't use every value create before
# do it right: create evenly partitioned vectors for parameters, then create
# parameter space of all combinations, then choose randomly from this

for (ii in CAL.drag_coef) {
for (jj in CAL.ext) {
for (kk in CAL.par_stressHum) {
for (ll in CAL.res_leaf_min) {
for (mm in CAL.rss_a) {
for (nn in CAL.rss_b) {
for (oo in CAL.wc_etmax) {
for (pp in CAL.wstressmax) {
# run main script
source(pipe(paste0("sed -n ", CAL.lstart, ",", CAL.lend, "p ", CAL.main.path)))
# adapt calibration parameters
drag_coef <- CAL.drag_coef[ii]
ext <- CAL.ext[jj]
par_stressHum <- CAL.par_stressHum[kk]
res_leaf_min <- CAL.res_leaf_min[ll]
rss_a <- CAL.rss_a[mm]
rss_b <- CAL.rss_b[nn]
wc_etmax <- CAL.wc_etmax[oo]
wstressmax <- CAL.wstressmax[pp]
# generate input files
echseCtrl_cnf(engine, tstart, tend, dt)
echseCtrl_out(engine, output, et.choice[1])
echseCtrl_loc(engine, selection$DAT, A, locs)
echseCtrl_fil(engine, selection$DAT, A)
echseCtrl_shpar(engine, sharedParamNum, selection$SHP)
echseCtrl_par(engine, paramNum, selection$PAR)
echseCtrl_ini(engine, selection$STV, selection$I)
# run model
system(paste0("cd ~/uni/projects/", engine, "/run; ./run cnf_default"))


# READ MODEL OUTPUT ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

CAL.out <- read.table(CAL.out.path)

}  # end wstressmax loop
}  # end wc_etmax loop
}  # end rss_b loop
}  # end rss_a loop
}  # end res_leaf_min loop
}  # end par_stressHum loop
}  # end ext loop
}  # end drag_coef loop
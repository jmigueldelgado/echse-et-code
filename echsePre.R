################################################################################
# Author: Julius Eberhard
# Last Edit: 2016-11-27
# Project: ECHSE Evapotranspiration
# Functions: echseSelect, echseTimeSeq
# Aim: Providing Information for Data Preprocessing
################################################################################

echseSelect <- function(output  # output variable of engine
                        ) {

  # creates lists of 0/1 (no/yes) information and vectors regarding the usage of
  # data, parameters, and variables for any engine output

  if (output == "evap") {
    # input data needed for outputs
    DAT <- list(alb=1, apress=1, cano_height=1, cloud=1, doy=1, glorad=1,
                glorad_max=1, hour=1, lai=1, rad_long=1, rad_net=1, 
                rad_net_soil=1, radex=1, rhum=1, soilheat=1, sundur=1, 
                temp_max=1, temp_min=1, temper=1, totalheat=1, utc_add=1,
                wc_vol_root=1, wc_vol_top=1, wind=1)
    # individual parameters needed for outputs
    PAR <- list(bubble=1, crop_faoref=1, crop_makk=1, elev=1, glo_half=1,
                lat=1, lon=1, par_stressHum=1, pores_ind=1, res_leaf_min=1,
                soil_dens=1, wc_etmax=1, wc_pwp=1, wc_res=1, wc_sat=1,
                wstressmax=1, wstressmin=1)
    # shared parameters needed for outputs
    SHP <- list(choice_et=1, choice_gloradmax=1, choice_plantDispl=1,
                choice_rcs=1, choice_roughLen=1, drag_coef=1, eddy_decay=1,
                emis_a=1, emis_b=1, ext=1, f_day=1, f_night=1, fcorr_a=1,
                fcorr_b=1, h_humMeas=1, h_tempMeas=1, h_windMeas=1, na_val=1,
                radex_a=1, radex_b=1, res_b=1, rough_bare=1, rss_a=1, rss_b=1)
    # state variables for outputs
    STV <- "s_longrad"
    # set initial values of state variables
    I <- 20
  } else if (output == "glorad") {
    # input data needed for outputs
    DAT <- list(alb=0, apress=0, cano_height=0, cloud=1, doy=1, glorad=0,
                glorad_max=0, hour=0, lai=0, rad_long=0, rad_net=0,
                rad_net_soil=0, radex=0, rhum=0, soilheat=0, sundur=1,
                temp_max=0, temp_min=0, temper=0, totalheat=0, utc_add=0,
                wc_vol_root=0, wc_vol_top=0, wind=0)
    # individual parameters needed for outputs
    PAR <- list(bubble=0, crop_faoref=0, crop_makk=0, elev=0, glo_half=0,
                lat=1, lon=0, par_stressHum=0, pores_ind=0, res_leaf_min=0,
                soil_dens=0, wc_etmax=0, wc_pwp=0, wc_res=0, wc_sat=0,
                wstressmax=0, wstressmin=0)
    # shared parameters needed for outputs
    SHP <- list(choice_et=0, choice_gloradmax=0, choice_plantDispl=0,
                choice_rcs=0, choice_roughLen=0, drag_coef=0, eddy_decay=0,
                emis_a=0, emis_b=0, ext=0, f_day=0, f_night=0, fcorr_a=0,
                fcorr_b=0, h_humMeas=0, h_tempMeas=0, h_windMeas=0, na_val=0,
                radex_a=1, radex_b=1, res_b=0, rough_bare=0, rss_a=0, rss_b=0)
    # state variables for outputs
    STV <- "none"
    # set initial values of state variables
    I <- 0
  } else if (output == "gloradmax") {
    # input data needed for outputs
    DAT <- list(alb=0, apress=0, cano_height=0, cloud=0, doy=1, glorad=0,
                glorad_max=0, hour=1, lai=0, rad_long=0, rad_net=0,
                rad_net_soil=0, radex=0, rhum=0, soilheat=0, sundur=0,
                temp_max=0, temp_min=0, temper=0, totalheat=0, utc_add=1,
                wc_vol_root=0, wc_vol_top=0, wind=0)
    # individual parameters needed for outputs
    PAR <- list(bubble=0, crop_faoref=0, crop_makk=0, elev=1, glo_half=0,
                lat=1, lon=1, par_stressHum=0, pores_ind=0, res_leaf_min=0,
                soil_dens=0, wc_etmax=0, wc_pwp=0, wc_res=0, wc_sat=0,
                wstressmax=0, wstressmin=0)
    # shared parameters needed for outputs
    SHP <- list(choice_et=0, choice_gloradmax=1, choice_plantDispl=0,
                choice_rcs=0, choice_roughLen=0, drag_coef=0, eddy_decay=0,
                emis_a=0, emis_b=0, ext=0, f_day=0, f_night=0, fcorr_a=0,
                fcorr_b=0, h_humMeas=0, h_tempMeas=0, h_windMeas=0, na_val=0,
                radex_a=1, radex_b=1, res_b=0, rough_bare=0, rss_a=0, rss_b=0)
    # state variables for outputs
    STV <- "s_glorad_max"
    # set initial values of state variables
    I <- 20
  } else if (output == "rad_net") {
    # input data needed for outputs
    DAT <- list(alb=1, apress=0, cano_height=0, cloud=0, doy=1, glorad=1,
                glorad_max=0, hour=1, lai=0, rad_long=0, rad_net=0,
                rad_net_soil=0, radex=0, rhum=1, soilheat=0, sundur=0,
                temp_max=0, temp_min=0, temper=1, totalheat=0, utc_add=1,
                wc_vol_root=0, wc_vol_top=0, wind=0)
    # individual parameters needed for outputs
    PAR <- list(bubble=0, crop_faoref=0, crop_makk=0, elev=1, glo_half=0,
                lat=1, lon=1, par_stressHum=0, pores_ind=0, res_leaf_min=0,
                soil_dens=0, wc_etmax=0, wc_pwp=0, wc_res=0, wc_sat=0,
                wstressmax=0, wstressmin=0)
    # shared parameters needed for outputs
    SHP <- list(choice_et=0, choice_gloradmax=1, choice_plantDispl=0,
                choice_rcs=0, choice_roughLen=0, drag_coef=0, eddy_decay=0,
                emis_a=1, emis_b=1, ext=0, f_day=0, f_night=0, fcorr_a=1,
                fcorr_b=1, h_humMeas=0, h_tempMeas=0, h_windMeas=0, na_val=0,
                radex_a=1, radex_b=1, res_b=0, rough_bare=0, rss_a=0, rss_b=0)
    # state variables for outputs
    STV <- "s_glorad_max"
    # set initial values of state variables
    I <- 20
  } else if (output == "radex") {
    # input data needed for outputs
    DAT <- list(alb=0, apress=0, cano_height=0, cloud=0, doy=1, glorad=0,
                glorad_max=0, hour=1, lai=0, rad_long=0, rad_net=0,
                rad_net_soil=0, radex=0, rhum=0, soilheat=0, sundur=0,
                temp_max=0, temp_min=0, temper=0, totalheat=0, utc_add=1,
                wc_vol_root=0, wc_vol_top=0, wind=0)
    # individual parameters needed for outputs
    PAR <- list(bubble=0, crop_faoref=0, crop_makk=0, elev=0, glo_half=0,
                lat=1, lon=1, par_stressHum=0, pores_ind=0, res_leaf_min=0,
                soil_dens=0, wc_etmax=0, wc_pwp=0, wc_res=0, wc_sat=0,
                wstressmax=0, wstressmin=0)
    # shared parameters needed for outputs
    SHP <- list(choice_et=0, choice_gloradmax=0, choice_plantDispl=0,
                choice_rcs=0, choice_roughLen=0, drag_coef=0, eddy_decay=0,
                emis_a=0, emis_b=0, ext=0, f_day=0, f_night=0, fcorr_a=0,
                fcorr_b=0, h_humMeas=0, h_tempMeas=0, h_windMeas=0, na_val=0,
                radex_a=0, radex_b=0, res_b=0, rough_bare=0, rss_a=0, rss_b=0)
    # state variables for outputs
    STV <- "none"
    # set initial values of state variables
    I <- 0
  } else if (output == "soilheat") {
    # input data needed for outputs
    DAT <- list(alb=0, apress=0, cano_height=0, cloud=0, doy=1, glorad=0,
                glorad_max=0, hour=1, lai=1, rad_long=0, rad_net=1,
                rad_net_soil=0, radex=0, rhum=0, soilheat=0, sundur=0,
                temp_max=0, temp_min=0, temper=0, totalheat=0, utc_add=1,
                wc_vol_root=0, wc_vol_top=0, wind=0)
    # individual parameters needed for outputs
    PAR <- list(bubble=0, crop_faoref=0, crop_makk=0, elev=1, glo_half=0,
                lat=1, lon=1, par_stressHum=0, pores_ind=0,
                res_leaf_min=0, soil_dens=0, wc_etmax=0, wc_pwp=0,
                wc_res=0, wc_sat=0, wstressmax=0, wstressmin=0)
    # shared parameters needed for outputs
    SHP <- list(choice_et=1, choice_gloradmax=1, choice_plantDispl=0,
                choice_rcs=0, choice_roughLen=0, drag_coef=0, eddy_decay=0,
                emis_a=0, emis_b=0, ext=1, f_day=1, f_night=1, fcorr_a=0,
                fcorr_b=0, h_humMeas=0, h_tempMeas=0, h_windMeas=0, na_val=0,
                radex_a=1, radex_b=1, res_b=0, rough_bare=0, rss_a=0, rss_b=0)
    # state variables for outputs
    STV <- "none"
    # set initial values of state variables
    I <- 0
  }

  list(DAT=DAT, PAR=PAR, SHP=SHP, STV=STV, I=I)

}

#----

echseTimeSeq <- function(t.index,  # time index, POSIX formatted
                         tstart,  # start time
                         dt  # time interval in seconds
                         ) {

  # creates time sequence based on given start time & time interval

  i <- match(T, unlist(lapply(strsplit(as.character(t.index), " "),
                              function(x)
                                x[2] == strsplit(tstart, " ")[[1]][2])))
  seq(t.index[i], tail(t.index, 1), by=paste(dt, "sec"))

}

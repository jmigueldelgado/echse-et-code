###########################################################
# Author: Julius Eberhard
# Last Edit: 2017-06-16
# Project: ECHSE Evapotranspiration
# Function: echseAlbSin
# Aim: Writing Albedo Time Series with Sinusoidal Behavior,
#      Assuming a One-Year Length Period
###########################################################

echseAlbSin <- function(tstart,  # start date, format "Y-m-d hh:mm:ss"
                        tend,    # end date, format "Y-m-d hh:mm:ss"
                        intv,    # time interval, "halfhour", "hour", "day"
                        albmin,  # minimum albedo
                        albmax,  # maximum albedo
                        tmax     # date of maximum albedo (doy)
                        ) {

  if (system("hostname", intern=T) == "jurek-PC") {
    source("C:/Users/jurek/R/jufun.R")
  } else {
    source("/home/jurek/R/jufun.R")
  }
  
  tmax    <- tmax
  
  years   <- seq(as.numeric(format(as.Date(tstart), "%Y")), 
                 as.numeric(format(as.Date(tend), "%Y")))
  
  t_lg    <- vector("numeric", length(years))
  t_ls    <- list(NA, length(years))   # time steps for whole year
  alb_ls  <- list(NA, length(years))   # albedo values for whole year
  
  # calculate albedo for one year
  
  albmean <- mean(c(albmin, albmax))
  
  if (intv == "halfhour") {
    
    dati    <- seq(as.POSIXlt(tstart), as.POSIXlt(tend), "30 min")
    ifelse(is.leapyear(years), t_lg <- 366*48, t_lg <- 365*48)
    for (k in 1:length(years)) t_ls[[k]] <- seq(1, t_lg[k])
    alb     <- vector("numeric", sum(do.call(rbind, lapply(t_ls, length))) - 1)
    i       <- 1
    for (j in 1:length(t_ls)) {
      while (i <= sum(do.call(rbind, lapply(head(t_ls, n=j), length)))) {
          ## should be either 365 or 366 (vvv), of course
        alb[i]  <- sin(t_ls[[j]][i]*2*pi/(365*48) - tmax*2*pi/(365*48) + pi/2)*(albmax - albmean) + albmean
        i       <- i + 1
      }
    }
    ## datistart<- which(t_ls[[1]] == as.numeric(format(as.Date(tstart), "%j")) + 1)
    tnumstart <- as.numeric(format(as.POSIXlt(tstart), "%j%H%M"))
    minstart  <- tnumstart - floor(tnumstart / 100) * 100
    hourstart <- floor(tnumstart / 100) - floor(tnumstart / 10000) * 100
    doystart  <- floor(tnumstart / 10000)
    ######## !!!
    tnumend   <- as.numeric(format(as.POSIXlt(tstart), "%j%H%M"))
    minend    <- tnumstart - floor(tnumstart / 100) * 100
    hourend   <- floor(tnumstart / 100) - floor(tnumstart / 10000) * 100
    doyend    <- floor(tnumstart / 10000)
    doyend  <- which(t_ls[[length(t_ls)]] == as.numeric(format(as.Date(tend), "%j")) + 1)
    alb     <- head(alb, n=-length(t_ls[[length(t_ls)]]) + doyend)
    alb     <- alb[-c(seq(1, doystart-1), seq(doyend))]
    
  } else if (intv == "hour") {
    
    
    
  } else if (intv == "day") {
    
    dati    <- seq(as.Date(tstart), as.Date(tend), "day")
    ifelse(is.leapyear(years), t_lg <- 366, t_lg <- 365)
    for (k in 1:length(years)) t_ls[[k]] <- seq(1, t_lg[k])
    alb     <- vector("numeric", sum(do.call(rbind, lapply(t_ls, length))) - 1)
    i       <- 1
    for (j in 1:length(t_ls)) {
      while (i <= sum(do.call(rbind, lapply(head(t_ls, n=j), length)))) {
         ## should be either 365 or 366 (vvv), of course
        alb[i]  <- sin(t_ls[[j]][i]*2*pi/365 - tmax*2*pi/365 + pi/2)*(albmax - albmean) + albmean
        i       <- i + 1
      }
    }
    doystart<- which(t_ls[[1]] == as.numeric(format(as.Date(tstart), "%j")) + 1)
    doyend  <- which(t_ls[[length(t_ls)]] == as.numeric(format(as.Date(tend), "%j")) + 1)
    alb     <- head(alb, n=-length(t_ls[[length(t_ls)]]) + doyend)
    alb     <- alb[-c(seq(1, doystart-1), seq(doyend))]
  
  }
  
  alb_df  <- data.frame(end_of_interval=dati, alb)
  names(alb_df)[2] <- "HS"
  return(alb_df)
  
}
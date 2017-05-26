################################################################################
# Author: Julius Eberhard
# Last Edit: 2017-05-26
# Project: ECHSE Evapotranspiration
# Function: echsePost
# Aim: Model Run and Data Postprocessing
# TODO(2017-05-26): x axis labels for comparison plots: don't want weekdays
################################################################################

echsePost <- function(engine,  # name of ECHSE engine
                      et.choice,  # etp or eta
                      ma.width,  # width of moving average filter in time units
                      field.station,  # field station name
                      comp = NA,  # variable for comparison bet. model & obs
                      wc_etmax  # min soil water content for max eta
                      ) {

  system(paste0("cd ~/uni/projects/", engine, "/run; ./run cnf_default"))

  # SET AND LOAD ---------------------------------------------------------------

  library(TTR)
  results <- read.delim(paste0("~/uni/projects/", engine, "/run/out/test1.txt"),
                        sep="\t")

  # POSTPROCESSING -------------------------------------------------------------

  dstart <- strsplit(tstart, " ")[[1]][1]
  dend <- strsplit(tend, " ")[[1]][1]
  res.xts <- xts(results[, 2], 
                 order.by=as.POSIXct(results$end_of_interval, tz="UTC"))
  res.mean <- mean(results[, 2])

  # PLOT -----------------------------------------------------------------------

  if (comp == "evap") {
  # evapotranspiration

    # NORMAL PLOT

    if (engine == "evap_portugal") {
    # Portugal
      pdf(paste0("plot_et_compare_portugal_", field.station, "_", dstart, "_",
                 dend, ".pdf"))
      ma.width <- ma.width
      period <- seq(as.POSIXct(tstart), as.POSIXct(tend), by="1 hour")
      xl <- c(as.POSIXct(tstart), as.POSIXct(tend))
      rad <- as.numeric(meteo.list[[3]][period])  # total incoming radiation
      rad[is.na(rad)] <- 0
      rad.ma <- runMean(rad, n=ma.width)
      hum <- as.numeric(meteo.list[[6]][period])  # air humidity
      hum[is.na(hum)] <- 0
      hum.ma <- runMean(hum, n=ma.width)

      layout(matrix(1:7, 7, 1), heights=c(.2, rep(.1, 5), .3))
      if (field.station == "HS") {
      # Hauptstation
        radn <- as.numeric(HS.list[[1]][period])  # net radiation
        radn[is.na(radn)] <- 0
        radn.ma <- runMean(radn, n=ma.width)
        temp <- as.numeric(HS.list[[2]][period])  # air temperature
        temp[is.na(temp)] <- mean(as.numeric(HS.list[[2]]))
        temp.ma <- runMean(temp, n=ma.width)
        sohe <- as.numeric(HS.list[[3]][period])  # soil heat flux
        sohe[is.na(sohe)] <- 0
        sohe.ma <- runMean(sohe, n=ma.width)
        somo <- as.numeric(HS.list[[4]][period])  # soil moisture
        somo[is.na(somo)] <- wc_res
        somo.ma <- runMean(somo, n=ma.width)
        wind <- as.numeric(HS.list[[11]][period])  # wind
        wind[is.na(wind)] <- 0
        wind.ma <- runMean(wind, n=ma.width)

        AddAxis <- function(data
                            ) {
            pr <- pretty(range(data, na.rm=TRUE))
            atlab <- c(pr[2], tail(pr, 2)[1])
            axis(2, at=atlab, labels=atlab)
        }
        
        par(mar=c(0, 5, 5, 2))
        plot(period, rad.ma, xlim=xl, type="l", ylab=(Rad~(W~m^{-2})),
             axes=FALSE,
             main=c(paste0("Engine: ", engine, ", ", field.station), et.choice))
        AddAxis(rad.ma)
        par(mar=c(0, 5, 0, 2))
        plot(period, temp.ma, xlim=xl, type="l", ylab=(Temp~({}^o*C)),
             axes=FALSE)
        AddAxis(temp.ma)
        plot(period, sohe.ma, xlim=xl, type="l", ylab=(SHF~(W~m^{-2})),
             axes=FALSE)
        AddAxis(sohe.ma)
        plot(period, somo.ma, xlim=xl, type="l",
             ylab=expression(S.moist~({}-{})), axes=F)
        AddAxis(somo.ma)
        plot(period, hum.ma, xlim=xl, type="l", ylab=(Rel.hum.~("%")),
             axes=FALSE)
        AddAxis(hum.ma)
        plot(period, wind.ma, xlim=xl, type="l", ylab=(Wind~(m~s^{-1})),
             axes=FALSE)
        AddAxis(wind.ma)
        par(mar=c(4, 5, 0, 2))
        plot(period, as.numeric(HS.list[[8]][period]), xlim=xl,
             ylim=c(-.1, 2), xlab="", ylab=(ET~(mm)), type="l", col="gray40")
      } else {
      # Nebenstation A
        radn <- as.numeric(NSA.list[[1]][period])  # net radiation
        radn[is.na(radn)] <- 0
        radn.ma <- runMean(radn, n=ma.width)
        temp <- as.numeric(NSA.list[[2]][period])  # air temperature
        temp[is.na(temp)] <- mean(as.numeric(NSA.list[[2]]))
        temp.ma <- runMean(temp, n=ma.width)
        sohe <- as.numeric(NSA.list[[3]][period])  # soil heat flux
        sohe[is.na(sohe)] <- 0
        sohe.ma <- runMean(sohe, n=ma.width)
        somo <- as.numeric(NSA.list[[4]][period])  # soil moisture
        somo[is.na(somo)] <- wc_res
        somo.ma <- runMean(somo, n=ma.width)
        wind <- as.numeric(NSA.list[[11]][period])  # wind
        wind[is.na(wind)] <- 0
        wind.ma <- runMean(wind, n=ma.width)

        par(mar=c(0, 5, 5, 2))
        plot(period, rad.ma, xlim=xl, type="l", ylab=(Rad~(W~m^{-2})), xaxt="n",
             main=c(paste0("Engine: ", engine, ", ", field.station), et.choice))
        #lines(index(NSA.list[[1]]), radn.ma, xlim=xl, type="l", col=2)
        #legend("topright", c("global rad.", "net rad."), lty=1, col=c(1, 2))
        par(mar=c(0, 5, 0, 2))
        plot(period, temp.ma, xlim=xl, type="l", ylab=(Temp~({}^o*C)), xaxt="n")
        plot(period, sohe.ma, xlim=xl, type="l",
             ylab=(SHF~(W~m^{-2})), xaxt="n")
        plot(period, somo.ma, xlim=xl, type="l", 
             ylab=expression(S.moist~({}-{})), xaxt="n")
        plot(period, hum.ma, xlim=xl, type="l",
             ylab=(Rel.hum.~("%")), xaxt="n")
        plot(period, wind.ma, xlim=xl, type="l",
             ylab=(Wind~(m~s^{-1})), xaxt="n")
        par(mar=c(4, 5, 0, 2))
        plot(period, as.numeric(NSA.list[[8]][period]), xlim=xl,
             ylim=c(-.1, 2), xlab="", ylab=(ET~(mm)), type="l", col="gray40")
      }
      par(new=T)
      plot(period[-1], as.numeric(res.xts), xlim=xl, ylim=c(-.1, 2), col=2,
           xlab="Date", ylab="", type="l")
      legend("topright", c("simulation", "observation"), lty=1,
             col=c(2, "gray40"), bty="n")
      dev.off()
    } else {
    # Morocco
      pdf("plot_et_compare_morocco.pdf")
      ma.width <- ma.width
      period <- seq(as.POSIXct(tstart, tz="UTC"), 
                    as.POSIXct(tend, tz="UTC"), by="1 hour")
      xl <- c(as.POSIXct(tstart, tz="UTC"), as.POSIXct(tend, tz="UTC"))
      rad <- as.numeric(meteo.list[[2]])  # total incoming radiation
      rad[is.na(rad)] <- 0
      rad.ma <- runMean(rad, n=ma.width)
      hum <- as.numeric(meteo.list[[1]])  # air humidity
      hum[is.na(hum)] <- 0
      hum.ma <- runMean(hum, n=ma.width)
      radn.ma <- rad.ma * .8  # net radiation
      temp <- as.numeric(meteo.list[[3]])  # air temperature
      temp[is.na(temp)] <- mean(as.numeric(meteo.list[[4]]))
      temp.ma <- runMean(temp, n=ma.width)
      sohe.ma <- NA  # soil heat flux
      somo.ma <- .9 * wc_sat  # soil moisture
      wind <- as.numeric(meteo.list[[4]])  # wind
      wind[is.na(wind)] <- 0
      wind.ma <- runMean(wind, n=ma.width)
      # calculate daily ET sums for comparison with observations
      # -> maybe faster with apply.daily(..., sum)?
      etsum <- c()
      for (i in 1:365)
        etsum[i] <- sum(as.numeric(res.xts[format(index(res.xts), "%j") == i,
                                           1]))

      layout(matrix(1:7, 7, 1), heights=c(.2, rep(.1, 5), .3))
      par(mar=c(0, 5, 5, 2))
      plot(index(meteo.list[[1]]), rad.ma, xlim=xl, type="l", xlab="",
           ylab=(Rad~(W~m^{-2})), xaxt="n", main=c(engine, et.choice))
      #lines(index(meteo.list[[1]]), radn.ma, xlim=xl, type="l", col=2)
      #legend("topright", c("global rad.", "net rad."), col=c(1, 2))
      par(mar=c(0, 5, 0, 2))
      plot(index(meteo.list[[1]]), temp.ma, xlim=xl, type="l", 
           ylab=(Temp~({}^o*C)), xaxt="n")
      plot(index(meteo.list[[1]]), rep(sohe.ma, length(index(meteo.list[[1]]))), 
           xlim=xl, ylim=c(0, 0), type="l", ylab=(SHF~(W~m^{-2})), xaxt="n")
      plot(index(meteo.list[[1]]), rep(somo.ma, length(index(meteo.list[[1]]))),
           xlim=xl, type="l", ylab=expression(S.moist~({}-{})), xaxt="n")
      plot(index(meteo.list[[1]]), hum.ma, xlim=xl, type="l",
           ylab=(Rel.hum.~("%")), xaxt="n")
      plot(index(meteo.list[[1]]), wind.ma, xlim=xl, type="l",
           ylab=(Wind~(m~s^{-1})), xaxt="n")
      par(mar=c(4, 5, 0, 2))
      plot(index(eta.day.xts), as.numeric(eta.day.xts), xlim=xl,
           ylim=c(-1, max(c(as.numeric(eta.day.xts), etsum))), xlab="",
           ylab=(ET~(mm)), type="l", col=2)
      lines(seq(as.POSIXct("2013-01-01"), as.POSIXct("2013-12-31"), by="day"),
            etsum)
      legend("topright", c("simulation", "observation"), lty=1, col=c(1, 2),
             bty="n")
      dev.off()
    }

    # CUMULATIVE PLOT

    if (engine == "evap_portugal") {
    # Portugal
      pdf(paste0("plot_et_cum_portugal_", field.station, "_", dstart, "_", dend,
                 ".pdf"))
      plot(cumsum(res.xts), ylim=c(0, 50), main=paste(engine, "cumulative"),
           ylab="cumulative ET (mm)")
      if (field.station == "HS") {
      # Hauptstation
        lines(cumsum(na.exclude(HS.list[[8]][index(HS.list[[8]]) >=
                                             index(res.xts)[1]])),
              col=2)
      } else {
      # Nebenstation A
        lines(cumsum(na.exclude(NSA.list[[8]][index(NSA.list[[8]]) >=
                                              index(res.xts)[1]])),
              col=2)
        legend("topright", c("simulation", "observation"), lty=1, col=c(1, 2))
      }
      dev.off()
    } else {
    # Morocco
      pdf("plot_et_cum_morocco.pdf")
      plot(cumsum(res.xts), ylim=c(0, 50), main=paste(engine, "cumulative"),
           ylab="cumulative ET (mm)")
      lines(cumsum(na.exclude(eta.day.xts[index(eta.day.xts) >=
                                          index(res.xts)[1]])), col=2)
      legend("topright", c("simulation", "observation"), lty=1, col=c(1, 2))
      dev.off()
    }

  } else if (comp == "glorad") {
  # global radiation

    # NORMAL PLOT
    if (tail(strsplit(engine, "_")[[1]], 1) == "portugal") {
      rad.plot <- apply.daily(meteo.list[[3]], mean)
      index(rad.plot) <- as.POSIXlt(index(rad.plot))
      index(rad.plot)$hour <- "00"
    } else {
      rad.plot <- apply.daily(meteo.list[[2]], mean)
      index(rad.plot) <- as.POSIXlt(index(rad.plot))
      index(rad.plot)$hour <- "00"
    }

    if (tail(strsplit(engine, "_")[[1]], 1) == "portugal") {
      pdf(paste0("plot_glorad_compare_portugal_", field.station, "_", dstart,
                 "_", dend, ".pdf"))
    } else {
      pdf("plot_glorad_compare_morocco.pdf")
    }

    plot(res.xts, ylim=c(0, 500), main=engine, ylab=comp)
    lines(rad.plot, col=2)
    legend("topright", legend=c("simulation", "observation"), col=c(1, 2),
           lty=c(1, 1))
    dev.off()

    # CUMULATIVE PLOT
    if (tail(strsplit(engine, "_")[[1]], 1) == "portugal") {
      pdf(paste0("plot_glorad_cum_portugal_", field.station, "_", dstart, "_",
                 dend, ".pdf"))
    } else {
      pdf("plot_glorad_cum_morocco.pdf")
    }

    plot(cumsum(res.xts), ylim=c(0, 50000), main=paste(engine, "cumulative"),
         ylab="cumulative radiation (W m2)")
    lines(cumsum(rad.plot[index(rad.plot) >= index(res.xts)[1]]), col=2)
    legend("topright", legend=c("simulation", "observation"), col=c(1, 2),
           lty=c(1, 1))
    dev.off()

  } else if (comp == "rad_net") {
  # net radiation

    # NORMAL PLOT
    if (tail(strsplit(engine, "_")[[1]], 1) == "portugal") {
      rnet.plot <- get(paste0(field.station, ".list"))[[1]]
      pdf(paste0("plot_rad_net_compare_portugal_", field.station, "_", dstart,
                 "_", dend, ".pdf"))
#      plot(res.xts, ylim=c(0, 800), main=engine, ylab=comp)
      plot(res.xts[round(length(res.xts) / 3):round(length(res.xts) / 1.5)],
           ylim=c(0, 800), main=engine, ylab=comp)
      lines(rnet.plot, col=2)
      legend("topright", legend=c("simulation", "observation"), col=c(1, 2),
             lty=c(1, 1))
      dev.off()
    }

    # CUMULATIVE PLOT
    if (tail(strsplit(engine, "_")[[1]], 1) == "portugal") {
      pdf(paste0("plot_rad_net_cum_portugal_", field.station, "_", dstart, "_",
                 dend, ".pdf"))
      plot(cumsum(res.xts), ylim=c(1, 30000), main=paste(engine, "cumulative"),
           ylab="cumulative net radiation (Wm2)")
      lines(cumsum(rnet.plot[index(rnet.plot) >= index(res.xts)[1]]), col=2)
      legend("topright", legend=c("simulation", "observation"), col=c(1, 2),
             lty=c(1, 1))
      dev.off()
    }

  } else if (comp == "soilheat") {
  # soil heat flux

    # NORMAL PLOT
    if (tail(strsplit(engine, "_")[[1]], 1) == "portugal") {
      sheat.plot <- get(paste0(field.station, ".list"))[[3]]
      pdf(paste0("plot_soilheat_compare_portugal_", field.station, "_", dstart,
                 "_", dend, ".pdf"))

      plot(res.xts, ylim=c(-20, 150), main=engine, ylab=comp)
      lines(sheat.plot, col=2)
      legend("topright", legend=c("simulation", "observation"), col=c(1, 2),
             lty=c(1, 1))
      dev.off()
    }

    # CUMULATIVE PLOT
    if (tail(strsplit(engine, "_")[[1]], 1) == "portugal") {
      pdf(paste0("plot_soilheat_cum_portugal_", field.station, "_", dstart, "_",
                 dend, ".pdf"))

      plot(cumsum(res.xts), ylim=c(-100, 1800),
           main=paste(engine, "cumulative"),
           ylab="cumulative soil heat flux (Wm2)")
      lines(cumsum(sheat.plot[index(sheat.plot) >= index(res.xts)[1]]), col=2)
      legend("topright", legend=c("simulation", "observation"), col=c(1, 2),
             lty=c(1, 1))
      dev.off()
    }

  }

  # return mean evapotranspiration for sensitivity analysis
  #return(res.mean)

}

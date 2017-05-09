################################################################################
# Author: Julius Eberhard
# Last Edit: 2017-05-09
# Project: ECHSE evapotranspiration
# Function: echseParEst
# Aim: Estimation of Model Parameters from Observations,
#      Works for alb, emis_*, f_*, fcorr_*, radex_*
# TODO(2017-05-09): resume at fcorr_a, b estimation from lmodels (L22x)
################################################################################

echseParEst <- function(parname,  # name of parameter group to estimate
                                  # [radex_[a/b], fcorr_[a/b], emis_[a/b],
                                  # f_[day/night], alb]
                        grfile = NA,  # file with global radiation data*
                        hrfile = NA,  # file with relative humidity
                        rnetfile = NA,  # file with net radiation data*
                        rldfile = NA,  # file with downward lw rad. data*
                        rlufile = NA,  # file with upward lw rad. data*
                        rsufile = NA,  # file with upward sw rad. data*
                        rxfile = NA,  # file with extraterrestrial rad. data*
                        sheatfile = NA,  # file with soil heat flux data*
                        tafile = NA,  # file with mean air temperature data*
                                      # *Supply complete file path!
                        emis_a = NA,  # net emissivity coefficient
                        emis_b = NA,  # ditto
                        lat = 0,  # latitude for calculating sunrise/-set
                        lon = 0,  # longitude for... ditto
                        radex_a = NA,  # parameter for estimating fcorr_*
                        radex_b = NA,  # ditto
                        r.quantile = 0.05,  # lower quantile for min rad.ratio
                        emismeth = NA,  # emissivity method [Brunt, Idso, both]
                        plots = TRUE  # plots for visual diagnosis?
                        ) {

  if (length(grep("alb", parname)) != 0) {
  # albedo

    # read global radiation
    rsd <- read.delim(grfile, sep="\t")
    rsd <- xts(rsd[, 2], order.by=as.POSIXct(rsd[, 1], tz="UTC"))
    # read upward short-wave radiation
    rsu <- readRDS(rsufile)
    # select common time window
    est.dat <- merge(rsd, rsu, join="inner")
    names(est.dat) <- c("rsd", "rsu")
    # restrict to times between 8:00 and 16:00 to avoid odd night effects
    # and to times where gr & rsu != 0
    ix <- as.numeric(format(index(est.dat), "%H")) < 17 &
          as.numeric(format(index(est.dat), "%H")) > 7 &
          est.dat$rsd != 0 & est.dat$rsu != 0
    alb.series <- with(est.dat, rsu[ix] / rsd[ix])
    # diagnostic plot
    if (plots) {
      pdf("doku/plot_alb.pdf", width=6, height=4)
      plot(apply.daily(alb.series[alb.series < 1], mean),
           ylab=expression(mu), main="", type="p", pch=20)
      dev.off()
    }
    alb <- mean(alb.series[alb.series < 1])
    return(alb)

  } else if (length(grep("radex", parname)) != 0) {
  # radex parameters

    # read extraterrestrial radiation
    radex <- read.delim(rxfile, sep="\t")
    radex.xts <- xts(radex[, 2], order.by=as.POSIXct(radex[, 1], tz="UTC"))
    # read global radiation input
    glorad <- read.delim(grfile, sep="\t")
    glorad.xts <- xts(glorad[, 2], order.by=as.POSIXct(glorad[, 1], tz="UTC"))
    # select common time window
    tstart <- max(index(radex.xts)[1], index(glorad.xts)[1])
    tend <- min(tail(index(radex.xts), 1), tail(index(glorad.xts), 1))
    rx.full <- radex.xts[paste0(tstart, "/", tend)]
    gr.full <- glorad.xts[paste0(tstart, "/", tend)]
    # restrict to times between 8:00 and 16:00 to avoid odd night effects
    rx <- rx.full[as.numeric(format(index(rx.full), "%H")) < 17 &
                  as.numeric(format(index(rx.full), "%H")) > 7]
    gr <- gr.full[as.numeric(format(index(gr.full), "%H")) < 17 &
                  as.numeric(format(index(gr.full), "%H")) > 7]
    # calculate ratio of radex and glorad
    rad.ratio <- as.numeric(gr) / as.numeric(rx)
    MaxRadRatio <- function(i) {
      out <- NA
      if (any(as.numeric(format(index(rx), "%H")) == i))
        rad.ratio2 <- rad.ratio[as.numeric(format(index(rx),
                                           "%H")) == i]
      if (exists("rad.ratio2"))
        out <- max(rad.ratio2[which(rad.ratio2 < 1)], na.rm=T)
        return(out)
    }
    r.max <- max(sapply(6:18, MaxRadRatio), na.rm=T)
    # diagnostic plots
    if (plots) {
      # plot histogram of calculated ratios
      S <- FALSE
      repeat {
        hist(rad.ratio, xlab="glorad/radex", breaks="Sturges", main="",
             xlim=c(0, 1))
        if (S)
          break
        pdf("doku/plot_radex1.pdf")
        S <- TRUE
      }
      dev.off()
      # plot rad.ratio over hours of day to detect subdaily trends
      S <- FALSE
      repeat {
        plot(as.numeric(format(index(rx), "%H")), rad.ratio, ylim=c(0, 1),
             xlab="hour of day", ylab="glorad/radex")
        abline(h=quantile(rad.ratio, r.quantile, na.rm=T), lty="dashed")
        if (S)
          break
        pdf("doku/plot_radex2.pdf")
        S <- TRUE
      }
      dev.off()
      # plot extraterr. and global radiation to detect time shifts
      plot(rx.full, type="l", ylim=c(0, max(as.numeric(rx.full))),
           xlab="date", ylab="rad (black: rx, red: gr)", main="")
      lines(gr.full, col=2)
    }
    # return parameters
    out <- c(# radex_a
             as.numeric(quantile(rad.ratio, r.quantile, na.rm=T)),
             # radex_b
             r.max - as.numeric(quantile(rad.ratio, r.quantile, na.rm=T)))
    return(out)

  } else if (length(grep("fcorr", parname)) != 0) {
  # fcorr parameters

    # calculate net emissivity between ground and atmosphere
    # (Maidment 1993; Idso & Jackson 1969)
    ta <- read.delim(tafile, sep="\t")  # mean air temperature, in degC
    ta <- xts(ta[, 2], order.by=as.POSIXct(ta[, 1], tz="UTC"))
    if (emismeth == "Idso") {
    # method from Idso & Jackson 1969; adapted by Maidment 1993
      emis <- xts(-0.02 + 0.261 * exp(-7.77E-4 * ta ^ 2),
                  order.by=index(ta))
    } else if (emismeth == "Brunt") {
    # method from Brunt 1932
      hr <- read.delim(hrfile, sep="\t")
      hr <- xts(hr[, 2], order.by=as.POSIXct(hr[, 1], tz="UTC"))
      # "Magnus equation" for saturated vap pressure in hPa, Dyck & Peschke
      vap <- 6.11 * 10 ^ (7.5 * ta / (237.3 + ta)) * hr / 100
      # Brunt equation uses vap in kPa
      emis <- emis_a + emis_b * sqrt(vap / 10)
    } else if (emismeth == "both") {
    # both methods for comparison
      emis.idso <- xts(-0.02 + 0.261 * exp(-7.77E-4 * ta ^ 2),
                       order.by=index(ta))
      hr <- read.delim(hrfile, sep="\t")
      hr <- xts(hr[, 2], order.by=as.POSIXct(hr[, 1], tz="UTC"))
      # "Magnus equation" for saturated vap pressure in hPa, Dyck & Peschke
      vap <- 6.11 * 10 ^ (7.5 * ta / (237.3 + ta)) * hr / 100
      # Brunt equation uses vap in kPa
      emis.brunt <- emis_a + emis_b * sqrt(vap / 10)
    } else {
      stop("Unknown emismeth! Choose either 'Idso' or 'Brunt' or 'both'.")
    }

    # calculate fcorr (adapted Stefan-Boltzmann law)
    rld <- readRDS(rldfile)  # downward lw radiation
    rlu <- readRDS(rlufile)  # upward lw radiation
    sig <- 5.670367E-8  # Stefan constant
    # make inner join of time series
    if (emismeth == "both") {
      est.dat <- merge(rld, rlu, emis.brunt, emis.idso, ta, all=FALSE)
      names(est.dat)[3:4] <- c("emis.brunt", "emis.idso")
    } else {
      est.dat <- merge(rld, rlu, emis, ta, all=FALSE)
      names(est.dat)[3] <- "emis"
    }
    if (plots) {
      with(est.dat, plot(as.numeric(ta), as.numeric(rld - rlu),
           main="", xlab=expression("Mean air temperature"~({}^o~C)),
           ylab=expression("Net LW radiation"~(W~m^{-2}))))
      if (emismeth == "both") {
        par(mfrow=c(2, 1), mar=c(1, 5, 2, 1))
        with(est.dat,
             plot(as.numeric(emis.brunt), as.numeric(rld - rlu), xaxt="n",
                  main="", xlab="Net emissivity",
                  ylab=expression(Net~LW~radiation~(W~m^{-2}))))
        text(0.22, -80, "Brunt")
        par(mar=c(5, 5, 0, 1))
        with(est.dat,
             plot(as.numeric(emis.idso), as.numeric(rld - rlu),
                  main="", xlab="Net emissivity",
                  ylab=expression(Net~LW~radiation~(W~m^{-2}))))
        text(0.18, -80, "Idso & Jackson")
      } else {
        par(mfrow=c(1, 1))
        with(est.dat,
             plot(as.numeric(emis), as.numeric(rld - rlu),
                  main=emismeth, xlab="Net emissivity",
                  ylab=expression("Net LW radiation"~(W~m^{-2}))))
      }
    }
    if (emismeth == "both") {
      fcorr.brunt <- with(est.dat,
                          -(rld - rlu) / (emis.brunt * sig * (ta + 273.15) ^ 4))
      fcorr.idso <- with(est.dat,
                         -(rld - rlu) / (emis.idso * sig * (ta + 273.15) ^ 4))
    } else {
      fcorr <- with(est.dat,
                    -(rld - rlu) / (emis * sig * (ta + 273.15) ^ 4))
    }

    # estimate fcorr_a, fcorr_b from fcorr, rsd, rx
    rsd <- read.delim(grfile, sep="\t")
    rsd <- xts(rsd[, 2], order.by=as.POSIXct(rsd[, 1], tz="UTC"))
    rx <- read.delim(rxfile, sep="\t")
    rx <- xts(rx[, 2], order.by=as.POSIXct(rx[, 1], tz="UTC"))
    if (emismeth == "both") {
      est.dat <- merge(fcorr.brunt, fcorr.idso, rsd, rx, all=FALSE)
      names(est.dat)[1:2] <- c("fcorr.brunt", "fcorr.idso")
    } else {
      est.dat <- merge(fcorr, rsd, rx, all=FALSE)
      names(est.dat)[1] <- "fcorr"
    }
    ix <- as.numeric(format(index(est.dat), "%H")) < 17 &
          as.numeric(format(index(est.dat), "%H")) > 7 &
          est.dat$rx != 0
    est.dat$rsdmax <- (radex_a + radex_b) * est.dat$rx
    if (emismeth == "both") {
      lm.brunt <- with(est.dat[ix],
                       lm(as.numeric(fcorr.brunt) ~ as.numeric(rsd / rsdmax)))
      isct.brunt <- coef(lm.brunt)[1]
      mod.brunt <- lm(c(isct.brunt, 1) ~ c(0, 1))
      # ...
      # resume here
      # ...
      mod.idso <- with(est.dat[ix],
                       lm(as.numeric(fcorr.idso) ~ as.numeric(rsd / rsdmax)))
    } else {
      mod <- with(est.dat[ix],
                  lm(as.numeric(fcorr) ~ as.numeric(rsd / rsdmax)))
    }
    if (plots) {
      if (emismeth == "both") {
        pdf("doku/plot_fcorr_both.pdf")
        par(mfrow=c(2, 1), mar=c(3, 4, 1, 1))
        with(est.dat[ix],
             plot(as.numeric(rsd / rsdmax), as.numeric(fcorr.brunt),
                  xaxt="n", main="", xlab="", ylab="fcorr"))
        axis(1, at=seq(0, 1, 0.2), labels=seq(0, 1, 0.2))
        abline(mod.brunt, col=4)
        text(0.5, 1.2, "emis: Brunt")
        par(mar=c(4, 4, 0, 1))
        with(est.dat[ix],
             plot(as.numeric(rsd / rsdmax), as.numeric(fcorr.idso),
                  main="", xlab=expression(R[inS]/R[inS,cs]),
                  ylab="fcorr"))
        abline(mod.idso, col=4)
        text(0.5, 1.3, "emis: Idso & Jackson")
        dev.off()
      } else {
        with(est.dat[ix],
             plot(as.numeric(rsd / rsdmax), as.numeric(fcorr),
                  main=emismeth, xlab=expression(R[inS]/R[inS,cs]),
                  ylab="fcorr"))
        abline(mod, col=4)
      }
    }

    # return parameters
    if (emismeth == "both") {
      return(data.frame(Method=c("Brunt", "Idso & Jackson"),
                        a=c(as.numeric(coef(mod.brunt)[1]),
                            as.numeric(coef(mod.idso)[1])),
                        b=c(as.numeric(coef(mod.brunt)[2]),
                            as.numeric(coef(mod.idso)[2]))))
    } else {
      return(data.frame(a=as.numeric(coef(mod)[2]),  # fcorr_a
                        b=as.numeric(coef(mod)[1])))  # fcorr_b
    }

  } else if (length(grep("emis", parname)) != 0) {
  # emis parameters

    # It's not possible to estimate emis_a, emis_b from the available data.
    return(c(.34, -.14))

  } else if (length(grep("f", parname)) != 0) {
  # soil heat fraction parameters

    # load RAtmosphere library for calculating sunrise/sunset hours
    library(RAtmosphere)
    # read net radiation (internally calculated)
    rad_net <- read.delim(rnetfile, sep="\t")
    rad_net.xts <- xts(rad_net[, 2],
                       order.by=as.POSIXct(rad_net[, 1], tz="UTC"))
    # read soil heat flux (record)
    soilheat <- read.delim(sheatfile, sep="\t")
    soilheat.xts <- xts(soilheat[, 2],
                        order.by=as.POSIXct(soilheat[, 1], tz="UTC"))
    # select common time window
    tstart <- max(index(rad_net.xts)[1], index(soilheat.xts)[1])
    tend <- min(tail(index(rad_net.xts), 1), tail(index(soilheat.xts), 1))
    rnet <- rad_net.xts[paste0(tstart, "/", tend)]
    sheat <- soilheat.xts[paste0(tstart, "/" ,tend)]
    # divide into daytime and nighttime
    sun <- suncalc(as.numeric(format(index(rnet), "%j")), lat, lon)
    rnet.day <- rnet[as.numeric(format(index(rnet), "%H")) > sun$sunrise
                     & as.numeric(format(index(rnet), "%H")) < sun$sunset]
    rnet.night <- rnet[as.numeric(format(index(rnet), "%H")) < sun$sunrise
                       | as.numeric(format(index(rnet), "%H")) > sun$sunset]
    sheat.day <- sheat[as.numeric(format(index(sheat), "%H")) > sun$sunrise
                       & as.numeric(format(index(sheat), "%H")) < sun$sunset]
    sheat.night <- sheat[as.numeric(format(index(sheat), "%H")) < sun$sunrise
                         | as.numeric(format(index(sheat), "%H")) > sun$sunset]
    # diagnostic plots
    if (plots) {
      # time window
      t.win <- "2014-05-01/2014-05-20"
      # heat ratio
      heat.ratio.day <- abs(sheat.day[t.win]) / abs(rnet.day[t.win])
      heat.ratio.night <- abs(sheat.night[t.win]) / abs(rnet.night[t.win])
      # daytime plots
      plot(sheat.day[t.win], ylim=c(-22, 750),
           ylab="black: sheat.day, red: rnet.day", main="")
      lines(rnet.day, col=2)
      plot(heat.ratio.day, ylab="soil heat/net radiation", main="day")
      # nighttime plots
      plot(sheat.night[t.win], ylim=c(-50, 100),
           ylab="black: sheat.night, red: rnet.night", main="")
      lines(rnet.night, col=2)
      plot(heat.ratio.night, ylab="soil heat/net radiation", main="night")
    }

    # return parameters
    return(c(mean(c(abs(sheat.day) / abs(rnet.day)), na.rm=T),  # f_day
             mean(c(abs(sheat.night) / c(rnet.night)), na.rm=T)))  # f_night

  } else {
    
    stop("Unknown parameter name. Possible choices: radex*, fcorr*, f*, emis*, alb.")
  
  }
  
}

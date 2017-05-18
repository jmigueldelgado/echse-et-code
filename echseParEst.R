################################################################################
# Author: Julius Eberhard
# Last Edit: 2017-05-18
# Project: ECHSE evapotranspiration
# Function: echseParEst
# Aim: Estimation of Model Parameters from Observations,
#      Works for alb, emis_*, f_*, fcorr_*, radex_*
# TODO(2017-05-12): L344 (emis estimation)
################################################################################

echseParEst <- function(parname,  # name of parameter group to estimate
                                  # [radex_[a/b], fcorr_[a/b], emis_[a/b],
                                  # f_[day/night], alb]
                        rsdfile = NA,  # file with global radiation data*
                        hrfile = NA,  # file with relative humidity data*
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

  EmisBrunt <- function(emis_a,  # emissivity parameter a (intersect)
                        emis_b,  # emissivity parameter b (slope)
                        vap  # water vapor pressure, in kPa (!)
                        ) {
    # calculates net emissivity between ground and atmosphere
    # after Brunt (1932)

    return(emis_a + emis_b * sqrt(vap))
  }

  EmisIdso <- function(ta  # mean air temperature, in degC
                       ) {
    # calculates net emissivity between ground and atmosphere
    # after Idso & Jackson (1969), modified by Maidment (1993)

    return(-0.02 + 0.261 * exp(-7.77E-4 * ta ^ 2))
  }

  VapMagnus <- function(ta,  # mean air temperature, in degC
                        hr  # relative humidity, in %
                        ) {
    # calculates vapor pressure in hPa
    # using the Magnus equation, see Dyck & Peschke

    return(6.11 * 10 ^ (7.5 * ta / (237.3 + ta)) * hr / 100)
  }

  ReadToXts <- function(file  # path to file, 1st column POSIX-like dates,
                              #               2nd column data
                        ) {
    # reads delimiter separated file and converts it to xts object

    return(xts(read.delim(file)[, 2],
               order.by=as.POSIXct(read.delim(file)[, 1])))
  }

  ReadToHlyMean <- function(file  # path to xts object
                            ) {
    # reads xts object (with sub-hourly data) and returns hourly means as xts

    f <- readRDS(file)
    ep <- endpoints(f, on="hours") + 1
    return(xts(period.apply(f, ep[-length(ep)], mean),
               order.by=index(f)[ep[-c(1, length(ep))]]))
  }

  GenerateEstDat <- function(vars  # vector of variable names
                             ) {
    # generates common xts object of data required for parameter estimation

    vars.ls <- list()
    for (i in 1:length(vars)) {
      if (vars[i] %in% c("rsd", "rsu", "rld", "rlu")) {
        vars.ls[[i]] <- ReadToHlyMean(get(paste0(vars[i], "file")))
      } else {
        vars.ls[[i]] <- ReadToXts(get(paste0(vars[i], "file")))
      }
    }

    est.dat <- vars.ls[[1]]
    if (length(vars.ls) > 1) {
      for (i in 2:length(vars))
        est.dat <- merge(est.dat, vars.ls[[i]], join="inner")
      names(est.dat) <- vars
      return(est.dat)
    }
  }

  if (length(grep("alb", parname)) != 0) {
  # albedo
  # requires: rsd, rsu

    # collect estimation data
    est.dat <- GenerateEstDat(c("rsd", "rsu"))
    # restrict to times between 8:00 and 16:00 to avoid odd night effects
    # and to times when gr & rsu != 0
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
  # requires: rsd, rx

    # collect estimation data
    est.dat <- GenerateEstDat(c("rx", "rsd"))
    # restrict to times between 8:00 and 16:00 to avoid odd night effects
    # and to times when rx != 0 and where rsd > 50 W.m-2
    ix <- as.numeric(format(index(est.dat), "%H")) < 17 &
          as.numeric(format(index(est.dat), "%H")) > 7 &
          est.dat$rx != 0 & est.dat$rsd > 50
    # calculate ratio of rx and rsd
    rad.ratio <- with(est.dat[ix], as.numeric(rsd) / as.numeric(rx))
    # maximum ratio per hour
    MaxRadRatio <- function(i) {
      out <- NA
      if (any(as.numeric(format(index(est.dat[ix]), "%H")) == i))
        rad.ratio2 <- rad.ratio[as.numeric(format(index(est.dat[ix]),
                                                  "%H")) == i]
      if (exists("rad.ratio2")) {
        out <- max(rad.ratio2[rad.ratio2 < 1], na.rm=T)
        return(out)
      }
    }
    r.max <- max(sapply(8:16, MaxRadRatio), na.rm=T)
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
        plot(as.numeric(format(index(est.dat$rx[ix]), "%H")), rad.ratio,
             ylim=c(0, 1), xlab="hour of day", ylab="glorad/radex")
        abline(h=quantile(rad.ratio, r.quantile, na.rm=T), lty="dashed")
        if (S)
          break
        pdf("doku/plot_radex2.pdf")
        S <- TRUE
      }
      dev.off()
      # plot extraterr. and global radiation to detect time shifts
      plot(est.dat$rx, type="l", ylim=c(0, max(as.numeric(est.dat$rx))),
           xlab="date", ylab="rad (black: rx, red: gr)", main="")
      lines(est.dat$rsd, col=2)
    }
    # return parameters
    out <- c(# radex_a
             as.numeric(quantile(rad.ratio, r.quantile, na.rm=T)),
             # radex_b
             r.max - as.numeric(quantile(rad.ratio, r.quantile, na.rm=T)))
    return(out)

  } else if (length(grep("fcorr", parname)) != 0) {
  # fcorr parameters
  # requires: hr, rld, rlu, rsd, rx, ta, radex_a, radex_b

    # collect estimation data
    est.dat <- GenerateEstDat(c("ta", "hr", "rld", "rlu", "rsd", "rx"))

    # calculate net emissivity between ground and atmosphere
    if (emismeth == "Idso") {
      est.dat$emis <- EmisIdso(est.dat$ta)
    } else if (emismeth == "Brunt") {
      est.dat$vap <- VapMagnus(est.dat$ta, est.dat$hr)
      # vap in kPa!
      est.dat$emis <- EmisBrunt(0.34, -0.14, est.dat$vap / 10)
    } else if (emismeth == "both") {
    # both methods for comparison
      est.dat$vap <- VapMagnus(est.dat$ta, est.dat$hr)
      est.dat$emis.idso <- EmisIdso(est.dat$ta)
      # vap in kPa!
      est.dat$emis.brunt <- EmisBrunt(0.34, -0.14, est.dat$vap / 10)
    } else {
      stop("Unknown emismeth! Choose either 'Idso' or 'Brunt' or 'both'.")
    }

    # calculate fcorr (adapted Stefan-Boltzmann law)
    sig <- 5.670367E-8  # Stefan constant
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
      est.dat$f.brunt <- with(est.dat,
                              -(rld - rlu) /
                                (emis.brunt * sig * (ta + 273.15) ^ 4))
      est.dat$f.idso <- with(est.dat,
                             -(rld - rlu) /
                               (emis.idso * sig * (ta + 273.15) ^ 4))
    } else {
      est.dat$f <- with(est.dat, -(rld - rlu) / (emis * sig * (ta + 273.15) ^ 4))
    }

    # estimate fcorr_a, fcorr_b from fcorr, rsd, rx
    ix <- as.numeric(format(index(est.dat), "%H")) < 17 &
          as.numeric(format(index(est.dat), "%H")) > 7 &
          est.dat$rx != 0
    est.dat$rsdmax <- (radex_a + radex_b) * est.dat$rx
    if (emismeth == "both") {
      lmod.brunt <- with(est.dat[ix],
                         lm(as.numeric(f.brunt) ~ as.numeric(rsd / rsdmax)))
      lmod.idso <- with(est.dat[ix],
                        lm(as.numeric(f.idso) ~ as.numeric(rsd / rsdmax)))
      isct.brunt <- coef(lmod.brunt)[1]
      isct.idso <- coef(lmod.idso)[1]
      # Force models to explain (x, y) == (1, 1)
      # because fcorr_a + fcorr_b must = 1.
      # Intersect of model taken from original linear regression.
      mod.brunt <- lm(c(isct.brunt, 1) ~ c(0, 1))
      mod.idso <- lm(c(isct.idso, 1) ~ c(0, 1))
    } else {
      lmod <- with(est.dat[ix], 
                   lm(as.numeric(f) ~ as.numeric(rsd / rsdmax)))
      # See previous comment.
      mod <- lm(c(coef(lmod)[1], 1) ~ c(0, 1))
    }
    # suggested model by Shuttleworth in Maidment (1993)
    mod.maid <- lm(c(-0.35, 1) ~ c(0, 1))

    if (plots) {
      if (emismeth == "both") {
        pdf("doku/plot_fcorr_both.pdf")
        par(mfrow=c(2, 1), mar=c(3, 4, 1, 1))
        # plot data with Brunt model
        with(est.dat[ix],
             plot(as.numeric(rsd / rsdmax), as.numeric(f.brunt),
                  xaxt="n", main="", xlab="", ylab="fcorr"))
        axis(1, at=seq(0, 1, 0.2), labels=seq(0, 1, 0.2))
        abline(mod.brunt, col=4)
        abline(mod.maid, lty="dashed", col=4)
        legend("topleft", c("adapted regression", "Maidment (1993)"),
               lty=c("solid", "dashed"), col=c(4, 4))
        text(0.5, 1.2, "emis: Brunt (1932)")
        par(mar=c(4, 4, 0, 1))
        with(est.dat[ix],
             plot(as.numeric(rsd / rsdmax), as.numeric(f.idso),
                  main="", xlab=expression(R[inS]/R[inS*","*cs]),
                  ylab="fcorr"))
        abline(mod.idso, col=4)
        abline(mod.maid, lty="dashed", col=4)
        legend("topleft", c("adapted regression", "Maidment (1993)"),
               lty=c("solid", "dashed"), col=c(4, 4))
        text(0.5, 1.3, "emis: Idso & Jackson (1969)")
        dev.off()
      } else {
        with(est.dat[ix],
             plot(as.numeric(rsd / rsdmax), as.numeric(f),
                  main=emismeth, xlab=expression(R[inS]/R[inS,cs]),
                  ylab="fcorr"))
        abline(mod, col=4)
        abline(mod.maid, lty="dashed", col=4)
        legend("topleft", c("adapted regression", "Maidment (1993)"),
               lty=c("solid", "dashed"), col=c(4, 4))
      }
    }

    # return parameters
    if (emismeth == "both") {
      return(data.frame(Method=c("Brunt", "Idso & Jackson"),
                        a=c(as.numeric(coef(mod.brunt)[2]),
                            as.numeric(coef(mod.idso)[2])),
                        b=c(as.numeric(coef(mod.brunt)[1]),
                            as.numeric(coef(mod.idso)[1]))))
    } else {
      return(data.frame(a=as.numeric(coef(mod)[2]),  # fcorr_a
                        b=as.numeric(coef(mod)[1])))  # fcorr_b
    }

  } else if (length(grep("emis", parname)) != 0) {
  # emis parameters
  # requires: hr, rsd, rld, rlu, rx, ta, radex_a, radex_b

    # collect estimation data
    est.dat <- GenerateEstDat(c("rsd", "rx", "rld", "rlu", "ta", "hr"))
    est.dat$rsdmax <- (radex_a + radex_b) * est.dat$rx

    # calculate vapor pressure, Magnus equation
    est.dat$vap <- VapMagnus(est.dat$ta, est.dat$hr)
    sig <- 5.670367E-8  # Stefan constant

    # compare "observed" emissivity with models of Brunt and Idso-Jackson:
    # (1) select times when global radiation is approximately "clear-sky"
    ix <- with(est.dat, rsd / rsdmax > .9 & rsd / rsdmax <= 1)
    # (2) calculate net emissivity with Stefan-Boltzmann (f assumed to be 1)
    # TODO(2017-05-12): check lw balance: emis is very low, maybe select from rl?
    # ...
    # resume here
    # ...
    est.dat$emis <- with(est.dat[ix],
                         - (rld - rlu) / (sig * (ta + 273.15) ^ 4))
    # (3) plot observation-based emissivity against models
    pdf("doku/plot_emis_both.pdf", height=5, width=9)
    par(mfrow=c(1, 2))
    with(est.dat[ix],
         plot(as.numeric(EmisBrunt(0.34, -0.14, est.dat[ix]$vap / 10)), emis,
              xlab=expression(epsilon*", predicted by Brunt model"),
              ylab=expression(epsilon*", derived from observations")))
    lines(0:1, 0:1)
    with(est.dat[ix],
         plot(as.numeric(EmisIdso(est.dat[ix]$ta)), emis, ylab="",
              xlab=expression(epsilon*", predicted by Idso-Jackson model")))
    lines(0:1, 0:1)
    dev.off()

    # It's not possible to estimate emis_a, emis_b from the available data.
    return(c(.34, -.14))

  } else if (length(grep("f", parname)) != 0) {
  # soil heat fraction parameters

    # load RAtmosphere library for calculating sunrise/sunset hours
    library(RAtmosphere)

    # collect estimation data
    est.dat <- GenerateEstDat(c("rnet", "sheat"))

    # divide into daytime and nighttime
    sun <- suncalc(as.numeric(format(index(est.dat$rnet), "%j")), lat, lon)
    ix.day <- as.numeric(format(index(est.dat$rnet), "%H")) > sun$sunrise &
              as.numeric(format(index(est.dat$rnet), "%H")) < sun$sunset
    ix.night <- as.numeric(format(index(est.dat$rnet), "%H")) < sun$sunrise |
                as.numeric(format(index(est.dat$rnet), "%H")) > sun$sunset
    est.dat$rnet.day <- est.dat$rnet[ix.day]
    est.dat$rnet.night <- est.dat$rnet[ix.night]
    est.dat$sheat.day <- est.dat$sheat[ix.day]
    est.dat$sheat.night <- est.dat$sheat[ix.night]

    # diagnostic plots
    if (plots) {
      # time window
      t.win <- "2014-05-01/2014-05-20"
      # heat ratio
      heat.ratio.day <- with(est.dat[t.win],
                             abs(sheat.day) / abs(rnet.day))
      heat.ratio.night <- with(est.dat[t.win],
                               abs(sheat.night) / abs(rnet.night))
      # daytime plots
      plot(est.dat$sheat.day[t.win], ylim=c(-22, 750),
           ylab="black: sheat.day, red: rnet.day", main="")
      lines(est.dat$rnet.day, col=2)
      plot(heat.ratio.day, ylab="soil heat/net radiation", main="day")
      # nighttime plots
      plot(est.dat$sheat.night[t.win], ylim=c(-50, 100),
           ylab="black: sheat.night, red: rnet.night", main="")
      lines(est.dat$rnet.night, col=2)
      plot(heat.ratio.night, ylab="soil heat/net radiation", main="night")
    }

    # return parameters
    return(with(est.dat,
                c(# f_day
                  mean(c(abs(sheat.day) / abs(rnet.day)), na.rm=T),
                  # f_night
                  mean(c(abs(sheat.night) / abs(rnet.night)), na.rm=T))))

  } else {
    
    stop("Unknown parameter name. Possible choices: radex*, fcorr*, f*, emis*, alb.")
  
  }
  
}

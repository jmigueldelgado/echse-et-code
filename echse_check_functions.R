# functions, taken from ECHSE code

res_stom_leaf <- function(res_min,
                          cond_vap,
                          cond_water
                          ) {

  res_min / (cond_vap * cond_water)

}

stress_soilwater <- function(wc,
                             wc_sat,
                             wc_res,
                             bubble,
                             pores_ind,
                             wstressmin,
                             wstressmax
                             ) {

  # problems with this function; see original code (resistances.h)

  sat_rel <- (wc - wc_res) / (wc_sat - wc_res)
  m <- pores_ind / (pores_ind + 1)

  if (sat_rel >= 0.999) {
    suction <- 0
  } else {
    suction <- (1 / (sat_rel^(1 / m)) - 1)^(1 / (pores_ind + 1)) * bubble
  }

  if (suction < wstressmin) {
    return(1)
  } else if (suction >= wstressmax) {
    return(0.01)
  } else {
    return(1 - (suction - wstressmin) / (wstressmax - wstressmin))  # Guentner 2002
  }
}

stress_humidity <- function(vap_deficit,
                            par
                            ) {

  # problems with this function; see original code (resistances.h)

  return(1 / (1 + par * vap_deficit))
}

satVapPress_overWater <- function(temp
                                  ) {
 return(6.11 * 10^(7.5 * temp / (237.3 + temp)))
}

vapPress_overWater <- function(temp,
                               relhum
                               ) {
  return(satVapPress_overWater(temp) * relhum / 100)
}

slopeSatVapPress <- function(temp
                             ) {
  return(satVapPress_overWater(temp) * 4098. / (237.3 + temp)^2)
}


latentHeatEvap <- function(temp
                           ) {
  return(2501. - 2.37 * temp)
}

psychroConst <- function(temp,
                         airpress
                         ) {
  return(0.016286 * airpress / latentHeatEvap(temp));
}



et_sw <- function(lambda,  # Latent heat of water evaporation (J/kg)
                  delta,  # slope of saturation vapor pressure curve (hPa/K)
                  H_net,  # net incoming ( (1-alb) * short-wave + long-wave) radiation (at reference/measurement height) (Wm-2)
                  H_soil,	 # net incoming (short-wave + long-wave) radiation hitting the soil surface (Wm-2)
                  totalheat,  # heat conduction into soil AND plants (Wm-2)
                  soilheat,  # soil heat flux (Wm-2)
                  rho_air,  # air density (kgm-3)
                  ez_0,	 # saturation vapor pressure of air (hPa)
                  ez,	 # vapor pressure of air (hPa)
                  gamma,  # psychrometric constant (hPa/K)
                  r_cs,	 # Bulk stomatal resistance of the canopy (sm-1)
                  r_ca,	 # Bulk boundary layer resistance of the vegetative elements in the canopy (sm-1)
                  r_ss,	 # soil surface resistance (sm-1)
                  r_sa,	 # aerodynamic resistance between soil surface and canopy source height (sm-1)
                  r_aa  # Aerodynamic resistance between canopy source height and reference/measurement height (sm-1)
                  ) {

  # Total available energy (Wm-2), Suttleworth & Wallace (1985), eq. 3
  A_total <- H_net - totalheat

  # Energy available at substrat (Wm-2), Suttleworth & Wallace (1985), eq. 5
  A_s <- H_soil - soilheat

  # calculate vapor pressure deficit at reference/measurement height (hPa)
  D <- ez_0 - ez;

  # calculate term of canopy transpiration (SW eq. 12) (Wm-2)
  PM_c <- (delta * A_total + ((rho_air * SPHEATMOIST * D) - (delta * r_ca * A_s)) / (r_aa + r_ca)) /
    (delta + (gamma * (1 + r_cs / (r_aa + r_ca))))

  # calculate term of soil evaporation (SW eq. 13) (Wm-2)
  PM_s <- (delta * A_total + ((rho_air * SPHEATMOIST * D) - (delta * r_sa * (A_total-A_s)) ) / (r_aa + r_sa)) /
    (delta + (gamma * (1. + r_ss / (r_aa + r_sa))))

  # SW eqs. 16-18
  R_a <- (delta + gamma) * r_aa
  R_s <- (delta + gamma) * r_sa + gamma * r_ss
  R_c <- (delta + gamma) * r_ca + gamma * r_cs

  # coefficients, SW eqs. 14, 15 (-)
  C_c <- 1. / (1. + R_c * R_a / (R_s * (R_c + R_a)))
  C_s <- 1. / (1. + R_s * R_a / (R_c * (R_s + R_a)))

  # compute evapotranspiration rate, eq. 11 (mm/s)
  et <- (C_c * PM_c + C_s * PM_s) / lambda

  # different from ECHSE function: returns only et/1000, possibly < 0!
  return(et/1000.)
}




# my parameters

bubble <- 8.08
par_stressHum <- 0.03  # Guentner, WASA code
pores_ind <- 0.45
res_leaf_min <- 50  # s.m-1
wc_res <- 0.049
wc_sat <- 0.387
wstressmax <- 15000
wstressmin <- 10

# read data

library(xts)

rhum <- read.delim("~/uni/projects/evap_portugal/data/forcing/meteo/05_meteofill/out/HS/rhum_data.dat")
rhum <- xts(rhum[, 2], order.by=as.POSIXct(rhum[, 1]))
temper <- read.delim("~/uni/projects/evap_portugal/data/forcing/meteo/05_meteofill/out/HS/temper_data.dat")
temper <- xts(temper[, 2], order.by=as.POSIXct(temper[, 1]))
wc_vol_root <- read.delim("~/uni/projects/evap_portugal/data/forcing/meteo/05_meteofill/out/HS/wc_vol_root_data.dat")
wc_vol_root <- xts(wc_vol_root[, 2], order.by=as.POSIXct(wc_vol_root[, 1]))
mdata <- merge(rhum, temper, wc_vol_root, all=FALSE)

# calculate internal (and possibly erroneous) results

ez0 <- c()
ez <- c()
cond_vap <- c()
cond_water <- c()
res_leaf <- c()

for (ix in 1:2000) { #seq(index(mdata)[1], tail(index(mdata))[1], by="hours")) {

  ez0[ix] <- with(mdata[ix], satVapPress_overWater(temper))
  ez[ix] <- with(mdata[ix], vapPress_overWater(temper, rhum))

  cond_vap[ix] <- with(mdata[ix], stress_humidity(ez0[ix] - ez[ix], par_stressHum))
  cond_water[ix] <- with(mdata[ix], stress_soilwater(wc_vol_root, wc_sat, wc_res, bubble, pores_ind, wstressmin, wstressmax))

  res_leaf[ix] <- res_stom_leaf(res_leaf_min, cond_vap[ix], cond_water[ix])

}

#H_net <- (1. - alb) * glorad + H_long

par(mfrow=c(2, 1), mar=c(4, 4, 0, 1))
plot(cond_water, type="l")
plot(wc_vol_root[1:2000], main="")

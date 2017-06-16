
echseDepend <- function(x,  # opted variable name
                        choice_et,
                        ch_gloradmax,
                        ch_rcs,
                        ch_roughLen,
                        ch_plantDispl) {

# parameters (individual and shared)
for (i in c("bubble",       # individual
            "crop_faoref",
            "crop_makk",
            "elev",
            "glo_half",
            "lat",
            "lon",
            "par_stressHum",
            "pores_ind",
            "res_leaf_min",
            "soil_dens",
            "wc_etmax",
            "wc_pwp",
            "wc_res",
            "wc_sat",
            "wstressmin",
            "wstressmax",
            "drag_coef",    # shared
            "eddy_decay",
            "emis_a", 
            "emis_b",
            "ext",
            "fcorr_a",
            "fcorr_b",
            "f_day",
            "f_night",
            "h_humMeas",
            "h_tempMeas",
            "h_windMeas",
            "radex_a",
            "radex_b",
            "res_b",
            "rough_bare",
            "rss_a",
            "rss_b")) {
  assign(i, list(i))
  attributes(i) <- list(type="parameter")
}

# pure internal variables (i.e. calculated from internal parameters)


# pure input variables
alb <- list("alb")
  attributes(alb) <- list(type="input")
cano_height <- list("cano_height")
  attributes(cano_height) <- list(type="input")
cloud <- list("cloud") 
  attributes(cloud) <- list(type="dummy input")
doy <- list("doy")
  attributes(doy) <- list(type="input")
hour <- list("hour")
  attributes(hour) <- list(type="input")
lai <- list("lai")
  attributes(lai) <- list(type="input")
rad_net_soil <- list("rad_net_soil")
  attributes(rad_net_soil) <- list(type="input")
rhum <- list("rhum")
  attributes(rhum) <- list(type="input")
sundur <- list("sundur")
  attributes(sundur) <- list(type="input")
temp_max <- list("temp_max")
  attributes(temp_max) <- list(type="input")
temp_min <- list("temp_min")
  attributes(temp_min) <- list(type="input")
utc_add <- list("utc_add")
  attributes(utc_add) <- list(type="input")
wc_vol_root <- list("wc_vol_root")
  attributes(wc_vol_root) <- list(type="input")
wc_vol_top <- list("wc_vol_top")
  attributes(wc_vol_top) <- list(type="input")
wind <- list("wind")
  attributes(wind) <- list(type="input")

# composite variables
radex <- lapply(list(doy,
                     hour,
                     lat,
                     lon,
                     utc_add), unlist)
apress <- lapply(list(elev), 
                 unlist)
temper <- lapply(list(temp_max, 
                      temp_min), unlist)
glorad <- lapply(list(cloud,
                      doy,
                      lat,
                      radex,
                      radex_a,
                      radex_b,
                      sundur), unlist)
glorad_max <- lapply(ifelse(ch_gloradmax==1, 
                            list(radex,
                                 radex_a,
                                 radex_b),
                            list(radex,
                                 elev)), unlist)
rad_net <- lapply(list(alb,
                       emis_a,
                       emis_b,
                       fcorr_a,
                       fcorr_b,
                       glorad,
                       glorad_max,
                       rhum,
                       temper), unlist)
rad_long <- lapply(list(emis_a,
                        emis_b,
                        fcorr_a,
                        fcorr_b,
                        glorad,
                        glorad_max,
                        rhum,
                        temper), unlist)
soilheat <- lapply(list(rad_net,
                        f_day,
                        f_night), unlist)
totalheat <- lapply(list(rad_net,
                         f_day,
                         f_night), unlist)

unique(unlist(get(x)))

}
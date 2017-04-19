
# TODO:
# - what value of field capacity to use? 316 hPa, 63 hPa or 330 hPa (Wikipedia)?
# - nfk should rather be fk - theta_pwp insteadt of fk - theta_r (Wikipedia)?
# - in doc of pft.rawls suction: problem, maybe of units? All calculated values larger than ranges in Maidment Table 5.5.5)
#   - in pft.rawls: ret_val[, "S_f"] = ret_val[, "S_f"] * 10 -> values in mm? But nevertheless values are still high
# - ksat sometimes zero (very low clay and/or silt contents)?!
# - fk sometimes larger than theta_s?!
# - theta_r sometimes larger than pwp
# - water content values sometimes unrealistic (compared to ranges given in Maidment)

install.packages("soilwaterfun", repos="http://R-Forge.R-project.org")
library(soilwaterfun)
#install.packages("soilwaterptf", repos="http://R-Forge.R-project.org")
library(soilwaterptf)
# or subversion (Linux commands):
# svn checkout svn://scm.r-forge.r-project.org/svnroot/soilwater/
# if your R version is too old and you don't want to update R:
# get an older package version with 'svn update PATH -r REV' PATH=path to download dir, REV=revision number you want
# install that version: 'R CMD INSTALL FILE' FILE=tar.gz file with package source data

#install.packages("soiltexture")
library(soiltexture)


### INPUT ###

setwd("/home/tobias/Promotion/Modellierung/Bengue/Preprocessing/soil_parameters/")

# ascii file containing basic soil information and texture data
# needed: ID, description, flag if soil is alluvial (1:alluvial, 0:not alluvial),
# flag if there is bedrock below soil profile (1:yes, 0:no), no of layers, for every layer: 
# depth from top of soil to bottom of layer, bulk density [g/cm3]=[kg/dm3],
# organic carbon, silt, clay, coarse fragments each in [%] of soil weight
file_soil_basic <- "soil_properties_jacomine.dat"

# output directory
dir_out <- "out_t"

# particle size [mm] classes definition of your soil data source
part_def <- data.frame(class_id=c(1,2,3),
                       description=c("clay", "silt", "sand"),
                       upper_limit=c(0.002, 0.05, 2))

# define value ranges for all parameters to issue a warning if strange values are calculated
#par_ranges <- data.frame(theta_r = c())

### INTERNAL FUNCTION K FACTOR ###
# computes k factor based on sand, silt, clay and organic carbon content in [%]
# according to Williams 1995, p. 934 (eq. 25.123)
k_factor <- function(sand, silt, clay, oc) {
  
  temp1 <- 0.2 + 0.3*exp(-0.0256*sand*(1-silt/100))
  
  temp2 <- ( silt / (clay+silt) )^0.3
  
  temp3 <- 1.0 - ( 0.25*oc / (oc+exp(3.72-2.95*oc)) )
  
  sn1 <- 1 - sand/100
  
  temp4 <- 1.0 - ( 0.7*sn1 / (sn1+exp(-5.51+22.9*sn1)) )
  
  k_fac <- temp1 * temp2 * temp3 * temp4
  
  return(k_fac)
}



### CALCULATIONS ###

# read parameters
soil_par <- read.table(file_soil_basic, header=T, quote="", sep="\t")

# ksat from mm/h to mm/day
r_ks <- grep("ks_", names(soil_par))
soil_par[,r_ks] <- soil_par[,r_ks] * 24

# prepare horizon specific output object
hor_par_out <- matrix(data=NA, nrow=1, ncol=18,
                       dimnames=list(NULL, c("pid","description","soil_id","position","theta_r",
                                             "theta_pwp","fk","fk63","nfk","theta_s","thickness",
                                             "ks","suction","pore_size_i","bubb_pres","coarse_frag",
                                             "shrinks", "soil_dens")))

# prepare soil profile specific output object
soil_par_out <- matrix(data=NA, nrow=nrow(soil_par), ncol=13,
                       dimnames=list(NULL, c("pid", "description", "bedrock_flag", "alluvial_flag", "b_om",
                                             "a_musle_k", "a_clay", "a_silt", "a_sand", "a_f_csand", "a_f_cl_si",
                                             "a_f_orgc", "a_f_hisand")))

# prepare particle distribution output object
part_distr_out <- matrix(data=NA, nrow=nrow(soil_par)*3, ncol=3,
                         dimnames=list(NULL,c("soil_id","class_id","fraction")))

i <- 0
# loop over soil types
for (s in 1:nrow(soil_par)) {
  
  # number of layers for this soil
  nlay <- soil_par[s,]$nlayer
  
  # collect soil profile specific information
  soil_par_out[s,"pid"] <- soil_par[s, "ID"]
  soil_par_out[s,"description"] <- paste0(soil_par[s, "descr"])
  soil_par_out[s,"bedrock_flag"] <- soil_par[s, "bedrock"]
  soil_par_out[s,"alluvial_flag"] <- soil_par[s, "alluvial"]
  
  # compute musle k factor
  soil_par_out[s,"a_musle_k"] <- k_factor(soil_par[s,"sand_1"],
                                        soil_par[s,"silt_1"],
                                        soil_par[s,"clay_1"],
                                        soil_par[s,"oc_1"]
                                        )
  
  # topsoil organic mater content [-]
  soil_par_out[s,"b_om"] <- soil_par[s,"oc_1"] * 1.74 / 100
  
  # other parameters (needed for internal calculations later on, set NA)
  soil_par_out[s,7:13] <- NA
  
  # collection information about particle distribution (topsoil only)
  part_distr_out[((s-1)*3+1):((s-1)*3+3), "soil_id"] <- soil_par[s, "ID"]
  part_distr_out[((s-1)*3+1):((s-1)*3+3), "class_id"] <- 1:3
  part_distr_out[(s-1)*3+1, "fraction"] <- soil_par[s,"clay_1"]/100
  part_distr_out[(s-1)*3+2, "fraction"] <- soil_par[s,"silt_1"]/100
  part_distr_out[(s-1)*3+3, "fraction"] <- soil_par[s,"sand_1"]/100
  
  
  # loop over layers
  for (l in 1:nlay) {
    
    i <- i+1
    # depth (thickness) of horizon [mm]
    if (l == 1) {
      depth <- soil_par[s, paste0("depth_", l)]
    } else {
      depth <- soil_par[s, paste0("depth_", l)] - soil_par[s, paste0("depth_", l-1)]
    }
    
    if (depth <= 9)
      stop(paste0("For soil ", s, " horizon ", l, " depth <= 9 mm which seems to be very unlikely!"))
    
    # prepare data.frame of soil properties as input for ptfs
    soilprop <- data.frame(clay = soil_par[s, paste0("clay_", l)],
                           bulkD = soil_par[s, paste0("bulkd_", l)],
                           silt = soil_par[s, paste0("silt_", l)],
                           om = soil_par[s, paste0("oc_", l)] * 1.74 #convert OC to OM [%]
                           )
    
    # basic soil information to output object
    out_t <- c(NA, paste(soil_par[s,"description"], l, sep="_"), soil_par[s, "ID"], l)
    
    # residual volumetric water content - theta_r
    theta_r <- pft.rawls(soilprop, parameters="theta_r", h=0)[,"theta_r"]
    
    # vol. water content at permanent wilting point (1500 kPa/15000 cm) - theta_pwp
    theta_pwp <- pft.rawls(soilprop, parameters="theta", h=15000)[,"theta"]
    
    # vol. water content at field capacity (316 hPa / pF=2.6) - fk
    fk <- pft.rawls(soilprop, parameters="theta", h=316)[,"theta"]
    
    # vol. water content at field capacity (63 hPa / pF=1.8) - fk63
    fk63 <- pft.rawls(soilprop, parameters="theta", h=63)[,"theta"]
    
    # usable field capacity - nfk
    nfk <- fk - theta_r
    
    # Saturated volumetric soil water content - theta_s
    theta_s <- ptf.wosten.theta.s(soilprop$clay, soilprop$bulkD, soilprop$silt,
                                  soilprop$om, ifelse(l==1,1,0))
    
    # make sure that values are plausible (use values from wikipedia, https://en.wikipedia.org/wiki/Water_content, and Maidment: Handbook of Hydrology)
    if(theta_r > theta_pwp)
      warning(paste0("For soil no. ", s, ", layer ", l, ": residual water content is greater than permanent wilting point!"))
    
    if(theta_pwp > fk)
      warning(paste0("For soil no. ", s, ", layer ", l, ": permanent wilting point is greater than field capacity!")) 
    
    if(fk > theta_s)
      warning(paste0("For soil no. ", s, ", layer ", l, ": field capacity is greater than saturated water content!"))
    
    if(theta_s > 0.5 | theta_s < 0.2)
      warning(paste0("For soil no. ", s, ", layer ", l, ": value of saturated water content is ", theta_s, " wich seems rather unlikely!"))
      
    if(fk > 0.35 | fk < 0.1)
      warning(paste0("For soil no. ", s, ", layer ", l, ": value of field capacity is ", fk, " wich seems rather unlikely!"))
    
    if(theta_pwp >       0.25 | theta_pwp < 0.01)
      warning(paste0("For soil no. ", s, ", layer ", l, ": value of permanent wilting point is ", theta_pwp, " wich seems rather unlikely!"))
    
    if(theta_r > 0.1 | theta_r < 0.001)
      warning(paste0("For soil no. ", s, ", layer ", l, ": value of residual water content is ", theta_r, " wich seems rather unlikely!"))
    
    
    # saturated hydraulic conductivity - ks [mm/day]
    if(!any(grepl("ks", names(soil_par)))) {
      ks <- ptf.wosten.ksat(soilprop$clay, soilprop$bulkD, soilprop$silt,
                             soilprop$om, ifelse(l==1,1,0), c("mm", "day"))
    } else {
      ks <- soil_par[s,paste0("ks_", l)]
    }

    if(ks > 6000 | ks < 10)
      warning(paste0("For soil no. ", s, ", layer ", l, ": value of saturated hydraulic conductivity is ", ks, " mm/day wich seems rather unlikely!"))
    
    
    # suction suction at the wetting front - suction [cm]
    suction <- pft.rawls(soilprop, parameters="S_f", h=0)[,"S_f"]
    
    if(suction > 150 | suction < 1)
      warning(paste0("For soil no. ", s, ", layer ", l, ": value of suction at wetting front is ", suction, " cm wich seems rather unlikely!"))
    
    
    # pore-size-index - pore_size_i
    lambda <- pft.rawls(soilprop, parameters="lambda", h=0)[,"lambda"]
    
    if(lambda > 1 | lambda < 0.04)
      warning(paste0("For soil no. ", s, ", layer ", l, ": value of pore-size-index is ", lambda, " wich seems rather unlikely!"))
    
    
    # bubbling pressure - bubb_pres [cm]
    bubb_pres <- pft.rawls(soilprop, parameters="h_b", h=0)[,"h_b"]
    
    if(bubb_pres > 190 | bubb_pres < 1)
      warning(paste0("For soil no. ", s, ", layer ", l, ": value of bubbling pressure is ", bubb_pres, " cm wich seems rather unlikely!"))
    
    
    # write into output object
    out_t <- c(i, paste(soil_par[s,"descr"], l, sep="_"), soil_par[s, "ID"], l,
              theta_r, theta_pwp, fk, fk63, nfk, theta_s, depth, ks, suction, lambda, 
              bubb_pres, soil_par[s, paste0("rock_", l)]/100, 0, soilprop$bulkD*1000)
    hor_par_out <- rbind(hor_par_out, out_t)
  }
}

hor_par_out <- hor_par_out[-1,]

# write output
write.table(hor_par_out, file=paste0(dir_out, "/horizons.dat"), quote=F, sep="\t",
            row.names=F)

write.table(soil_par_out, file=paste0(dir_out, "/soil.dat"), quote=F, sep="\t",
            row.names=F)

write.table(part_distr_out, file=paste0(dir_out, "/r_soil_contains_particles.dat"), quote=F, sep="\t",
            row.names=F)

write.table(part_def, file=paste0(dir_out, "/particle_classes.dat"), quote=F, sep="\t",
            row.names=F)

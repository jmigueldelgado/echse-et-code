################################################################################
# Author: Julius Eberhard
# Last Edit: 2017-06-04
# Project: ECHSE evapotranspiration
# Functions: echseCtrl_cnf, echseCtrl_out, echseCtrl_loc, echseCtrl_fil, 
#            echseCtrl_shpar, echseCtrl_par, echseCtrl_ini
# Aim: Writing Control Files for ECHSE Engine
################################################################################

echseCtrl_cnf <- function(engine,  # engine name
                          tstart,  # start of model period
                          tend,  # end of model period
                          dt  # time step
                          ) {

  # writes main control file

  writeLines(c(
    "########################################################################",
    "# Computational settings",
    "trap_fpe=true",
    "#multithread=false",
    "number_of_threads=1",
    "singlethread_if_less_than=1",
    "########################################################################",
    "# Declaration of models (id and group)",
    "table_objectDeclaration=../data/catchment/objDecl.dat",
    "########################################################################",
    "# Definition of the simulation time window",
    paste0("simStart=", tstart),
    paste0("simEnd=", tend),
    paste0("delta_t=", dt),
    "########################################################################",
    "# Specification of model outputs",
    "table_selectedOutput=output_selection.txt",
    "table_debugOutput=output_debug.txt",
    "table_stateOutput=output_state.txt",
    paste("output_columnSeparator=", "", sep="\t"),
    "output_lineComment=#",
    "#outputDirectory=./out/",
    "outputFormat=tab",
    "saveFinalState=true",
    "########################################################################",
    "# Special characters in all input files (except for this control file)",
    "input_columnSeparator=	 ;",
    "input_lineComment=#",
    "########################################################################",
    "# Initial values",
    "table_initialValues_scal=../data/initials/init_scal.dat",
    "table_initialValues_vect=../data/initials/init_vect.dat",
    "########################################################################",
    "# Boundary conditions",
    "table_inputOutputRelations=../data/catchment/objLink.dat",
    "externalInput_bufferSize=1",
    "table_externalInput_locations=../data/forcing/inputs_ext_locations.dat",
    "table_externalInput_datafiles=../data/forcing/inputs_ext_datafiles.dat",
    "########################################################################",
    "# Class-specific input files",
    "########################################################################",
    "# dummy parameters",
    "dummy_numParamsIndividual=../data/parameter/dummy_num.dat",
    "dummy_funParamsIndividual=../data/parameter/dummy_fun.dat",
    "dummy_numParamsShared=../data/parameter/dummy_num.dat",
    "dummy_funParamsShared=../data/parameter/dummy_fun.dat",
    paste0("# ", engine, " parameters"),
    paste0(engine,
           "_numParamsIndividual=../data/parameter/paramNum_WASA_svc.dat"),
    paste0(engine,
           "_funParamsIndividual=../data/parameter/dummy_fun.dat"),
    paste0(engine,
           "_numParamsShared=../data/parameter/sharedParamNum_WASA_svc.dat"),
    paste0(engine,
           "_funParamsShared=../data/parameter/dummy_fun.dat")),
    con=paste0("~/uni/projects/", engine, "/run/cnf_default"))
  
}

#----

echseCtrl_out <- function(engine,    # engine name
                          output,    # output variable
                          et.choice  # etp or eta
                          ) {

  # writes control file for output selection

  writeLines(c(
    "#",
    "# Selection of output models and variables.",
    "#",
    "# Required columns are:",
    "#   'object': ID of the object",
    "#   'variable': Variable (must be an output of the respective model family)",
    "#",
    "# Further columns with meta-information may follow. These columns must also be",
    "# readable (i.e. properly separated) but the contents is ignored.", 
    "",
    "object    variable  digits",
    "",
    paste0("test1     ", ifelse(output == "evap", et.choice,
                                paste0(output, "_out")), "       10")),
    con=paste0("~/uni/projects/", engine, "/run/output_selection.txt"))

}

#----

echseCtrl_loc <- function(engine,     # engine name
                          needed,     # list of 0/1 info on required data
                          available,  # list of 0/1 info on data availability
                          locs        # list of location names for input data
                          ) { 

  # writes control file for external input locations

  object <- "test1"
  variable <- unlist(mapply(function(x, y) {
                              if (y == 1) return(x)
                            }, names(needed), needed))
  location <- unlist(mapply(function(x, y, z) {
                              if (x == 1) {
                                if (y == 1) z else "dummy"
                              }
                            }, needed, available, locs))
  weight <- 1
  write.matrix(data.frame(object=rep(object, length(variable)), 
                          variable, location,
                          weight=rep(weight, length(variable))),
               paste0("~/uni/projects/", engine, 
                      "/data/forcing/inputs_ext_locations.dat"),
               sep="\t")
  
}

#----

echseCtrl_fil <- function(engine,
                          field.station,
                          needed,
                          available,
                          past  # logical; set past as true or false?
                          ) {

  # writes control file for data file directories

  l <- unlist(
         mapply(function(x, y, z) {
                  if (x==1)
                    paste(y, ifelse(y=="sundur", "  true", "  false"),
                          ifelse(past, "  true", "  false"), "  ../data/",
                          ifelse(z == 0,
                                 "forcing/meteo/dummy_ts.dat",
                                 ifelse(y == "alb" || y == "cano_height"
                                        || y == "doy" || y == "hour"
                                        || y == "lai" || y == "utc_add",
                                        paste0("vegPar_time_series/",
                                               field.station, "/", y,
                                               "_data.dat"),
                                        paste0("forcing/meteo/05_meteofill/out/",
                                               field.station, "/", y,
                                               "_data.dat"))),
                          sep="")
                }, needed, names(needed), available)
       )

  writeLines(c(
    "# Specification of files from which the models' external inputs is to be read.",
    "# For each external variable (used by any class), a single record is required.",
    "",
    "# 'variable': string - Name of the external input variable.",
    "# 'file':     string - File containing the time series data for that variable.",
    "# 'sums':     bool - Treat data as sums (not as averages) when disaggregating?",
    "# 'past':     bool - Do times refer to the end (not the start) of an interval?",
    "",
    "variable  sums  past  file",
    "",
    l), con=paste0("~/uni/projects/", engine, 
                   "/data/forcing/inputs_ext_datafiles.dat"))
}

#----

echseCtrl_shpar <- function(engine,
                            parameters,
                            needed
                            ) { 

  # writes control file with shared scalar parameters

  parameter <- unlist(mapply(function(x, y) {
                               if (y == 1) return(unlist(x))
                             }, names(parameters), needed))
  value <- unlist(mapply(function(x, y) {
                           if (y == 1) return(x)
                         }, parameters, needed))
  if (length(parameter > 0))
    write.matrix(data.frame(parameter, value), 
                 paste0("~/uni/projects/", engine, 
                        "/data/parameter/sharedParamNum_WASA_svc.dat"),
                 sep="\t")
}

#----

echseCtrl_par <- function(engine,
                          parameters,  # list of par values
                          needed  # list specifying which par are needed
                          ) {

  # writes control file with individual scalar parameters

  parameter <- c("object",
                 unlist(mapply(function(x, y) {
                                 if (y == 1) return(unlist(x))
                               }, names(parameters), needed)))
  value <- c("test1",
             unlist(mapply(function(x, y) {
                             if (y == 1) return(as.numeric(x))
                           }, parameters, needed)))
  write(c(parameter, value), ncolumns=length(parameter), 
        paste0("~/uni/projects/", engine, 
               "/data/parameter/paramNum_WASA_svc.dat"), 
        sep="\t")
}

#----

echseCtrl_ini <- function(engine,
                          state.var,  # vector of state variable names
                          initials  # vector of initial values
                          ) {

  # writes control file with initial values of state variables

  object <- "test1"
  write.matrix(data.frame(object, variable=state.var, value=initials), 
               paste0("~/uni/projects/", engine, 
                      "/data/initials/init_scal.dat"), 
               sep="\t")
}

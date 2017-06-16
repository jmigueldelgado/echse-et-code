################################################################################
# Author: Julius Eberhard
# Last Edit: 2017-06-16
# Project: ECHSE Evapotranspiration
# Function: echseInput
# Aim: Writing Data Input Files for ECHSE Engines
################################################################################

echseInput <- function(engine,       # engine name
                       variable,     # variable name in engine
                       const = NA,   # constant value if var. not in data lists
                       na.val = 0,   # NA value, maybe a better solution soon..
                       stn = "Met",  # station name/data frame containing data
                       column = NA,  # column index in xts object
                       t.seq,        # time sequence
                       directory     # target directory or path, 
                                     # starting from "<engine>/data/...",
                                     # ending before "...variable_data.dat"
                       ) {

  if (is.na(const)) {
  # recorded value
    if (stn == "tower") {
      var.df <- data.frame(end_of_interval=index(tower[[column]][t.seq]),
                           tower[[column]][t.seq],
                           row.names=seq(nrow(tower[[column]][t.seq])))
    } else if (stn == "HS" || stn == "any") {
      var.df <- data.frame(end_of_interval=index(HS[[column]][t.seq]), 
                           HS[[column]][t.seq],
                           row.names=seq(nrow(HS[[column]][t.seq])))
    } else if (stn == "NSA") {
      var.df <- data.frame(end_of_interval=index(NSA[[column]][t.seq]),
                           NSA[[column]][t.seq],
                           row.names=seq(nrow(NSA[[column]][t.seq])))
    }
  } else {
  # constant value
    var.df <- data.frame(end_of_interval=index(tower[[1]][t.seq]),
                         const,
                         row.names=seq(nrow(tower[[1]][t.seq])))
  }

  if (length(which(is.na(var.df[, 2]))) != 0)
    var.df[which(is.na(var.df[, 2])), 2] <- na.val
  names(var.df)[2] <- ifelse(stn == "tower" || stn == "HS" || stn == "NSA" ||
                             stn == "any",
                             stn,
                             ifelse(stn == "le", "tower", "any"))
  write.matrix(var.df,
               paste0(directory, variable, "_data.dat"), sep="\t")

}

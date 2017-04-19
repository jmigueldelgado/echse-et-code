###############################################################################
## ECHSE evapotranspiration
## Functions: ptfWoesten, ptfWeynants, ptfRawls
## Aim: Computing Van Genuchten Parameters with Pedotransfer Functions
## Author: Julius Eberhard
## Last Edit: 2016-10-06
###############################################################################

# after Woesten et al. 1999:

ptfWoesten <- function(clay,  # in %
                       silt,  # in %
                       sand,  # in %
                       som,  # soil organic matter in %
                       topsoil  # boolean, TRUE if less than 30 cm of depth
                       ) {

  bd <- 1.5  # estimated from literature values (e.g. wikipedia/Porosity)
  topsoil <- ifelse(topsoil, 1, 0)
  theta_s <- .7919 + .001691*clay - .29619*bd - .000001491*silt^2 + 
             .0000821*som + .02427/clay + .01113/silt + .01472*log(silt) - 
             .0000733*som*clay - .000619*bd*clay - .001183*bd*som - 
             .0001664*topsoil*silt
  theta_r <- 0
  list(theta_r, theta_s)

}


# after Weynants et al. 2009:

ptfWeynants <- function(clay,  # in %
                        silt,  # in %
                        sand,  # in %
                        som,  # soil organic matter in %
                        topsoil  # boolean, TRUE if less than 30 cm of depth
                        ) {

  bd <- 1.5  # bulk density, est. from literature values (e.g. wikipedia/Porosity)
  theta_s <- .6355 + .0013*clay - .1631*bd
  theta_r <- 0
  list(theta_r, theta_s)

}


# after Rawls & Brakensiek:

ptfRawls <- function(clay,  # in %
                     silt,  # in %
                     sand,  # in %
                     som,  # soil organic matter in %
                     topsoil  # boolean, TRUE if less than 30 cm of depth
                     ) {

  bd <- 1.5  # bulk density, est. from literature values (e.g. wikipedia/Porosity)
  por <- .4  # porosity, est. from literature values
  theta_s <- 1 - bd/2.65
  theta_r <- -.0182482 + .00087269*sand + .00513488*clay + .02939286*por - 
             .00015395*clay^2 - .0010827*sand*por - .00018233*clay^2*por^2 + 
             .00030703*clay^2*por - .0023584*clay*por^2

  list(theta_r, theta_s)

}
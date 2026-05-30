# File hydromodInR.R
# Part of the hydroPSO R package, https://github.com/hzambran/hydroPSO
#                                 http://cran.r-project.org/web/packages/hydroPSO
#                                 http://www.rforge.net/hydroPSO/
# Copyright 2024-2026 Mauricio Zambrano-Bigiarini & Rodrigo Rojas
# Distributed under GPL 2 or later

################################################################################
#                           hydromod                                           #
################################################################################
# Purpose    : This is a wrapper function for the internal 'hydromodInR.eval'  #
#              function.                                                       #
#              To run an R-based hydrological/environmental model for a single #
#              particle, and get a goodness-of-fit value by comparing the      #
#              simulated values against observations                           #
################################################################################
# Output     : A list of two elements:                                         #
#              1) sim: simulated values obtained by running the hydrological   #
#                      model                                                   #
#              2) GoF: goodness-of fit of the simualted values against observed#
#                      ones, by using THE USER-DEFINED 'gof' measure           # 
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
# Started: 19-Nov-2020                                                         #
# Updates: 10-Jul-2024                                                         #
#          30-May-2026                                                         #
################################################################################
hydromodInR <- function(
                        part=1, 
                        Particles, 
                        model.FUN, 
                        model.FUN.args
                        ) {

  if (missing(Particles)) {
    Particles <- part
    part <- 1
  } # IF end

  if (is.null(dim(Particles))) {
    if (!is.numeric(Particles))
      stop("Invalid argument: 'Particles' must be a numeric vector, matrix or data.frame")

    particle.names <- names(Particles)
    if (is.null(particle.names) || any(is.na(particle.names) | !nzchar(particle.names)))
      stop("Invalid argument: when 'Particles' is a vector, it must be named")

    if (length(part) != 1 || is.na(part) || part != 1)
      stop("Invalid argument: when 'Particles' is a vector, 'part' must be 1")

    Particles <- matrix(Particles, nrow=1,
                        dimnames=list(NULL, particle.names))
  } # IF end
                     
  hydromodInR.eval(part, Particles, model.FUN, model.FUN.args)

} # 'hydromodInR' END

# File hydromodInR.R
# Part of the hydroPSO R package, https://github.com/hzambran/hydroPSO
#                                 http://cran.r-project.org/web/packages/hydroPSO
#                                 http://www.rforge.net/hydroPSO/
# Copyright 2024-2024 Mauricio Zambrano-Bigiarini & Rodrigo Rojas
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
################################################################################
hydromodInR <- function(
                        part, 
                        Particles, 
                        model.FUN, 
                        model.FUN.args
                        ) {
                     
  hydromodInR.eval(part, Particles, model.FUN, model.FUN.args)

} # 'hydromodInR' END

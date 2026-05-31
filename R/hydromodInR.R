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
#          30-May-2026 ; 31-May-2026                                           #
################################################################################
hydromodInR <- function(
                        part=1, 
                        param.values, 
                        model.FUN, 
                        model.FUN.args,
                        ...
                        ) {

  dots <- list(...)
  dot.names <- names(dots)
  if (is.null(dot.names)) dot.names <- rep("", length(dots))
  invalid.args <- setdiff(dot.names, "Particles")
  if (length(invalid.args) > 0)
    stop("Invalid argument(s): ", paste(invalid.args, collapse=", "))

  if ("Particles" %in% dot.names) {
    if (!missing(param.values))
      stop("Invalid argument: use either 'param.values' or deprecated 'Particles', not both")
    warning("'Particles' is deprecated; use 'param.values' instead.", call.=FALSE)
    param.values <- dots[["Particles"]]
  } # IF end

  if (missing(param.values)) {
    param.values <- part
    part <- 1
  } # IF end

  if (is.null(dim(param.values))) {
    if (!is.numeric(param.values))
      stop("Invalid argument: 'param.values' must be a numeric vector, matrix or data.frame")

    particle.names <- names(param.values)
    if (is.null(particle.names) || any(is.na(particle.names) | !nzchar(particle.names)))
      stop("Invalid argument: when 'param.values' is a vector, it must be named")

    if (length(part) != 1 || is.na(part) || part != 1)
      stop("Invalid argument: when 'param.values' is a vector, 'part' must be 1")

    param.values <- matrix(param.values, nrow=1,
                           dimnames=list(NULL, particle.names))
  } # IF end
                     
  hydromodInR.eval(part, param.values, model.FUN, model.FUN.args)

} # 'hydromodInR' END

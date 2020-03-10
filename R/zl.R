# File zFirst.R
# Part of the hydroPSO R package, https://github.com/hzambran/hydroPSO
#                                 http://cran.r-project.org/web/packages/hydroPSO
#                                 http://www.rforge.net/hydroPSO/
# Copyright 2011-2020 Mauricio Zambrano-Bigiarini & Rodrigo Rojas
# Distributed under GPL 2 or later

.onAttach <- function(libname, pkgname) {

  packageStartupMessage("(C) 2011-2020 M. Zambrano-Bigiarini and R. Rojas (GPL >=2 license)\n",
                         "Type 'citation('hydroPSO')' to see how to cite this package")
  invisible()
    
} # '.onAttach' END


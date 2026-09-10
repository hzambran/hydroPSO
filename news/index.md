# Changelog

## Changes in version 0.6-3 10-Sep-2026

### Enhancements

``` R
    o 'read_out', 'read_results', and 'plot_results' have a new argument 'skip.incompatible.obs' to allow post-processing to continue when the observed series and simulated outputs in 'Model_out.txt' have incompatible lengths. The default preserves the previous strict error.

    o 'read_verification_results' is a new function for reading the 'Verification-ModelOut.txt' and 'Verification-ParamValues.txt' files written by 'verification(write2disk=TRUE)' and rebuilding the four main verification outputs from disk.

    o 'plot_verification_results' is a new function for plotting parameter-set and model-output diagnostics from files written by 'verification(write2disk=TRUE)'.
```

## Changes in version 0.6-1 01-Jun-2026 (after addressing CRAN comments)

### Bug fixes

``` R
    o Fixed broken local vignette links in inst/doc/r-based-models.html by pointing them to the html vignette files that are shipped in inst/doc.
```

## Changes in version 0.6-0 31-May-2026 (after addressing CRAN comments in 2024)

### Enhancements:

``` R
    o 'hydroPSO'     : -) it has the following two new arguments: 'change.type' and 'refValue' to allow changing parameters in 
                          three different ways for **R functions** and **R-based models**: 
                            * 'replacement' (the old and default way in hydroPSO), 
                            * 'additive' (where the old value is summed up with the value computed during the optimisation), and 
                            * 'multiplicative' (where the old value is multiplied by the value computed during the optimisation). 
                          By default 'change.type="repl"'', which means all the paramters are changed by replacemnt, which is 
                          exactly the old behaviour previous to v0.6-0 -> no change in existing hydroPSO workflows.

                       -) now it allows to change parameters in three different ways for **R-external models**: 
                          * 'replacement' (the old and default way in hydroPSO), 
                          * 'additive' (where the old value is summed up with the value computed during the optimisation), and 
                          * 'multiplicative' (where the old value is multiplied by the value computed during the optimisation). 
                          The type of change is specified in the 'ParameterValues2InputFiles.R' and 'ModifyInputFile.R' files, 
                          using additional columns. 

                       -) ParamRanges.txt file now supports three new columns: 'TypeChange', 'Min4Change', 'Max4Change', 
                          representing respectively the type of change to be made to each parameter (replacement, additive, 
                          multiplicative), the absolute minimum value that a parameter can take, and the absolute maximum value 
                          that a parameter can take. When a paramteter take a value outside the range [Min4Change, MaxChange], 
                          it is set to its closest boundary. By default 'TypeChange="repl"'' for all parameters, which means all  
                          paramters are changed by replacemnt, which is exactly the old behaviour previous to v0.6-0 -> no change 
                          in existing hydroPSO workflows.

                       -) 'param.ranges' argument is no longer passed in the 'control' argument' but in 'model.FUN.args' to be 
                          used directly by the 'hydromod' function.

                       -) ParamFiles.txt file now supports one additional column 'RefValue', which is used as reference for 
                          additive or multiplicative changes.

                       -) Now it is possible to initialise particle's positions with quasi-random Sobol sequences, by using 
                          \code{Xini.type='sobol'} within the 'Xini.type' element of the 'control' argument.

                       -) Two new items within the 'control' argument in order to avoid modification of the .GlobalEnv variable  
                          when using PSOCK clusters (parallel == "parallelWin"), which require explicitly exporting objects. The 
                          two new items are:
                          - par.env: environment from which the objects required by the objective function \code{fn} are exported 
                                     to the parallel workers.
                          - par.export: names of additional objects to be exported from \code{par.env} to the parallel workers

    o 'hydromod'     : 
                       -) ParamRanges.txt file now supports three new columns: 'TypeChange', 'Min4Change', 'Max4Change', 
                          representing respectively the type of change to be made to each parameter (replacement, additive, 
                          multiplicative), the absolute minimum value that a parameter can take, and the absolute maximum value 
                          that a parameter can take. When a paramteter take a value outise the range [Min4Change, MaxChange], 
                          it is set to its closest boundary.

                       -) 'param.ranges' argument is no longer passed in the 'control' argument' but in 'model.FUN.args' to be 
                          used directly by the 'hydromod' function.

                       -) ParamFiles.txt file now supports one additional column 'RefValue', which is used as reference for 
                          additive or multiplicative changes.

                       -) Now it can run a single named parameter vector directly, in addition to matrix/data.frame objects

    o 'verification' : -) now it runs correctly when using "parallel='parallel'" both for R-based and R-external models.
                          It should also work correctly for "parallel='parallelWin'", but it could not be tested.

                       -) it has the following two new arguments: 'change.type' and 'refValue' to allow changing parameters in 
                          three different ways for **R functions** and **R-based models**: 
                            * 'replacement' (the old and default way in hydroPSO), 
                            * 'additive' (where the old value is summed up with the value computed during the optimisation), and 
                            * 'multiplicative' (where the old value is multiplied by the value computed during the optimisation). 
                          By default 'change.type="repl"'', which means all the paramters are changed by replacemnt, which is 
                          exactly the old behaviour previous to v0.6-0 -> no change in existing hydroPSO workflows.

                       -) now it allows to change parameters in three different ways for **R-external models**: 
                          * 'replacement' (the old and default way in hydroPSO), 
                          * 'additive' (where the old value is summed up with the value computed during the optimisation), and 
                          * 'multiplicative' (where the old value is multiplied by the value computed during the optimisation). 
                          The type of change is specified in the 'ParameterValues2InputFiles.R' and 'ModifyInputFile.R' files, 
                          using additional columns. 

                       -) ParamRanges.txt file now supports three new columns: 'TypeChange', 'Min4Change', 'Max4Change', 
                          representing respectively the type of change to be made to each parameter (replacement, additive, 
                          multiplicative), the absolute minimum value that a parameter can take, and the absolute maximum value 
                          that a parameter can take. When a paramteter take a value outside the range [Min4Change, MaxChange], 
                          it is set to its closest boundary. By default 'TypeChange="repl"'' for all parameters, which means all  
                          paramters are changed by replacemnt, which is exactly the old behaviour previous to v0.6-0 -> no change 
                          in existing hydroPSO workflows.

                       -) 'param.ranges' argument is no longer passed in the 'control' argument' but in 'model.FUN.args' to be 
                          used directly by the 'hydromod' function. 
                          'param.ranges' argument is not needed at all for R-based models ('fn="hydromodInR"').

                       -) ParamFiles.txt file now supports one additional column 'RefValue', which is used as reference for 
                          additive or multiplicative changes.

    o 'read.ParameterRanges': now has a new argument 'verbose'

    o 'read_out'            : -) new argument 'obs.tzone', only relevant for sub-daily observations. 
                                 Time zone used for reading the observations from the \sQuote{Observations.txt} output file.

    o 'read_results'        : -) new argument 'obs.tzone', only relevant for sub-daily observations. 
                                 Time zone used for reading the observations from the \sQuote{Observations.txt} output file.

    o 'plot_out'            : -) now it is compatible with sub-daily observations

    o 'plot_results'        : -) now it is compatible with sub-daily observations

                              -) new argument 'obs.tzone', only relevant for sub-daily observations. 
                                 Time zone used for reading the observations from the \sQuote{Observations.txt} output file.

                              -) graphical parameters are safely restored on exit.

                              -) new arguments: 'params.png.width' and 'params.png.height', to allow a higher degree of customisation in output figures.

    o 'plot_GofPerParticle' : improved management of graphical screen size when " ptype='many' ".

    o 'params2ecdf'         : graphical parameters are safely restored on exit.

    o 'plot_NparOF'         : graphical parameters are safely restored on exit.

    o 'plot_ParamsPerIter'  : graphical parameters are safely restored on exit.

    o 'plot_convergence'    : graphical parameters are safely restored on exit.

    o 'plot_params'         : graphical parameters are safely restored on exit.

    o 'read_particles'      : new arguments: 'params.png.width' and 'params.png.height', to allow a higher degree of customisation in output figures.

    o 'plot_particles'      : new arguments: 'params.png.width' and 'params.png.height', to allow a higher degree of customisation in output figures.
```

### Package files:

``` R
    o hydroPSO now has a graphical logo.  
    o hydroPSO now has a webpage (https://hzambran.github.io/hydroPSO/), created by pkgdown.  
    o CITATION file : The DOI was changed from the one given by Zenodo to the new DOI provided by CRAN (10.32614/CRAN.package.hydroPSO) since June 2024.
    o NAMESPACE file : 'hydromodInR.eval' is now exported.
    o New Vignette v1.1 for hydroPSO with TUWmodel     
    o New Vignette v0.5 for hydroPSO with GR4J     
    o New Vignette v0.4 for hydroPSO with CemaNeige-GR6J     
    o New Vignette v0.1 for hydroPSO with SWAT+     
```

### Documentation:

``` R
    o 'hydroPSO'               : -) Its name is written now in between single quotes in the Description field of the DESCRIPTION file.
                                 -) Two new items within the 'control' argument are now documented: 'par.env' and 'par.export'.
                                 -) Improved descritpion of the 'boundary.wall' item within the 'control' argument.
                                 -) Improved description of the 'parallel' argument

    o 'hydroPSO2pest'          : -) Now there is a \code{value} field in the documentation, explicitly exaplaining the output of this function.
    
    o 'params2ecdf'            : -) Now there is a \code{value} field in the documentation, explicitly exaplaining the output of this function.
    
    o 'plot_2parOF'            : -) Now there is a \code{value} field in the documentation, explicitly exaplaining the output of this function.
    
    o 'plot_NparOF'            : -) Now there is a \code{value} field in the documentation, explicitly exaplaining the output of this function.
                                 -) Now, \code{\dontrun} use in examples was replaced by \code{\donttest}, following CRAN recommendation.
                                 -) Change of the current diirectory to the home user directory was replaced by the usage of a temporal directory, which is then restored to the original directory where the user was located before running the example (oldwd <- getwd(), on.exit(setwd(oldwd)), setwd(tempdir()).
    
    o 'plot_ParamsPerIter'     : -) Now there is a \code{value} field in the documentation, explicitly exaplaining the output of this function.
                                 -) Now, \code{\dontrun} use in examples was replaced by \code{\donttest}, following CRAN recommendation.
                                 -) Change of the current diirectory to the home user directory was replaced by the usage of a temporal directory, which is then restored to the original directory where the user was located before running the example (oldwd <- getwd(), on.exit(setwd(oldwd)), setwd(tempdir()).
    
    o 'read_best'              : -) Now there is a \code{value} field in the documentation, explicitly exaplaining the output of this function.
    
    o 'ReadPlot_GofPerParticle': -) Now there is a \code{value} field in the documentation, explicitly exaplaining the output of this function.
                                 -) Now, \code{\dontrun} use in examples was replaced by \code{\donttest}, following CRAN recommendation.
                                 -) Change of the current diirectory to the home user directory was replaced by the usage of a temporal directory, which is then restored to the original directory where the user was located before running the example (oldwd <- getwd(), on.exit(setwd(oldwd)), setwd(tempdir()).

    o 'wquantile'              : -) Now there is a \code{value} field in the documentation, explicitly exaplaining the output of this function.
                                 -) Improved description of the function, its arguments and also better examples.

    o 'ReadPlot_results'       : -) Now, \code{\dontrun} use in examples was replaced by \code{\donttest}, following CRAN recommendation.
                                 -) Change of the current diirectory to the home user directory was replaced by the usage of a temporal directory, which is then restored to the original directory where the user was located before running the example (oldwd <- getwd(), on.exit(setwd(oldwd)), setwd(tempdir()).
                                 -) Improved description of the function and some arguments.

    o 'ReadPlot_particles'     : -) Now, \code{\dontrun} use in examples was replaced by \code{\donttest}, following CRAN recommendation.
                                 -) Change of the current diirectory to the home user directory was replaced by the usage of a temporal directory, which is then restored to the original directory where the user was located before running the example (oldwd <- getwd(), on.exit(setwd(oldwd)), setwd(tempdir()).

    o 'ReadPlot_params'        : -) Now, \code{\dontrun} use in examples was replaced by \code{\donttest}, following CRAN recommendation.
                                 -) Change of the current diirectory to the home user directory was replaced by the usage of a temporal directory, which is then restored to the original directory where the user was located before running the example (oldwd <- getwd(), on.exit(setwd(oldwd)), setwd(tempdir()).

    o 'ReadPlot_out'           : -) Now, \code{\dontrun} use in examples was replaced by \code{\donttest}, following CRAN recommendation.
                                 -) Change of the current diirectory to the home user directory was replaced by the usage of a temporal directory, which is then restored to the original directory where the user was located before running the example (oldwd <- getwd(), on.exit(setwd(oldwd)), setwd(tempdir()).

    o 'ReadPlot_convergence'   : -) Now, \code{\dontrun} use in examples was replaced by \code{\donttest}, following CRAN recommendation.
                                 -) Change of the current diirectory to the home user directory was replaced by the usage of a temporal directory, which is then restored to the original directory where the user was located before running the example (oldwd <- getwd(), on.exit(setwd(oldwd)), setwd(tempdir()).

    o 'read_best'              : -) Now, \code{\dontrun} use in examples was replaced by \code{\donttest}, following CRAN recommendation.
                                 -) Change of the current directory to the home user directory was replaced by the usage of a temporal directory, which is then restored to the original directory where the user was located before running the example (oldwd <- getwd(), on.exit(setwd(oldwd)), setwd(tempdir()).

    o 'quant2ecdf'             : -) Improved description of the function, its arguments and also better examples.

    o 'params2ecdf'            : -) Improved description of the function, its arguments and also better examples.
                                 -) Now, \code{\dontrun} use in examples was replaced by \code{\donttest}, following CRAN recommendation.

    o 'lhoat'                  : -) Now, \code{\dontrun} use in examples was replaced by \code{\donttest}, following CRAN recommendation.

    o 'hydroPSO'               : -) Now, \code{\dontrun} use in examples was replaced by \code{\donttest}, following CRAN recommendation.
```

### Bug fixes

``` R
    o 'plot_NparOF'            : The legend position is now correctly computed when "dp3D.names='auto'" and 'param.names' is used to define 
                                 a smaller number of model parameters to be plotted.
    o 'hydroPSO'               : now works corectly when the number of parameters to be optimised is equal to 1.
```

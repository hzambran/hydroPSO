NEWS for hydroPSO

--------------------------
# Changes in version 0.5-9  22-Jan-2024

## Bug fixes

        o 'hydroPSO'     : -) When 'fn='hydromodInR', all the arguments defined in 'model.FUN.args' are now passed to the execution of 'model.FUN'. Existing vignettes of TUWmodel (v0.93) y GR4J (v0.3), because of the way the 'TUWhydromod' function was built in those vignettes.
                           -) Parallel execution now works correctly under Windows OS. Previously, errors like "object 'XX' not found" appeared when hydroPSO was run in parallel under Windows OS. This problem did not ocurred in GNU/Linux systems. Thanks Rodrigo Marinao for providing a solution.

        o 'verification' : -) now it allows parallel computations ! (with progress bar).
                           -) When 'fn='hydromodInR', now all the arguments defined in 'model.FUN.args' are passed to the execution of 'model.FUN'. Existing vignettes of TUWmodel (v0.93) y GR4J (v0.3), because of the way the 'TUWhydromod' function was consruted in those vignettes.
                           -) Parallel execution now works correctly under Windows OS. Previously, errors like "object 'XX' not found" appeared when hydroPSO was run in parallel under Windows OS. This problem did not ocurred in GNU/Linux systems. Thanks Rodrigo Marinao for providing a solution.

        o 'hydromod'     : -) new argument 'exe.args' to allow passing additional argument ot the model executable file (e.g., Raven https://cran.r-project.org/package=Rraven) .
                           -) now it allows to change parameters in three different ways: 'replacement' (the old and default way in hydroPSO), 'additive' (where the old value is summed up with the value computed during the optimisation), and 'multiplicative' (where the old value is multiplied by the value computed during the optimisation). Changes were carried out in the 'ParameterValues2InputFiles.R' and 'ModifyInputFile.R' files.


        o 'modifyInputFile' -) Since this version it is possible to make additive and multiplicative changes in parameter values, in addition to the traditional 'replacement' type of change. This change implied the addition of the following new arguments: 'change.type', 'refValue', 'minValue', 'maxValue'. This change is compatible with previous versions of hydroPSO.
                            -) new argument: 'change.type', to indicate if the parameter value change is by replacement ('repl', the default), multiplicative ('mult') or additive ('addi').
                            -) new argument: 'refValue': numeric, only used when TypeChange == "mult" |  TypeChange == "addi", reference value for making de parameter modification
                            -) new argument: 'minValue'
                            -) new argument: 'maxValue'
                            -) This is an internal function, not visible for the user. However, it is quite important for the modification of the model input files.

        o New function 'hydromodinR' to run an R-based model using a single parameter set taken from a matrix/data.frame with many parameter sets. This function is for internal use within the package, and it is only implemented in case it be useful to other packages.

## Package files:

        o NAMESPACE file : 'pblapply' and 'pboptions' are now imported from the 'pbapply' package.

        o CITATION file  : citEntry changed to bibentry, after notes that prevented acceptance on CRAN.

        o Now it passes TRAVIS-CI without any warning ('tcl' and 'tk' were added to .travis.yml to avoid error related to the installation of 'vioplot' and 'sm')

        o Github Actions are used now to test the source code against stable and development R versions on Windows, Ubuntu and MacOS.
  
        o pkgdown is now used to created the webpage of the package: https://hzambran.github.io/hydroTSM.

        
# 0.5-1	28-Apr-2020
        o Fixed error "object 'GoF' not found". This error appeared in version 0.5-0 when "fn" is neither "hydromod" nor "hydromodInR".
        o Updated [GR4J](https://CRAN.R-project.org/package=airGR) [vignette v0.3](https://doi.org/10.5281/zenodo.3774533), now including uncertainty in flow duration curves. 
        o Updated TUWmodel](https://CRAN.R-project.org/package=TUWmodel) [vignette v0.9.3](https://doi.org/10.5281/zenodo.3772176), now including uncertainty in flow duration curves.

 
# 0.5-0	17-March-2020
        o Package tested against R 4.0.0 (unstable) (2020-02-27 r77862) -- "Unsuffered Consequences", following an imperative request made by CRAN.
        o Added full compatibility with (hydrological/environmental) models implemented as R functions (e.g., TUWmodel, GR4, etc) 
        o New vignette showing how to calibrate TUWmodel with hydroPSO.
        o New vignette showing how to calibrate TUWmodel with GR4J.
        o New dataset 'Trancura9414001' with daily data on P, Temp, PET, and Q from 1979 to 2016.
        o A reference to the 2012 vignette showing how to use hydroPSO to calibrate SWAT2005 and MODFLOW 2005 is now included in the package.
        o 'hydroPSO'      : -) the 'fn' argument now accepts the character 'hydromodInR' to indicate that hydroPSO will be used to optimise an (hydrological/environmental) model implemented as R function. 
                            -) When 'fn="hydromodInR"', the argument 'model.FUN' must provide and R function used to run the model and evaluate the goodness-of-fit corresponding to a given parameter set.
                              This function MUST fulfill the following requirements:
                              1) It first agument must be named 'param.values', which represents the numer values of the parameters to be optimised
                              2) One of its arguments must be named 'obs', and represents the observed values whcih will be compared against the simulated values ('sim')
                              3) It must return a list object with -at least- the following elements:
                                 3.1) "GoF": the goodness-of-fit of obtained when the hydrological model is run with 'param.values' as parameters
                                 3.2) "sim": the model output obtained when the hydrological model is run with 'param.values' as parameters. 'sim' MUST have the same object class (and dimensions) thn the observed vlues provided by the user in 'obs'.
                              4) It is a good practice, even if is not a reuirement, to identify all the arguments used by the evaluation of the model, in order to have them correctly identified in the 'hydroPSO_logfile.txt' output file.
                            -) testing if 'parallel' package is installed is now optmised, replacing 'if ( !is.na( match("parallel", installed.packages()[,"Package"] ) ) )' by '( length(find.package("parallel", quiet=TRUE)) == 0 )'
                            -) testing if 'lhs' package is installed is now optmised, replacing 'if ( !is.na( match("lhs", installed.packages()[,"Package"] ) ) )' by '( length(find.package("lhs", quiet=TRUE)) == 0 )'
                            -) broken link in documtnation related to swarm topolies was fixed by using Lynn et al. (2018)
                            -) if 'multicore' option is selected, it is changed automatically to 'parallel' (because the 'multicore' pacakge was merged with the base 'parallel' pacakge.
        o 'verification'  : -) now it is fully compatible with R-based models and allows parallelisation
                            -) simulations for each parameter sets are now returned in the new 'sims' lement of the output object.
                            -) new parameter '...' ensure compatibility with any R function already calibrated with hydroPSO.
        o 'quant2ecdf'    : Detection of 'matrix' and 'data.frame' class for its first argument is now made with the 'is' function, in order to be compatible with R 4.0.0
        o 'read_out'      : -) If 'MinMax' is not provided, it is read from the 'PSO_logfile.txt' file
                            -) Much faster now: it uses data.table:fread instead of 'read.table'
        o 'read_results'  : -) If 'MinMax' is not provided, it is read from the 'PSO_logfile.txt' file
                            -) Much faster now: thanks to the improvement in 'read_out'
        o 'plot_results'  : -) If 'MinMax' is not provided, it is read from the 'PSO_logfile.txt' file
                            -) Much faster now: thanks to the improvement in 'read_out'
                          : testing if 'zoo' package is installed is now optmised, replacing 'if ( !is.na( match("zoo", installed.packages()[,"Package"] ) ) )' by '( length(find.package("zoo", quiet=TRUE)) == 0 )'
        o 'plot_2parOF'   : testing if 'scatter3d' package is installed is now optmised, replacing 'if ( !is.na( match("scatter3d", installed.packages()[,"Package"] ) ) )' by '( length(find.package("scatter3d", quiet=TRUE)) == 0 )'
        o 'hydromod'      : testing if 'hydroGOF' package is installed is now optmised, replacing 'if ( !is.na( match("hydroGOF", installed.packages()[,"Package"] ) ) )' by '( length(find.package("hydroGOF", quiet=TRUE)) == 0 )'
        o 'plot_out'      : testing if 'hydroGOF' package is installed is now optmised, replacing 'if ( !is.na( match("hydroGOF", installed.packages()[,"Package"] ) ) )' by '( length(find.package("hydroGOF", quiet=TRUE)) == 0 )'
        o DESCRIPTION     : -) 'knitr' and 'rmarkdown' were added to allow building the vignettes
                            -) ORCHID reference was added for M. Zambrano-Bigiarini
                            -) Added dependency on R >= 3.5.0 because serialized objects in serialize/load version 3 cannot be read in older versions of R ( 'hydroPSO/data/Trancura9414001.RData')
                            -) 'data.table' was added tom 'Imports' field 
        o NAMESPACE       : 'fread' is now imported from the 'data.table' package

# 0.4-1	12-Jun-2018
        o 'Keywords' field removed after CRAN request.
        o DOI added just after zenodo created it, based on Github realease 0.4-0

# 0.4-0	10-Jun-2018
        o Repository management moved from SVN to GIT, first using rforge and finally using Github.
        o Main page in Github list articles reporting the use of hydroPSO until May 31th 2018.
        o Package tested against R version 3.5.0 (2018-04-23) -- "Joy in Playing", following the request made by CRAN.
        o TRAVIS CI used to test source code against stable and development R versions.
        o zenodo used to manage DOI of different versions of the package

# 0.3-4-6	09-May-2016
        o New field 'Additional_repositories' in the DESCRIPTION file, following the instructions given by CRAN.
        o Minor modification of the 'Description' field, following the instructions given by CRAN (avoiding redundancies).
        o CITATION file modified to allow working without package installed (following CRAN request). 
        o DOI field added to CITATION file.
        o CRAN website removed from URL field in DESCRIPTION file (following CRAN request).
        o 'read_out'    : observations (in 'obs' or 'Observations.txt') are allowed to be of class (Date", "POSIXct", "POSIXt"), not only 'Date' as before this enhancement.
        o 'plot_results': observations (obtained with 'read_results.txt') are allowed to be of class (Date", "POSIXct", "POSIXt"), not only 'Date' as before this enhancement.
        o 'hydroPSO'    : -) Issue #4. hydroPSO is now able to optimise 1D functions, both R-defined functions or R-external model codes (Thanks to Azza Ahmed and @cjzilverberg!). 
                          -) Issue #3. hydroPSO now is able to correctly apply the 'method="wfips"' when 'topology="random"' (Thanks to @drknexus !). 
                                       For a 2-dimensioanl problem wit 40 particles, it raised an error similar to:
                                         "Error in is.data.frame(x) : 
                                          dims [product 2] do not match the length of object [40]"
                          -) minor bug fixed (Thanks to Aaron Jones !). 
                             This bug might have triggered when the user provided more than 1 parameter set as a first guess for the optimal solution (i.e, a matrix/data.frame), 
                             using the 'par' optional argument. Some of the parameters provided by the user might have been incorrectly compared against the 'lower' and 'upper' values. 


# 0.3-4	11-Abr-2014
        o Package tested against R version 3.1.0 (2014-04-10) -- "Spring Dance".
        o 'lhoat'              : -) when 'normalise=TRUE' the 'ParameterSets' output is now given in the real range of each parameter, and not in the 
                                    normalised range [0, 1] as up to version 0.3.2. The 'lhoat-gof.txt' output file has always been written in the real 
                                    range of each parameter when 'normalise=TRUE'. Thanks to Romain Lardy !
                                 -) fixed error: 'Error in Thetas * (UPPER.ini - LOWER.ini) : non-conformable arrays', which was triggered when 
                                    'normalise=TRUE' & 'model.FUN="hydromod"'.
                                 -) now works correctly on Windows machines (when 'parallel="parallelWin"')
                                    (thanks to Dipangkar Kundu !)
                                 -) fixed error : ' Error in argfun(start + i - 1L) : object 'model.out.text.file' not found', which was triggered when
                                    'parallel="parallelWin" AND 'write2disk=FALSE'
                                 -) 'parallel_logfile.txt' is now removed before start running this function (when 'parallel!="none"')
                                 -) 'parallel::makeCluster' was replaced by 'makeCluster' (and importFrom in NAMESPACE)
                                 -) 'parallel::makeForkCluster' was replaced by 'makeForkCluster' (and importFrom in NAMESPACE)
                                 -) 'parallel::detectCores' was replaced by 'detectCores' (and importFrom in NAMESPACE)
                                 -) 'parallel::clusterCall' was replaced by 'clusterCall' (and importFrom in NAMESPACE)
                                 -) 'parallel::clusterExport' was replaced by 'clusterExport' (and importFrom in NAMESPACE)
                                 -) 'parallel::mclapply' was replaced by 'mclapply' (and importFrom in NAMESPACE)
                                 -) 'parallel::parRapply' was replaced by 'parRapply' (and importFrom in NAMESPACE)
                                 -) 'parallel::clusterApply' was replaced by 'clusterApply' (and importFrom in NAMESPACE)
                                 -) 'parallel::stopCluster' was replaced by 'stopCluster' (and importFrom in NAMESPACE)
                                 -) 'formatC' calls were wrapped into a 'suppressWarnings' statement (to avoid unnecessary warnings, due to changes in  R 3.0.2).
        o 'hydroPSO'           : -) The 'Dimension' field is now correctly written into the 'PSO_logfile.txt' when 'parallel!=none' (in version 0.3-3 the 
                                    number of cores/nodes was written instead of the dimension of the problem)
                                 -) 'parallel_logfile.txt' is now removed before start running this function (when 'parallel!="none"')
                                 -) now it shows the number of detected cores (when 'parallel!="none" and 'verbose=TRUE')
                                 -) 'parallel:::makeCluster' was replaced by 'makeCluster' (and importFrom in NAMESPACE)
                                 -) 'parallel:::makeForkCluster' was replaced by 'makeForkCluster' (and importFrom in NAMESPACE)
                                 -) 'formatC' calls were wrapped into a 'suppressWarnings' statement (to avoid unnecessary warnings, due to changes in  R 3.0.2).
        o 'wquantile'          : -) fixed error: 'Error en setTxtProgressBar(pbar, i) : object 'pbar' not found', which was triggered when 'verbose=FALSE'.
                                    Thanks to Antonio Piccolboni !
        o 'plot_NparOF'        : 'formatC' calls were wrapped into a 'suppressWarnings' statement (to avoid unnecessary warnings, due to changes in  R 3.0.2).
        o 'verification'       : 'formatC' calls were wrapped into a 'suppressWarnings' statement (to avoid unnecessary warnings, due to changes in  R 3.0.2).
        o 'ModifyInputFile'    : 'formatC' calls were wrapped into a 'suppressWarnings' statement (to avoid unnecessary warnings, due to changes in  R 3.0.2).
        o 'plot_results'       : 'x11()' was replaced by 'dev.new()'
        o 'plot_particles'     : 'x11()' was replaced by 'dev.new()'
        o 'params2ecdf.default': 'x11()' was replaced by 'dev.new()'
        o 'read_out'           : 'require(zoo)' was reaplaced by '!is.na( match("zoo", installed.packages()[,"Package"] ) )'
        o 'lhs' package was moved from 'Suggests' into 'Imports' in NAMESPACE file.
        o 'zoo' package removed from 'Suggests' (due to changes in CRAN policies) but it stays on 'Imports' 


# 0.3-3	04-Jun-2013
        o 'hydroPSO'      : fixed error introduced (not intentionally, of course !) in version 0.3-2:
                            Error in FUN(1:number[[1L]], ...) : 
                              unused arguments (Particles = ...)
        o 'pest2hydroPSO' : -) typos fixed in the (automatically created) 'hydroPSO-Rscript.R' file (Thanks to Ramadan Abdel A. !):
                                *) line 38: ending comma removed
                                   out.FUN="read_output"   
                                *) line 57: added ending ', sep=""', to avoid white spaces in the filename
                                   obs.fname <- paste(model.drty, "/PSO.in/", obs.fname, sep="")  
                            -) file 'ParamFiles.txt' (automatically created) uses the correct row number for each input file.
                               Up to hydroPSO <= 0.3-2 the row number for each parameter had a shift of 1 line (e.g., it used '5' when the correct 
                               number was 4)
        o NAMESPACE: functions 'read.ParameterRanges' and 'rLHS' are now exported        

# 0.3-2	29-May-2013
        o 'lhoat'         : -) parallel capable
                            -) much faster than in previous hydroPSO versions, even without using the new 'parallel' option (this function was 
                               completely re-written, in order to make it parallel capable)
                            -) new 'normalise' parameter for the 'control' argument, in order to improve the sampling design when the search space is 
                               not a hypercube
                            -) new 'RelativeImportance.Norm' column at the end of the output ranking. The values in 'RelativeImportance.Norm' provide
                               normalised values of the relative importance of each parameter. The sum of all the 'RelativeImportance.Norm' must be 1. 
                               (Thanks to Rui Esteves for suggesting this feature !)
                            -) 'control' argument now allows the following new components: 'REPORT', 'parallel', 'par.nnodes', 'par.pkgs', 'normalise'
                            -) new argument '...' to allow function arguments that are kept constant during the sensitivity analysis.
        o 'pest2hydroPSO' : -) now it handles correctly cases where the fifth row of 'pst.fname' uses tab instead of white spaces as separator 
                               (Thanks to Ramadan Abdel A. !)
        o 'hydroPSO'      : -) minor bug fixed. When the goodness-of-fit value of a given parameter set is not finite, the parameter values are now 
                               correctly written into 'BestParamPerIter.txt'
                            -) when 'fn.name!="hydromod"' AND 'normalise=TRUE' (i.e., an R function and NOT an external model run from the system 
                               console) the function 'fn' is now evaluated with the correct (non normalised) parameter values 
                               (up to hydroPSO <= 0.3-1-1 'fn' was evaluated with the (non-correct) normalised values when 
                               'fn.name!="hydromod"' AND 'normalise=TRUE')
        
# 0.3-1-1	10-May-2013
        o Suggested package 'multicore' (superseded) was replaced by 'parallel'
        o 'hydroPSO: functions from the 'multicore' package were replaced by equivalent from the 'parallel' package (detectCores, mclapply)

# 0.3-1	10-May-2013
        o new 'SWAT2005.zip' file, with and updated tutorial for calibrating SWAT 2005, compatible with hydroPSO >= 0.3-0   
          The old zip file was renamed to 'SWAT2005_old.zip', which is compatible with hydroPSO <= 0.1-58
          The new 'SWAT2005.zip' file now includes a new 'swat2005.out' binary file, in order to allow running SWAT2005 in GNU/Linux machines
        o new 'MF2005.zip' file, with and updated tutorial for calibrating MODFLOW 2005, compatible with hydroPSO >= 0.3-0   
          The old zip file was renamed to 'MF2005_old.zip', which is compatible with hydroPSO <= 0.1-58    
        o 'lhoat'           : -) fixed bug that led to all parameters have the same Ranking (equal to 0) when 'fn!="hydromod"' (Thanks to Rui Esteves !)
                              -) fixed bug that led to the following error message "object 'model.out.text.fname' not found" when 'write2disk=FALSE'
                              -) 'fn' argument now can be any R function or a character. In the latter case, it can be "hydromod" or the name of a 
                                  valid R function. In previous versions of 'lhoat' only a character type was accepted (Thanks to Rui Esteves !)
        o 'hydroPSO'        : -) if a 'parallel' directory was not successfully removed during a previous run, it is removed before running the 
                                 algorithm in parallel mode again.
        o 'ReadPlot_results': updated description of the 'dp3D.names' argument    
        o 'plot_out'        : 'main' argument is not overwritten to "Observed vs 'best' Simulation" when this argument is provided and 'ptype=="ts"'
        o 'plot_results'    : 'main' argument is now (internally) passed to the 'plot_out' function.
        o 'plot_particles'  : 'main' argument is now omitted for the 3D dotty plots ('plot_NparOF' function), in order to avoid confusing (and 
                              sometimes unnecessary) titles when 'main' is too long        
        o 'plot_NparOF'     : 'main' argument was disabled, in order to avoid confusing titles when 'main is a long text


# 0.3-0	19-Dec-2012
        o 'hydroPSO'    : -) parallel capable (OS-independent)
                             * new argument 'parallel' to define how to parallelise the evaluation of the objective function
                             * new 'control' argument 'par.nnodes'  to define the number of cores/nodes to be used in a multi-core machine or 
                               network cluster
                             * new 'control' argument 'par.pkgs' (only needed for Windows machines) with the list of package that need to be 
                               loaded on each core/node 
                          -) Default number of particles for 'method=spso2007' was changed  from 'ceiling(10+2*sqrt(n))' to 'floor(10+2*sqrt(n))', 
                             following Clerc 2012

        o New vignette (14/Dec/2012)
                             
        o 'read_out'    : -) new argument 'nsim' used to specify the number of model outputs. It is useful when the model to be calibrated returns NA
                             instead of the simulated values for some particles (e.g., MODFLOW)
                           
        o 'read_results': -) See 'read_out' above

        o 'plot_results': -) See 'read_out' above
                      
                             
-- Previous Releases: see old file 'ChangeLog' for details

# 0.2-0	  29-Nov-2012
# 0.1-58  14-Sep-2012
# 0.1-57  29-Jun-2012
# 0.1-56  14-Jun-2012
# 0.1-55  11-May-2012
# 0.1-54  01-Apr-2012
# 0.1.53  23-Mar-2012
# 0.1.52  09-Mar-2012
# 0.1.51  24-Feb-2012
# 0.1.50  23-Feb-2012
# 0.1.49  13-Feb-2012
# 0.1.48  22-Jan-2012
# 0.1.47  16-Jan-2012
# 0.1.46  29-Nov-2011
# 0.1-45  11-Nov-2011
# 0.1-44  28-Oct-2011
# 0.1-43  17-Oct-2011
# 0.1-42  08-Oct-2011
# 0.1-41  05-Sep-2011  
# 0.1-40  01-Sep-2011  
--        24-Jun-2011 
--           May-2011
--           Apr-2011
--        29-Dec-2010 
--        23-Dec-2010
--           Dec-2010
--           Nov-2010
--       2009-Oct2010: hydroPSO hibernation :(
--          ~Jul-2008: hydroPSO beginning...

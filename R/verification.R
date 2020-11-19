## File verification.R
# Part of the hydroPSO R package, https://github.com/hzambran/hydroPSO
#                                 http://cran.r-project.org/web/packages/hydroPSO
#                                 http://www.rforge.net/hydroPSO/
## Copyright 2011-2020 Mauricio Zambrano-Bigiarini & Rodrigo Rojas
## Distributed under GPL 2 or later

################################################################################
#                           verification                                       #
################################################################################
# Purpose    : Run a hydrological model with different parameter sets (obtained#
#              during calibration) and to obtain the simulated time series for #
#              each one of the parameter sets                                  #
################################################################################
# Output     : A list of two elements:                                         #
#              1) sim : simulated values obtained by running the hydrological  #
#                       model                                                  #
#              2) gofs: goodness-of fit of the simulated values against        #
#                       observed ones, by using THE USER-DEFINED 'gof'         #
#                       measure                                                #
#              3) best.gof: goodness-of fit of the "best" parameter set        #
#              4) best.par: parameter values of the "best" paraemter set       # 
################################################################################
# Author  : Mauricio Zambrano-Bigiarini                                        #
# Started : 18-Jan-2011 at JRC Ispra                                           #
# Updates : 12-May-2011 ; 13-Feb-2012  ; 23-Feb-2012                           #
#           09-Abr-2014                                                        #
#           09-Mar-2020 ; 12-Mar-2020 ; 15-Mar-2020 ; 14-Nov-2020 ; 19-Nov-2020#
################################################################################
verification <- function(
                         fn="hydromod",  
                         par,
                         ...,
                         control=list(),
                         model.FUN=NULL,
                         model.FUN.args=list()                                
                        ) {
                     
  
  ##############################################################################
  #                            INPUT CHECKING                                  #
  ##############################################################################
        
  # Checking the name of the objective function
  if (missing(fn)) {
    stop("Missing argument: 'fn' must be provided")
  } else 
      if ( is.character(fn) | is.function(fn) )  {
        if (is.character(fn)) {
          if (fn=="hydromod") {
            fn.name <- fn
	        fn      <- match.fun(fn)
          } else if (fn=="hydromodInR") {
              fn.name <- fn
              fn      <- match.fun(model.FUN)
            } else stop("Invalid argument: valid character values for 'fn' are only: c('hydromod', 'hydromodInR')")
	    } else if (is.function(fn)) {
	       fn.name <- as.character(substitute(fn))
	       fn      <- fn
	      } # ELSE end
      } else stop("Missing argument: 'class(fn)' must be in c('function', 'character')")      
        
  # Checking 'par'
  if (missing(par)) stop("Missing argument: 'par' must be provided")
                 
  ########################################################################        
  con <- list(
                
             drty.in=getwd(),
             drty.out="verification", # Character, with the name of the directory that will store the results of the LH-OAT. 
             digits=7,
                
             gof.name="GoF",          # Character, only used for identifying the goodness-of-fit of each model run
             MinMax=c("min", "max"),  # Character, indicating if PSO have to find a minimum or a maximum for the objective function. \cr
                                      # Valid values are in: \code{c('min', 'max')} \cr
             do.plots=FALSE,
             write2disk=TRUE,
             verbose= TRUE,           # logical, indicating if progress messages have to be printed
             REPORT=10, 
          
             parallel=c("none", "parallel", "multicore", "parallelWin"),
             par.nnodes=NA,
	         par.pkgs= c()
             )
             
  MinMax        <- match.arg(control[["MinMax"]], con[["MinMax"]])     
  parallel      <- match.arg(control[["parallel"]], con[["parallel"]])    

  nmsC <- names(con)
  con[(namc <- names(control))] <- control
  
  if (length(noNms <- namc[!namc %in% nmsC])) 
    warning("[Unknown names in control: ", paste(noNms, collapse = ", "), " (not used) !]")
    
  drty.in        <- con[["drty.in"]]
  drty.out       <- con[["drty.out"]]
  digits         <- con[["digits"]]

  gof.name       <- con[["gof.name"]]
  MinMax         <- con[["MinMax"]]
  do.plots       <- as.logical(con[["do.plots"]])  
  write2disk     <- as.logical(con[["write2disk"]])
  verbose        <- as.logical(con[["verbose"]])
  par.nnodes     <- con[["par.nnodes"]]
  par.pkgs       <- con[["par.pkgs"]] 
        
  ########################################################################
  ##################### Dummy checkings ##################################
     
  # 'hydromod' checkings
  if (fn.name=="hydromod") {

    # checking that 'model.FUN' is a valid function          
    if ( is.null(model.FUN) ) {
      stop( "'model.FUN' has to be defined !" )
    } else  {
        model.FUN.name <- model.FUN
        model.FUN      <- match.fun(model.FUN)
      } # ELSE end
  
    # checking 'model.FUN.args'
    if ( length(model.FUN.args)==0 ) {
      warning( "'model.FUN.args' is an empty list. Are you sure your model doesn't have any argument(s) ?" )
    } else {
        # Assigning the numeric values provided by the user in ' model.FUN.args' 
        # to the arguments of the hydrological model
        model.FUN.argsDefaults <- formals(model.FUN)
        model.FUN.args         <- modifyList(model.FUN.argsDefaults, model.FUN.args) 
      } # ELSe end
             
  } # IF end 

  if (fn.name=="hydromodInR") {
    if ( is.null(model.FUN) ) {
      stop( "'model.FUN' has to be defined !" )
    } else  {
        model.FUN.name <- as.character(substitute(model.FUN))
        model.FUN      <- match.fun(model.FUN)   
      } # ELSE end

    if (!("param.values" %in% names(formals(model.FUN)) ))
      stop("[ Invalid argument: 'param.values' must be the first argument of the 'model.FUN' function! ]")

    if (!("obs" %in% names(formals(model.FUN)) )) 
      stop("[ Invalid argument: 'obs' must be an argument of the 'model.FUN' function! ]")
   
    model.FUN.argsDefaults <- formals(model.FUN)
    if ( length(model.FUN.args) > 0 ) {
      model.FUN.args <- modifyList(model.FUN.argsDefaults, model.FUN.args) 
    } else model.FUN.args <- model.FUN.argsDefaults

  } # IF end 
          
  if ( is.matrix(par) | is.data.frame(par) ) {
    # Computing 'P', the Dimension of the Solution Space
    P <- ncol(par)

    # Computing the number of parameter sets
    nparamsets <- nrow(par)

    # Meaningful name of each one of the parameters
    if (is.null(colnames(par))) {
      Parameter.names <- paste("Param", 1:nparamsets, sep="")
    } else Parameter.names <- colnames(par)  
  
  } else if (is.numeric(par)) {
      # Computing 'P', the Dimension of the Solution Space
      P <- length(par)
 
      # Computing the number of parameter sets
      nparamsets <- 1

      # Meaningful name of each one of the parameters
      if (is.null(names(par))) {
        Parameter.names <- paste("Param", 1:P, sep="")
      } else Parameter.names <- names(par)    

    } else stop("Invalid argument: 'class(par)' must be in c('numeric', 'matrix', 'data.frame')")
   
  if (verbose) message( "[ Number of parameter sets read: ", nparamsets, "            ]" )   
  if (verbose) message( "[ Parameter names              : ", paste(Parameter.names, collapse = ", "), " ]")
  
  # If the user only provided a single parameter set, it is transformed into matrix
  if (nparamsets==1) par <- matrix(par, nrow=1)
        
  # Adding the parent path of 'drty.out', if it doesn't have it
  if (drty.out == basename(drty.out) )
    drty.out <- paste( getwd(), "/", drty.out, sep="")
        
  # Verifying that 'drty.out' directory exists. IF not, it is created
  if (!file.exists(file.path(drty.out))) {
    if (write2disk) {
      dir.create(file.path(drty.out))
      if (verbose) message("                                            ")
      if (verbose) message("[ Output directory '", basename(drty.out), "' was created on: '", dirname(drty.out), "' ]") 
      if (verbose) message("                                            ")
    } # IF end
  } # IF end  
  
  
  ##############################################################################
  #                            Writing Info File
  ##############################################################################  
     
  # File 'Verification-logfile.txt' #        
  InfoTXT.fname <- paste(file.path(drty.out), "/", "Verification-logfile.txt", sep="")
  InfoTXT.TextFile  <- file(InfoTXT.fname , "w+")
  #c(isOpen(Tfile, "r"), isOpen(Tfile, "w")) # both TRUE
  writeLines("================================================================================", InfoTXT.TextFile) # writing a separation line with a carriage return
    writeLines(c("Platform             :", sessionInfo()[[1]]$platform), InfoTXT.TextFile, sep="  ")
    writeLines("", InfoTXT.TextFile) # writing a blank line with a carriage return
    writeLines(c("R version            :", sessionInfo()[[1]]$version.string), InfoTXT.TextFile, sep="  ")
    writeLines("", InfoTXT.TextFile) # writing a blank line with a carriage return
    writeLines(c("hydroPSO version     :", sessionInfo()$otherPkgs$hydroPSO$Version), InfoTXT.TextFile, sep="  ")
    writeLines("", InfoTXT.TextFile) #writing a blank line with a carriage return
    writeLines(c("hydroPSO Built       :", sessionInfo()$otherPkgs$hydroPSO$Built), InfoTXT.TextFile, sep="  ")
    writeLines("", InfoTXT.TextFile) # writing a blank line with a carriage return
    writeLines("================================================================================", InfoTXT.TextFile) # writing a separation line with a carriage return
    Time.Ini <- Sys.time()
    writeLines(c("Starting Time        :", date()), InfoTXT.TextFile, sep="  ")
    writeLines("", InfoTXT.TextFile) # writing a blank line with a carriage return
    writeLines("================================================================================", InfoTXT.TextFile) # writing a separation line with a carriage return
    writeLines(c("drty.in              :", drty.in), InfoTXT.TextFile, sep=" ") 
    writeLines("", InfoTXT.TextFile) # writing a blank line with a carriage return
    writeLines(c("drty.out             :", drty.out), InfoTXT.TextFile, sep=" ") 
    writeLines("", InfoTXT.TextFile) # writing a blank line with a carriage return
    writeLines(c("digits               :", digits), InfoTXT.TextFile, sep=" ") 
    writeLines("", InfoTXT.TextFile) # writing a blank line with a carriage return
    writeLines(c("gof.name             :", gof.name), InfoTXT.TextFile, sep=" ") 
    writeLines("", InfoTXT.TextFile) # writing a blank line with a carriage return
    writeLines(c("do.plots             :", do.plots), InfoTXT.TextFile, sep=" ") 
    writeLines("", InfoTXT.TextFile) # writing a blank line with a carriage return
    writeLines(c("write2disk           :", write2disk), InfoTXT.TextFile, sep=" ") 
    writeLines("", InfoTXT.TextFile) # writing a blank line with a carriage return
    writeLines(c("verbose              :", verbose), InfoTXT.TextFile, sep=" ") 
    writeLines("", InfoTXT.TextFile) # writing a blank line with a carriage return
    writeLines(c("parallel          :", parallel), InfoTXT.TextFile, sep=" ")  
    writeLines("", InfoTXT.TextFile)  
    if (parallel!="none") {
      writeLines(c("par.nnodes        :", par.nnodes), InfoTXT.TextFile, sep=" ") 
      writeLines("", InfoTXT.TextFile)
      writeLines(c("par.pkgs          :", par.pkgs), InfoTXT.TextFile, sep=" ") 
      writeLines("", InfoTXT.TextFile)     
    } # IF end
    if (fn.name=="hydromod") {
      try(writeLines(c("hydromod function    :", model.FUN.name), InfoTXT.TextFile, sep=" ") , TRUE)
      writeLines("", InfoTXT.TextFile) # writing a blank line with a carriage return
      writeLines(c("hydromod args        :"), InfoTXT.TextFile, sep=" ") 
      writeLines("", InfoTXT.TextFile) # writing a blank line with a carriage return
      for ( i in 1:length(model.FUN.args) ) {              
        arg.name  <- names(model.FUN.args)[i]
        arg.name  <- format(paste("  ", arg.name, sep=""), width=20, justify="left" )
        arg.value <- ""
        arg.value <- try(as.character( as.character(model.FUN.args[i])), TRUE)
             
        writeLines(c(arg.name, ":", arg.value), InfoTXT.TextFile, sep=" ") 
        writeLines("", InfoTXT.TextFile) # writing a blank line with a carriage return             
      } # FOR end
  } # IF end
  # Closing the text file
  close(InfoTXT.TextFile) 
  
  ########################################################################  
  #                        Text Files initialization                     #
  ########################################################################  

  # File 'Verification-ModelOut.txt' #
  model.out.text.fname <- paste(file.path(drty.out), "/", "Verification-ModelOut.txt", sep="")
  OFout.Text.file  <- file(model.out.text.fname, "w+")
  #c(isOpen(Tfile, "r"), isOpen(Tfile, "w")) # both TRUE
  #writeLines( paste("t", 1:length(obs)), OFout.Text.file, sep=" ")   
  writeLines(c("Param", "GoF", "Model_Output"), OFout.Text.file, sep="  ") 
  writeLines("", OFout.Text.file) # writing a blank line with a carriage return
  close(OFout.Text.file)  
        

  # File 'Verification-ParamValues.txt' #
  # with the parameters values for each partcile in each iteration
  gof.text.fname <- paste(file.path(drty.out), "/", "Verification-ParamValues.txt", sep="")
  Params.Text.file  <- file(gof.text.fname, "w+")           
  #c(isOpen(Tfile, "r"), isOpen(Tfile, "w")) # both TRUE
  writeLines(c("ParamNmbr", gof.name, Parameter.names), Params.Text.file, sep=" ")
  writeLines("", Params.Text.file) # writing a blank line with a carriage return
  close(Params.Text.file)          
        

  ############################################################################
  ##                                parallel                                 #
  ############################################################################
  if (parallel != "none") {
    
  #  if ( ( (parallel=="multicore") | (parallel=="parallel") ) & 
  #     ( (R.version$os=="mingw32") | (R.version$os=="mingw64") ) )
  #     stop("[ Fork clusters are not supported on Windows =>  'parallel' can not be set to '", parallel, "' ]")

  if (parallel=="multicore") {
     warning("[ Package 'parallel' is not available anymore in CRAN. It was changed to 'parallel='parallel' ]")
     parallel <- "parallel"
  } # IF end
    
  ifelse(parallel=="parallelWin", parallel.pkg <- "parallel",  parallel.pkg <- parallel) 
  if ( length(find.package(parallel.pkg, quiet=TRUE)) == 0 ) {               
    warning("[ Package '", parallel.pkg, "' is not installed =>  parallel='none' ]")
    parallel <- "none"
  }  else { 
      
       if (verbose) message("                                                 ")
       if (verbose) message("[ Parallel initialization ...                   ]")
      
       fn1 <- function(i, x) fn(x[i,])

       #require(parallel)           
       nnodes.pc <- parallel::detectCores()
       if (verbose) message("[ Number of cores/nodes detected: ", nnodes.pc, "             ]")
           
       if ( (parallel=="parallel") | (parallel=="parallelWin") ) {             
          logfile.fname <- paste(file.path(drty.out), "/", "parallel_logfile.txt", sep="") 
          if (file.exists(logfile.fname)) file.remove(logfile.fname)
       } # IF end
             
       if (is.na(par.nnodes)) {
         par.nnodes <- nnodes.pc
       } else if (par.nnodes > nnodes.pc) {
           warning("[ 'nnodes' > number of detected cores (", par.nnodes, ">", nnodes.pc, ") =>  par.nnodes=", nnodes.pc, "            ] !",)
           par.nnodes <- nnodes.pc
         } # ELSE end
 
       if (verbose) message("[ Number of cores/nodes used    : ", par.nnodes, "             ]")                 
               
       if (parallel=="parallel") {
           ifelse(write2disk, 
                  cl <- parallel::makeForkCluster(nnodes = par.nnodes, outfile=logfile.fname),
                  cl <- parallel::makeForkCluster(nnodes = par.nnodes) )         
       } else if (parallel=="parallelWin") {      
           ifelse(write2disk,
                  cl <- parallel::makeCluster(par.nnodes, outfile=logfile.fname),
                  cl <- parallel::makeCluster(par.nnodes) )
           pckgFn <- function(packages) {
             for(i in packages) library(i, character.only = TRUE)
           } # 'packFn' END
           parallel::clusterCall(cl, pckgFn, par.pkgs)
           parallel::clusterExport(cl, ls.str(mode="function",envir=.GlobalEnv) )
             if ( (fn.name=="hydromod") | (fn.name == "hydromodInR") ) {
             parallel::clusterExport(cl, model.FUN.args$out.FUN)
             #parallel::clusterExport(cl, model.FUN.args$gof.FUN)
           } # IF end  
           if (fn.name == "hydromodInR") {
               fn.default.vars <- as.character(formals(model.FUN))
               parallel::clusterExport(cl, fn.default.vars[fn.default.vars %in% ls(.GlobalEnv)])
           } # IF end                   
         } # ELSE end                   
                            
         if (fn.name=="hydromod") {
           if (!("model.drty" %in% names(formals(hydromod)) )) {
              stop("[ Invalid argument: 'model.drty' has to be an argument of the 'hydromod' function! ]")
           } else { # Copying the files in 'model.drty' as many times as the number of cores
             
               model.drty <- path.expand(model.FUN.args$model.drty)
                 
               files <- list.files(model.drty, full.names=TRUE, include.dirs=TRUE) 
               tmp <- which(basename(files)=="parallel")
               if (length(tmp) > 0) files <- files[-tmp]
               parallel.drty <- paste(file.path(model.drty), "/parallel", sep="")

               if (file.exists(parallel.drty)) {                      
                 if (verbose) message("[ Removing the 'parallel' directory ... ]")    
                 try(unlink(parallel.drty, recursive=TRUE, force=TRUE))
               } # IF end 
               dir.create(parallel.drty)

               mc.dirs <- character(par.nnodes)
               if (verbose) message("                                                     ")
               for (i in 1:par.nnodes) {
                 mc.dirs[i] <- paste(parallel.drty, "/", i, "/", sep="")
                 dir.create(mc.dirs[i])
                 if (verbose) message("[ Copying model input files to directory '", mc.dirs[i], "' ... ]")
                 file.copy(from=files, to=mc.dirs[i], overwrite=TRUE, recursive=TRUE)
               } # FOR end
                 
               tmp       <- ceiling(nparamsets/par.nnodes)        
               part.dirs <- rep(mc.dirs, tmp)[1:nparamsets]  
             } # ELSE end                 
         } # IF end
           
       } # ELSE end  
  
  }  # IF end    
  ############################################################################## 

  ##############################################################################
  #                            MAIN BODY
  ##############################################################################
  
  # Opening the file 'Verification-ModelOut.txt' for appending
  OFout.Text.file <- file(model.out.text.fname, "a")   

  # Opening the file 'Verification-ParamValues.txt' for appending
  Params.Text.file <- file(gof.text.fname, "a")

  gof.all <- numeric(nparamsets)

  # Evaluating an R Function
  if ( (fn.name != "hydromod") & (fn.name != "hydromodInR") ) {          
    if (parallel=="none") {
      out <- apply(par, fn, MARGIN=1, ...)
    } else             
        if ( (parallel=="parallel") | (parallel=="parallelWin") ) #{
           out <- parallel::parRapply(cl= cl, x=par, FUN=fn, ...)
        #} else if (parallel=="multicore")
        #    hydromod.out <- unlist(parallel::mclapply(1:npart, FUN=fn1, x=par, ..., mc.cores=par.nnodes, mc.silent=TRUE)) 
  } else 
      if (fn.name == "hydromodInR") { # Evaluating an R-based model
        if (verbose) message("                                                 ")
        if (verbose) message("=================================================")
        if (verbose) message("[ Running the model ...                         ]") 
        

        if ("verbose" %in% names(model.FUN.args)) {
	      verbose.FUN <- model.FUN.args[["verbose"]] 
	    } else verbose.FUN <- verbose


	    if (parallel=="none") {
	        out <- lapply(1:nparamsets, FUN=hydromodInR.eval,       
                          Particles=par, 
                          model.FUN=model.FUN, 
                          model.FUN.args=model.FUN.args 
                          )
                   
         } else if ( (parallel=="parallel") | (parallel=="parallelWin") ) {
 print("golall")        
print(cl)
print(nparamsets)
print(hydromodInR.eval)  
print(summary(par))      
print(model.FUN)
             out <- parallel::clusterApply(cl=cl, x=1:nparamsets, fun= hydromodInR.eval,                                  
                                           Particles=par, 
                                           model.FUN=model.FUN, 
                                           model.FUN.args=model.FUN.args 
                                           ) # sapply END


         
#            for (part in 1:npart){         
#              GoF                    <- out[[part]][["GoF"]] 
#              Xt.fitness[iter, part] <- GoF            
#              ModelOut[[part]]       <- out[[part]][["sim"]]  
#              nfn <- nfn + 1 
#              if(is.finite(GoF)) nfn.eff <- nfn.eff + 1                     
#             } #FOR part end  
         
          } # ELSE IF end
      }   else 
           if (fn.name == "hydromod") {

             if ("verbose" %in% names(model.FUN.args)) {
	           verbose.FUN <- model.FUN.args[["verbose"]] 
	         } else verbose.FUN <- verbose
	     
	         if (parallel=="none") {
	          out <- lapply(1:nparamsets, hydromod.eval,       
                            Particles=par, 
                            iter=1, 
                            npart=nparamsets, 
                            maxit=1, 
                            REPORT=REPORT, 
                            verbose=verbose.FUN, 
                            digits=digits, 
                            model.FUN=model.FUN, 
                            model.FUN.args=model.FUN.args, 
                            parallel=parallel, 
                            ncores=par.nnodes, 
                            part.dirs=mc.dirs  
                            )
                   
             } else if ( (parallel=="parallel") | (parallel=="parallelWin") ) {
                 
                 out <- parallel::clusterApply(cl=cl, x=1:nparamsets, fun= hydromod.eval,                                  
                                               Particles=par, 
                                               iter=1, 
                                               npart=nparamsets, 
                                               maxit=1, 
                                               REPORT=REPORT, 
                                               verbose=verbose.FUN, 
                                               digits=digits, 
                                               model.FUN=model.FUN, 
                                               model.FUN.args=model.FUN.args, 
                                               parallel=parallel, 
                                               ncores=par.nnodes, 
                                               part.dirs=part.dirs                          
                                               ) # sapply END
               } # if ( (parallel=="parallel") | (parallel=="parallelWin") ) 
	 
           } # IF 'fn.name == "hydromod"' END

  print("listo")
  # Evaluating a system-console-based model
  for ( p in 1:nparamsets) {  

    # Getting the parameter set
    param.values.p <- as.numeric(par[p,])
    
#    ##########################################################################
#    # 2)                 Running the hydrological model                      #
#    ##########################################################################
#    
#    # If the user wants to create a plot with obs vs sim
#    if (do.plots) {
#      do.png         <- TRUE
#      png.fname      <- paste(drty.out, "/ParameterSet_", p, ".png", sep="")
#      main           <- paste("Parameter Set:", p, sep="")
#      model.FUN.args <- modifyList(model.FUN.args, list(do.png=do.png, png.fname=png.fname, main=main)) 
#    } # IF end
#    
#    # Model evaluation 
#    if (fn.name == "hydromod") {
#
#      if (verbose) message("                    |                      ")
#      if (verbose) message("                    |                      ") 
#      if (verbose) message("==============================================================")
#      if (verbose) message( paste("[ Running parameter set ", 
#                                  format( p, width=4, justify="left" ), 
#                                  "/", nparamsets, " => ", 
#                                  format( round(100*p/nparamsets,2), width=7, justify="left" ), "%",
#                                  ". Starting... ]", sep="") )
#      if (verbose) message("==============================================================")
#
#      # Running the console-based model
#      model.FUN.args <- modifyList(model.FUN.args, list(param.values=param.values.p)) 
#      hydromod.out   <- do.call(model.FUN, as.list(model.FUN.args)) 
#
#      if (verbose) message("                              |                               ")
#      if (verbose) message("                              |                               ") 
#      if (verbose) message("==============================================================")
#      if (verbose) message("[================ Verification finished ! ===================]")
#      if (verbose) message("==============================================================")
#    } # IF end
     
        
    ############################################################################
    # 3)                     Extracting simulated values                       #                                 
    ############################################################################
                  
    # Extracting the simulated values and the goodness of fit
    if ( fn.name == "hydromod" ) {
      sims <- as.numeric(out[["sim"]])
      GoF  <- as.numeric(out[["GoF"]])
    } else if ( fn.name == "hydromodInR" ) {
        sims  <- out[[p]][["sim"]]
        GoF   <- out[[p]][["GoF"]] 
      } else {
          sims <- as.numeric(out[p])
          GoF  <- as.numeric(out[p])
        } # ELSE end     
   
    gof.all[p] <- GoF

    # Writing to the 'Verification-ModelOut.txt' file
    suppressWarnings( tmp <- formatC(GoF, format="E", digits=digits, flag=" ") )
    if ( (fn.name != "hydromod") & (fn.name != "hydromodInR") ) {
      writeLines(as.character(c(p, tmp, tmp)), OFout.Text.file, sep="  ") 
    } else suppressWarnings( writeLines(as.character(c(p, tmp, formatC(sims, format="E", digits=digits, flag=" ") )), OFout.Text.file, sep="  ") )
    writeLines("", OFout.Text.file) # writing a blank line with a carriage return  
    
    # Writing to the 'Verification-ParamValues.txt' file
    suppressWarnings( writeLines(as.character(c(p, tmp, formatC(param.values.p, format="E", digits=digits, flag=" ") )), Params.Text.file, sep="  ") )
    writeLines("", Params.Text.file) # writing a blank line with a carriage return 
    
  } # FOR end

  # Closing the output text files
  close(OFout.Text.file)
  close(Params.Text.file)
  
  ##############################################################################
  # 4)                    Writing Ending and Elapsed Time                      #                                 
  ##############################################################################
  InfoTXT.TextFile <- file(InfoTXT.fname, "a")    
  #c(isOpen(Tfile, "r"), isOpen(Tfile, "w")) # both TRUE
  writeLines("================================================================================", InfoTXT.TextFile) # writing a separation line with a carriage return
  writeLines(c("Ending Time            :", date()), InfoTXT.TextFile, sep=" ")
  writeLines("", InfoTXT.TextFile) # writing a blank line with a carriage return
  writeLines("================================================================================", InfoTXT.TextFile) # writing a separation line with a carriage return
  # Total time of the simulations
  Time.Fin <- Sys.time()
  writeLines(c("Elapsed Time           :", format(round(Time.Fin - Time.Ini, 2))), InfoTXT.TextFile, sep=" ")
  writeLines("", InfoTXT.TextFile) # writing a blank line with a carriage return
  writeLines("================================================================================", InfoTXT.TextFile) # writing a separation line with a carriage return
  close(InfoTXT.TextFile)

  ##############################################################################
  ##                                parallel                                   #
  ##############################################################################
  if (parallel!="none") {
    if ( (parallel=="parallel") | (parallel=="parallelWin") )   
         parallel::stopCluster(cl)   
    if (fn.name=="hydromod") {
      if (verbose) message("                                         ")
      if (verbose) message("[ Removing the 'parallel' directory ... ]")    
      unlink(dirname(mc.dirs[1]), recursive=TRUE)
    } # IF end
         
  } # IF end
  ##############################################################################


  
  ##############################################################################
  # 5)                    Creating the output                                  #                                 
  ##############################################################################

  if (verbose) message("=================================================")
  if (verbose) message("[ Reading 'Verification-ModelOut.txt' file ...  ]") 
  sims <- data.table::fread(file=model.out.text.fname, skip=1, data.table=FALSE)
  nc   <- ncol(sims)
  # Removing the first 2 columns in 'sims': ParameterSetNmbr, GoF
  sims <- sims[, 3:nc]  
  colnames(sims) <- paste0("sim", 1:(nc-2))
  colnames(sims) <- paste0("par", 1:nparamsets)


  ifelse(MinMax=="min", best.rowindex <- which.min(gof.all),
                        best.rowindex <- which.max(gof.all)) 

  best.gof <- gof.all[best.rowindex]                       
  best.par <- par[best.rowindex,]

  out <- list(gofs=gof.all, model.values=sims, best.gof=best.gof, best.param=best.par )

  if (verbose) message("=================================================")
  if (verbose) message("[                Finished !                     ]") 
  if (verbose) message("=================================================")

  return(out)

} # 'verification' END

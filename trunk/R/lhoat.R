# File lhoat.R
# Part of the hydroPSO R package, http://www.rforge.net/hydroPSO/ ; 
#                                 http://cran.r-project.org/web/packages/hydroPSO
# Copyright 2011-2012 Mauricio Zambrano-Bigiarini & Rodrigo Rojas
# Distributed under GPL 2 or later


################################################################################
#                  Latin-Hypercube One-At-a-Time                               #
################################################################################
# Purpose   : Run a sensitivity analysis for the parameters of the             #
#             hydrological model, by using Latin-Hypercupe One-factor-At-a-Time#  
#             developed by  van Griensven et al., 2006                         #
################################################################################
# Reference : A. van Griensven, T. Meixner, S. Grunwald, T. Bishop, M. Diluzio, 
#             R. Srinivasan, A global sensitivity analysis tool for the 
#             parameters of multi-variable catchment models, Journal of Hydrology, 
#             Volume 324, Issues 1-4, 15 June 2006, Pages 10-23, ISSN 0022-1694, 
#             DOI: 10.1016/j.jhydrol.2005.09.008.
#             (http://www.sciencedirect.com/science/article/pii/S0022169405004488)
################################################################################
# Output    : A list of two elements:                                          #
#             1) ParameterSets: A matrix with all the parameter sets used in   #
#                               the LH-OAT                                     #
#             2) Ranking      : a single column matrix with a ranking of the   #
#                               sensitivity of each parameter, sorted in       #
#                               decreasing order, from the most sensitive to   #
#                               the least one.                                 #
################################################################################
# Author  : Mauricio Zambrano-Bigiarini                                        #
# Started : 23-Jun-2011                                                        #
# Updates : 26-Jan-2012 ; 02-Feb-2012 ; 13-Feb-2012 ; 23-Feb-2012              #
#           09-May-2013                                                        #
################################################################################

lhoat <- function(
                  fn="hydromod",  
                  lower=-Inf,
                  upper=Inf,
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
          fn.name <- fn
          fn      <- match.fun(fn)
	} else if (is.function(fn)) {
	    fn.name <- as.character(substitute(fn))
	    fn      <- fn
	  } # ELSE end
      } else stop("Missing argument: 'class(fn)' must be in c('function', 'character')")      
        
  # checking length of 'lower' and 'upper'
  if (length(lower) != length(upper) )
    stop( paste( "Invalid argument: 'length(lower) != length(upper) (", length(lower), "!=", length(upper), ")'", sep="" ) )
        
          
  ########################################################################        
  con <- list(
        
          N=3,                            # number of strata to be used for each parameter
          f=0.15,                         # fraction by which each single parameter is changed within the Morris OAT design
                
          drty.in="PSO.in",
          drty.out="LH_OAT",              # Character, with the name of the directory that will store the results of the LH-OAT. 
          param.ranges="ParamRanges.txt", # Character, with the name of the file that stores the desired range of variation for each parameter                          
          digits=7,
                
          gof.name="GoF",
          do.plots=FALSE,
          write2disk=TRUE,
          verbose= TRUE                   # logical, indicating if progress messages have to be printed
             )
              
  nmsC <- names(con)
 
  con[(namc <- names(control))] <- control
  
  if (length(noNms <- namc[!namc %in% nmsC])) 
    warning("[Unknown names in control: ", paste(noNms, collapse = ", "), " (not used) !]")
    
  N              <- con[["N"]]
  f              <- con[["f"]]
  drty.in        <- con[["drty.in"]]
  drty.out       <- con[["drty.out"]]
  param.ranges   <- con[["param.ranges"]]         
  digits         <- con[["digits"]]

  gof.name       <- con[["gof.name"]]
  do.plots       <- as.logical(con[["do.plots"]])  
  write2disk     <- as.logical(con[["write2disk"]])
  verbose        <- as.logical(con[["verbose"]])
        
  ########################################################################
  ##################### Dummy checkings ##################################

  # Checking that 'N' is integer
  if ( trunc(N) != N ) stop( "Invalid argument: 'N' must be integer" )
     
  # Checking that '0 < f < 1' 
  if ( (f <= 0) | (f >= 1) ) stop( "Invalid argument: 'f' must be in [0,1]" )
   
        
  # 'hydromod' checkings
  if (fn.name=="hydromod") {

    # Checking that 'param.ranges' really exists
    if ( !file.exists( param.ranges ) )
       stop( paste("Invalid argument: The file '", param.ranges, "' doesn't exist !", sep="") ) 
             
    # checking that 'model.FUN' is a valid function          
    if ( is.null(model.FUN) ) {
      stop( "'model.FUN' has to be defined !" )
    } else  {
        model.FUN.name <- model.FUN
        model.FUN      <- match.fun(model.FUN)
      } # ELSE end
  
    # checking 'out.FUN.args'
    if ( length(model.FUN.args)==0 ) {
      warning( "'model.FUN.args' is an empty list. Are you sure your model doesn't have any argument(s) ?" )
    } else {
        # Modifying the arguments of the hydrological model
        model.FUN.argsDefaults <- formals(model.FUN)
        model.FUN.args         <- modifyList(model.FUN.argsDefaults, model.FUN.args) 
      } # ELSe end
             
  } # IF end    


  # checking 'X.Boundaries' 
  if (fn.name=="hydromod") {
        
     if (verbose) message("==============================================================")
     if (verbose) message("[   1)   Reading 'param.ranges' ...                          ]")
     if (verbose) message("==============================================================")
  
      # Reading the desired range of variation for each parameter 
      X.Boundaries <- read.ParameterRanges(ParamRanges.fname=param.ranges) 
          
  } else {
      if ( (lower[1L] == -Inf) || (upper[1L] == Inf) ) {
      #if (any(upper==Inf | lower==-Inf))
        stop( "Invalid argument: 'lower' and 'upper' boundaries must be finite !!'" )
      } else X.Boundaries <- cbind(lower, upper)              
    } # ELSE end
          
  # Computing 'P', the Dimension of the Solution Space
  P <- nrow(X.Boundaries)

  # Meaningful name of each one of the parameters
  if (is.null(rownames(X.Boundaries))) {
    Parameter.names <- paste("Param", 1:P, sep="")
  } else Parameter.names <- rownames(X.Boundaries)    
        
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
  if (write2disk) {   
    # File 'Verification-logfile.txt' #        
    InfoTXT.fname <- paste(file.path(drty.out), "/", "LH_OAT-logfile.txt", sep="")
    InfoTXT.TextFile  <- file(InfoTXT.fname , "w+")
    #c(isOpen(Tfile, "r"), isOpen(Tfile, "w")) # both TRUE
    writeLines("================================================================================", InfoTXT.TextFile) # writing a separation line with a carriage return
    writeLines(c("hydroPSO version     :", sessionInfo()$otherPkgs$hydroPSO$Version), InfoTXT.TextFile, sep="  ")
    writeLines("", InfoTXT.TextFile) #writing a blank line with a carriage return
    writeLines(c("hydroPSO Built       :", sessionInfo()$otherPkgs$hydroPSO$Built), InfoTXT.TextFile, sep="  ")
    writeLines("", InfoTXT.TextFile) # writing a blank line with a carriage return
    writeLines(c("R version            :", sessionInfo()[[1]]$version.string), InfoTXT.TextFile, sep="  ")
    writeLines("", InfoTXT.TextFile) # writing a blank line with a carriage return
    writeLines(c("Platform             :", sessionInfo()[[1]]$platform), InfoTXT.TextFile, sep="  ")
    writeLines("", InfoTXT.TextFile) # writing a blank line with a carriage return
    writeLines("================================================================================", InfoTXT.TextFile) # writing a separation line with a carriage return
    Time.Ini <- Sys.time()
    writeLines(c("Starting Time        :", date()), InfoTXT.TextFile, sep="  ")
    writeLines("", InfoTXT.TextFile) # writing a blank line with a carriage return
    writeLines("================================================================================", InfoTXT.TextFile) # writing a separation line with a carriage return
    writeLines(c("param.ranges file    :", param.ranges), InfoTXT.TextFile, sep=" ") 
    writeLines("", InfoTXT.TextFile) # writing a blank line with a carriage return
    writeLines(c("N (number of strata) :", N), InfoTXT.TextFile, sep=" ") 
    writeLines("", InfoTXT.TextFile) # writing a blank line with a carriage return
    writeLines(c("f (changing factor)  :", f), InfoTXT.TextFile, sep=" ") 
    writeLines("", InfoTXT.TextFile) # writing a blank line with a carriage return
    writeLines(c("drty.in              :", drty.in), InfoTXT.TextFile, sep=" ") 
    writeLines("", InfoTXT.TextFile) # writing a blank line with a carriage return
    writeLines(c("drty.out             :", drty.out), InfoTXT.TextFile, sep=" ") 
    writeLines("", InfoTXT.TextFile) # writing a blank line with a carriage return
    writeLines(c("param.ranges         :", param.ranges), InfoTXT.TextFile, sep=" ") 
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
  } # IF end       

  
  ########################################################################  
  #                        Text Files initialization                     #
  ########################################################################  
  if (write2disk) { 
    # File 'LH_OAT-out.txt' #
    model.out.text.fname <- paste(file.path(drty.out), "/", "LH_OAT-out.txt", sep="")
    model.out.text.file  <- file(model.out.text.fname, "w+")
    #c(isOpen(Tfile, "r"), isOpen(Tfile, "w")) # both TRUE
    #writeLines( paste("t", 1:length(obs)), model.out.text.file, sep=" ") 
    close(model.out.text.file)  
        

    # File 'LH_OAT-gof.txt' #
    # with the parameters values for each partcile in each iteration
    gof.text.fname <- paste(file.path(drty.out), "/", "LH_OAT-gof.txt", sep="")
    gof.text.file  <- file(gof.text.fname, "w+")           
    #c(isOpen(Tfile, "r"), isOpen(Tfile, "w")) # both TRUE
    writeLines(c(gof.name, Parameter.names), gof.text.file, sep=" ") 
    writeLines("", gof.text.file) # writing a blank line with a carriage return
    close(gof.text.file)          
  } # IF end
        

  ##############################################################################
  #                            MAIN BODY
  ##############################################################################
  
  if (verbose) message("                                                              ")
  if (verbose) message("==============================================================")
  if (verbose) message("[  2)   Initial  LHS ...                                     ]")
  if (verbose) message("==============================================================")
  
  # Initial N points with LHS
  Theta.Ini <- rLHS(n=N, ranges=X.Boundaries)
  
  if (verbose) message("                                                              ")
  if (verbose) message("==============================================================")
  if (verbose) message("[  3)   Starting LHS-OAT ...                                 ]")
  if (verbose) message("==============================================================")
  
  # Output Sensitivity Matrix
  S           <- matrix(NA, ncol=P, nrow=N)
  colnames(S) <- Parameter.names
  
  # Total Number of parameter sets to be used in the LH-OAT
  nparamsets  <- (P+1)*N
  
  # Parameter sets that will be used in the LH-OAT
  Thetas           <- matrix(NA, nrow=nparamsets, ncol=P)
  colnames(Thetas) <- Parameter.names
  
  # Goodness-of-fit of each paremter set used in the LH-OAT
  gof    <- numeric(nparamsets)
    
  # counter of the number of parameter set  
  n <- 0
  
  # Counter of the points belonging to the initial LHS
  j <- 1
  
  ##############################################################################
  #                3) Loop for each point of the initial LHS                   #
  ##############################################################################
  while (j <= N ) {
  
    if (write2disk) { 
      # Opening the file 'LH_OAT-out.txt' for appending
      model.out.text.file <- file(model.out.text.fname, "a")   
      # Opening the file 'LH_OAT-gof.txt' for appending
      gof.text.file <- file(gof.text.fname, "a") 
    } # IF end
  
    if (verbose) message("                             |                                ") 
    if (verbose) message("==============================================================")
    if (verbose) message( "[ Running point ", j, "/", N, " of the initial LHS ... ]" ) 
    if (verbose) message( "[ Parameter set ", 
                                format( n+1, width=4, justify="left" ), 
                                "/", nparamsets, " => ", 
                                format( round(100*(n+1)/nparamsets,2), width=7, justify="left" ), "%",
                                ". Starting... ]" )
    if (verbose) message("==============================================================")
    
    gof.is.numeric <- FALSE
    
    while (!gof.is.numeric) {
  
      ##############################
      # j point of the initial LHS #
      Theta.Zero   <- Theta.Ini[j,]
      n            <- n+1
      Thetas[n,]   <- Theta.Zero
      param.values <- as.numeric(formatC(Theta.Zero, format="E", digits=digits))
    
      ############################################################################
      # 4)                       Running the hydrological model                  #
      ############################################################################
    
      # If the user wants to create a plot with obs vs sim
      if (do.plots) {
        do.png         <- TRUE
        png.fname      <- paste(drty.out, "/LHS-Point_0_", j, ".png", sep="")
        k              <- ceiling(P/5)
        title          <- character(k)
        for (m in 1:k) {
          namess   <- format(names(Theta.Zero[(5*(m-1)+1):(5*m)]), 10, justify="left")
          values   <- format(round(Theta.Zero[(5*(m-1)+1):(5*m)], 3), 7, justify="left")
          title[m] <- paste(namess, "=", values, " ; ", collapse="")
        } # FOR end
        main <- paste(title, sep="\n")
        model.FUN.args <- modifyList(model.FUN.args, list(do.png=do.png, png.fname=png.fname, main=main)) 
      } # IF end
    
      # Model evaluation for the 'Theta.Zero' point
      if ( fn.name == "hydromod" ) {
        model.FUN.args <- modifyList(model.FUN.args, list(param.values=Theta.Zero)) 
        hydromod.out   <- do.call(model.FUN, as.list(model.FUN.args)) 
      } else hydromod.out <- do.call(fn, list(Theta.Zero))
        
      ############################################################################
      # 5)                     Extracting simulated values                       #                                 
      ############################################################################
                  
      # Extracting the simulated values and the goodness of fit
      if ( fn.name == "hydromod" ) {
        sims       <- as.numeric(hydromod.out[["sim"]])
        M.Zero     <- as.numeric(hydromod.out[["GoF"]])
      } else {
          sims     <- as.numeric(hydromod.out)
          M.Zero   <- as.numeric(hydromod.out)
        } # ELSe end
        
      # Finding out if the GoF had a finite value or not
      ifelse(is.finite(M.Zero), gof.is.numeric <- TRUE, gof.is.numeric <- FALSE)
      
      # If the current point of the initial LHS leads to a GoF=NA, it is replaced
      if (!gof.is.numeric) {
        tmp           <- rLHS(n=N, ranges=X.Boundaries)
        Theta.Ini[j,] <- tmp[j,]
        n             <- n - 1
      } # IF end
        
    } # WHILE '(!gof.is.numeric)' end
       
    # Storing the GoF corresponding to the 'j' point of the LHS    
    gof[n] <- M.Zero

    if (write2disk) { 
      # Writing to the 'LH_OAT-out.txt' file
      writeLines(as.character(sims), model.out.text.file, sep=" ") 
      writeLines("", model.out.text.file) # writing a blank line with a carriage return
    
      # Writing to the 'LH_OAT-gof.txt' file
      writeLines( as.character( c(formatC(gof[n], format="E", digits=digits, flag=" "), # GoF
                                  formatC(param.values, format="E", digits=digits, flag=" ")                                            
                                            ) ), gof.text.file, sep="  ") 
                                             
      writeLines("", gof.text.file) # writing a blank line with a carriage return
    
      # Closing the output text files
      close(model.out.text.file)
      close(gof.text.file) 
    } # IF end
    
    ############################################################################
    ############################################################################
    # Loop for each parameter of the j point of the initial LHS
    i <- 1
    while(i <= P) {
    
      if (write2disk) { 
        # Opening the file 'LH_OAT-out.txt' for appending
        model.out.text.file <- file(model.out.text.fname, "a")   
        # Opening the file 'LH_OAT-gof.txt' for appending
        gof.text.file <- file(gof.text.fname, "a") 
      } # IF end
    
      if (verbose) message("                             |                                ") 
      if (verbose) message("==============================================================")
      if (verbose) message( "[ Changing '", Parameter.names[i], 
                                "' in the point ", j, "/", N, " of the initial LHS ...]" ) 
      if (verbose) message( "[ Running parameter set ", 
                                format( n+1, width=4, justify="left" ), 
                                "/", nparamsets, " => ", 
                                format( round(100*(n+1)/nparamsets,2), width=7, justify="left" ), "%",
                                ". Starting... ]" )
      if (verbose) message("==============================================================")
      
      gof.is.numeric <- FALSE
    
      while (!gof.is.numeric) {
     
        # Displacement of the j point of the initial LHS into the i direction
        canonical    <- rep(1, P)
        canonical[i] <- 1 + f*sign(rnorm(1))
     
        Theta.New    <- Theta.Ini[j,]*canonical
        n            <- n+1
        Thetas[n,]   <- Theta.New
        param.values <- as.numeric(formatC(Theta.New, format="E", digits=digits))
     
        ##########################################################################
        # 6)                       Running the hydrological model                #
        ##########################################################################
      
        # If the user wants to create a plot with obs vs sim
        if (do.plots) {
          do.png         <- TRUE
          png.fname      <- paste(drty.out, "/LHS-Point_", Parameter.names[i], "_", j, ".png", sep="")
          k              <- ceiling(P/5)
          main           <- NULL
          title          <- character(k)
          for (m in 1:k) {
            namess   <- format(names(Theta.New[(5*(m-1)+1):(5*m)]), 10, justify="left")
            values   <- format(round(Theta.New[(5*(m-1)+1):(5*m)], 3), 7, justify="left")
            title[m] <- paste(namess, "=", values, " ; ", collapse="")
          } # FOR end
          main <- paste(title, sep="\n")
          model.FUN.args <- modifyList(model.FUN.args, list(do.png=do.png, png.fname=png.fname, main=main)) 
        } # IF end    
    
        # Model evaluation for the 'Theta.New' point
        if ( fn.name == "hydromod" ) {
          model.FUN.args <- modifyList(model.FUN.args, list(param.values=Theta.New)) 
          hydromod.out   <- do.call(model.FUN, as.list(model.FUN.args)) 
        } else hydromod.out <- do.call(fn, list(Theta.Zero)) 
      
        ##########################################################################
        # 7)                     Extracting simulated values                     #                                 
        ##########################################################################
                  
        # Extracting the simulated values and the goodness of fit
        if ( fn.name == "hydromod" ) {
          sims       <- as.numeric(hydromod.out[["sim"]])
          M.New      <- as.numeric(hydromod.out[["GoF"]])
        } else {
            sims     <- as.numeric(hydromod.out)
            M.New    <- as.numeric(hydromod.out)
          } # ELSe end
          
        # Finding out if the GoF had a finite value or not
        ifelse(is.finite(M.New), gof.is.numeric <- TRUE, gof.is.numeric <- FALSE)
      
        # If the current point of the initial LHS leads to a GoF=NA, it is replaced
        if (!gof.is.numeric) {
          n <- n - 1
        } # IF end
        
      } # WHILE '(!gof.is.numeric)' end
    
      # Storing the GoF corresponding to the OAT 'i' point    
      gof[n] <- M.New      

      if (write2disk) { 
        # Writing to the 'LH_OAT-out.txt' file
        writeLines(as.character(sims), model.out.text.file, sep=" ") 
        writeLines("", model.out.text.file) # writing a blank line with a carriage return
    
        # Writing to the 'LH_OAT-gof.txt' file 
        writeLines( as.character( c(formatC(gof[n], format="E", digits=digits, flag=" "), # GoF
                                  formatC(param.values, format="E", digits=digits, flag=" ")                                            
                                            ) ), gof.text.file, sep="  ")
        writeLines("", gof.text.file) # writing a blank line with a carriage return
    
        # Closing the output text files
        close(model.out.text.file)
        close(gof.text.file) 
      } # IF end
    
      ##########################################################################
      # 8)                  Updating the sensitivity matrix                    #                                 
      ##########################################################################
    
      S[j,i] <- abs( (M.New - M.Zero) / ( (M.New + M.Zero) / 2) ) * ( 100 / f)
      
      i <- i + 1
     
    } # WHILE i end
    
    j <- j + 1
    
  } # WHILE j end
  
  ##############################################################################
  # 9)                    Sensitivity of each Parameter                        #                                 
  ##############################################################################
  
  # Mean value for each parameter
  Ranking <- colMeans(S, na.rm=TRUE)
  
  if (verbose) message("                             |                                ")
  if (verbose) message("                             |                                ") 
  if (verbose) message("==============================================================")
  if (verbose) message("[==================    LH-OAT finished !    =================]")
  if (verbose) message("==============================================================")
  
  
  ##############################################################################
  # 10)                    Writing Ending and Elapsed Time                     #                                 
  ##############################################################################
  if (write2disk) { 
    InfoTXT.TextFile <- file(InfoTXT.fname, "a")    
    #c(isOpen(Tfile, "r"), isOpen(Tfile, "w")) # both TRUE
    writeLines("================================================================================", InfoTXT.TextFile) # writing a separation line with a carriage return
    writeLines(c("Ending Time          :", date()), InfoTXT.TextFile, sep=" ")
    writeLines("", InfoTXT.TextFile) # writing a blank line with a carriage return
    writeLines("================================================================================", InfoTXT.TextFile) # writing a separation line with a carriage return
    # Total time of the simulations
    Time.Fin <- Sys.time()
    writeLines(c("Elapsed Time         :", format(round(Time.Fin - Time.Ini, 2))), InfoTXT.TextFile, sep=" ")
    writeLines("", InfoTXT.TextFile) # writing a blank line with a carriage return
    writeLines("================================================================================", InfoTXT.TextFile) # writing a separation line with a carriage return
    close(InfoTXT.TextFile)
  } # IF end
  
  ##############################################################################
  # 11)        Computing the Final Ranking of Sensitivity                      #                                 
  ##############################################################################
  # Sorting the parameters, from the most sensitive to the least one
  Ranking <- sort(Ranking, decreasing=TRUE, na.last=TRUE)
  
  # Adding a column with a sequential number for the ranking
  Ranking <- data.frame(RankingNmbr=format(as.character(1:P), width=11, justify="left"), 
                        ParameterName=format(names(Ranking), width=13, justify="left"), 
                        RelativeImportance=as.numeric(Ranking) )                        
  Ranking[, "RankingNmbr"] <- as.character(Ranking[, "RankingNmbr"])
                        
  # Assigning the same worst ranking to all the insensitive parameters
  row.index <- which(Ranking[,"RelativeImportance"]==0)
  ninsens   <- length(row.index)
  if (ninsens > 0)
    Ranking[row.index, "RankingNmbr"] <- format(as.character(rep(P, ninsens)), width=11, justify="left")
  
  ##############################################################################
  # 12)                    Creating the output                                 #                                 
  ##############################################################################
  
  if (write2disk) { 
    # Writing the output file 'LH_OAT-Ranking.txt'
    Ranking.fname <- paste(file.path(drty.out), "/", "LH_OAT-Ranking.txt", sep="")
    write.table(Ranking, file=Ranking.fname, row.names=FALSE, quote=FALSE)
  } # IF end
  
  ## "pre-allocate" an empty list of length 2
  out <- vector("list", 2)
  
  out[[1]] <- Thetas
  out[[2]] <- Ranking
  names(out) <- c("ParameterSets", "Ranking")
                    
  
  return(out)

} # 'lhoat' END

# File pest2hydroPSO.R
# Part of the hydroPSO package, http://www.rforge.net/hydroPSO/
# Copyright 2012-2012 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
##                             .pst2paramranges                               ##
################################################################################
# Author : Mauricio Zambrano-Bigiarini & Rodrigo Rojas                        ##
################################################################################
# Created: 08-Nov-2012                                                        ##
# Updates: 09-Nov-2012                                                        ##
################################################################################
# Purpose  : To write the 'ParamRanges.txt' hydroPSO input file               ##
################################################################################

# 'drty.model': character, with the name of the directory with the input and exe
#               files of the model
# 'names'     : character vector, with the names of the parameters to be calibrated
# 'ini'       : numeric, with the initial guess used in PEST for each parameter
#               to be calibrated (not used in hydroPSO)
# 'min'       : numeric, with the lowest boundaries for the the parameters to be 
#               calibrated
# 'max'       : numeric, with the highest boundaries for the the parameters to be 
#               calibrated
# 'fname.out' : character, with the name of the output text file with the boudaries
#               for each parameter to be calibrated


.pst2paramranges <- function(drty.model, names, ini, min, max, 
                             fname.out="ParamRanges.txt") {

  drty.bak <- getwd()
  setwd(drty.model)
  
  nparam <- length(names)
  
  if ( (nparam!=length(ini)) | (nparam!=length(min)) | (nparam!=length(max)) )
    stop("Invalid argument: dimensions do not match !")
    
  field.names <- c("ParameterNmbr", "ParameterName", "MinValue", "MaxValue", "IniPEST")
  
  tmp <- matrix(NA, ncol=5, nrow=nparam)
  tmp <- as.data.frame(tmp)
  tmp[,1] <- 1:nparam
  tmp[,2] <- names
  tmp[,3] <- min
  tmp[,4] <- max
  tmp[,5] <- ini
  
  colnames(tmp) <- field.names
  
  write.table(tmp, file=fname.out, row.names=FALSE, quote=FALSE)
  
  setwd(drty.bak)

} # '.pst2paramranges' END



################################################################################
##                             .pst2paramfiles                               ##
################################################################################
# Author : Mauricio Zambrano-Bigiarini & Rodrigo Rojas                        ##
################################################################################
# Created: 08-Nov-2012                                                        ##
# Updates: 09-Nov-2012                                                        ##
################################################################################
# Purpose  : To write the 'ParamFiles.txt' hydroPSO input file               ##
################################################################################
.pst2paramfiles <- function(drty.model, tpls, inputs, param.names, 
                            fname.out="ParamFiles.txt", DecimalPlaces=5) {

  drty.bak <- getwd()
  setwd(drty.model)
  
  ntpl    <- length(tpls)
  ninputs <- length(inputs)
  nparam  <- length(param.names)
  
  if ( (ntpl!=ninputs) )
    stop("Invalid argument: dimensions do not match !")
    
  field.names <- c("ParameterNmbr", "ParameterName", "Filename", 
                   "Row.Number", "Col.Start", "Col.End", "DecimalPlaces")
  
  #tmp <- matrix(NA, ncol=length(filed.names), nrow=nparam)
  
  # output creation
  tmp <- matrix(NA, ncol=length(field.names), nrow=1)
  tmp <- as.data.frame(tmp)
  colnames(tmp) <- field.names
  
  out <- vector("list", nparam)
  for (i in 1:nparam) out[[i]] <- tmp
  
  
  for (p in 1:nparam) {
  
  for (f in 1:ntpl) {
  
    if (!file.exists(tpls[f])) stop("Invalid argument: file '", tpls[f], " does not exist !!")
    x <- readLines(tpls[f])
    
    # getting the token
    token <- strsplit(x[1], " ", useBytes=TRUE)[[1]][2]
    
      for (l in 2:length(x)) {
          
          exists <- grep(param.names[p], x[l], useBytes=TRUE)   
          
          if (length(exists) > 0) {
          
            token.pos <- which(strsplit(x[l], '', useBytes=TRUE)[[1]]==token)
            
            out[[p]] <- rbind(out[[p]], c(p, param.names[p], inputs[f], l, token.pos[1], token.pos[2], DecimalPlaces) )
            if (length(token.pos) >2) {
              for ( t in seq(3, length(token.pos), by=2) )
              out[[p]] <- rbind(out[[p]], c(p, param.names[p], inputs[f], l, token.pos[t], token.pos[t+1], DecimalPlaces) )
            } # IF end
            
          } # IF end
    
      } # FOR l end
    
    } # FOR f end
    
     # removing dummy 1st row
     out[[p]] <- out[[p]][-1,]
   } # FOR 'p' end
   
  for (p in 1:nparam) tmp <- rbind(tmp, out[[p]])
  
  # removing dummy 1st row
  tmp <- tmp[-1, ]
  
  # identifying possible errors
  error.index <- which ( (as.numeric(tmp[,6]) - as.numeric(tmp[,5]) + 1 ) <=  DecimalPlaces)
  if (length(error.index) > 0) {
    warning("In ParamFiles.txt, decimal places have to be manually corrected:", paste(error.index) )
  } # IF

  # Writing the output file
  write.table(tmp, file=fname.out, row.names=FALSE, quote=FALSE)
  
  setwd(drty.bak)

} # '.pst2paramfiles' END


################################################################################
##                             pest2hydroPSO                                  ##
################################################################################
# Author : Mauricio Zambrano-Bigiarini & Rodrigo Rojas                        ##
################################################################################
# Created: 08-Nov-2012                                                        ##
# Updates: 09-Nov-2012                                                        ##
################################################################################
# Purpose  : To import the PEST input files (.pst, .tpl) to be used within    ##
#            hydroPSO (ParamFiles.txt, ParamRanges.txt, hydroPSO_Rscript.R)   ##
################################################################################
pest2hydroPSO <- function(pst.fname, 
                          drty.pest=NULL, 
                          drty.model=NULL, 
                          drty.out="PSO.in",
                          paramfiles.fname="ParamFiles.txt",
                          paramranges.fname="ParamRanges.txt",
                          DecimalPlaces=5) {
   
  if (missing(pst.fname)) stop("PEST control file is missing ('pst.fname')")                      
  if (is.null(drty.pest)) drty.pest <- dirname(pst.fname)
  if (is.null(drty.model)) drty.model <- dirname(pst.fname)
  
  if (drty.out=="PSO.in") drty.out <- paste(dirname(pst.fname), "/PSO.in", sep="")
  if (!file.exists(drty.out)) dir.create(drty.out, recursive=TRUE)
  
  if (basename(paramfiles.fname)==paramfiles.fname) 
    paramfiles.fname <- paste(drty.out, "/", paramfiles.fname, sep="")
    
  if (basename(paramranges.fname)==paramranges.fname) 
    paramranges.fname <- paste(drty.out, "/", paramranges.fname, sep="")

  ##############################################################################
  # Reading .pst file
  x <- readLines(pst.fname)
  
  ##############################################################################
  # 1.a) Getting the number of parameters and observations
  values    <- as.numeric(strsplit(x[4], " ")[[1]])
  nna.index <- which(!is.na(values))
  
  nparam <- values[nna.index][1]
  nobs   <- values[nna.index][2]
  
  # 1.b) Getting the number of input files (.tpl) and output files (.ins)
  files     <- strsplit(x[5], " ")[[1]]
  spc.index <- which(files=="")  
  files     <- files[-spc.index]
  ntpl      <- as.numeric(files[1])
  nins      <- as.numeric(files[2])
  
  
  ##############################################################################
  # 2) Getting Param names and Ranges
  L <- 0
  params.stg <- "* parameter data"
  ini.index <- which(x==params.stg) 
  
  L <- length(ini.index)
  if (L > 0) {
    suppressWarnings(tmp <- read.table(file=pst.fname,skip=ini.index,header=FALSE,nrows=nparam,colClasses
        =c("character","NULL","NULL","numeric","numeric","numeric","NULL","NULL","NULL","NULL"),fill=TRUE,na.strings=""))
        
    param.names <- tmp[,1]
    param.ini   <- as.numeric(tmp[,2])
    param.min   <- as.numeric(tmp[,3])
    param.max   <- as.numeric(tmp[,4])
  } else stop("Invalid pst file: ", params.stg, " does not exist !")
  
  # writing 'ParamRanges.txt'
  .pst2paramranges(drty.model=drty.model, names=param.names, ini=param.ini,
                  min=param.min, max=param.max, 
                  fname.out=paramranges.fname)
  
  
  ##############################################################################
  # 3) Getting observations
  L <- 0
  obs.stg <- "* observation data"
  ini.index <- which(x==obs.stg) 
  
  L <- length(ini.index)
  if (L > 0) {
    suppressWarnings(tmp <- read.table(file=pst.fname,skip=ini.index,header=FALSE,nrows=nobs,colClasses
        =c("NULL","numeric","NULL","NULL"),fill=TRUE,na.strings=""))
        
    obs <- tmp[,1]
  } else stop("Invalid pst file: ", obs.stg, " does not exist !")
  
  # Writing the output file
  obs.fname <- paste(drty.out, "/PEST2hydroPSO_OBS.txt", sep="")
  write.table(obs, file=obs.fname, col.names=FALSE, row.names=FALSE, quote=FALSE)
  

  ##############################################################################  
  # 4) Model command line
  L <- 0
  model.stg <- "* model command line"
  ini.index <- which(x==model.stg) 
  
  L <- length(ini.index)
  if (L > 0) {        
    model.exe <- x[ini.index+1]
  } else stop("Invalid pst file: ", model.stg, " does not exist !")
  

  ##############################################################################  
  # 5) Getting Param filenames and locations (.tpl's and .ins's)
  L <- 0
  io.stg <- "* model input/output"
  ini.index <- which(x==io.stg) 
  
  L <- length(ini.index)
  if (L > 0) {
    suppressWarnings(tmp <- read.table(file=pst.fname,skip=ini.index,header=FALSE,nrows=ntpl+nins,colClasses
        =c("character","character"),fill=TRUE,na.strings=""))
        
    tpls   <- tmp[1:ntpl,1]
    inputs <- tmp[1:ntpl,2]
    ins    <- tmp[(ntpl+1):(ntpl+nins),]
  } else stop("Invalid pst file: ", io.stg, " does not exist !")
  
  # Writing ParamFiles.txt
  .pst2paramfiles(drty.model=drty.model, tpls=tpls, inputs=inputs, 
                 param.names=param.names, fname.out=paramfiles.fname, 
                 DecimalPlaces=DecimalPlaces)
                 
                 
  ##############################################################################  
  # 6) Creating the Rscript used for running hydroPSO
  rscript.fname <- system.file("Rscripts/hydroPSO-Rscript.R", package="hydroPSO")
  dst.fname     <- paste(drty.model, "/Rscripts/hydroPSO-Rscript.R", sep="")
  if (!file.exists(dirname(dst.fname))) dir.create(dirname(dst.fname), recursive=TRUE)
  file.copy(rscript.fname, dst.fname, overwrite=TRUE)
  
  ##############################################################################  
  # 7) Modifying the Rscript used for running hydroPSO
  
  # rading the Rscript
  x <- readLines(dst.fname)
  
  # Changing model directory
  x[25] <- paste("model.drty <- \"", drty.model, "\"", sep="")
  
  # Changing model exe name
  x[64] <- paste("exe.fname= \"", model.exe, "\"", sep="")  
  
  # Writing the modified file
  if (file.exists(dst.fname)) file.remove(dst.fname)
  writeLines(x, con=dst.fname)
  
  ##############################################################################  
  # 8) Output
  message("[          PEST2hydroPSO finished !!                     ]")
  message("[R script to run hydroPSO available in: '", dst.fname, "']")
  message("[Before running hydroPSO, you MUST modify the section:    ")
  message("  'User-defined variables' in '", basename(dst.fname), "']")
  
} # 'pest2hydroPSO' END

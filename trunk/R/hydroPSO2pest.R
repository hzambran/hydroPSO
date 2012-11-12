# File hydroPSO2pest.R
# Part of the hydroPSO package, http://www.rforge.net/hydroPSO/
# Copyright 2012-2012 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
##                             .read_paramrange                               ##
################################################################################
# Author : Mauricio Zambrano-Bigiarini & Rodrigo Rojas                        ##
################################################################################
# Created: 12-Nov-2012                                                        ##
# Updates: 12-Nov-2012                                                        ##
################################################################################
# Purpose  : To read the 'ParamRanges.txt' hydroPSO input file               ##
################################################################################

# 'drty.model': character, with the name of the directory with the input and exe
#               files of the model

.read_paramranges <- function(drty.model, fname="ParamRanges.txt") {
  
  setwd(drty.model)
  
  # Reading the file
  x <- read.table(fname, header=TRUE, stringsAsFactors=FALSE)
  
  return(x)

} # '.read_paramranges' END



################################################################################
##                             .read_paramfiles                               ##
################################################################################
# Author : Mauricio Zambrano-Bigiarini & Rodrigo Rojas                        ##
################################################################################
# Created: 12-Nov-2012                                                        ##
# Updates: 12-Nov-2012                                                        ##
################################################################################
# Purpose  : To read the 'ParamFiles.txt' hydroPSO input file                ##
################################################################################

# 'drty.model': character, with the name of the directory with the input and exe
#               files of the model
.read_paramfiles <- function(fname="ParamFiles.txt") {

  x <- read.table(fname, header=TRUE, stringsAsFactors=FALSE)

} # '.read_paramfiles' END


################################################################################
##                           .read_observations                               ##
################################################################################
# Author : Mauricio Zambrano-Bigiarini & Rodrigo Rojas                        ##
################################################################################
# Created: 12-Nov-2012                                                        ##
# Updates: 12-Nov-2012                                                        ##
################################################################################
# Purpose  : To read the 'Observations.txt' hydroPSO output file              ##
################################################################################

# 'drty.model': character, with the name of the directory with the input and exe
#               files of the model
.read_observations <- function(fname="Observations.txt") {

  # reading the first line of the file
  x <- read.table(fname, header=FALSE, nrows=1, stringsAsFactors=FALSE)
  
  if (NCOL(x) == 1) {
    x <- read.table(fname, header=FALSE)
  } else x <- coredata(read.zoo(fname, header=FALSE))
  
  return(as.numeric(x[,1]))

} # '.read_observations' END


################################################################################
##                               .write.tpl                                   ##
################################################################################
# Author : Mauricio Zambrano-Bigiarini & Rodrigo Rojas                        ##
################################################################################
# Created: 12-Nov-2012                                                        ##
# Updates: 12-Nov-2012                                                        ##
################################################################################
# Purpose  : To write a .tpl PEST input file                                  ##
################################################################################
.write.tpl <- function(tpl.fname, modelin.fname, paramfiles, token="#") {

   # Getting the line for the input file
   row.index <- which(paramfiles[,3] == basename(modelin.fname))
   
   # Reading the model input file
   x <- readLines(tpl.fname)
   
   # Writing the tpl file
   for (r in row.index ) {
     # parameter name
     ParameterName <- as.character(paramfiles[r, 2])
     # Row location
     RowNumber     <- paramfiles[r, 4]
     # Columns location
     ColStart      <- paramfiles[r, 5]
     ColEnd        <- paramfiles[r, 6]
     
     L       <- ColEnd - ColStart + 1
     nspaces <- L - 2  - nchar(ParameterName)
     spaces  <- paste(rep(" ", nspaces), collapse="")
     new.stg <- paste(token, ParameterName, spaces, token, sep="")
     
     # Replacing the line with the token and parameter name
     substr(x[RowNumber], start=ColStart, stop=ColEnd) <- new.stg
   } # FOR end
   
   # writing the .tpl to disk
   writeLines(x, con=tpl.fname)
  
} # '.write.tpl' END



################################################################################
##                               hydroPSO2pest                                ##
################################################################################
# Author : Mauricio Zambrano-Bigiarini & Rodrigo Rojas                        ##
################################################################################
# Created: 12-Nov-2012                                                        ##
# Updates: 12-Nov-2012                                                        ##
################################################################################
# Purpose  : To export the content of the hydroPSO files 'ParamRanges.txt'    ##
#            'ParamFiles.txt' to PEST, as a single .pst files with its corres-##
#            ponding .tpl and .ins files                                      ##
################################################################################
hydroPSO2pest <- function(
                          paramfiles.fname="ParamFiles.txt",
                          paramranges.fname="ParamRanges.txt",
                          observations.fname="Observations.txt",
                          exe.fname,
                          drty.model=getwd(),
                          #rscript.fname,
                          pst.fname="hydroPSO2PEST.pst"                       
                          
                          ) {
   
  if (missing(paramfiles.fname)) 
    stop("Missing argument: 'paramfiles.fname'")                      
  
  if (missing(paramranges.fname))
    stop("Missing argument: 'paramranges.fname'")                      
  
  if (missing(exe.fname))
    stop("Missing argument: 'exe.fname'")     
    
  if (missing(observations.fname))
    stop("Missing argument: 'observations.fname'")                      
  
  if (!file.exists(drty.model)) stop("Invalid argument: 'drty.model' does not exist !")
  
  ##############################################################################
  
  if (basename(paramfiles.fname)==paramfiles.fname) 
    paramfiles.fname <- paste(drty.model, "/PSO.in/", paramfiles.fname, sep="")
    
  if (basename(paramranges.fname)==paramranges.fname) 
    paramranges.fname <- paste(drty.model, "/PSO.in/", paramranges.fname, sep="")
    
  if (basename(observations.fname)==observations.fname) 
    observations.fname <- paste(drty.model, "/PSO.out/", observations.fname, sep="") 
    
  if (basename(pst.fname)==pst.fname) 
    pst.fname <- paste(drty.model, "/", pst.fname, sep="")    
    
  if (!file.exists(paramfiles.fname)) stop("Invalid argument: 'paramfiles.fname' does not exist !")
  if (!file.exists(paramranges.fname)) stop("Invalid argument: 'paramranges.fname' does not exist !")
  if (!file.exists(observations.fname)) stop("Invalid argument: 'observations.fname' does not exist in '", dirname(observations.fname), "' !")

  ##############################################################################
  
  # 1) Reading ParamRanges.txt
  x <- .read_paramranges(drty.model, fname=paramranges.fname)
  
  # Param numbers
  param.nmbrs <- x[, 1]
  # Param names
  param.names <- x[, 2]
  # Param boundaries
  param.min   <- x[, 3]
  param.max   <- x[, 4]
  
  # Number of parameters
  nparam <- length(param.names)
  
  ##############################################################################
  
  # 2) Reading Observations.txt
  obs <- .read_observations(fname=observations.fname)
  
  # Number of observations
  nobs <- length(obs)
  
  ##############################################################################
  
  # 3) Reading ParamFiles.txt
  x <- .read_paramfiles(fname=paramfiles.fname)
  
  # Param numbers
  ParameterNmbr <- x[, 1] 
  # Param names
  ParameterName <- x[, 2]
  # Model input names
  Filename      <- as.factor(x[, 3])
  # Row location
  RowNumber     <- x[, 4]
  # Columns location
  ColStart      <- x[, 5]
  ColEnd        <- x[, 6]
  # Number of decimal places
  DecimalPlaces <- x[, 7]
  
  # Name of single-occurrence input files to be modified
  tpl.fnames <- as.character(levels(Filename))
  
  # Number of tpl files
  ntpls <- length(tpl.fnames)
  
  ##############################################################################
  # 4) tpl creation
  
  for (t in 1:ntpls) {
  
    tmp <- t
    l <- nchar(t)
    if (l < 3) tmp <- paste(paste(rep("0", 3-l), collapse=""), t, sep="")
    tpl.fname <- paste(drty.model, "/", "file", tmp, ".tpl", sep="")
    fname.in  <- paste(drty.model, "/", tpl.fnames[t], sep="")
    file.copy(fname.in, tpl.fname, overwrite=TRUE)
    
    # Reading the new .tpl
    tmp <- readLines(tpl.fname)
    
    # Adding a 1st line with the token
    token <- "#"
    first.line <- paste("ptf ", token, sep="")
    tmp <- c(first.line, tmp)
   
    # writing the .tpl to disk
    writeLines(tmp, con=tpl.fname)
    
    .write.tpl(tpl.fname=tpl.fname, modelin.fname=fname.in, paramfiles=x, token=token) 
  } # FOR 't' end
  
  
  ##############################################################################
  # 5) ins creation
  
  x <- c(NA, NA, NA, NA, NA)
  x[1] <- "***THIS FILE IS TO BE CREATED BY THE USER BEFORE RUNNING PEST. SEE TEMPLATE BELOW***"
  x[2] <- "pif @ "
  x[3] <- "@***OUTPUT KEYWORD***@"
  x[4] <- "l1 (obs1)StartCol:EndCol"
  x[5] <- "..."
  
  # writing to disk
  fname.out <- paste(drty.model, "/ModelOut.ins", sep="")
  writeLines(x, con=fname.out)
  
  
  ##############################################################################
  # 6) pst creation
  
  ##pst.template <- system.file("hydroPSO2pest.pst", package="hydroPSO")
  pst.template <- "/dataMZB/2012/mzb_functions/R-mzb_packages/SVN/hydroPSO/trunk/inst/hydroPSO2pest.pst"
  if (!file.exists(dirname(pst.fname))) dir.create(dirname(pst.fname), recursive=TRUE)
  file.copy(pst.template, pst.fname, overwrite=TRUE)
  
  # modifying the .pst
  x <- readLines(pst.fname)
  L <- length(x)
  
  # Number of parameters
  tmp <- as.character(nparam)
  l <- nchar(tmp)
  if (l < 4) tmp <- paste(paste(rep(" ", 4-l), collapse=""), nparam, sep="")
  substr(x[4], start=2, stop=5) <- tmp
  
  # Number of observations
  tmp <- as.character(nobs)
  l <- nchar(tmp)
  if (l < 6) tmp <- paste(paste(rep(" ", 6-l), collapse=""), nobs, sep="")
  substr(x[4], start=7, stop=12) <- tmp
  
  # Number of parameter groups
  tmp <- as.character(nparam)
  l <- nchar(tmp)
  if (l < 5) tmp <- paste(paste(rep(" ", 5-l), collapse=""), nparam, sep="")
  substr(x[4], start=14, stop=18) <- tmp
  
  # Number of .tpl
  tmp <- as.character(ntpls)
  l <- nchar(tmp)
  if (l < 4) tmp <- paste(paste(rep(" ", 4-l), collapse=""), ntpls, sep="")
  substr(x[5], start=2, stop=5) <- tmp
  
  # Number of .ins
  #substr(x[5], start=7, stop=12) <- "1"
  # it should be changed by the user
  
        
    
  # * parameter groups
  x <- c(x[1:11], rep(x[12], nparam), x[13:L])  
  L <- L + nparam - 1
  for (i in 1:nparam) {
    tmp <- param.names[i]
    l <- nchar(tmp)
    if (l < 17) tmp <- paste(paste(rep(" ", 17-l), collapse=""), param.names[i], sep="")
    substr(x[11+i], start=1, stop=17) <- tmp
  } # FOR end
  
  
  # * parameter data
  x <- c(x[1:(13+nparam-1)], rep(x[14+nparam-1], nparam), x[(15+nparam-1):L])  
  L <- L + nparam - 1
  for (i in 1:nparam) {
    # param names
    tmp <- param.names[i]
    l <- nchar(tmp)
    if (l < 17) tmp <- paste(paste(rep(" ", 17-l), collapse=""), param.names[i], sep="")
    substr(x[13+nparam-1+i], start=1, stop=17) <- tmp
    
    # param ini
    tmp <- format((param.max[i] + param.min[i])/2, scientific=TRUE, digits=7)
    tmp <- as.character(tmp)
    l <- nchar(tmp)
    if (l < 13) tmp <- paste(paste(rep(" ", 13-l), collapse=""), tmp, sep="")
    substr(x[13+nparam-1+i], start=33, stop=45) <- tmp
    
    # param min
    tmp <- format(param.min[i], scientific=TRUE, digits=7)
    tmp <- as.character(tmp)
    l <- nchar(tmp)
    if (l < 13) tmp <- paste(paste(rep(" ", 13-l), collapse=""), tmp, sep="")
    substr(x[13+nparam-1+i], start=47, stop=59) <- tmp
    
    # param max
    tmp <- format(param.max[i], scientific=TRUE, digits=7)
    tmp <- as.character(tmp)
    l <- nchar(tmp)
    if (l < 15) tmp <- paste(paste(rep(" ", 15-l), collapse=""), tmp, sep="")
    substr(x[13+nparam-1+i], start=61, stop=76) <- tmp
    
  } # FOR end
  
  
  # * observation data
  x <- c(x[1:(17+2*(nparam-1))], rep(x[17+2*(nparam-1)+1], nobs), x[(17+2*(nparam-1)+2):L])  
  
    
  L <- L + nobs - 1
  for (i in 1:nobs) {
  
    # i-th observation name
    l <- nchar(i)
    if (l < 5) tmp <- paste("obs", paste(rep("0", 5-l), collapse=""), i, sep="")
    substr(x[17+2*(nparam-1)+i], start=1, stop=8) <- tmp
    
    # i-th observation value
    tmp <- format(obs[i], scientific=TRUE, digits=18)
    tmp <- as.character(tmp)
    l <- nchar(tmp)
    if (l < 23) tmp <- paste(paste(rep(" ", 23-l), collapse=""), tmp, sep="")
    substr(x[17+2*(nparam-1)+i], start=14, stop=36) <- tmp
  
  } # FOR end


  
  # * model command line
  x[20+2*(nparam-1)+(nobs-1)] <- exe.fname
  
  # * model input/output
  x <- c(x[1:(21+2*(nparam-1)+(nobs-1))], rep(x[21+2*(nparam-1)+(nobs-1)+1], ntpls), x[(21+2*(nparam-1)+(nobs-1)+2):L])  
  L <- L + ntpls - 1
  
  for (i in 1:ntpls) {
  
    # i-th .tpl filename
    l <- nchar(i)
    if (l < 3) tmp <- paste("file", paste(rep("0", 3-l), collapse=""), i, sep="")
    substr(x[21+2*(nparam-1)+(nobs-1)+i], start=1, stop=11) <- tmp
    
    # i-th model input file
    tmp <- as.character(tpl.fnames[i])
    l <- nchar(tmp)
    if (l < 12) tmp <- paste(paste(rep(" ", 12-l), collapse=""), tpl.fnames[i], sep="")
    
    substr(x[21+2*(nparam-1)+(nobs-1)+i], start=13, stop=13+l+1) <- tmp
  
  } # FOR end
  
  # writing to disk
  writeLines(x, con=pst.fname)
  
  
  ##############################################################################  
  # 8) Output
  message("[                                                        ]")
  message("[          hydroPSO2PEST finished !!                     ]")
  message("[ PEST input files available in: '", drty.model, "']")
  message("[ Before running PEST, you MUST check *.pst and *.ins files")
  
} # 'pest2hydroPSO' END



#setwd("/mnt/netapp1/nahaUsers/rojasro/test_functions/SWAT2005_hydroPSO")
#hydroPSO2pest(            paramfiles.fname="ParamFiles.txt",
#                          paramranges.fname="ParamRanges.txt",
#                          observations.fname="PEST2hydroPSO_OBS.txt",
#                          exe.fname="./swat2005.out",
#                          drty.model="/mnt/netapp1/nahaUsers/rojasro/test_functions/SWAT2005_hydroPSO",
#                          #rscript.fname,
#                          pst.fname="hydroPSO2PEST_test1.pst"                       
#                          
#                          )

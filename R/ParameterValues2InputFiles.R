# Part of the hydroPSO package, http://www.rforge.net/hydroPSO/
# Copyright 2010-2024 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
#                       ParameterValues2InputFiles                             #
################################################################################
# Purpose    : To write several values into several plain text files, by using #
#              the filenames, row and column positions defined in the          #
#              'ParamFiles.fname' file                                         #
################################################################################
# Output     : A mofified text file ('filename')                               #
################################################################################
# Author     : Mauricio Zambrano-Bigiarini                                     #
# Started    : 15-Dec-2010 at JRC Ispra                                        #
# Updates    : 12-May-2011                                                     #
#              27-Jan-2021                                                     #
#              14-Nov-2024 ; 30-Nov-2024                                       #
################################################################################
ParameterValues2InputFiles <- function(NewValues,
                                       ParamFiles.fname="ParamFiles.txt",
                                       ParamRanges.fname="ParamRanges.txt",
                                       verbose=TRUE
                                       ) {

  # Checking 'NewValues'
  if ( is.na( match(class(NewValues), c("numeric", "integer") ) ) )
    stop("Invalid argument: class(NewValues) has to be in c('numeric', 'integer')") 
  
  # Number of values provided by the user
  nval <- length(NewValues)
  
  # Reading the file with the location of the paramters
  ParamFiles      <-  read.paramfile(file=ParamFiles.fname)
  ParamRangesFull <-  read.ParameterRanges(ParamRanges.fname=ParamRanges.fname, 
                                           flag.full=TRUE, verbose=verbose)
  
  n1 <- length(levels(as.factor(ParamFiles[,1]))) # Number of Param IDs
  n2 <- length(levels(as.factor(ParamFiles[,2]))) # Number of Param Names
  if ( n1 != n2)
    stop( paste("In '", ParamFiles.fname, "' : Number of 'ParameterNmbr' != Number of 'ParameterName' (", n1, " != ", n2, ")", sep="" ) ) 
  if ( n1 != nval)
    stop( paste("Number of Parameters != Number of Values' (", n1, " != ", nval, ")", sep="" ) )
       
  # Number of files that have to be changed
  nfiles <- nrow(ParamFiles)

  # Loop in all the files that have to be changed
  for (i in 1:nfiles) {

    ParamID    <- ParamFiles[i,1]
    ParamName  <- ParamFiles[i,2]
    filename   <- ParamFiles[i,3]
    lrow       <- ParamFiles[i,4]
    col.ini    <- ParamFiles[i,5]
    col.fin    <- ParamFiles[i,6]
    decimals   <- ParamFiles[i,7]
    
    if(ncol(ParamFiles) >= 8){
      refValue    <- ParamFiles[i,8] # numeric, only used when change.type == "mult" |  change.type == "addi", reference value for making de parameter modification
      change.type <- ParamRangesFull[ParamName,5] # character, specification of the type of parameter modification ("repl", "mult", "addi")
      minValue    <- ParamRangesFull[ParamName,6]
      maxValue    <- ParamRangesFull[ParamName,7]
    }else{
      change.type <- "repl"
      refValue    <- 0
      minValue    <- 0
      maxValue    <- 0      
    }

    ModifyInputFile(ParamID=ParamName, newvalue=NewValues[ParamID], 
                    filename=filename, row=lrow, col.ini=col.ini, col.fin=col.fin, 
                    decimals=decimals, change.type=change.type, 
                    refValue=refValue, minValue=minValue, maxValue=maxValue,
                    verbose=verbose)
 
  } # FOR end
              
  
} # 'ParameterValues2InputFiles' end


#ParameterValues2InputFiles( NewValues=c(100.111, 200.122, 300.133), ParamFiles.fname="./PSO/ParamFiles.txt" )

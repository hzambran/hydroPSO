# File read_verification_results.R
# Part of the hydroPSO R package, https://github.com/hzambran/hydroPSO
#                                 http://cran.r-project.org/web/packages/hydroPSO
#                                 http://www.rforge.net/hydroPSO/
# Copyright 2026 Mauricio Zambrano-Bigiarini & Rodrigo Rojas
# Distributed under GPL 2 or later

################################################################################
#                         'read_verification_results'                          #
################################################################################
# Author : Mauricio Zambrano-Bigiarini & Rodrigo Rojas                         #
# Started: 09-Sep-2026                                                         #
################################################################################
# Purpose:                                                                     #
# This function reads the output files produced by 'verification' when          #
# 'write2disk=TRUE':                                                           #
# 1) Verification-ModelOut.txt    : model outputs and goodness-of-fit values   #
# 2) Verification-ParamValues.txt : parameter values and goodness-of-fit values#
################################################################################
# Output:                                                                      #
# a list with the following elements                                           #
# 1) gofs        : numeric with all goodness-of-fit values computed during     #
#                  verification                                                #
# 2) model.values: data.frame with the model outputs corresponding to each     #
#                  parameter set                                               #
# 3) best.gof    : numeric with the best goodness-of-fit value                 #
# 4) best.param  : numeric with the best parameter set                         #
# 5) params      : data.frame with all verification parameter sets             #
################################################################################

read_verification_results <- function(drty.out="Verification.out",
                                      MinMax=c("min", "max"),
                                      beh.thr=NA,
                                      modelout.cols=NULL,
                                      verbose=TRUE) {

   ########################       Checkings      ###############################
   # Checking 'drty.out'
   if ( !is.character(drty.out) || (length(drty.out) != 1) || !nzchar(drty.out) )
      stop("Invalid argument: 'drty.out' must be a non-empty character string")

   # Checking 'MinMax'
   MinMax <- match.arg(MinMax)

   # Checking 'beh.thr'
   if ( !is.na(beh.thr) ) {
      if ( !is.numeric(beh.thr) || (length(beh.thr) != 1) )
         stop("Invalid argument: 'beh.thr' must be a numeric value")
   } # IF end

   # Full path to 'drty.out'
   if (basename(drty.out) == drty.out)
      drty.out <- file.path(getwd(), drty.out)
   drty.out <- path.expand(drty.out)

   if ( !dir.exists(drty.out) )
      stop("Invalid argument: The directory '", drty.out, "' does not exist")

   model.file <- file.path(drty.out, "Verification-ModelOut.txt")
   param.file <- file.path(drty.out, "Verification-ParamValues.txt")

   if ( !file.exists(model.file) )
      stop("Invalid argument value: The file '", basename(model.file), "' doesn't exist")

   if ( !file.exists(param.file) )
      stop("Invalid argument value: The file '", basename(param.file), "' doesn't exist")

   ######################## I) Reading #########################################
   if (verbose) message("[                                               ]")
   if (verbose) message("[     Reading verification output files ...     ]")
   if (verbose) message("[                                               ]")

   # File 'Verification-ModelOut.txt'
   if (verbose) message("[ Reading the file '", basename(model.file), "' ... ]")
   model.out <- data.table::fread(file=model.file, header=FALSE, skip=1,
                                  fill=TRUE, data.table=FALSE)

   if (ncol(model.out) < 3)
      stop("Invalid file: '", basename(model.file), "' must have at least three columns")

   names(model.out)[1:2] <- c("ParamNmbr", "GoF")

   # Getting the goodness-of-fit values
   gofs <- model.out[, 2]

   # Getting the model outputs
   model.values <- model.out[, -(1:2), drop=FALSE]
   colnames(model.values) <- paste("sim", 1:ncol(model.values), sep="")

   # If the user only wants some columns of the model output file
   if (!is.null(modelout.cols)) {
      if ( !is.numeric(modelout.cols) )
         stop("Invalid argument: 'modelout.cols' must be numeric")

      if ( any(modelout.cols < 1) || any(modelout.cols > ncol(model.values)) )
         stop("Invalid argument: some values in 'modelout.cols' are outside the available model output columns")

      model.values <- model.values[, modelout.cols, drop=FALSE]
   } # IF end

   rownames(model.values) <- paste("par", model.out[, 1], sep="")

   # File 'Verification-ParamValues.txt'
   if (verbose) message("[ Reading the file '", basename(param.file), "' ... ]")
   param.out <- read.table(file=param.file, header=TRUE, skip=0,
                           check.names=FALSE)

   if (ncol(param.out) < 3)
      stop("Invalid file: '", basename(param.file), "' must have at least three columns")

   if (nrow(param.out) != length(gofs))
      stop("Invalid files: 'Verification-ModelOut.txt' and 'Verification-ParamValues.txt' have different number of parameter sets")

   param.gofs <- param.out[, 2]
   if ( !isTRUE(all.equal(gofs, param.gofs, check.attributes=FALSE)) )
      warning("GoF values in 'Verification-ModelOut.txt' and 'Verification-ParamValues.txt' are different")

   params <- param.out[, -(1:2), drop=FALSE]

   # Filtering out those parameter sets above/below a certain threshold
   if (!is.na(beh.thr)) {
      ifelse(MinMax=="min", beh.row.index <- which(gofs <= beh.thr),
                           beh.row.index <- which(gofs >= beh.thr))

      if (length(beh.row.index) == 0)
         stop("Invalid argument: 'beh.thr' did not select any verification parameter set")

      gofs         <- gofs[beh.row.index]
      model.values <- model.values[beh.row.index, , drop=FALSE]
      params       <- params[beh.row.index, , drop=FALSE]

      if (verbose) message("[ Number of behavioural parameter sets: ", length(gofs), " ]")
   } # IF end

   ######################## II) Best parameter set #############################
   ifelse(MinMax=="min", best.rowindex <- which.min(gofs),
                        best.rowindex <- which.max(gofs))

   best.gof   <- gofs[best.rowindex]
   best.param <- as.numeric(params[best.rowindex, , drop=TRUE])
   names(best.param) <- colnames(params)

   # Creating the final output
   out <- list(gofs=gofs,
               model.values=model.values,
               best.gof=best.gof,
               best.param=best.param,
               params=params)

   return(out)

} # 'read_verification_results' END

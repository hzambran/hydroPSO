# File plot_verification_results.R
# Part of the hydroPSO R package, https://github.com/hzambran/hydroPSO
#                                 http://cran.r-project.org/web/packages/hydroPSO
#                                 http://www.rforge.net/hydroPSO/
# Copyright 2026 Mauricio Zambrano-Bigiarini & Rodrigo Rojas
# Distributed under GPL 2 or later

################################################################################
#                         'plot_verification_results'                          #
################################################################################
# Author : Mauricio Zambrano-Bigiarini & Rodrigo Rojas                         #
# Started: 10-Sep-2026                                                         #
################################################################################

plot_verification_results <- function(drty.out="Verification.out",
                                      ### plot.particles parameters ###
                                      param.names=NULL,
                                      gof.name="GoF",
                                      MinMax=c("min", "max"),
                                      beh.thr=NA,
                                      beh.col="red",
                                      beh.lty=1,
                                      beh.lwd=2,
                                      nrows="auto",
                                      col="black",
                                      ylab=gof.name,
                                      main=NULL,
                                      pch=19,
                                      cex=0.5,
                                      cex.main=1.7,
                                      cex.axis=1.3,
                                      cex.lab=1.5,
                                      breaks="Scott",
                                      freq=TRUE,
                                      do.pairs=FALSE,

                                      #######################################################
                                      #### For ECDFs of parameter values ('params2ecdf') ####
                                      weights=NULL,
                                      byrow=FALSE,
                                      leg.cex=1.2,

                                      #######################################################
                                      ## Parameters for the 3D dotty plots ('plot_NparOF') ###
                                      dp3D.names="auto",
                                      GOFcuts="auto",
                                      colorRamp= colorRampPalette(c("darkred", "red", "orange", "yellow", "green", "darkgreen", "cyan")),
                                      alpha=0.65,
                                      points.cex=0.7,

                                      #######################################################
                                      # Parameters for BestSim vs Obs ('plot_out')
                                      obs=NULL,
                                      obs.tzone=NULL,
                                      modelout.cols=NULL,
                                      ftype="o",
                                      FUN=mean,
                                      #### OPTIONS for ('plot_out') #####
                                      quantiles.desired= c(0.05, 0.5, 0.95),
                                      quantiles.labels= c("Q5", "Q50", "Q95"),

                                      #######################################################
                                      #####################     PNG options     #############
                                      do.png=FALSE,
                                      png.res=90,

                                      png.width=1500,
                                      png.height=900,

                                      params.png.width=1500,
                                      params.png.height=900,

                                      dotty.png.fname="Verification_Params_DottyPlots.png",
                                      hist.png.fname ="Verification_Params_Histograms.png",
                                      bxp.png.fname="Verification_Params_Boxplots.png",
                                      ecdf.png.fname ="Verification_Params_ECDFs.png",
                                      pruns.png.fname="Verification_Params_ValuesPerRun.png",
                                      dp3d.png.fname ="Verification_Params_dp3d.png",
                                      pairs.png.fname="Verification_Params_Pairs.png",
                                      modelout.best.png.fname="Verification_ModelOut_BestSim_vs_Obs.png",
                                      modelout.quant.png.fname="Verification_ModelOut_Quantiles.png",

                                      verbose=TRUE,
                                      skip.incompatible.obs=FALSE) {

   ######################## I) Reading #########################################

   # Full path to 'drty.out'
   if (basename(drty.out) == drty.out)
     drty.out <- paste(getwd(), "/", drty.out, sep="")

   drty.out <- path.expand(drty.out)

   # Checking 'drty.out' if necessary
   if ( !file.exists(drty.out) )
     stop("Invalid argument: the directory '", drty.out, "' does not exist !")

   # Checking 'MinMax'
   MinMax <- match.arg(MinMax)

   # Checking 'skip.incompatible.obs'
   if ( !is.logical(skip.incompatible.obs) || (length(skip.incompatible.obs) != 1L) || is.na(skip.incompatible.obs) )
     stop("Invalid argument: 'skip.incompatible.obs' must be a logical value")

   # PNG directory
   png.drty <- "pngs"

   # Full path to 'png.drty'
   if (basename(png.drty) == png.drty)
     png.drty <- file.path(drty.out, png.drty)

   # Creating 'png.drty' if necessary
   if ( do.png & (!file.exists(png.drty)) )
     dir.create(png.drty)

   # Adding path to PNG files, if necessary
   if (basename(dotty.png.fname) == dotty.png.fname)
      dotty.png.fname <- file.path(png.drty, dotty.png.fname)
   if (basename(hist.png.fname) == hist.png.fname)
      hist.png.fname <- file.path(png.drty, hist.png.fname)
   if (basename(bxp.png.fname) == bxp.png.fname)
      bxp.png.fname <- file.path(png.drty, bxp.png.fname)
   if (basename(ecdf.png.fname) == ecdf.png.fname)
      ecdf.png.fname <- file.path(png.drty, ecdf.png.fname)
   if (basename(pruns.png.fname) == pruns.png.fname)
      pruns.png.fname <- file.path(png.drty, pruns.png.fname)
   if (basename(dp3d.png.fname) == dp3d.png.fname)
      dp3d.png.fname <- file.path(png.drty, dp3d.png.fname)
   if (basename(pairs.png.fname) == pairs.png.fname)
      pairs.png.fname <- file.path(png.drty, pairs.png.fname)
   if (basename(modelout.best.png.fname) == modelout.best.png.fname)
      modelout.best.png.fname <- file.path(png.drty, modelout.best.png.fname)
   if (basename(modelout.quant.png.fname) == modelout.quant.png.fname)
      modelout.quant.png.fname <- file.path(png.drty, modelout.quant.png.fname)

   #############################################################################
   # 1.1) Reading all the results of verification
   res <- read_verification_results(drty.out=drty.out, MinMax=MinMax,
                                    beh.thr=beh.thr,
                                    modelout.cols=modelout.cols,
                                    verbose=verbose)
   #############################################################################

   # 1.2) Assignments
   params               <- res[["params"]]
   gofs                 <- res[["gofs"]]
   model.values         <- res[["model.values"]]

   ifelse(MinMax=="min", best.rowindex <- which.min(gofs),
                        best.rowindex <- which.max(gofs))
   model.best <- as.numeric(model.values[best.rowindex, , drop=TRUE])

   # If 'param.names' was provided
   if (!is.null(param.names)) {

     # Number of parameters that will be analysed
     npar <- length(param.names)

     # Checking 'param.names'
     for ( i in 1:npar) {
       if ( !(param.names[i] %in% colnames(params)) )
         stop("Invalid argument: The field '", param.names[i], "' does not exist in 'params' !")
     } # FOR end

     # Subsetting
     params <- params[, param.names, drop=FALSE]

   } # IF end

   ######################## I.3) Observations ##################################
   dates <- NULL

   if (is.null(obs)) {
      fname <- file.path(drty.out, "Observations.txt")
      if (file.exists(fname)) {
         if  ( length(find.package("zoo", quiet=TRUE)) != 0 ) {
            if ( !is.null(obs.tzone) ) {
               obs <- zoo::read.zoo(fname, tz=obs.tzone)
            } else obs <- zoo::read.zoo(fname)

            dates <- time(obs)
            if ( TRUE && !( is(dates, "POSIXct") | is(dates, "POSIXt") | is(dates, "Date") ) )
               obs <- coredata(obs)
         } else {
            obs <- read.table(fname, header=FALSE, skip=0)
            obs <- obs[, 1]
         } # ELSE end
      } else obs <- NA
   } # IF end

   if (!is.null(modelout.cols) && is.numeric(obs) && (length(obs) >= max(modelout.cols)))
      obs <- obs[modelout.cols]

   model.obs.length.compatible <- TRUE
   if ( !is.null(obs) & is.numeric(obs) ) {
      if ( length(obs) != ncol(model.values) ) {
         model.obs.length.compatible <- FALSE
         msg <- paste0("'length(obs) != ncol(sims)' ", length(obs), "!=", ncol(model.values),
                       " !!")
         if (skip.incompatible.obs) {
            warning("Skipping observation-dependent plots because ", msg)
         } else stop("Invalid argument: ", msg)
      } # IF end
   } # IF end

   ######################## II) Plotting #######################################
   if (verbose) message("[                                               ]")
   if (verbose) message("[                  Plotting ...                 ]")
   if (verbose) message("[                                               ]")

   # 2.1) Plotting parameter values:
   #      1) Dotty Plots,
   #      2) Histograms,
   #      3) Boxplots
   #      4) Correlation Matrix (optional)
   #      5) Empirical CDFs
   #      6) Parameter Values Against Number of Model Evaluations
   #      7) (pseudo)3D dotty plots
   plot_particles(#### 'plotparam' parameters ####
                  params=params,
                  gofs= gofs,
                  gof.name=gof.name,
                  MinMax=MinMax,
                  beh.thr=NA,
                  beh.col=beh.col,
                  beh.lty=beh.lty,
                  beh.lwd=beh.lwd,
                  nrows=nrows,
                  col=col,
                  ylab=ylab,
                  main=main,
                  pch=pch,
                  cex=cex,
                  cex.main=cex.main,
                  cex.axis=cex.axis,
                  cex.lab=cex.lab,
                  breaks=breaks,
                  freq=freq,
                  do.pairs=do.pairs,

                  #####################################################
                  # For ECDFs of parameter values ('params2ecdf')
                  weights=weights,
                  byrow=byrow,
                  leg.cex=leg.cex,

                  #### Parameters for the 3D dotty plots ('plot_NparOF') ####
                  dp3D.names=dp3D.names,
                  GOFcuts=GOFcuts,
                  colorRamp= colorRamp,
                  alpha=alpha,
                  points.cex=points.cex,

                  #####################     PNG options     ####################
                  do.png=do.png,
                  png.res=png.res,
                  png.width=png.width,
                  png.height=png.height,
                  params.png.width=params.png.width,
                  params.png.height=params.png.height,
                  dotty.png.fname=dotty.png.fname,
                  hist.png.fname=hist.png.fname,
                  bxp.png.fname=bxp.png.fname,
                  ecdf.png.fname=ecdf.png.fname,
                  runs.png.fname=pruns.png.fname,
                  dp3d.png.fname=dp3d.png.fname,
                  pairs.png.fname=pairs.png.fname
                  )

   # 2.2) Plotting Sim vs Obs
   obs.is.zoo <- FALSE

   if (model.obs.length.compatible) {

   if ( (length(obs) > 1) & is.numeric(obs) ) {

     L     <- nchar(modelout.best.png.fname)
     fname <- substr(modelout.best.png.fname, 1, L-4)

   if ( zoo::is.zoo(obs) ) {
      if ( TRUE & any(class(time(obs)) %in% c("Date", "POSIXct", "POSIXt")) ) {
      obs.is.zoo <- TRUE
      }
   } # IF end

     # 2.2.1) Correlation between Best Sim and Obs
     if ( obs.is.zoo ) {
       fname2 <- paste(fname, "-Corr.png", sep="")
     } else fname2 <- modelout.best.png.fname

     if (!do.png) dev.new()
     plot_out(sim=model.best,
              obs=obs,
              dates=dates,
              ptype="corr",
              MinMax=MinMax,
              ftype=ftype,
              FUN=FUN,
              verbose=TRUE,

              ####
              main=main,

              leg.cex=leg.cex,
              cex.axis=cex.axis,
              cex.main=cex.main,
              cex.lab=cex.lab,
              #### PNG options ###
              do.png=do.png,
              png.width=png.width,
              png.height=png.height,
              png.res=png.res,
              png.fname=fname2
             )

     # 2.2.2) ggof between Best Sim and Obs
     if( obs.is.zoo ) {
        fname2 <- paste(fname, "-ggof.png", sep="")
        if (!do.png) dev.new()
        plot_out(sim=model.best,
                 obs=obs,
                 dates=dates,
                 ptype="ts",
                 MinMax=MinMax,
                 ftype=ftype,
                 FUN=FUN,
                 verbose=TRUE,

                 ####
                 main=main,

                 leg.cex=leg.cex,
                 cex.axis=cex.axis,
                 cex.main=cex.main,
                 cex.lab=cex.lab,
                 #### PNG options ###
                 do.png=do.png,
                 png.width=png.width,
                 png.height=png.height,
                 png.res=png.res,
                 png.fname=fname2
                )
     } # IF end

   } # IF end

   # 2.3) Plotting ECDFs for model's output OR ECDFS for quantiles of model's output
   if ( (length(obs) > 1) & is.numeric(obs) ) {
   if (!do.png) dev.new()
   if( obs.is.zoo ) {
        plot_out(sim=model.values,
                 obs=obs,
                 dates=dates,
                 ptype="quant2ecdf",
                 MinMax=MinMax,
                 ftype=ftype,
                 FUN=FUN,
                 verbose=TRUE,
                 ####
                 weights=weights,
                 byrow=TRUE,
                 quantiles.desired= quantiles.desired,
                 quantiles.labels= quantiles.labels,
                 ylab="Probability",
                 col="blue",

                 ####
                 main=main,

                 leg.cex=leg.cex,
                 cex.axis=cex.axis,
                 cex.main=cex.main,
                 cex.lab=cex.lab,
                 #### PNG options ###
                 do.png=do.png,
                 png.width=png.width*0.67,
                 png.height=png.height*0.67,
                 png.res=png.res,
                 png.fname=modelout.quant.png.fname
                )
     } else {
        # ecdf only
        plot_out(sim=model.values,
                 obs=obs,
                 dates=dates,
                 ptype="ecdf",
                 MinMax=MinMax,
                 ftype=ftype,
                 FUN=FUN,
                 verbose=TRUE,
                 ####
                 weights=weights,
                 byrow=TRUE,
                 quantiles.desired= quantiles.desired,
                 quantiles.labels= quantiles.labels,
                 ylab="Probability",
                 col="blue",

                 leg.cex=leg.cex,
                 cex.axis=cex.axis,
                 cex.main=cex.main,
                 cex.lab=cex.lab,
                 #### PNG options ###
                 do.png=do.png,
                 png.width=png.width,
                 png.height=png.height,
                 png.res=png.res,
                 png.fname=modelout.quant.png.fname
                )
        # IF end
     } # ELSE end
   } # IF end

   } else if (verbose) {
       message("[ Skipping model-output plots because 'length(obs) != ncol(sims)' in 'Verification-ModelOut.txt' ]")
     } # ELSE end

   # 3) END
   if (verbose) message("[                                               ]")
   if (verbose) message("[             Plots are finished !!             ]")
   if (verbose) message("[                                               ]")

} # 'plot_verification_results' END

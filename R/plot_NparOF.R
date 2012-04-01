# File plot_NparOF.R
# Part of the hydroPSO R package, http://www.rforge.net/hydroPSO/ ; 
#                                 http://cran.r-project.org/web/packages/hydroPSO
# Copyright 2010-2012 Mauricio Zambrano-Bigiarini & Rodrigo Rojas
# Distributed under GPL 2 or later

################################################################################
#                             'plot_NparOF'                                    #
################################################################################
# Author : Mauricio Zambrano Bigarini                                          #
# Started: Nov 30th, 2010                                                      #   
# Updates: 17-Jan-2011 ; 28-Jan-2011 ; 09-Mar-2011                             #
#          17-Feb-2012 ; 21-Feb-2012 ; 09-Mar-2012 ; 23-Mar-2012               #    
################################################################################
# Purpose: For 'n' user-defined parameters, it produces 'sum(1:(npar-1))'      #
#         'plot_2parOF' plots, with the  values of the objective funtion in    #
#         a 2D box,  where the boundaries of each parameter are used as axis.  #
#         The 'sum(1:(npar-1)) plots corresponds to all the posible            #
#         combinations of 2 parameters among all the 'n' parameters provided   #
################################################################################
# nrows  : numeric, with the amount of rows to be used in the plotting window. 
#          If \code{nrows='auto'} the number of columns is automatically computed 
#          depending on the number of parameters in \code{params}

plot_NparOF <- function(params, 
                        gofs,
                        param.names=colnames(params),
                        MinMax=c("min", "max"),
                        nrows="auto",
                        gof.name="GoF", 
                        main=paste(gof.name, "Surface"),
                        GOFcuts="auto",
                        colorRamp= colorRampPalette(c("darkred", "red", "orange", "yellow", "green", "darkgreen", "cyan")),
                        points.cex=0.7, 
                        alpha=1,                       
                        axis.rot=c(0, 0),
                        verbose=TRUE
                        ) {

    
    # Checking 'params'
    if (missing(params)) 
      stop("Missing argument: 'params' must be provided !!" )
      
    # Number of parameter sets
    n <- nrow(params)

    # Checking 'gofs'
    if (missing(gofs)) {
      stop("Missing argument: 'gofs' must be provided !!" )
    } else if (length(gofs) != n)
        stop("Invalid argument: 'length(gofs) != nrow(params)' (", length(gofs), "!=", n, ") !!" )    
        
    # Setting 'MinMax' 
    MinMax <- match.arg(MinMax)

    # Number of parameters that will be analysed
    npar <- length(param.names)

    # creating the varaible that will store the position of the selected parameters within 'params'
    par.pos <- numeric(npar)

    # Checking 'param.names'
    for ( i in 1:npar) {
      if ( !(param.names[i] %in% colnames(params)) )
        stop("Invalid argument: The field '", param.names[i], "' doesn't exist in 'params'")
      
      par.pos[i] <- which(colnames(params) == param.names[i])
    } # FOR end  

    # If the user didn't provide 'GOFcuts', the 5 quantiles are used
    if (length(GOFcuts) == 1){
      if (GOFcuts=="auto") 
        GOFcuts <- unique( quantile( as.numeric(gofs), 
                           probs=c(0, 0.25, 0.5, 0.75, 0.9, 0.95, 1), na.rm=TRUE) )
    } # IF end
   
    # Number of plots that will be drawn   
    nplots <- sum(1:(npar-1))

    plots <- vector("list", nplots)
    
    pos <- 1
    for ( i in 1:(npar-1) ) {
      for (j in ((i+1):npar) ) {
        if (verbose) message("[ Plotting '", param.names[i], "' vs '", param.names[j], "' ]")
        plots[[pos]] <- plot_2parOF(params=params, gofs=gofs, MinMax=MinMax, p1.name=param.names[i], 
                                     p2.name=param.names[j], gof.name=gof.name,
                                     type="sp", main=main, GOFcuts=GOFcuts, colorRamp=colorRamp, 
                                     alpha=alpha, axis.rot=axis.rot, auto.key=FALSE, 
                                     points.cex=points.cex )
                                     
        pos <- pos + 1
      } # FOR j end
    } # For i end

    # Computing the number of rows for the plot 
    nplots <- nplots + 1
    if (nrows == "auto") {
      if ( nplots <= 5 )                   lnr <- 1
      if ( (nplots > 5) & (nplots <= 14) ) lnr <- 2
      if ( nplots > 14 )                   lnr <- ceiling(nplots/7)
    } else lnr <- nrows          
    
    # Defining the plotting window
    nr <- lnr
    nc <- ceiling(nplots/lnr)
    #par(oma=c(1,1,3,0))
    
    pos <- 1
    for (row in 1:nr) {
      for (col in 1:nc) {
        if (pos <= nplots) {
          if ( ( (row==nr) & (col==nc) ) | (pos==nplots) ) {
            #print(plots[[pos]], split = c(col, row, nc, nr), more = FALSE)
          } else print(plots[[pos]], split = c(col, row, nc, nr), more = TRUE)
        } # IF end
        pos <- pos + 1
      } # FOR end
    } # FOR end

    # Drawing the legend, with a dummy empty plot
    gof.levels <- cut(gofs, GOFcuts)
    nlevels    <- length(levels(gof.levels))    
        
    #require(grid)
    a <- lattice::xyplot(1~1, 
                groups=gof.levels,
                type="n", xlab="", ylab="", scales=list(draw=FALSE),
                key = list(x = .5, y = .5, corner = c(0.5, 0.5),
                           title=gof.name,
                           points = list(pch=16, col=colorRamp(nlevels), cex=1.5),
                           text = list(levels(gof.levels))                     
                           ),
                # removing outter box. From: https://stat.ethz.ch/pipermail/r-help/2007-September/140098.html
                par.settings = list(axis.line = list(col = "transparent")),
                axis = function(side, ...) {
                    lattice::axis.default(side = side, ...)
                }
             )
    col <- nc -(nc*nr-nplots)
    print(a, split = c(col, nr, nc, nr), more = FALSE)

} # 'plot_NparOF' END

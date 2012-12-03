%% File ReadPlot_out.Rd
%% Part of the hydroPSO R package, http://www.rforge.net/hydroPSO/ ; 
%%                                 http://cran.r-project.org/web/packages/hydroPSO
%% Copyright 2011-2012 Mauricio Zambrano-Bigiarini & Rodrigo Rojas
%% Distributed under GPL 2 or later

\name{ReadPlot_out}
\Rdversion{1.1}
\alias{read_out}
\alias{plot_out}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Reading/Plotting the 'Model_out.txt' output file of hydroPSO
}
\description{
This function reads the values of the objective function/model output for each particle and iteration with optional plot
}
\usage{
read_out(file="Model_out.txt", modelout.cols=NULL, obs, MinMax=NULL, 
     beh.thr=NA,verbose=TRUE,plot=TRUE,ptype=c("corr","ts","ecdf","quant2ecdf"), 
     ftype="dm", FUN=mean, weights=NULL, byrow=TRUE,
     quantiles.desired= c(0.05,0.5,0.95), 
     quantiles.labels= c("Q5", "Q50", "Q95"), main=NULL, ylab="Probability", 
     col="blue", leg.cex=1.2, leg.pos="bottomright", 
     cex.axis=1.2, cex.main=1.2, cex.lab=1.2, do.png=FALSE, png.width=1500, 
     png.height=900, png.res=90, png.fname="ModelOut_vs_Obs.png")
plot_out(sim, obs, dates=NULL, ptype=c("corr","ts","ecdf","quant2ecdf"), 
     MinMax=NULL, ftype="o", FUN=mean, verbose=TRUE, weights=NULL, byrow=TRUE, 
     quantiles.desired= c(0.05,0.5,0.95), quantiles.labels=c("Q5","Q50","Q95"), 
     main=NULL, ylab="Probability", col="blue", leg.cex=1.2, 
     leg.pos="bottomright", cex.axis=1.2, cex.main=1.2, cex.lab=1.2, 
     do.png=FALSE, png.width=1500, png.height=900, png.res=90, 
     png.fname="ModelOut_vs_Obs.png")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
character, name (including path) of the output file with the values of the model / objective function for each particle and iteration
}
  \item{modelout.cols}{
numeric, column number in \code{file} that store the outputs that have to be read/plotted, without counting the first three that correspond to iteration, particle and GoF.  If \code{modelout.cols=NULL}, all the columns in \code{will be read}, but the first trhee that contains the iteration number, the particle number and the corresponding goodness-of-fit.
}
 \item{sim}{
numeric or zoo vector, simulated equivalent values of the model / objective funtion to be compared against observations
}
  \item{obs}{
OPTIONAL. numeric or zoo vector, observations to be compared against the best simulated value. If \code{obs} is not provided, its values are read from the output \sQuote{Observations.txt} file in the results directory (by default \sQuote{PSO.out})
}
 \item{dates}{
OPTIONAL. character or Date object used to assign time stamps to each element of \code{sim} and \code{obs}. If \code{sim} and/or \code{obs} already have a time stamp, it is over-written by \code{dates} \cr
It must have the same length of \code{sim} and \code{obs} numeric or zoo vectors
}
  \item{MinMax}{
OPTIONAL. character, indicates whether the optimum value corresponds to the minimum or maximum of the the objective function. It is used to filter out model outputs with a non-acceptable performance\cr
Valid values are in: \code{c('min', 'max')}
}
  \item{beh.thr}{
OPTIONAL. numeric, used for selecting only the behavioural parameter sets, i.e. those with a goodness-of-fit value larger/lower than \code{beh.th}, depending on the value of \code{MinMax} \cr
It is used for drawing a horizontal line used for separating behavioural from non behavioural parameter sets
}
  \item{verbose}{
logical, if TRUE, progress messages are printed
}
  \item{plot}{
logical, indicates if a plot with the convergence measures has to be produced
}
  \item{ptype}{
character, type of plot. Valid values are: \cr
-) \kbd{corr}: Scatterplot between the observed values and its best simulated counterpart \cr
-) \kbd{ts}: Only possible for observed values of zoo type. A graphical comparison between observed values and its best simulated counterpart along time. It requires the \pkg{hydroGOF} package. See \code{\link[hydroGOF]{ggof}} \cr
-) \kbd{ecdf}: Empirical CDFs computed and plotted for each column of \code{sim} \cr
-) \kbd{quant2ecdf}: For each model output corresponding to a different parameter set (in rows or columns of \code{sim}, according to the value of \code{byrow}), different quantiles are computed (as many as indicated in \code{quantiles.desired}, and then Empirical CDFs are computed and plotted for each one of the previous quantiles) \cr
}
  \item{ftype}{
OPTIONAL. Only used when \code{plot=TRUE} and \code{ptype=="ts"}.See \code{\link[hydroGOF]{ggof}}
}
  \item{FUN}{
OPTIONAL. Only used when \code{plot=TRUE} and \code{ptype=="ts"}.See \code{\link[hydroGOF]{ggof}}
}
 \item{weights}{
numeric vector, values of the weights to be used for computing the quantiles. See \code{\link{quant2ecdf}}  \cr
Omitting the \code{weights} argument or specifying \code{NULL} or a zero-length vector will result in the usual un-weighted estimates
}
  \item{byrow}{
logical, indicates whether the computations have to be made for each column or for each row of \code{x}. See \code{\link{quant2ecdf}}
When the simulated equivalents are stored in columns, \code{byrow} must be \kbd{TRUE} \cr
When the simulated equivalents are stored in rows, \code{byrow} must be \kbd{FALSE}
}
  \item{quantiles.desired}{
numeric vector, quantiles to be computed for model outputs. Default values are \kbd{c(.025, .5, .975)} ( => 2.5\%, 50\%, 97.5\% ). See \code{\link{quant2ecdf}}
}
  \item{quantiles.labels}{
OPTIONAL. Only used when \code{plot=TRUE} \cr
character vector, names to \code{quantiles.desired}. Default value is \kbd{c("Q5", "Q50", "Q95")}. See \code{\link{quant2ecdf}} 
}
  \item{main}{
OPTIONAL. Only used when \code{plot=TRUE} \cr
title for the plot
}
  \item{ylab}{
OPTIONAL. Only used when \code{plot=TRUE} \cr
title for the y axis. See \code{\link[graphics]{plot}}
}
  \item{col}{
OPTIONAL. Only used when \code{plot=TRUE} \cr
specification for the default plotting colour. See \code{par}
}
  \item{leg.cex}{
OPTIONAL. Only used when \code{plot=TRUE} \cr
character expansion factor *relative* to current 'par("cex")'. Used for text, and provides the default for 'pt.cex' and 'title.cex' Default value = 1.2
}
  \item{leg.pos}{
OPTIONAL. Only used when \code{plot=TRUE} \cr
keyword to be used to position the legend. See \code{\link[graphics]{legend}}
}
  \item{cex.axis}{
OPTIONAL. Only used when \code{plot=TRUE} \cr
numeric, magnification to be used for the axis annotation relative to 'cex'. See \code{\link[graphics]{par}}
}
  \item{cex.main}{
OPTIONAL. Only used when \code{plot=TRUE} \cr
numeric, representing the magnification to be used for main titles relative to the current setting of \code{cex}
}
  \item{cex.lab}{
OPTIONAL. Only used when \code{plot=TRUE} \cr
numeric, representing the magnification to be used for x and y labels relative to the current setting of 'cex'. See \code{\link[graphics]{par}}
}
  \item{do.png}{
logical, indicates if the plot with the comparison between model outputs and observations has to be saved into a PNG file instead of the screen device
}
  \item{png.width}{
OPTIONAL. Only used when \code{do.png=TRUE} \cr
numeric, width of the device. See \code{\link[grDevices]{png}}
}
  \item{png.height}{
OPTIONAL. Only used when \code{do.png=TRUE} \cr
numeric, height of the device. See \code{\link[grDevices]{png}}
}
  \item{png.res}{
OPTIONAL. Only used when \code{do.png=TRUE} \cr
numeric, nominal resolution in ppi which will be recorded in the PNG file, if a positive integer of the device. See \code{\link[grDevices]{png}}
}
  \item{png.fname}{
OPTIONAL. Only used when \code{do.png=TRUE} \cr
character, name of the output PNG file. See \code{\link[grDevices]{png}}
}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{
list with three elements:
\item{model.values}{matrix/data.frame (or numeric) with the values of the model / objective function for each particle and iteration}
\item{model.gofs}{numeric vector with the goodness-of-fit value for each row (or value) in \sQuote{model.values} }
\item{model.best}{numeric with the best model / objective function value. In order to be computed, the user has to provide a valid value for \code{MinMax}}
\item{model.obs}{numeric with the observed values used during the optimisation. See \code{obs}}
}
%%\references{
%%
%%}
\author{
Mauricio Zambrano-Bigiarini, \email{mzb.devel@gmail.com}
}
%%\note{
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{read_results}}, \code{\link{plot_results}}, \code{\link{quant2ecdf}} 
}
\examples{
\dontrun{
# Setting the user home directory as working directory
setwd("~")

# Number of dimensions to be optimised
D <- 5

# Boundaries of the search space (Sphere test function)
lower <- rep(-100, D)
upper <- rep(100, D)

# Setting the seed
set.seed(100)

# Runing PSO with the 'Sphere' test function, writting the results to text files
hydroPSO(fn=sphere, lower=lower, upper=upper,
        control=list(maxit=100, topology="gbest", write2disk=TRUE, plot=TRUE)  
        )

# Reading the convergence measures got by running hydroPSO
setwd("PSO.out")
read_out(MinMax="min") # each particle in a different pannel
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{graph}
\keyword{manip}% __ONLY ONE__ keyword per line

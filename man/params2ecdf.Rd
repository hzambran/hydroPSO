%% File params2ecdf.Rd
%% Part of the hydroPSO R package, http://www.rforge.net/hydroPSO/ ; 
%%                                 http://cran.r-project.org/web/packages/hydroPSO
%% Copyright 2011-2012 Mauricio Zambrano-Bigiarini & Rodrigo Rojas
%% Distributed under GPL 2 or later

\name{params2ecdf}
\Rdversion{1.1}
\alias{params2ecdf}
\alias{params2ecdf.default}
\alias{params2ecdf.matrix}
\alias{params2ecdf.data.frame}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Parameter Values -> Empirical CDFs
}
\description{
This function computes (weighted) empirical CDFs (ECDFs) for each calibrated parameter, by using the parameter values obtained during the optimisation with \code{\link{hydroPSO}} (with optional plot)
}
\usage{
params2ecdf(params, ...)

\method{params2ecdf}{default}(params, param.names=NULL, gofs=NULL, MinMax=NULL, 
        beh.thr=NA, weights=NULL, byrow=FALSE, plot=TRUE, obs=NULL, main=NULL, 
        nrows="auto", ylab="Probability", col="blue", leg.cex=1.2, 
        leg.pos="topleft", cex.axis=1.2, cex.main=1.2, cex.lab=1.2, 
        verbose=TRUE, ..., do.png=FALSE, png.width=1500, png.height=900, 
        png.res=90, png.fname="Params_ECDFs.png")
              
\method{params2ecdf}{matrix}(params, param.names=colnames(params), gofs=NULL, 
        MinMax=NULL, beh.thr=NA, weights, byrow=FALSE, plot=TRUE, obs=NULL, 
        main=NULL, nrows="auto", ylab="Probability", col="blue", leg.cex=1.2, 
        leg.pos="topleft", cex.axis=1.2, cex.main=1.2, cex.lab=1.2, 
        verbose=TRUE, ..., do.png=FALSE, png.width=1500, png.height=900, 
        png.res=90, png.fname="Params_ECDFs.png")
              
\method{params2ecdf}{data.frame}(params, param.names=colnames(params), gofs=NULL, 
        MinMax=NULL, beh.thr=NA, weights, byrow=FALSE, plot=TRUE, obs=NULL, 
        main=NULL, nrows="auto", ylab="Probability", col="blue", leg.cex=1.2, 
        leg.pos="topleft", cex.axis=1.2, cex.main=1.2, cex.lab=1.2, 
        verbose=TRUE, ..., do.png=FALSE, png.width=1500, png.height=900, 
        png.res=90, png.fname="Params_ECDFs.png")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{params}{
matrix or data.frame with the parameter values, where each row represent a different parameter set and each column represent the value of a different model parameter
}
  \item{param.names}{
character vector, names to be used for each parameter in \code{params} (by default its column names)
}
  \item{gofs}{
OPTIONAL. numeric with the values of goodness-of-fit values for each parameter in \code{params} (in the same order!)
}
  \item{MinMax}{
OPTIONAL. character, indicates if the optimum value in \code{params} corresponds to the minimum or maximum of the the objective function. Only used to identify the optimum in the plot \cr
Valid values are in: \code{c('min', 'max')}
}
  \item{beh.thr}{
numeric, used for selecting only the behavioural parameter sets, i.e. those with a goodness-of-fit value (as given in \code{gofs}) greater/less than or equal to \code{beh.thr}, depending on the value of \code{MinMax}\cr
By default \code{beh.thr=NA} and all the parameter sets are considered for the subsequent anlysis
}
  \item{weights}{
numeric vector, values of the weights to be used for computing the empirical CDFs \cr
Omitting the \code{weights} argument or specifying \code{NULL} or a zero-length vector will result in the usual un-weighted estimates
}
  \item{byrow}{
logical, indicates whether the computations have to be made for each column or for each row of \code{params} \cr
When the parameter sets are stored in rows, i.e. values for different model's parameter are stored in columns, \code{byrow} must be \kbd{FALSE} \cr
When the parameter sets are stored in columns, i.e. values for different model's parameter are stored in rows, \code{byrow} must be \kbd{TRUE}
}
  \item{plot}{
logical, indicates whether a plot with the Empirical CDF for each model's parameter has to be produced or not
}
  \item{obs}{
OPTIONAL. Only used when \code{plot=TRUE} \cr
Numeric or zoo object with observed values (one for each \code{params}), which are used in the output plot
}
  \item{main}{
an overall title for the plot
}
  \item{nrows}{
OPTIONAL. Only used when \code{plot=TRUE} \cr
numeric, number of rows to be used in the plotting window. If \code{nrows} is set to \kbd{auto}, the number of rows is automatically computed depending on the number of columns of \code{params}
}
  \item{ylab}{
OPTIONAL. Only used when \code{plot=TRUE} \cr
a title for the y axis. See \code{\link[graphics]{plot}}
}
  \item{col}{
OPTIONAL. Only used when \code{plot=TRUE} \cr
a specification for the default plotting colour. See \code{\link[graphics]{par}}
}
  \item{leg.cex}{
OPTIONAL. Only used when \code{plot=TRUE} \cr
character expansion factor *relative* to current 'par("cex")'. Used for text, and provides the default for 'pt.cex' and 'title.cex'. Default value = 1.2
}
  \item{leg.pos}{
OPTIONAL. Only used when \code{plot=TRUE} \cr
keyword to be used to position the legend. See \code{\link[graphics]{legend}}
}
  \item{cex.axis}{
OPTIONAL. Only used when \code{plot=TRUE} \cr
numeric, magnification to be used for axis annotation relative to the current setting of \code{cex}
}
  \item{cex.main}{
OPTIONAL. Only used when \code{plot=TRUE} \cr
numeric, magnification to be used for main titles relative to the current setting of \code{cex}
}
  \item{cex.lab}{
OPTIONAL. Only used when \code{plot=TRUE} \cr
numeric, magnification to be used for x and y labels relative to the current setting of \code{cex}
}
  \item{verbose}{
logical, if TRUE, progress messages are printed 
}
  \item{\dots}{
further arguments passed to the \code{plot} function or from other methods
}
  \item{do.png}{
logical, indicates if all the figures have to be saved into PNG files instead of the screen device
}
  \item{png.width}{
OPTIONAL. Only used when \code{do.png=TRUE} \cr
numeric with the width of the device. See \code{\link[grDevices]{png}}
}
  \item{png.height}{
OPTIONAL. Only used when \code{do.png=TRUE} \cr
numeric with the height of the device. See \code{\link[grDevices]{png}}
}
  \item{png.res}{
OPTIONAL. Only used when \code{do.png=TRUE} \cr
numeric with the nominal resolution in ppi which will be recorded in the PNG file, if a positive integer of the device. See \code{\link[grDevices]{png}}
}
  \item{png.fname}{
OPTIONAL. Only used when \code{do.png=TRUE} \cr
character, with the filename used to store the PNG file
}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{
a list with \code{nparam} elements, where is the number of parameters given in \code{params}, computed as the number of columns in \code{params}. 

Each element of the list contains the (weighted) empirical CDFs (ECDFs) for each parameter in \code{params}.

Additionally, if \code{plot=TRUE} a maximum of \kbd{nfigs} different figures with the Empirical CDF for each model's parameter is produced, where the maximum number of figures to be produced is computed as \code{nfigs=ceiling(nparam/21)}.
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
%%\references{
%%}
\author{
Mauricio Zambrano-Bigiarini, \email{mzb.devel@gmail.com}
}
%%\note{
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[Hmisc]{wtd.Ecdf}}, \code{\link{quant2ecdf}}
}
\examples{
\dontrun{
# Setting the user's home directory as working directory
setwd("~")

# matrix with 100 random uniform parameter sets (in rows) for 10 different model's 
# parameters (in columns)
params           <- matrix(rnorm(1000), ncol=10, nrow=100)
colnames(params) <- paste("Param", 1:10, sep="")

# empirical CDFs for each one of the 10 parameters in 'params', with equal weight 
# for each one of the 100 parameter sets
params2ecdf(params, weights=rep(1,100)) 
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{math}
\keyword{manip}
\keyword{graph}

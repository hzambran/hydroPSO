%% File ReadPlot_params.Rd
%% Part of the hydroPSO R package, http://www.rforge.net/hydroPSO/ ; 
%%                                 http://cran.r-project.org/web/packages/hydroPSO
%% Copyright 2008-2012 Mauricio Zambrano-Bigiarini & Rodrigo Rojas
%% Distributed under GPL 2 or later

\name{ReadPlot_params}
\Rdversion{1.1}
\alias{read_params}
\alias{read_params.default}
\alias{plot_params}
\alias{plot_params.default}
\alias{plot_params.data.frame}
\alias{plot_params.matrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Reading/Plotting the values of different parameter sets
}
\description{
This function reads a file containing different parameter sets and their corresponding goodness-of-fit values \cr

If \code{file} is one of the following: \cr
-) \kbd{modelpara.out},       created by the GLUE algorithm of SWAT-CUP, \cr
-) \kbd{modelpara.beh},       created by the GLUE algorithm of SWAT-CUP, \cr
-) \kbd{goal.sf2},            created by the SUFI-2 algorithm of SWAT-CUP \cr
-) \kbd{goal.pso},            created by the PSO algorithm of SWAT-CUP \cr
-) \kbd{ParameterValues.log}, created by Nimbus calibration tool (Lisflood model) \cr

\code{header} and \code{skip} are automatically set, in other case, they need to be provided
}
\usage{
read_params(file, ...) 

\method{read_params}{default}(file, header=TRUE, skip=0, param.cols, param.names,
     of.col=NULL, of.name="GoF", na.strings="-9999", plot=TRUE, 
     ptype=c("histogram", "dottyplot", "boxplot", "vioplot", "pairs"), 
     MinMax=NULL, beh.thr=NA, beh.col="red", beh.lty=1, beh.lwd=2, 
     nrows="auto", col="#00000030", ylab=of.name, main=NULL, pch=19, 
     cex=0.5, cex.main=1.5, cex.axis=1.5, cex.lab=1.5, 
     breaks="Scott", freq=TRUE, verbose=TRUE, ..., do.png=FALSE, png.width=1500, 
     png.height=900, png.res=90, png.fname="Parameters.png")

plot_params(params, ...)  
         
\method{plot_params}{default}(params, gofs=NULL, 
     ptype=c("histogram", "dottyplot", "boxplot", "vioplot", "pairs"),  
     param.cols=1:ncol(params), param.names=colnames(params), of.name="GoF", 
     MinMax=NULL, beh.thr=NA,  beh.col="red", beh.lty=1, beh.lwd=2, 
     nrows="auto", col="#00000030", ylab=of.name, main=NULL, pch=19, cex=0.5, 
     cex.main=1.5, cex.axis=1.5, cex.lab=1.5, breaks="Scott", freq=TRUE,
     verbose=TRUE, ..., do.png=FALSE, png.width=1500, png.height=900,
     png.res=90, png.fname="Parameters.png")
     
\method{plot_params}{data.frame}(params, gofs=NULL, 
     ptype=c("histogram", "dottyplot", "boxplot", "vioplot", "pairs"),  
     param.cols=1:ncol(params), param.names=colnames(params), of.name="GoF", 
     MinMax=NULL, beh.thr=NA,  beh.col="red", beh.lty=1, beh.lwd=2, 
     nrows="auto", col="#00000030", ylab=of.name, main=NULL, pch=19, cex=0.5, 
     cex.main=1.5, cex.axis=1.5, cex.lab=1.5, breaks="Scott", freq=TRUE,
     verbose=TRUE, ..., do.png=FALSE, png.width=1500, png.height=900,
     png.res=90, png.fname="Parameters.png")
     
\method{plot_params}{matrix}(params, gofs=NULL, 
     ptype=c("histogram", "dottyplot", "boxplot", "vioplot", "pairs"),  
     param.cols=1:ncol(params), param.names=colnames(params), of.name="GoF", 
     MinMax=NULL, beh.thr=NA,  beh.col="red", beh.lty=1, beh.lwd=2, 
     nrows="auto", col="#00000030", ylab=of.name, main=NULL, pch=19, cex=0.5, 
     cex.main=1.5, cex.axis=1.5, cex.lab=1.5, breaks="Scott", freq=TRUE, 
     verbose=TRUE, ..., do.png=FALSE, png.width=1500, png.height=900,
     png.res=90, png.fname="Parameters.png")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
character, name (including path) of the file containing the results
}
  \item{params}{
data.frame whose rows represent the values of different parameter sets
}
  \item{gofs}{
OPTIONAL. numeric with the values of goodness-of-fit values for each one of the parameters in \code{params} (in the same order!)
}
  \item{header}{
logical, indicates whether the file contains the names of the variables as its first line \cr
If \code{file} is in \kbd{c('modelpara.out', 'modelpara.beh', 'goal.sf2', 'goal.pso', 'ParameterValues.log')} then \code{header} is automatically set
}
  \item{skip}{
numeric (integer), lines of the data file to skip before beginning to read data \cr
If \code{file} is in \kbd{c('modelpara.out', 'modelpara.beh', 'goal.sf2', 'goal.pso', 'ParameterValues.log')} then \code{skip} is automatically set
}
  \item{param.cols}{
numeric, number of the columns in \code{file} that store the values of each parameter   
}
  \item{param.names}{
character, name of the parameters defined by \code{param.cols}
}
  \item{of.col}{
OPTIONAL. numeric, number of the column in \code{file} that store the values of objective function
}
  \item{of.name}{
OPTIONAL. Only used when \code{of.col} is provided. \cr
character, name that will be given to the column \code{of.col}
}
  \item{na.strings}{
character, string which is to be interpreted as NA values. \code{\link[utils]{read.table}}
}
  \item{plot}{
logical, indicates if a dotty-plot with the parameter values versus the objective function has to be produced
}
  \item{ptype}{
OPTIONAL. Only used when \code{plot=TRUE} \cr
character, indicating the type of plot to be done. It must be in: \cr
-) \kbd{dottyplot}: dotty plots for each parameter in \code{params} or \code{file}, with the value of the objective function against the parameter value \cr
-) \kbd{histogram}: histogram for each parameter in \code{params} or \code{file}, with an estimate of the probability distribution each parameter \cr
-) \kbd{boxplot}: box plots (or box-and-whisker diagram) for each parameter in \code{params} or \code{file}, with a graphical summary of the distribution of each parameter, through their five-number summary \cr
-) \kbd{vioplot}: beanplots for each parameter in \code{params} or \code{file}, similar to the boxplots, except that beanplots also show the probability density of the data at different values. See \code{\link[vioplot]{vioplot}}. It requires the \pkg{vioplot} package.
-) \kbd{pairs}: Visualization of a correlation matrix among the parameters and goodness-of-fits measures in \code{params} (or \code{file}) and \code{gofs}. See \code{\link[hydroTSM]{hydropairs}}. It requires the \pkg{hydroTSM} package.
}
  \item{MinMax}{
OPTIONAL \cr
character, indicates whether the optimum value in \code{params} corresponds to the minimum or maximum of the the objective function given in \code{of.col}. It is used to filter out model outputs with a non-acceptable performance \cr
Valid values are in: \code{c('min', 'max')}
}
  \item{beh.thr}{
OPTIONAL \cr
numeric, threshold value used for selecting parameter sets that have to be used in the analysis (\sQuote{behavioural parameters}, using the GLUE terminology) \cr
If \code{MinMax='min'}, only parameter sets with a goodness-of-fit value (given by \code{gofs}) less than or equal to \code{beh.thr} will be considered for the subsequent analysis. \cr
If \code{MinMax='max'}, only parameter sets with a goodness-of-fit value (given by \code{gofs}) greater than or equal to \code{beh.thr} will be considered for the subsequent analysis
}
  \item{beh.col}{
OPTIONAL. Only used when \code{plot=TRUE} \cr
character, colour for drawing a horizontal line for separating behavioural from non behavioural parameter sets
}
  \item{beh.lty}{
OPTIONAL. Only used when \code{plot=TRUE} \cr
numeric, line type for drawing a horizontal line for separating behavioural from non behavioural parameter sets
}
  \item{beh.lwd}{
OPTIONAL. Only used when \code{plot=TRUE} \cr
numeric, width for drawing a horizontal line for separating behavioural from non behavioural parameter sets
}
  \item{nrows}{
OPTIONAL. Only used when \code{plot=TRUE} \cr
numeric, number of rows to be used in the plotting window \cr
If \code{nrows}is set to \kbd{auto}, the number of rows is automatically computed depending on the number of columns of \code{params}
}
  \item{col}{
OPTIONAL. Only used when \code{plot=TRUE} \cr
character, colour to be used for drawing the points of the dotty plots 
}
  \item{ylab}{
OPTIONAL. Only used when \code{plot=TRUE} \cr
character, label for the 'y' axis
}
  \item{main}{
chracter, title for the plot
}
  \item{pch}{
OPTIONAL. Only used when \code{plot=TRUE} \cr
numeric, type of symbol to be used for drawing the points of the dotty plots (e.g., 1: white circle)
}
  \item{cex}{
OPTIONAL. Only used when \code{plot=TRUE} \cr
numeric, values controlling the size of text and points with respect to the default
}
  \item{cex.main}{
OPTIONAL. Only used when \code{plot=TRUE} \cr
numeric, magnification for the main title relative to the current setting of \code{cex}
}
  \item{cex.axis}{
OPTIONAL. Only used when \code{plot=TRUE} \cr
numeric, magnification for axis annotation relative to the current setting of \code{cex}
}
  \item{cex.lab}{
OPTIONAL. Only used when \code{plot=TRUE} \cr
numeric, magnification for x and y labels relative to the current setting of \code{cex}
}
  \item{breaks}{
breaks used for plotting the histograms of the parameter sets. See \code{\link[graphics]{hist}}
}
  \item{freq}{
logical, if TRUE, the histogram graphic is a representation of frequencies, the counts component of the result; if FALSE, probability densities, component density, are plotted (so that the histogram has a total area of one). See \code{\link[graphics]{hist}}
}
  \item{verbose}{
logical, if TRUE, progress messages are printed
}
  \item{\dots}{
OPTIONAL. Only used when \code{plot=TRUE} \cr
further arguments passed to the plot command or from other methods
}
  \item{do.png}{
logical, indicates if the plot with the convergence measures has to be saved into a PNG file instead of the screen device
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
A list with the following elements: \cr
\item{params}{data.frame with the parameter sets tested during the optimisation}
\item{gofs}{numeric with the fitness values computed during the optimisation (each element in 'gofs' corresponds to one row of 'params')}
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
\code{\link[vioplot]{vioplot}}
}
\examples{
# Number of dimensions to be optimised
nparam <- 4

\dontrun{
# Setting the user home directory as working directory
setwd("~")

# Setting the seed
set.seed(100)

# Runing PSO with the 'sphere' test function, writting the results to text files
hydroPSO(
        fn= "sphere", lower=rep(-100, nparam), upper=rep(100, nparam),
        control=list(npart=2*nparam, maxit=100, write2disk=TRUE, plot=TRUE)  
        ) 

# 1) reading ALL the parameter sets used in PSO, and histograms (by default)
params <- read_params(file="~/PSO.out/Particles.txt",  param.cols=4:7, of.col=3)

# 2) summary of the parameter sets and their goodness-
                    
# plotting the parameter sets as dotty plots
plot_params(params=params[["params"]], gofs=params[["gofs"]], 
            ptype="dottyplot", main=fn, MinMax="min", freq=TRUE)
                    
# plotting the parameter sets as boxplots                   
plot_params(params=params[["params"]], ptype="boxplot", MinMax="min")

# plotting the parameter sets as violing plots 
library(vioplot)
plot_params(params=params[["params"]], ptype="vioplot", MinMax="min")


# 2) reading only the parameter sets with a goodness-of-fit measure <= 'beh.thr', 
#    and dotty plots (by default)
params <- read_params(file="~/PSO.out/Particles.txt", param.cols=4:7, of.col=3, 
                      beh.thr=1000, MinMax="min")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{graph}
\keyword{files}% __ONLY ONE__ keyword per line

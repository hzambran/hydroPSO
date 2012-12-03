%% File ReadPlot_particles.Rd
%% Part of the hydroPSO R package, http://www.rforge.net/hydroPSO/ ; 
%%                                 http://cran.r-project.org/web/packages/hydroPSO
%% Copyright 2011-2012 Mauricio Zambrano-Bigiarini & Rodrigo Rojas
%% Distributed under GPL 2 or later

\name{ReadPlot_particles}
\Rdversion{1.1}
\alias{read_particles}
\alias{plot_particles}
\alias{read_velocities}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Reading/Plotting the 'Particles.txt' output file
}
\description{
The function \code{read_particles} reads the \sQuote{Particles.txt} output file, which stores all the parameter sets tested during the optimisation along with their corresponding goodness-of-fit values \cr

The function \code{plot_particles} takes the parameter sets and their corresponding goodness-of-fit value, read by \code{read_particles}, and produces the following plots: \cr
1) Dotty plots \cr
2) Histograms \cr
3) Boxplots \cr
4) Correlation matrix (optional) \cr
5) Empirical CDFs \cr
6) Parameter values vs Number of Model Evaluations \cr
7) (pseudo) 3D dotty plots
}
\usage{
read_particles(file="Particles.txt", verbose=TRUE, plot=TRUE,
     gof.name="GoF", MinMax=NULL, beh.thr=NA, beh.col="red", beh.lty=1, 
     beh.lwd=2, nrows="auto", col="black", ylab=gof.name, main=NULL, 
     pch=19, cex=0.5, cex.main=1.5, cex.axis=1.5, cex.lab=1.5, 
     %%...,
     breaks="Scott", freq=TRUE, do.pairs=FALSE, 
     dp3D.names="auto", GOFcuts="auto", 
     colorRamp= colorRampPalette(c("darkred", "red", "orange", "yellow", 
     "green", "darkgreen", "cyan")), alpha=1, points.cex=0.7,
     legend.pos="topleft", do.png=FALSE, png.width=1500, 
     png.height=900, png.res=90, 
     dotty.png.fname="Params_DottyPlots.png", 
     hist.png.fname="Params_Histograms.png",
     bxp.png.fname="Params_Boxplots.png",
     ecdf.png.fname="Params_ECDFs.png", 
     runs.png.fname="Params_ValuesPerRun.png",
     dp3d.png.fname="Params_dp3d.png", 
     pairs.png.fname="Params_Pairs.png")
           
plot_particles(params, gofs, gof.name="GoF", MinMax=NULL, beh.thr=NA,  
     beh.col="red", beh.lty=1, beh.lwd=2, nrows="auto", col="black",  
     ylab=gof.name, main=NULL, pch=19, cex=0.5, cex.main=1.5,  
     cex.axis=1.5, cex.lab=1.5,
     %%..., 
     breaks="Scott", freq=TRUE, do.pairs=FALSE,                          
     weights=NULL, byrow=FALSE, leg.cex=1.5, 
     dp3D.names="auto", GOFcuts="auto", 
     colorRamp= colorRampPalette(c("darkred", "red", "orange", "yellow", 
     "green", "darkgreen", "cyan")), alpha=1, points.cex=0.7, 
     legend.pos="topleft", verbose=TRUE,  
     do.png=FALSE, png.width=1500, png.height=900, png.res=90, 
     dotty.png.fname="Params_DottyPlots.png", 
     hist.png.fname="Params_Histograms.png",
     bxp.png.fname="Params_Boxplots.png",
     ecdf.png.fname="Params_ECDFs.png", 
     runs.png.fname="Params_ValuesPerRun.png",
     dp3d.png.fname="Params_dp3d.png", 
     pairs.png.fname="Params_Pairs.png")
     
read_velocities(file="Velocities.txt", ... )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
character, name (including path) of the output file with the position and fitness value of each particle and for each iteration
}
  \item{params}{
data.frame whose rows represent the values of different parameter sets
}
  \item{gofs}{
OPTIONAL. numeric with the values of goodness-of-fit values for each parameter in \code{params} (in the same order!)
}
  \item{verbose}{
logical, if TRUE, progress messages are printed
}
  \item{plot}{
logical, indicates if the following figures has to be produced: dotty plots, histograms, empirical CDFs, Parameter Values Against Number of Model Evaluations, and 3D dotty plots of Parameter Values
}
  \item{gof.name}{
character, name to be given to the goodness-of-fit values in all the plots
}
  \item{MinMax}{
OPTIONAL. character, indicates if the optimum value in \code{params} corresponds to the minimum or maximum of the the objective function. Only used to identify the optimum in the plot \cr
Valid values are in: \code{c('min', 'max')}
}
  \item{beh.thr}{
numeric, used for selecting only the behavioural parameter sets, i.e. those with a goodness-of-fit value greater/less than or equal to \code{beh.thr}, depending on the value of \code{MinMax}\cr
By default \code{beh.thr=NA} and all the parameter sets are considered for the subsequent anlysis
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
character, colour for drawing the points of the dotty plots 
}
  \item{ylab}{
OPTIONAL. Only used when \code{plot=TRUE} \cr
character, label for the 'y' axis
}
  \item{main}{
OPTIONAL. Only used when \code{plot=TRUE} \cr
character, title for the plot
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
numeric, magnification for main titles relative to the current setting of \code{cex}
}
  \item{cex.axis}{
OPTIONAL. Only used when \code{plot=TRUE} \cr
numeric, magnification for axis annotation relative to the current setting of \code{cex}
}
  \item{cex.lab}{
OPTIONAL. Only used when \code{plot=TRUE} \cr
numeric, magnification for x and y labels relative to the current setting of \code{cex}
}
  \item{\dots}{
OPTIONAL. Only used when \code{plot=TRUE} \cr
further arguments passed to the plot command or from other methods
}
  \item{breaks}{
OPTIONAL. Only used when \code{plot=TRUE} \cr
breaks for plotting the histograms of the parameter sets. See \code{\link[graphics]{hist}}
}
  \item{freq}{
OPTIONAL. Only used when \code{plot=TRUE} \cr
logical, if TRUE, the histogram graphic is a representation of frequencies, the counts component of the result; if FALSE, probability densities, component density, are plotted (so that the histogram has a total area of one). Defaults to TRUE if and only if breaks are equidistant (and probability is not specified). See \code{\link[graphics]{hist}}
}
  \item{do.pairs}{
OPTIONAL. Only used when \code{plot=TRUE} \cr
logical, indicates whether a correlation matrix among parameters has to be plotted. If the number of parameter sets tried during the optimisation is large, it may require some time. 
}
  \item{weights}{
OPTIONAL. Only used when \code{plot=TRUE} \cr
numeric vector, values of the weights to be used for computing the empirical CDFs. See \code{\link{params2ecdf}}
}
  \item{byrow}{
OPTIONAL. Only used when \code{plot=TRUE} \cr
logical, indicates whether the computations have to be made for each column or for each row of \code{params}. See \code{\link{params2ecdf}}
}
  \item{leg.cex}{
OPTIONAL. Only used when \code{plot=TRUE} \cr
character expansion factor *relative* to current 'par("cex")'. Used for text, and provides the default for 'pt.cex' and 'title.cex'. Default value = 1.2
}
  \item{dp3D.names}{
character, name of all the parameters (usually only the most sensitive ones) that will be used for plotting pseudo-3D plots \cr
If \code{dp3D.names='auto'} half the number of parameters in \code{file} are chosen randomly for plotting. See \code{\link{plot_NparOF}}
}
  \item{GOFcuts}{
numeric, specifies at which values of the objective function \code{gof.name} the colours of the plot have to change. See \code{\link{plot_NparOF}}
}
  \item{colorRamp}{
R function defining the colour ramp to be used for colouring the pseudo-3D dotty plots of Parameter Values, OR character representing those colours. See \code{\link{plot_NparOF}}
}
  \item{alpha}{
numeric between 0 and 1 representing the transparency level to apply to the colors of the pseudo-3D dotty plots. See \code{\link{plot_NparOF}}
}
  \item{points.cex}{
size of the points to be plotted
}
  \item{legend.pos}{
not used yet ...
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
%%  \item{png.drty}{
%%OPTIONAL. Only used when \code{do.png=TRUE} \cr
%%character, with the directory path used to store the png files that are going to be produced.
%%}
%%  \item{png.fname}{
%%OPTIONAL. Only used when \code{do.png=TRUE} \cr
%%character, name of the output PNG file. See \code{\link[grDevices]{png}}
%%}
  \item{dotty.png.fname}{
OPTIONAL. Only used when \code{do.png=TRUE} \cr
character, filename used to store the PNG file with the dotty plots of the parameter values
}
  \item{hist.png.fname}{
OPTIONAL. Only used when \code{do.png=TRUE} \cr
character, filename used to store the PNG file with the histograms of the parameter values
}
  \item{bxp.png.fname}{
OPTIONAL. Only used when \code{do.png=TRUE} \cr
character, filename used to store the PNG file with the boxplots of the parameter values
}
  \item{ecdf.png.fname}{
OPTIONAL. Only used when \code{do.png=TRUE} \cr
character, filename used to store the PNG file with the empirical CDFs of the parameter values
}
  \item{runs.png.fname}{
OPTIONAL. Only used when \code{do.png=TRUE} \cr
character, filename used to store the PNG file with the parameter values vs the number of model evaluations
}
  \item{dp3d.png.fname}{
OPTIONAL. Only used when \code{do.png=TRUE} \cr
character, filename used to store the PNG file with the pseudo-3D plots of all the parameters defined in \code{dp3D.names}
}
  \item{pairs.png.fname}{
OPTIONAL. Only used when \code{do.png=TRUE} \cr
character, filename used to store the PNG file with the correlation matrix among the parameters and goodness-of-fits measures in \code{params} and \code{gofs}. See \code{\link{plot_params}} and \code{\link[hydroTSM]{hydropairs}}
}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{
%%  ~Describe the value returned
\code{read_particles} returns a list with four elements: 
\item{part.params}{numeric or matrix/data.frame with the parameter values for each particle and iteration}
\item{part.gofs}{numeric vector with the goodness-of-fit value for each particle and iteration}
\item{best.param}{numeric with the parameter values of the best particle. In order to be computed, the user has to provide a valid value for \code{MinMax}}
\item{best.gof}{numeric with the best godness-of-fit value among all the particles. In order to be computed, the user has to provide a valid value for \code{MinMax}}
%% ...
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
\code{\link{read_results}}, \code{\link{plot_results}}, \code{\link{read_params}}, \code{\link{plot_params}}
}
\examples{
\dontrun{
# Setting the user home directory as working directory
setwd("~")

# Number of dimensions to be optimised
D <- 4

# Boundaries of the search space (Sphere test function)
lower <- rep(-100, D)
upper <- rep(100, D)

# Setting the seed
set.seed(100)

# Runing PSO with the 'Sphere' test function, writting the results to text files
hydroPSO(fn=sphere, lower=lower, upper=upper,
        control=list(maxit=100, write2disk=TRUE, plot=TRUE)  )   

# reading the 'Particles.txt' output file of hydroPSO, and plotting dotty plots, 
# histograms, eCDFs, ...
setwd("PSO.out")
particles <- read_particles()

# reading only the particles in 'Particles.txt' with a goodness-of-fit value
# lower than 'beh.thr'
particles <- read_particles(beh.thr=1000, MinMax="min")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{graph}
\keyword{files}% __ONLY ONE__ keyword per line

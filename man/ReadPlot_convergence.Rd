%% File ReadPlot_convergence.R
%% Part of the hydroPSO R package, http://www.rforge.net/hydroPSO/ ; 
%%                                 http://cran.r-project.org/web/packages/hydroPSO
%% Copyright 2011-2012 Mauricio Zambrano-Bigiarini & Rodrigo Rojas
%% Distributed under GPL 2 or later

\name{ReadPlot_convergence}
\Rdversion{1.1}
\alias{read_convergence}
\alias{plot_convergence}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Reading/Plotting the values of different parameter sets
}
\description{
This function reads a file containing different parameter sets and ther corresponfing goodness-of-fit values
}
\usage{
read_convergence(file="ConvergenceMeasures.txt", MinMax=NULL, beh.thr=NA, 
     verbose=TRUE, plot=TRUE, col=c("black", "darkolivegreen"), lty=c(1,3), 
     lwd=c(2,2), main="Global Optimum & Normalized Swarm Radius vs Iteration Number", 
     xlab="Iteration Number", ylab=c("Global Optimum", expression(delta[norm])), 
     pch=c(15, 18), cex=1, cex.main=1.4, cex.axis=1.2, cex.lab=1.2, 
     legend.pos="topright", ..., do.png=FALSE, png.width=1500, png.height=900, 
     png.res=90,png.fname="ConvergenceMeasures.png")
           
plot_convergence(x, verbose=TRUE, col=c("black", "darkolivegreen"), lty=c(1,3),
     lwd=c(2,2), main="Global Optimum & Normalized Swarm Radius vs Iteration Number", 
     xlab="Iteration Number", ylab=c("Global Optimum", expression(delta[norm])), 
     pch=c(15, 18), cex=1, cex.main=1.4, cex.axis=1.2, cex.lab=1.2, 
     legend.pos="topright", ...,  do.png=FALSE, png.width=1500, png.height=900, 
     png.res=90, png.fname="ConvergenceMeasures.png")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
character, name (including path) of the file to be read 
}
  \item{verbose}{
logical; if TRUE, progress messages are printed
}
  \item{x}{
data.frame with the convergence outputs obtained with \code{read_convergence}.
}
  \item{MinMax}{
OPTIONAL \cr
character, indicates if the optimum value in \code{params} corresponds to the minimum or maximum of the the objective function. Valid values are in: \code{c('min', 'max')}
}
  \item{beh.thr}{
numeric, used for selecting only the behavioural parameter sets, i.e., those with a goodness-of-fit value larger/lowervalue than \code{beh.th}, depending on the value of \code{MinMax}. \cr
It is only used for drawing a horizontal line used for separating behavioural from non behavioural parameter sets.
}
  \item{plot}{
logical, indicates if a plot with the convergence measures has to be produced
}
  \item{col}{
OPTIONAL. Only used when \code{plot=TRUE} \cr
character, colour to be used for drawing the lines 
}
  \item{lty}{
OPTIONAL. Only used when \code{plot=TRUE} \cr
numeric, line type to be used
}
  \item{lwd}{
OPTIONAL. Only used when \code{plot=TRUE} \cr
numeric, line width 
}
  \item{xlab}{
OPTIONAL. Only used when \code{plot=TRUE} \cr
character, label for the 'x' axis 
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
numeric, type of symbol for drawing the points of the dotty plots (e.g., 1: white circle)
}
  \item{cex}{
OPTIONAL. Only used when \code{plot=TRUE} \cr
numeric, values controlling the size of text and points with respect to the default
}
  \item{cex.main}{
OPTIONAL. Only used when \code{plot=TRUE} \cr
numeric, magnification to be used for main titles relative to the current setting of \code{cex}
}
  \item{cex.axis}{
OPTIONAL. Only used when \code{plot=TRUE} \cr
numeric, magnification to be used for axis annotation relative to the current setting of \code{cex}
}
  \item{cex.lab}{
OPTIONAL. Only used when \code{plot=TRUE} \cr
numeric, magnification to be used for x and y labels relative to the current setting of \code{cex}
}
  \item{legend.pos}{
OPTIONAL. Only used when \code{plot=TRUE} \cr
character, position of the legend. Valid values are in \code{c("bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right", "center")}. See \code{\link[graphics]{legend}}
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
A list with the following elements:
%%  If it is a LIST, use
\item{Iter}{iteration number'}
\item{Gbest}{global optimum for each iteration}
\item{GbestRate}{rate of change of the global optimum (current iter/previous iter)}
\item{IterBestFit}{best performance for the current iteration}
\item{normSwarmRadius}{normalised swarm radious}
\item{[gbest-mean(pbest)]/mean(pbest)}{gbest: global optimum, mean(pbest): mean values of the personal best of all the particles}
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
\code{\link{read_results}}, \code{\link{plot_results}}
}
\examples{
# Setting the user home directory as working directory
setwd("~")

# Number of dimensions to be optimised
nparam <- 4

\dontrun{

# Setting the seed
set.seed(100)

# Runing PSO with the 'sphere' test function, writting the results to text files
hydroPSO(
        fn= sphere, lower=rep(-100, nparam), upper=rep(100, nparam),
        control=list(MinMax="min", write2disk=TRUE, plot=TRUE)  
        )
  
# Reading the convergence measures got by running hydroPSO
setwd("PSO.out")
read_convergence()

}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{graph}
\keyword{manip}% __ONLY ONE__ keyword per line

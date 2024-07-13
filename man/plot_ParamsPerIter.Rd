%% Part of the hydroPSO R package, http://www.rforge.net/hydroPSO/ ; 
%%                                 http://cran.r-project.org/web/packages/hydroPSO
%% Copyright 2011-2012 Mauricio Zambrano-Bigiarini & Rodrigo Rojas
%% Distributed under GPL 2 or later

\name{plot_ParamsPerIter}
\alias{plot_ParamsPerIter}
\alias{plot_ParamsPerIter.default}
\alias{plot_ParamsPerIter.matrix}
\alias{plot_ParamsPerIter.data.frame}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot parameter values against the iteration number
}
\description{
Function to plot the value of each parameter against the iteration number
}
\usage{
plot_ParamsPerIter(params,...)

\method{plot_ParamsPerIter}{default}(params, param.names=colnames(params), 
             main=NULL, xlab="Number of evaluations", nrows="auto", cex=0.5, 
             cex.main=1.2,cex.axis=1.7,cex.lab=1.5, col=rainbow(ncol(params)), 
             lty=3, verbose=TRUE, ..., do.png=FALSE, png.width=1500, 
             png.height=900, png.res=90, png.fname="Params_ValuePerRun.png" )
               
\method{plot_ParamsPerIter}{matrix}(params, param.names=colnames(params), 
             main=NULL, xlab="Number of evaluations", nrows="auto", cex=0.5, 
             cex.main=1.2,cex.axis=1.7,cex.lab=1.5, col=rainbow(ncol(params)), 
             lty=3, verbose=TRUE, ..., do.png=FALSE, png.width=1500, 
             png.height=900, png.res=90, png.fname="Params_ValuePerRun.png" )
               
\method{plot_ParamsPerIter}{data.frame}(params, param.names=colnames(params), 
             main=NULL, xlab="Number of evaluations", nrows="auto", cex=0.5, 
             cex.main=1.2,cex.axis=1.7,cex.lab=1.5, col=rainbow(ncol(params)), 
             lty=3, verbose=TRUE, ..., do.png=FALSE, png.width=1500, 
             png.height=900, png.res=90, png.fname="Params_ValuePerRun.png" )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{params}{
matrix or data.frame with the parameter values, where each row represent a different parameter set, and each column represent the value of a different model's parameter
}
  \item{param.names}{
character vector, names to be used for each model's parameter in \code{params} (by default its column names)
}
  \item{main}{
character, title for the plot
}
  \item{xlab}{
character, title for the x axis. See \code{\link[graphics]{plot}}
}
  \item{nrows}{
numeric, number of rows to be used in the plotting window. If \code{nrows} is set to \kbd{auto}, the number of rows is automatically computed depending on the number of columns of \code{params}
}
  \item{cex}{
numeric, magnification for text and symbols relative to the default. See \code{\link[graphics]{par}}
}
  \item{cex.main}{
numeric, magnification to be used for main titles relative to the current setting of \code{cex}. See \code{\link[graphics]{par}}
}
  \item{cex.axis}{
numeric, magnification to be used for axis annotation relative to the current setting of \code{cex}. See \code{\link[graphics]{par}}
}
  \item{cex.lab}{
numeric, magnification to be used for x and y labels relative to the current setting of \code{cex}. See \code{\link[graphics]{par}}
}
  \item{col}{
specification for the default plotting colour. See \code{\link[graphics]{par}}
}
  \item{lty}{
line type. See \code{\link[graphics]{par}}
} 
  \item{verbose}{
logical, if TRUE, progress messages are printed
}
  \item{\dots}{
further arguments passed to the \code{plot} function or from other methods.
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
A single figure with \code{nparam} number of panels (\code{nparam=ncol(params)}), where each panel has a plot the value of each parameter against the iteration number.
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
%%\references{
%% ~put references to the literature/web site here ~
%%}
\author{
Mauricio Zambrano-Bigiarini, \email{mzb.devel@gmail.com}
}
%%\note{
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{plot_results}}, \code{\link{plot_2parOF}}, \code{\link{plot_NparOF}}, \code{\link{plot_GofPerParticle}}
}
\examples{
# Number of dimensions to be optimised
D <- 5

# Boundaries of the search space (Griewank test function)
lower <- rep(-600, D)
upper <- rep(600, D)

\dontrun{
# Setting the user's home directory as working directory
setwd("~")

# Setting the seed
set.seed(100)

# Running PSO with the 'griewank' test function, writing the results to text files
hydroPSO(fn=griewank, lower=lower, upper=upper,    
         control=list(use.IW = TRUE, IW.type= "linear", IW.w= c(1.0, 0.4),                   
                     write2disk=TRUE) )
  
# reading the 'Particles.txt' output file of PSO
setwd("PSO.out")
particles <- read_particles(plot=FALSE)
                    
# plotting the value of each parameter and the objective function against the 
# iteration number
plot_ParamsPerIter(particles[["part.params"]])
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
\keyword{graph}% __ONLY ONE__ keyword per line

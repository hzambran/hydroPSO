%% Part of the hydroPSO R package, http://www.rforge.net/hydroPSO/ ; 
%%                                 http://cran.r-project.org/web/packages/hydroPSO
%% Copyright 2011-2024 Mauricio Zambrano-Bigiarini & Rodrigo Rojas
%% Distributed under GPL 2 or later

\name{ReadPlot_GofPerParticle}
\alias{read_GofPerParticle}
\alias{plot_GofPerParticle}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
read_GofPerParticle, plot_GofPerParticle
}
\description{
This function reads/plots the parameter values of each particle and the objective function against the iteration number
}
\usage{
read_GofPerParticle(file="Particles_GofPerIter.txt", na.strings="NA", 
     plot=TRUE, ptype="one", nrows="auto", main=NULL, 
     xlab="Number of Iterations", cex=0.4, cex.main=1.5, cex.axis=1.7, 
     cex.lab=1.5, col, lty=3, ylim, verbose=TRUE, do.png=FALSE, 
     png.width=1500, png.height=900, png.res=90, 
     png.fname="Particles_GofPerIter.png")
                 
plot_GofPerParticle(x, ptype="one", nrows="auto", main=NULL, 
     xlab="Number of Iterations", cex=0.4, cex.main=1.5, cex.axis=1.7, 
     cex.lab=1.5, col=rainbow(ncol(x)), lty=3, ylim=NULL, verbose=TRUE, ...,
     do.png=FALSE, png.width=1500, png.height=900, png.res=90, 
     png.fname="Particles_GofPerIter.png")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
character, name (including path) of the file to be read 
}
  \item{na.strings}{
character vector, strings which are to be interpreted as \code{NA} values. See \code{\link[utils]{read.table}}
}
  \item{plot}{
logical, indicates if a plot with the convergence measures has to be produced
}
  \item{x}{
data.frame with the goodness-of-fit measure of each particle per iteration. \cr
The number of columns in \code{x} has to be equal to the number of particles, whereas the number of rows in \code{x} has to be equal to the number of iterations \cr(
(\code{ncol(x)}=  number of particles ; \code{nrow(x)} = number of iterations)
}
  \item{ptype}{
character, representing the type of plot. Valid values are: in c("one", "many"), for plotting all the particles in the smae figure or in one windows per particle, respectively
}
  \item{nrows}{
OPTIONAL. Only used when \code{plot=TRUE} \cr
numeric, number of rows to be used in the plotting window \cr
If \code{nrows}is set to \kbd{auto}, the number of rows is automatically computed depending on the number of columns of \code{x}
}
  \item{main}{
OPTIONAL. Only used when \code{plot=TRUE} \cr
character, title for the plot
}
  \item{xlab}{
OPTIONAL. Only used when \code{plot=TRUE} \cr
character, label for the 'x' axis 
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
  \item{col}{
OPTIONAL. Only used when \code{plot=TRUE} \cr
character, colour to be used for drawing the lines 
}
  \item{lty}{
OPTIONAL. Only used when \code{plot=TRUE} \cr
numeric, line type to be used
}
  \item{ylim}{
numeric with the the \sQuote{y} limits of the plot
}
  \item{verbose}{
logical, if TRUE, progress messages are printed
}
  \item{\dots}{
OPTIONAL. Only used when \code{plot=TRUE} \cr
further arguments passed to the plot command or from other methods
}
  \item{do.png}{
logical, indicates if all the figures have to be saved into PNG files instead of the screen device
}
  \item{png.width}{
OPTIONAL. Only used when \code{do.png=TRUE} \cr
numeric, width of the PNG device. See \code{\link[grDevices]{png}}
}
  \item{png.height}{
OPTIONAL. Only used when \code{do.png=TRUE} \cr
numeric, height of the PNG device. See \code{\link[grDevices]{png}}
}
  \item{png.res}{
OPTIONAL. Only used when \code{do.png=TRUE} \cr
numeric, nominal resolution in ppi which will be recorded in the PNG file, if a positive integer of the device. See \code{\link[grDevices]{png}}
}
  \item{png.fname}{
OPTIONAL. Only used when \code{do.png=TRUE} \cr
character, filename used to store the PNG file wih the dotty plots of the parameter values
}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{
A data frame with the contents of the output file \sQuote{Particles_GofPerIter.txt} output file, where each column represents an individual model parameter and its corresponding GoF, and each row represents a different PSO iteration.

Additionally, when \code{plot=TRUE} a a multi-panel figure is produced by a call to the \code{\link{plot_GofPerParticle}} function, where each panel corresponds to a different model parameter and shows the the iteration number against the corresponging GoF value for each iteration.
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
\code{\link{read_results}}, \code{\link{plot_results}}, \code{\link{plot_2parOF}}, \code{\link{plot_NparOF}}, \code{\link{plot_ParamsPerIter}}
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
  
# Reading the convergence measures got by running hydroPSO
setwd("PSO.out")
read_GofPerParticle() # all the particles in the same window
read_GofPerParticle(ptype="many") # each particle in a different pannel

}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{files}
\keyword{graph}% __ONLY ONE__ keyword per line

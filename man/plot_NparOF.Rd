%% file plot_NparOF.Rd
%% Part of the hydroPSO R package, http://www.rforge.net/hydroPSO/ ; 
%%                                 http://cran.r-project.org/web/packages/hydroPSO
%% Copyright 2011-2012 Mauricio Zambrano-Bigiarini & Rodrigo Rojas
%% Distributed under GPL 2 or later

\name{plot_NparOF}
\alias{plot_NparOF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
N 2-dimensional plots of Parameter Values against the Objective Function
}
\description{
For \kbd{n} user-defined parameters, the function creates \kbd{sum(1:(npar-1))}  \code{\link{plot_2parOF}} plots, with the  values of the objective function in a 2D box, where the boundaries of each parameter are used as axis \cr
The \kbd{sum(1:(npar-1))} plots corresponds to all the possible combinations of 2 parameters among all the \kbd{n} parameters provided
}
\usage{
plot_NparOF(params, gofs, param.names=colnames(params), MinMax=c("min", "max"), 
           nrows="auto",  gof.name="GoF", main=paste(gof.name, "Surface"), 
           GOFcuts="auto", colorRamp= colorRampPalette(c("darkred", "red", 
           "orange", "yellow", "green", "darkgreen", "cyan")), points.cex=0.7, 
           alpha=1, axis.rot=c(0, 0), verbose=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{params}{
matrix or data.frame with the parameter values
}
  \item{gofs}{
numeric with the values of goodness-of-fit values for each one of the parameters in \code{params} (in the same order!)
}
  \item{param.names}{
character, names for the parameters in \code{params} that have to be plotted (\code{param.names} can be a subset of \code{params})
}
  \item{MinMax}{
character, indicates whether the optimum value in \code{gofs} corresponds to the minimum or maximum of the objective function. Valid values are in: \code{c('min', 'max')}
}
  \item{nrows}{
numeric, number of rows to be used in the plotting window \cr
If \code{nrows='auto'} the number of columns is automatically computed depending on the number of parameters in \code{params}
}
  \item{gof.name}{
character, name of the objective function to be plotted. It has to correspond to the name of one column of \code{params}
}
  \item{main}{
character, title for the plot
}
  \item{GOFcuts}{
numeric, specifies at which values of the objective function \code{gof.name} the colours of the plot have to change \cr
If \code{GOFcuts="auto"}, the interval for colours change are defined by the five quantiles of the objective function computed by \code{\link[stats]{fivenum}}
}
  \item{colorRamp}{
R function defining the colour ramp to be used for colouring the pseudo-3D dotty plots of Parameter Values, OR character representing those colours
}
  \item{points.cex}{
size of the points to be plotted
}
  \item{alpha}{
numeric between 0 and 1 representing the transparency level to apply to \code{colorRamp}, \sQuote{0} means fully transparent and \sQuote{1} means opaque
}
  \item{axis.rot}{
numeric vector of length 2 representing the angle (in degrees) by which the axis labels are to be rotated, left/bottom and right/top, respectively.
}
 \item{verbose}{
logical; if TRUE, progress messages are printed
}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
%%\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%%}
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
\code{\link{plot_2parOF}}, \code{\link{read_results}}, \code{\link{plot_results}}, \code{\link{plot_GofPerParticle}}, \code{\link{plot_ParamsPerIter}}
}
\examples{
# Number of dimensions to be optimised
nparam <- 5

\dontrun{

# Setting the user's home directory as working directory
setwd("~")

# Setting the seed
set.seed(100)

# Running PSO with the 'rosenbrock' test function, writing the results to text files
hydroPSO(fn="rosenbrock",
        lower=rep(-30, nparam), upper=rep(30, nparam),    
        control=list(MinMax="min", npart=2*nparam, write2disk=TRUE)
        ) # hydroPSO
  
# reading the 'Particles.txt' output file of hydroPSO
setwd("PSO.out")
particles <- read_particles(plot=FALSE)
               
# plotting the value of each parameter and the objective function against the 
# values of the objective function
plot_NparOF(params=particles[["part.params"]], gofs=particles[["part.gofs"]],
            gof.name="Rosenbrock", alpha=0.5)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{math}
\keyword{graph}% __ONLY ONE__ keyword per line
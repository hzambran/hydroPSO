%% File plot_2parOF.Rd
%% Part of the hydroPSO R package, http://www.rforge.net/hydroPSO/ ; 
%%                                 http://cran.r-project.org/web/packages/hydroPSO
%% Copyright 2011-2012 Mauricio Zambrano-Bigiarini & Rodrigo Rojas
%% Distributed under GPL 2 or later

\name{plot_2parOF}
\alias{plot_2parOF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
plot_2parOF
}
\description{
This function plots the values of the objective function in a two dimensional box, where the boundaries of each parameter are used as axis limit
}
\usage{
plot_2parOF(params, gofs, p1.name, p2.name, type="sp", MinMax=c("min", "max"), 
            gof.name="GoF", main=paste(gof.name, "Surface"), GOFcuts, 
            colorRamp= colorRampPalette(c("darkred", "red", "orange", "yellow",
            "green", "darkgreen", "cyan")), points.cex=0.7, alpha=1, 
            axis.rot=c(0, 0), auto.key=TRUE, key.space= "right")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{params}{
matrix or data.frame with the parameter values
}
  \item{gofs}{
numeric with the values of goodness-of-fit values for each one of the parameters in \code{params} (in the same order!)
}
  \item{p1.name}{
character, name of the 1st parameter to be plotted
}
  \item{p2.name}{
character, name of the 2nd parameter to be plotted
}
  \item{type}{
character, type of plot. Valid values are: \cr
-) \kbd{sp}: spatial plot \cr
-) \kbd{scatter3d}: 3d scatterogram
}
  \item{MinMax}{
character, indicates whether the optimum value in \code{gofs} corresponds to the minimum or maximum of the objective function. Valid values are in: \code{c('min', 'max')}
}
  \item{gof.name}{
character, name of the objective function to be plotted. It has to correspond to the name of one column of \code{params}
}
  \item{main}{
character with the title for the plot
}
  \item{GOFcuts}{
numeric, specifies at which values of the objective function \code{gof.name} the colours of the plot have to change \cr
If \code{GOFcuts} is missing, the interval for colours change are defined by the five quantiles of the objective function computed by \code{\link[stats]{fivenum}}
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
  \item{auto.key}{
logical, indicates whether the legend has to be drawn or not
}
  \item{key.space}{
character, position of the legend with respect to the plot
}
 
 %% \item{verbose}{
%%     ~~Describe \code{file} here~~
%%}
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
\code{\link{read_results}}, \code{\link{plot_results}}, \code{\link{plot_GofPerParticle}}, \code{\link{plot_ParamsPerIter}}
}
%%\examples{

%%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{math}
\keyword{graph}% __ONLY ONE__ keyword per line

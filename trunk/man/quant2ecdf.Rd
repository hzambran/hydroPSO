%% File quant2ecdf.Rd
%% Part of the hydroPSO R package, http://www.rforge.net/hydroPSO/ ; 
%%                                 http://cran.r-project.org/web/packages/hydroPSO
%% Copyright 2011-2012 Mauricio Zambrano-Bigiarini & Rodrigo Rojas
%% Distributed under GPL 2 or later

\name{quant2ecdf}
\Rdversion{1.1}
\alias{quant2ecdf}
\alias{quant2ecdf.default}
\alias{quant2ecdf.matrix}
\alias{quant2ecdf.data.frame}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Simulated Values -> Empirical CDFs
}
\description{
This function computes ECDFs for user-defined quantiles of the simulated equivalents, with optional plot
}
\usage{
quant2ecdf(sim, ...)
           
\method{quant2ecdf}{default}(sim, weights=NULL, byrow=TRUE, 
           quantiles.desired= c(0.05, 0.5, 0.95), plot=TRUE, obs=NULL, 
           quantiles.labels= c("Q5", "Q50", "Q95"), main=NULL, 
           ylab="Probability", col="blue", leg.cex=1.2, leg.pos="bottomright", 
           cex.axis=1.2, cex.main=1.2, cex.lab=1.2, verbose=TRUE, ...)
           
\method{quant2ecdf}{matrix}(sim, weights=NULL, byrow=TRUE, 
           quantiles.desired= c(0.05, 0.5, 0.95), plot=TRUE, obs=NULL, 
           quantiles.labels= c("Q5", "Q50", "Q95"), main=NULL, 
           ylab="Probability", col="blue", leg.cex=1.2, leg.pos="bottomright", 
           cex.axis=1.2, cex.main=1.2, cex.lab=1.2, verbose=TRUE, ...)
           
\method{quant2ecdf}{data.frame}(sim, weights=NULL, byrow=TRUE, 
           quantiles.desired= c(0.05, 0.5, 0.95), plot=TRUE, obs=NULL, 
           quantiles.labels= c("Q5", "Q50", "Q95"), main=NULL, 
           ylab="Probability", col="blue", leg.cex=1.2, leg.pos="bottomright", 
           cex.axis=1.2, cex.main=1.2, cex.lab=1.2, verbose=TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sim}{
matrix or data.frame with the simulated equivalents obtained with different parameter sets, which, by default, are stored in columns
}
  \item{weights}{
numeric vector, values of the weights to be used for computing the quantiles \cr
Omitting the \code{weights} argument or specifying \code{NULL} or a zero-length vector will result in the usual un-weighted estimates
}
  \item{byrow}{
logical, indicates whether the computations have to be made for each column or for each row of \code{x}
When the simulated equivalents are stored in columns, \code{byrow} must be \kbd{TRUE} \cr
When the simulated equivalents are stored in rows, \code{byrow} must be \kbd{FALSE}
}
  \item{quantiles.desired}{
numeric vector, quantiles to be computed. Default values are \kbd{c(.025, .5, .975)} ( => 2.5\%, 50\%, 97.5\% )
}
  \item{plot}{
logical, indicates if a plot with the ECDFs has to be produced
}
  \item{obs}{
OPTIONAL. Only used when \code{plot=TRUE} \cr
Numeric or zoo object with observed values, which are used in the output plot
}
  \item{quantiles.labels}{
OPTIONAL. Only used when \code{plot=TRUE} \cr
character vector, names to \code{quantiles.desired}. Default value is \kbd{c("Q5", "Q50", "Q95")} 
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
  \item{verbose}{
logical, if TRUE, progress messages are printed 
}
  \item{\dots}{
further arguments passed to the \code{plot} function or from other methods
}
}
\details{
Steps used in this function are: \cr
1) Computation of un-weighted quantiles (e.g., Q5, Q50, Q95) for the simulated equivalents \cr
2) Computation of ECDFs for each desired quantile, by weighting the quantiles of each parameter set by its corresponding weights (or less-formal likelihood in GLUE terminology)
}
\value{
A list whose elements \code{x} and \code{ecdf} correspond to unique sorted values of \code{sim}. If the first CDF estimate is greater than zero, a point (min(sim),0) is placed at the beginning of the estimates

A list, with as many elements as  the number of elements of \code{quantiles.desired} \cr
Each element of the list, has the following components:
\item{x}{ \kbd{n+1} elements (with \kbd{n} as the number of columns of \code{x}) }
\item{ecdf}{Description of 'comp2'}
%% ...
}
%%\references{
%%}
\author{
Mauricio Zambrano-Bigiarini, \email{mzb.devel@gmail.com}
}
\note{
It requires the \code{\link[Hmisc]{wtd.Ecdf}} function from the \pkg{Hmisc} package. 
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[Hmisc]{wtd.Ecdf}}, \code{\link{params2ecdf}}
}
\examples{
# random matrix with 100 simulated values (in columns) corresponding to 10
# different behavioural parameter sets
x <- matrix(rnorm(1000), ncol=10, nrow=100)

# empirical CDFs for the quantiles 0.05, 0.5 and 0.95, with equal weight for 
# each parameter set
quant2ecdf(sim=x, weights=1:10, byrow=FALSE) 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{math}
\keyword{manip}
\keyword{graph}

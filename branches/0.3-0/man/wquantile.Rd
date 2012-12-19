%% File wquantile.R
%% Part of the hydroPSO R package, http://www.rforge.net/hydroPSO/ ; 
%%                                 http://cran.r-project.org/web/packages/hydroPSO
%% Copyright 2011-2012 Mauricio Zambrano-Bigiarini & Rodrigo Rojas
%% Distributed under GPL 2 or later

\name{wquantile}
\Rdversion{1.1}
\alias{wquantile}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Weighted Quantiles
}
\description{
This function computes weighted quantiles of each column (by default, or for each row if specified by the user) of a matrix/data.frame  \cr

It is a wrapper to the \code{\link[Hmisc]{wtd.quantile}} function of the \bold{Hmisc} package, specially thought for a matrix containing streamflows simulated by different (behavioural) parameter sets
}
\usage{
wquantile(x, weights=NULL, byrow=FALSE, probs=c(.025, .5, .975), 
          normwt=TRUE, verbose=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
numeric or matrix for the computation of the weighted quantiles
}
  \item{weights}{
numeric vector, values of the weights to be used for computing the quantiles. See \code{\link[Hmisc]{wtd.quantile}}. \cr
Omitting the \code{weights} argument or specifying \code{NULL} or a zero-length vector will result in the usual unweighted estimates
}
  \item{byrow}{
logical, indicates if the computations have to be made for each column or for each row of \code{x} \cr
When the simulated values obtained with different behavioural parameter sets are stored in columns, \code{byrow} must be \kbd{TRUE} \cr
When the simulated values obtained with different behavioural parameter sets are stored in rows, \code{byrow} must be \kbd{FALSE}
}
  \item{probs}{
numeric vector, quantiles to be computed. \code{\link[Hmisc]{wtd.quantile}} \cr
Default value is  \kbd{c(.025, .5, .975)} ( => 2.5\%, 50\%, 97.5\% )
}
  \item{normwt}{
See \code{\link[Hmisc]{wtd.quantile}}. Specify \code{normwt=TRUE} to make \code{weights} sum to \code{length(x)} after deletion of NAs 
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
%\references{

%}
\author{
Mauricio Zambrano-Bigiarini, \email{mzb.devel@gmail.com}
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[Hmisc]{wtd.quantile}}
}
\examples{
# random matrix with 100 parameter sets (in rows) corresponding to 10
# different parameters
params <- matrix(rnorm(1000), ncol=10, nrow=100)
colnames(params) <- paste("Param", 1:10, sep="")

# empirical CDFs for each one of the 10 parameters of x, with equal weight for 
# each one of the 100 parameter sets
wquantile(params, weights=rep(1,100), byrow=FALSE) 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{math}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

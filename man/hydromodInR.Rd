%% File hydromodInR.Rd
%% Part of the hydroPSO R package, https://github.com/hzambran/hydroPSO
%%                                 https://cran.r-project.org/package=hydroPSO
%%                                 http://www.rforge.net/hydroPSO/
%% Copyright 2011-2012 Mauricio Zambrano-Bigiarini & Rodrigo Rojas
%% Distributed under GPL 2 or later

\name{hydromodInR}
\alias{hydromodInR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
hydromodInR
}
\description{
This function runs an R-based model using a single parameter set taken from a matrix/data.frame with many parameter sets.

This function is for internal use within the package, and it is only documented in case it be useful to other packages. See Details section for more information.
}
\usage{
hydromodInR(part, Particles, model.FUN, model.FUN.args )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{part}{
numeric, a single integer value indicating the row number in \code{Particles} which is used to select the parameter set used to run the model specified in \code{model.FUN}.
}
  \item{Particles}{
matrix/data.frame with all the parameter sets that can be used to run the model defined in \code{model.FUN}. 

It has as many columns as parameters used by the model, and as many rows as parameter sets provided by the user in \code{Particles}.
}
  \item{model.FUN}{
character, valid R function representing the model code to be calibrated/optimised.
} 
  \item{model.FUN.args}{
list with the arguments to be passed to \code{model.FUN}.
} 
  
%%  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
%%}
}
\details{
This function takes an R-based user-defined model, a matrix/data.frame with many parameter sets (e.g., randomly generated or the output of a previous optimisation/calibration), and index indicating which specific parameter set must be used from the matrix/data.frame, and then runs the model and returns a goodness-of-fit value as measure of model performance, by comparing observations against simulated equivalents. 

This function it was designed to run a model implemented as an R function, not an executable file that runs in the system console.
}
\value{
This function MUST return a list with two elements: 
\item{sim}{numeric, with the simulated values obtained by running the model}
\item{GoF}{numeric, goodness-of-fit value representing how close each one of the simulated values in \code{sim} are to their observed counterparts, by using the USER-DEFINED \code{gof.FUN} function}
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
\code{\link{hydroPSO}}
}
%%\examples{
%%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{files}
\keyword{optimisation}% __ONLY ONE__ keyword per line

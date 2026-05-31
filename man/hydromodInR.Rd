%% File hydromodInR.Rd
%% Part of the hydroPSO R package, https://github.com/hzambran/hydroPSO
%%                                 https://cran.r-project.org/package=hydroPSO
%%                                 http://www.rforge.net/hydroPSO/
%% Copyright 2011-2026 Mauricio Zambrano-Bigiarini & Rodrigo Rojas
%% Distributed under GPL 2 or later

\name{hydromodInR}
\alias{hydromodInR}

\title{
hydromodInR
}
\description{
This function runs an R-based model using a single parameter set taken from a matrix/data.frame with many parameter sets, or from a single named numeric vector.

This function is for internal use within the package, and it is only documented in case it be useful to other packages. See Details section for more information.
}

\usage{
hydromodInR(part=1, param.values, model.FUN, model.FUN.args, ... )
}

\arguments{
  \item{part}{
numeric, a single integer value indicating the row number in \code{param.values} which is used to select the parameter set used to run the model specified in \code{model.FUN}. When \code{param.values} is a named numeric vector, \code{part} must be equal to 1.
}
  \item{param.values}{
matrix/data.frame with all the parameter sets that can be used to run the model defined in \code{model.FUN}, or a single named numeric vector with one parameter set.

When \code{param.values} is a matrix/data.frame, it has as many columns as parameters used by the model, and as many rows as parameter sets provided by the user in \code{param.values}. When \code{param.values} is a named numeric vector, its names are used as parameter names and the vector is treated as a one-row parameter matrix.
}
  \item{model.FUN}{
character, valid R function representing the model code to be calibrated/optimised.
} 
  \item{model.FUN.args}{
list with the arguments to be passed to \code{model.FUN}.
} 
  \item{...}{
additional arguments. The deprecated argument \code{Particles} is temporarily accepted as an alias of \code{param.values}.
} 
  
}
\details{
This function takes an R-based user-defined model, a matrix/data.frame with many parameter sets (e.g., randomly generated or the output of a previous optimisation/calibration), and index indicating which specific parameter set must be used from the matrix/data.frame. Alternatively, \code{param.values} may be a single named numeric vector, which is converted internally into a one-row matrix. The function then runs the model and returns a goodness-of-fit value as measure of model performance, by comparing observations against simulated equivalents. \cr

This function it was designed to run a model implemented as an R function, not an executable file that runs in the system console.
}
\value{
This function MUST return a list with two elements: 

\item{sim}{numeric, with the simulated values obtained by running the model}.\cr 

\item{GoF}{numeric, goodness-of-fit value representing how close each one of the simulated values in \code{sim} are to their observed counterparts, by using the USER-DEFINED \code{gof.FUN} function}.
}

\author{
Mauricio Zambrano-Bigiarini, \email{mzb.devel@gmail.com}
}

\seealso{
\code{\link{hydroPSO}}, \code{\link{hydromod}}
}

\keyword{files}
\keyword{optimisation}% __ONLY ONE__ keyword per line

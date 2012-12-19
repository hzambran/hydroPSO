%% Part of the hydroPSO R package, http://www.rforge.net/hydroPSO/ ; 
%%                                 http://cran.r-project.org/web/packages/hydroPSO
%% Copyright 2011-2012 Mauricio Zambrano-Bigiarini & Rodrigo Rojas
%% Distributed under GPL 2 or later

\name{read_best}
\Rdversion{1.1}
\alias{read_best}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Reading the 'BestParameterSet.txt' output file
}
\description{
This function reads the contents of the the \sQuote{BestParameterSet.txt} output file, which stores the best parameter set and its corresponding goodness-of-fit value found during the optimisation
}
\usage{
read_best(file="BestParameterSet.txt", verbose=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
character, name (including path) of the output file with the best parameter set  and its corresponding best fitness value found during the optimisation
}
  \item{verbose}{
logical, if TRUE, progress messages are printed
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
  
# Reading the best parameter set and its corresponsing gof found by hydroPSO
setwd("PSO.out")
read_best()
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{graph}
\keyword{manip}% __ONLY ONE__ keyword per line

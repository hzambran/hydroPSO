%% Part of the hydroPSO R package, http://www.rforge.net/hydroPSO/ ; 
%%                                 http://cran.r-project.org/web/packages/hydroPSO
%% Copyright 2011-2012 Mauricio Zambrano-Bigiarini & Rodrigo Rojas
%% Distributed under GPL 2 or later

\name{test_functions}
\alias{ackley}
\alias{griewank}
\alias{rastrigrin}
\alias{rastrigin}
\alias{rosenbrock}
\alias{schafferF6}
\alias{schwefel}
\alias{sphere}
\alias{sackley}
\alias{sgriewank}
\alias{srastrigin}
\alias{srosenbrock}
\alias{sschwefel1_2}
\alias{ssphere}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Test Functions for Global Optimisation
}
\description{
Test functions commonly used as benchmark for global optimisation problems
}
\usage{
ackley(x)
griewank(x)
rastrigrin(x)
rosenbrock(x)
schafferF6(x)
schwefel(x)
sphere(x)
sackley(x, o=-32+64*runif(length(x)), fbias=-140)
sgriewank(x, o=-600+1200*runif(length(x)), fbias=-180)
srastrigin(x, o=-5+10*runif(length(x)), fbias=-330)
srosenbrock(x, o=-100+200*runif(length(x)), fbias=390)
sschwefel1_2(x, o=-100+200*runif(length(x)), fbias=-450)
ssphere(x, o=-100+200*runif(length(x)), fbias=-450)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
numeric vector to be evaluated
}
  \item{o}{
numeric shifting vector to be used, with the same length of \code{x}
}
  \item{fbias}{
numeric with the bias to be imposed
}
}
%\details{
%\code{Ackley}
%}
\value{
Each test function returns a single numeric value corresponding to the function evaluated on the vector \code{x}
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{

Test Functions for Unconstrained Global Optimization \cr
\cite{\url{http://www-optima.amp.i.kyoto-u.ac.jp/member/student/hedar/Hedar_files/TestGO_files/Page364.htm}}

GEATbx: Example Functions (single and multi-objective functions) \cr
\cite{\url{http://www.geatbx.com/docu/fcnindex-01.html}}

Problem Definitions and Evaluation Criteria for the CEC 2005 Special Session on Real-Parameter Optimization \cr
\cite{\url{www.lri.fr/~hansen/Tech-Report-May-30-05.pdf}}

Benchmark Problems \cr
\cite{\url{http://www.cs.cmu.edu/afs/cs/project/jair/pub/volume24/ortizboyer05a-html/node6.html}}

\cite{\url{http://www.zsd.ict.pwr.wroc.pl/files/docs/functions.pdf}}
}
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
\keyword{optim}
\keyword{math}% __ONLY ONE__ keyword per line

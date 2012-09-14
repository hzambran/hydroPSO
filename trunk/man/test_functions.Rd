%% Part of the hydroPSO R package, http://www.rforge.net/hydroPSO/ ; 
%%                                 http://cran.r-project.org/web/packages/hydroPSO
%% Copyright 2011-2012 Mauricio Zambrano-Bigiarini & Rodrigo Rojas
%% Distributed under GPL 2 or later

\name{test_functions}
\alias{sphere}
\alias{ackley}
\alias{griewank}
\alias{rastrigrin}
\alias{rastrigin}
\alias{rosenbrock}
\alias{schafferF6}

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
sphere(x)
rastrigrin(x)
rosenbrock(x)
schafferF6(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
numeric vector to be evaluated
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
\references{
\cite{\url{http://www.zsd.ict.pwr.wroc.pl/files/docs/functions.pdf}}

\cite{\url{http://www-optima.amp.i.kyoto-u.ac.jp/member/student/hedar/Hedar_files/TestGO_files/Page364.htm}}

\cite{\url{http://www.geatbx.com/docu/fcnindex-01.html}}
}
\author{
Mauricio Zambrano-Bigiarini, \email{mzb.devel@gmail.com}
}
%%\note{
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%%}
%%\examples{

%%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{optim}
\keyword{math}% __ONLY ONE__ keyword per line

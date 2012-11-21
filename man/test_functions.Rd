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
\details{
The \bold{Ackley} test function is multimodal and separable, with several local optima that, for the search range [-32, 32], look more like noise, although they are located at regular intervals. The Ackley function only has one global optimum located at the point \kbd{o=(0,...,0)}. It is defined by:
\deqn{ ackley = 20+\exp(1)-20\exp\left( -0.2\sqrt{\frac{1}{n}\sum_{i=1}^{n}x_{i}^2} \right)-\exp\left(\frac{1}{\textcolor{red}{n}}\sum_{i=1}^{n}\cos(2\pi x_{i})\right) ; -32 \leq x_i \leq 32 \ ; \ i=1,2,\ldots,n }


The generalized \bold{Rastrigin} test function is non-convex, multimodal and additively separable. It has several local optima arranged in a regular lattice, but it only has one global optimum located at the point \kbd{o=(0,...,0)}. The search range for the Rastrigin function is [-5.12, 5.12] in each variable. This function is a fairly difficult problem due to its large search space and its large number of local minima. It is defined by:

\deqn{ rastrigin = 10n+\sum_{i=1}^{n}\left[x_{i}^{2}-10\cos(2\pi x_{i})\right] \ ; \ -5.12 \leq x_i \leq 5.12 \ ; \ i=1,2,\ldots,n }


The \bold{Griewank} test function is multimodal and non-separable, with has several local optima within the search region defined by [-600, 600]. It is similar to the Rastrigin function, but the number of local optima is larger in this case. It only has one global optimum located at the point \kbd{o=(0,...,0)}. While this function has an exponentially increasing number of local minima as its dimension increases, it turns out that a simple multistart algorithm is able to detect its global minimum more and more easily as the dimension increases (Locatelli, 2003). It is defined by:

\deqn{ griewank = \frac{1}{4000}\sum_{i=1}^{n}x_{i}^{2}-\prod_{i=1}^{n}\cos\left(\frac{x_i}{\sqrt{i}}\right)+1 \ ; \ -600 \leq x_i \leq 600 \ ; \ i=1,2,\ldots,n }


The \bold{Rosenbrock} function is non-convex, unimodal and non-separable. It is also known as \emph{Rosenbrock's valley} or \emph{Rosenbrock's banana} function. The global minimum is inside a long, narrow, parabolic shaped flat valley. To find the valley is trivial. To converge to the global minimum, however, is difficult. It only has one optimum located at the point \kbd{o=(1,...,1)}. It is a quadratic function, and its search range is [-30, 30] for each variable. It is defined by:

\deqn{ rosenbrock = \sum_{i=1}^{n-1}\left[100(x_{i+1}\textcolor{red}{-}x_{i}^{2})^{2}+(1-x_{i})^{2}\right] \ ; \ -30 \leq x_i \leq 30 \ ; \ i=1,2,\ldots,n }


\deqn{ schafferF6 = 0.5+\frac{\sin^{2}\sqrt{\textcolor{red}{\sum_{i=1}^{n}}x_{i}^{2}}-0.5}{(1+0.001\textcolor{red}{\sum_{i=1}^{n}}x_{i}^{2})^{2}} \ ; \ -100 \leq x_i \leq 100 \ ; \ i=1,2,\ldots,n }

The \bold{Sphere} test function is one of the most simple test functions available in the specialized literature. This unimodal and additively separable test function can be scaled up to any number of variables. It belongs to a family of functions called quadratic functions and only has one optimum in the point \kbd{o=(0,...,0)}. The search range commonly used for the Sphere function is [-100, 100] for each decision variable. It is defined by:
\deqn{ sphere = \sum_{i=1}^{n} x_{i}^2 \ ; \ -100 \leq x_i \leq 100 \ ; \ i=1,2,\ldots,n  }


\deqn{ sackley = 20+\exp(1)-20\exp\left(-0.2\sqrt{\frac{1}{n}\sum_{i=1}^{n}z_{i}^2}\right)-\exp\left(\frac{1}{n}\sum_{i=1}^{n}\cos(2\pi z_{i})\right) + f\_bias , z=x-o ; \ i=1,2,\ldots,n }


\deqn{ sgriewank = \frac{1}{4000}\sum_{i=1}^{n}z_{i}^{2}-\prod_{i=1}^{n}\cos\left(\frac{z_i}{\sqrt{i}}\right)+1 + f\_bias \ , \ z=x-o ; \ i=1,2,\ldots,n }

\deqn{ ssphere = \sum_{i=1}^{n} z_{i}^2 + f\_bias \ , \ z=x-o ; \ i=1,2,\ldots,n }{%
sphere = sum( x^2 ) }
}
\value{
Each test function returns a single numeric value corresponding to the function evaluated on the vector \code{x}
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{

GEATbx: Example Functions (single and multi-objective functions) \cr
\cite{\url{http://www.geatbx.com/docu/fcnindex-01.html}}

Problem Definitions and Evaluation Criteria for the CEC 2005 Special Session on Real-Parameter Optimization \cr
\cite{\url{www.lri.fr/~hansen/Tech-Report-May-30-05.pdf}}

Benchmark Problems \cr
\cite{\url{http://www.cs.cmu.edu/afs/cs/project/jair/pub/volume24/ortizboyer05a-html/node6.html}}

\cite{Barrera, J., and C. Coello Coello. 2010, Test function generators for assessing the performance of PSO algorithms in multimodal optimization, in Handbook of Swarm Intelligence, vol. 8, edited by B. Panigrahi, Y. Shi, and M.-H. Lim, chap. Adaptation, Learning, and Optimization, pp. 89-117, Springer Berlin Heidelberg, doi:10.1007/978-3-642-17390-5 4}

\cite{\url{http://www.zsd.ict.pwr.wroc.pl/files/docs/functions.pdf}}

Test Functions for Unconstrained Global Optimization \cr
\cite{\url{http://www-optima.amp.i.kyoto-u.ac.jp/member/student/hedar/Hedar_files/TestGO_files/Page364.htm}}

Rosenbrock: \cite{\url{http://www.it.lut.fi/ip/evo/functions/node5.html}},  \cite{\url{http://en.wikipedia.org/wiki/Rosenbrock_function}}

Sphere: \cite{\url{http://www.it.lut.fi/ip/evo/functions/node2.html}}

Rastrigin: \cite{\url{http://www.it.lut.fi/ip/evo/functions/node6.html}}, \cite{\url{http://en.wikipedia.org/wiki/Rastrigin_function}}

Ackley: \cite{\url{http://www.it.lut.fi/ip/evo/functions/node14.html}}

Griewank: \cite{Locatelli, M. 2003. A note on the griewank test function, Journal of Global Optimization, 25 (2), 169-174, doi:10.1023/A:1021956306041}

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

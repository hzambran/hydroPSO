%% File hydroPSO-package.Rd
%% Part of the hydroPSO R package, http://www.rforge.net/hydroPSO/ ; 
%%                                 http://cran.r-project.org/web/packages/hydroPSO
%% Copyright 2011-2012 Mauricio Zambrano-Bigiarini & Rodrigo Rojas
%% Distributed under GPL 2 or later

\name{hydroPSO-package}
\alias{hydroPSO-package}
%%\alias{hydroPSO}
\docType{package}
\title{
A flexible and model-independent Particle Swarm Optimisation (PSO) package for calibration/optimisation of environmental models
}
\description{
hydroPSO is a package implementing an enhanced version of the canonical Particle Swarm Optimisation (PSO) algorithm developed by Kennedy and Eberhart (1995) and Eberhart and Kennedy (1995). PSO is a population-based stochastic optimisation technique inspired by social behaviour of bird flocking and shares few similarities with other evolutionary optimisation techniques such as Genetic Algorithms (GA). In PSO, however, the multi-dimensional solution space is explored on the basis of individual and global best-known ``particle positions'' with no presence of evolution operators. 

hydroPSO is capable of performing sensitivity analysis using the Latin Hypercube One-At-a-Time (LH-OAT) method (van Griensven et al., 2006), which together with advanced plotting summaries and detailed information about the evolution of hydroPSO's performance facilitate the interpretation and assessment of the model calibration. At the same time, hydroPSO features a suite of controlling options and PSO variants to fine-tuning and improve the performance of the calibration engine, thus, allowing the user to adapt it to different modelling problems. In principle, hydroPSO only needs to know ``which'' model parameters need to be calibrated and ``where'' they need to be written, but can also be interfaced with the model code through simple R wrapper functions. Then, it will take control over the model(s) to be calibrated until either a maximum number of iterations or an error tolerance are reached: both being problem-specific and user-defined.

The default control arguments in hydroPSO implements the Standard PSO 2011 - SPSO2011 (see Clerc 2012; Clerc et al., 2010). At the same time, hydroPSO can also implement 4 different topologies (random, von Neumann, lbest, gbest), (non-)linear / random / adaptive / best-ratio inertia weight definitions (IW.type), time-variant acceleration coefficients (use.TVc1 and use.TVc2), time-varying maximum velocity (use.TVlambda), regrouping strategy when premature convergence is detected (use.RG), options for clamping the maximal velocity (lambda), random or LHS initialization of positions and velocities (Xini.type and Vini.type), synchronous or asynchronous update, 5 types of boundary conditions (absorbing2011, absorbing2007, reflecting, damping, invisible) among others.

}
\details{
\tabular{ll}{
Package: \tab hydroPSO\cr
Type: \tab Package\cr
Version: \tab 0.1-58\cr
Date: \tab 2012-09-14\cr
License: \tab GPL (>=2)\cr
LazyLoad: \tab yes\cr
Packaged: \tab Fri Sep 14 16:30:03 CEST 2012; MZB \cr
BuiltUnder: \tab R version 2.15.2 (2012-10-26) -- "Trick or Treat"; x86_64-redhat-linux-gnu (64-bit)\cr
}
%%~~ An overview of how to use the package, including the most important functions ~~
}
\author{
Mauricio Zambrano-Bigiarini and Rodrigo Rojas

Maintainer: Mauricio Zambrano-Bigiarini <mzb.devel@gmail.com>
}
%\references{
%\cite{Zambrano-Bigiarini, M., and R. Rojas. 2012. hydroPSO: A model-independent particle swarm optimization software for calibration of environmental models, Environmental Modelling \& Software, (under-review)}
%}
%%~~ Optionally other standard keywords, one per line, from file KEYWORDS in ~~
%%~~ the R documentation directory ~~
\keyword{ package }
\seealso{
%%~~ Optional links to other man pages, e.g. ~~
%%\code{\link[hydroGOF:hydroGOF]{<pkg>}}
\url{http://www.rforge.net/hydroGOF/} \cr
\url{http://www.rforge.net/hydroTSM/}
}
%%\examples{
%%~~ simple examples of the most important functions ~~
%%}

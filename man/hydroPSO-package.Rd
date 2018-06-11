%% File hydroPSO-package.Rd
%% Part of the hydroPSO R package, http://www.rforge.net/hydroPSO/ ; 
%%                                 http://cran.r-project.org/web/packages/hydroPSO
%% Copyright 2011-2018 Mauricio Zambrano-Bigiarini & Rodrigo Rojas
%% Distributed under GPL 2 or later

\name{hydroPSO-package}
\alias{hydroPSO-package}
%%\alias{hydroPSO}
\docType{package}
\title{
A flexible and model-independent Particle Swarm Optimisation (PSO) package for calibration/optimisation of environmental models
}
\description{
hydroPSO implements a state-of-the-art version of the Particle Swarm Optimisation (PSO) algorithm developed by Kennedy and Eberhart (1995) and Eberhart and Kennedy (1995). PSO is a population-based stochastic optimisation technique inspired by social behaviour of bird flocking, which shares some similarities with other evolutionary optimisation techniques such as Genetic Algorithms (GA). In PSO, however, the multi-dimensional solution space is explored on the basis of individual and global best-known ``particle positions'' with no presence of evolution operators. 

hydroPSO can be used as a replacement of the 'optim' R function for (global) optimization of non-smooth and non-linear functions. However, the main focus of hydroPSO is the calibration of environmental and other real-world models that need to be executed from the system console. hydroPSO is model-independent, allowing the user to easily interface any computer simulation model with the calibration engine (PSO). hydroPSO  communicates with the model through the model's own input and output files, without requiring access to the model's source code. In principle, hydroPSO only needs to know ``which'' model parameters need to be calibrated and ``where'' they need to be written. Then, it will take control over the model(s) to be calibrated until either a maximum number of iterations or an error tolerance is reached: both being problem-specific and user-defined. hydroPSO is able to take advantage of multi-core machines or network clusters to alleviate the computational burden of complex models with "long" execution time.

hydroPSO includes sensitivity analysis, by using the Latin Hypercube One-At-a-Time (LH-OAT) method (van Griensven et al., 2006). In addition, advanced plotting summaries and detailed information about the evolution of hydroPSO's performance facilitate the interpretation and assessment of the calibration results. At the same time, hydroPSO features a suite of controlling options and PSO variants to fine-tune the performance of the calibration engine to the model for which parameters are sought, thus, allowing the user to customise it to different modelling problems. 

At the same time, hydroPSO includes 4 different topologies (random, von Neumann, lbest, gbest), (non-)linear / random / adaptive / best-ratio inertia weight definitions (IW.type), time-variant acceleration coefficients (use.TVc1 and use.TVc2), time-varying maximum velocity (use.TVlambda), regrouping strategy when premature convergence is detected (use.RG), options for clamping the maximal velocity (lambda), random or LHS initialization of positions and velocities (Xini.type and Vini.type), synchronous or asynchronous update, 5 types of boundary conditions (absorbing2011, absorbing2007, reflecting, damping, invisible) among others. The default control arguments in hydroPSO implements the Standard PSO 2011 - SPSO2011 (see Clerc 2012; Clerc et al., 2010), although (better) settings recommeneded by the authors are described in Zambrano-Bigiarini & Rojas 2012. 

}
\details{
\tabular{ll}{
Package: \tab hydroPSO\cr
Type: \tab Package\cr
Version: \tab 0.4-0\cr
Date: \tab 2018-06-11\cr
License: \tab GPL (>=2)\cr
LazyLoad: \tab yes\cr
Packaged: \tab Mon Jun 11 01:24:11 -04 2018; MZB \cr
BuiltUnder: \tab R version 3.5.0 (2018-04-23) -- "Joy in Playing"; x86_64-pc-linux-gnu (64-bit)\cr
}
%%~~ An overview of how to use the package, including the most important functions ~~
}
\author{
Mauricio Zambrano-Bigiarini and Rodrigo Rojas

Maintainer: Mauricio Zambrano-Bigiarini <mzb.devel@gmail.com>
}
\references{
\cite{Zambrano-Bigiarini, M.; R. Rojas (2013), A model-independent Particle Swarm Optimisation software for model calibration, Environmental Modelling & Software, 43, 5-25, doi:10.1016/j.envsoft.2013.01.004}

\cite{Zambrano-Bigiarini, M., M. Clerc, R. Rojas (2013), Standard Particle Swarm Optimisation 2011 at CEC-2013: A baseline for future PSO improvements, In Proceedings of 2013 IEEE Congress on Evolutionary Computation (CEC'2013). doi:10.1109/CEC.2013.6557848}

\cite{Abdelaziz, Ramadan, and Mauricio Zambrano-Bigiarini (2014), Particle Swarm Optimization for inverse modeling of solute transport in fractured gneiss aquifer. Journal of Contaminant Hydrology, 164, 285-298. doi:10.1016/j.jconhyd.2014.06.003}
}
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

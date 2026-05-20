%% File ReadPlot_Results.R
%% Part of the hydroPSO R package, http://www.rforge.net/hydroPSO/ ; 
%%                                 http://cran.r-project.org/web/packages/hydroPSO
%% Copyright 2010-2026 Mauricio Zambrano-Bigiarini & Rodrigo Rojas
%% Distributed under GPL 2 or later
\name{ReadPlot_results}
\Rdversion{1.1}
\alias{read_results}
\alias{plot_results}

\title{Read and Visualise hydroPSO Result Files}

\description{
\code{read_results} reads the main output files produced by \code{\link{hydroPSO}} and organises them into a single list for post-processing. See Details section.

\code{plot_results} builds on those outputs to generate a comprehensive suite of diagnostic plots for parameter values, goodness-of-fit evolution, model simulations, and convergence behaviour. See Details section.

These functions are particularly useful in hydrology because they streamline the interpretation of calibration results and their uncertainty. They help identify behavioural parameter sets, inspect equifinality, assess parameter sensitivity, compare simulated and observed responses, and summarise optimisation performance across all iterations.
}


\usage{
read_results(drty.out = "PSO.out", MinMax = NULL, beh.thr = NA,
             modelout.cols = NULL, nsim = NULL, obs.tzone = NULL,
             verbose = TRUE)

plot_results(drty.out = "PSO.out", param.names = NULL, gof.name = "GoF",
             MinMax = NULL, beh.thr = NA, beh.col = "red", beh.lty = 1,
             beh.lwd = 2, nrows = "auto", col = "black", ylab = gof.name,
             main = NULL, pch = 19, cex = 0.5, cex.main = 1.7,
             cex.axis = 1.3, cex.lab = 1.5, breaks = "Scott",
             freq = TRUE, do.pairs = FALSE, weights = NULL, byrow = FALSE,
             leg.cex = 1.2, dp3D.names = "auto", GOFcuts = "auto",
             colorRamp = colorRampPalette(c("darkred", "red", "orange",
             "yellow", "green", "darkgreen", "cyan")), alpha = 0.65,
             points.cex = 0.7, ptype = "one", nsim = NULL,
             obs.tzone = NULL, modelout.cols = NULL, ftype = "o",
             FUN = mean, quantiles.desired = c(0.05, 0.5, 0.95),
             quantiles.labels = c("Q5", "Q50", "Q95"),
             legend.pos = "topright", do.png = FALSE, png.res = 90, 
             png.width = 1500, png.height = 900,
             params.png.width=1500, params.png.height=900,
             dotty.png.fname = "Params_DottyPlots.png",
             hist.png.fname = "Params_Histograms.png",
             bxp.png.fname = "Params_Boxplots.png",
             ecdf.png.fname = "Params_ECDFs.png",
             pruns.png.fname = "Params_ValuesPerRun.png",
             dp3d.png.fname = "Params_dp3d.png",
             pairs.png.fname = "Params_Pairs.png",
             part.png.fname = "Particles_GofPerIter.png",
             vruns.png.fname = "Velocities_ValuePerRun.png",
             modelout.best.png.fname = "ModelOut_BestSim_vs_Obs.png",
             modelout.quant.png.fname = "ModelOut_Quantiles.png",
             conv.png.fname = "ConvergenceMeasures.png", verbose = TRUE)
}

\arguments{
  \item{drty.out}{Character string with the path to the directory containing the hydroPSO output files. If only a directory name is provided, it is interpreted relative to the current working directory.}

  \item{param.names}{Optional character vector with the names of the parameters to be plotted. By default, all parameters available in \sQuote{Particles.txt} are used. This is especially helpful when focusing on a subset of hydrologically relevant parameters such as soil storage, recession coefficients, or routing parameters.}

  \item{gof.name}{Character string used to label the goodness-of-fit metric in the plots, for example \code{"NSE"}, \code{"KGE"}, or \code{"RMSE"}.}

  \item{MinMax}{Optional character string indicating whether the objective function is to be minimised or maximised. Valid values are \code{"min"} and \code{"max"}. When omitted, the value is inferred from \sQuote{PSO_logfile.txt}. This argument affects how best and behavioural simulations are identified.}

  \item{beh.thr}{Optional numeric threshold used to retain only behavioural solutions, that is, simulations with acceptable performance according to the selected objective function. This is useful for GLUE-style analyses and for focusing on hydrologically plausible parameter sets.}

  \item{modelout.cols}{Optional numeric vector indicating which columns of \sQuote{Model_out.txt} should be read or plotted, excluding the first three bookkeeping columns (iteration, particle, and goodness-of-fit). If \code{NULL}, all model output columns after the first three are used.}

  \item{nsim}{Currently unused in the implementation, although it remains in the function signatures for backward compatibility.}

  \item{obs.tzone}{Optional time zone used when reading sub-daily observations from \sQuote{Observations.txt}. This is only relevant when the observed series has a time index and the time zone must be interpreted explicitly.}

  \item{verbose}{Logical value indicating whether progress messages should be printed.}

  \item{beh.col}{Colour used to draw the horizontal threshold line that separates behavioural from non-behavioural solutions in relevant plots.}

  \item{beh.lty}{Line type used for the behavioural threshold line.}

  \item{beh.lwd}{Line width used for the behavioural threshold line.}

  \item{nrows}{Number of rows in multi-panel plotting layouts. If \code{"auto"}, the layout is computed automatically from the number of parameters to be displayed.}

  \item{col}{Colour used for plotting points in the dotty plots.}

  \item{ylab}{Label for the y-axis in the dotty plots. The default is \code{gof.name}.}

  \item{main}{Optional main title passed to the relevant plotting routines.}

  \item{pch}{Plotting symbol used for the dotty plots.}

  \item{cex}{General expansion factor controlling the size of points and text in the plots.}

  \item{cex.main}{Expansion factor for main titles.}

  \item{cex.axis}{Expansion factor for axis annotations.}

  \item{cex.lab}{Expansion factor for axis labels.}

  \item{breaks}{Break specification for parameter histograms. Passed to \code{\link[graphics]{hist}}.}

  \item{freq}{Logical value indicating whether histograms should display frequencies (\code{TRUE}) or densities (\code{FALSE}).}

  \item{do.pairs}{Logical value indicating whether a parameter correlation matrix should also be plotted. This can be computationally expensive for large calibration experiments.}

  \item{weights}{Optional numeric vector of weights used when computing empirical cumulative distribution functions of parameter values or simulated outputs. This is useful when different parameter sets have unequal importance, for example when weighting behavioural solutions.}

  \item{byrow}{Logical value passed to ECDF-related routines. In \code{plot_results}, weighted ECDFs of model outputs are computed with \code{byrow = TRUE} internally when required.}

  \item{leg.cex}{Expansion factor for legend text.}

  \item{dp3D.names}{Names of the parameters to be used in the pseudo-3D dotty plots. If \code{"auto"}, a subset of parameters is selected automatically.}

  \item{GOFcuts}{Numeric vector defining the goodness-of-fit intervals used to colour the pseudo-3D dotty plots. If \code{"auto"}, these intervals are computed automatically.}

  \item{colorRamp}{Function or vector defining the colour ramp used in the pseudo-3D dotty plots.}

  \item{alpha}{Numeric value between 0 and 1 controlling colour transparency in the pseudo-3D dotty plots.}

  \item{points.cex}{Point size used in the pseudo-3D dotty plots.}

  \item{ptype}{Character string controlling how goodness-of-fit per particle is plotted. See \code{\link{plot_GofPerParticle}}.}

  \item{ftype}{Graphical type used when plotting observed and simulated time series. Passed to \code{\link{plot_out}} and, when relevant, to \code{\link[hydroGOF]{ggof}}.}

  \item{FUN}{Summary function used by time-series plotting methods when aggregation is needed. Passed to \code{\link{plot_out}}.}

  \item{quantiles.desired}{Numeric vector with the simulation quantiles to be plotted. The default is \code{c(0.05, 0.5, 0.95)}, corresponding to the 5th, 50th, and 95th percentiles. These are often useful to visualise uncertainty envelopes around simulated hydrographs.}

  \item{quantiles.labels}{Character vector with labels for \code{quantiles.desired}.}

  \item{legend.pos}{Legend position used in the convergence plots. Passed to \code{\link{plot_convergence}}.}

  \item{do.png}{Logical value indicating whether the plots created with this function should be saved as PNG files instead of being drawn only on the active graphics device.}

  \item{png.res}{Resolution of all the output PNG figures, in pixels per inch.}

  \item{png.width}{Width of the output PNG figures, in pixels. Only for used for the following figures:

   -) \sQuote{Params_ValuesPerRun.png} 
   -) \sQuote{Params_dp3d.png}
   -) \sQuote{Particles_GofPerIter.png} 
   -) \sQuote{Velocities_ValuePerRun.png}
   -) \sQuote{ModelOut_BestSim_vs_Obs.png}
   -) \sQuote{ModelOut_Quantiles.png}
   -) \sQuote{ConvergenceMeasures.png} 
   }

  \item{png.height}{Height of the output PNG figures, in pixels. Only for used for the following figures:

   -) \sQuote{Params_ValuesPerRun.png} 
   -) \sQuote{Params_dp3d.png}
   -) \sQuote{Particles_GofPerIter.png}
   -) \sQuote{Velocities_ValuePerRun.png} 
   -) \sQuote{ModelOut_BestSim_vs_Obs.png}
   -) \sQuote{ModelOut_Quantiles.png}
   -) \sQuote{ConvergenceMeasures.png} 
  }

  \item{params.png.width}{Width of the output PNG figures, in pixels. Only for used for the following figures:

   -) \sQuote{Params_DottyPlots.png} 
   -) \sQuote{Params_Histograms.png} 
   -) \sQuote{Params_Boxplots.png}
   -) \sQuote{Params_ECDFs.png}
   -) \sQuote{Params_Pairs.png}
  }

  \item{params.png.height}{Height of the output PNG figures, in pixels. Only for used for the following figures:

   -) \sQuote{Params_DottyPlots.png} 
   -) \sQuote{Params_Histograms.png} 
   -) \sQuote{Params_Boxplots.png}
   -) \sQuote{Params_ECDFs.png}
   -) \sQuote{Params_Pairs.png} 
  }

  \item{dotty.png.fname}{Output filename for the parameter dotty plots.}

  \item{hist.png.fname}{Output filename for the parameter histograms.}

  \item{bxp.png.fname}{Output filename for the parameter boxplots.}

  \item{ecdf.png.fname}{Output filename for the parameter ECDFs.}

  \item{pruns.png.fname}{Output filename for the parameter trajectories across model evaluations.}

  \item{dp3d.png.fname}{Output filename for the pseudo-3D parameter plots.}

  \item{pairs.png.fname}{Output filename for the parameter correlation matrix.}

  \item{part.png.fname}{Output filename for the goodness-of-fit by particle plot.}

  \item{vruns.png.fname}{Output filename for the velocity evolution plot.}

  \item{modelout.best.png.fname}{Output filename for the observed-versus-best-simulation comparison plot.}

  \item{modelout.quant.png.fname}{Output filename for the ECDF or quantile-based model output plot.}

  \item{conv.png.fname}{Output filename for the convergence diagnostics plot.}
}

\details{
\code{read_results} reads information from the standard hydroPSO results directory, including parameter values, particle velocities, model outputs, convergence metrics, and goodness-of-fit values. In particular, it reads the following output files of hydroPSO: \cr

1) \sQuote{BestParameterSet.txt}: best parameter set and its corresponding goodness-of-fit found during the optimisation \cr

2) \sQuote{Particles.txt}: parameter values and their corresponding goodness-of-fit value for all particles and iterations \cr

3) \sQuote{Velocities.txt}: velocity values and their corresponding goodness-of-fit value for all particles and iterations \cr

4) \sQuote{Model_out.txt}: values of the objective function/model output for each particle and iteration \cr

5) \sQuote{ConvergenceMeasures.txt}: convergence measures summarizing performance of \code{\link{hydroPSO}} \cr

6) \sQuote{Particles_GofPerIter.txt}: goodness-of-fit only for all the particles during all the iterations \cr

\code{plot_results} is a high-level wrapper that calls several lower-level plotting utilities in sequence. Depending on the available files and arguments, it can produce:

The function \code{plot_results} is a high-level wrapper that calls several lower-level plotting utilities in sequence. Depending on the available files and arguments, it can produce: takes the outputs of the \code{read_results} function and then produces the following plots: \cr

 1) Dotty plots of parameter values \cr

 2) Histograms of parameter values \cr

 3) Boxplots of parameter values \cr

 4) Correlation matrix among parameter values (optional) \cr

 5) Empirical CDFs of parameter values \cr

 6) Parameter values vs Number of Model Evaluations \cr

 7) (pseudo) 3D dotty plots of (selected) parameter values \cr

 8) GoF for each particle against Number of Model Evaluations \cr

 9) Velocity values vs Number of Model Evaluations \cr
10a) Scatterplot between Best Simulated values and Observations (OPTIONAL, only if \code{MinMax} is provided)  \cr

10b) Empirical CDFs for model's output (only produced if \code{obs} is NOT a zoo object)  \cr

10b) ggof (See \code{\link[hydroGOF]{ggof}}) between Best Simulated values and Observations (OPTIONAL, only if \code{obs} is a zoo object) \cr

10d) Empirical CDFs for selected quantiles of model's output (OPTIONAL, only if \code{obs} is a zoo object) \cr

11) Convergence Measures (Gbest and normSwarmRadius) vs Iteration Number.

}

\value{
\code{read_results} returns a list with the following components:
\itemize{
  \item \code{best.param}: numeric vector with the best parameter set found during the optimisation,
  \item \code{best.gof}: numeric value of the best objective-function score,
  \item \code{params}: data frame with all tested parameter sets,
  \item \code{gofs}: numeric vector with the goodness-of-fit values associated with \code{params},
  \item \code{velocities}: matrix or data frame with particle velocities,
  \item \code{model.values}: model outputs read from \sQuote{Model_out.txt},
  \item \code{model.best}: best simulated output identified according to \code{MinMax},
  \item \code{model.obs}: observed values used during calibration,
  \item \code{convergence.measures}: convergence diagnostics read from \sQuote{ConvergenceMeasures.txt}, and
  \item \code{part.GofPerIter}: goodness-of-fit values for all particles across iterations.
}

\code{plot_results} is invoked for its side effects. It produces a sequence of diagnostic plots on screen or saves them as PNG files when \code{do.png = TRUE}. It does not return a structured object.
}

\examples{
\donttest{
local({
  
# Setting the user temporal directory as working directory
oldwd <- getwd()      
on.exit(setwd(oldwd), add = TRUE) 
setwd(tempdir())

# Number of dimensions to be optimised
D <- 5

# Boundaries of the search space (Ackley test function)
lower <- rep(-32, D)
upper <- rep(32, D)

# Setting the seed
set.seed(100)

# Running PSO with the 'ackley' test function, writing the results to text files
hydroPSO(fn=ackley, lower=lower, upper=upper)
  
# Reading all the results and storing them in a variable
res <- read_results()

# Plotting all the results with a goodness-of-fit value lower than 5
plot_results(MinMax="min", beh.thr=5)

}) # local END
} # donttest END

\donttest{
local({

################################################################################
#######################  SPSO-2007 example START ###############################
################################################################################
# Number of dimensions to be optimised
D <- 10

# boundaries for the test function
lower <- rep(-100, D)      # sphere
#lower <- rep(-5.12, D)    # rastrigin
#lower <- rep(-32, D)      # ackley

fn <- sphere
#fn <-rastrigin
#fn <-ackley

#######################################
#####   SPSO-2007 parameters   ########
npart  <- 10+floor(2*sqrt(D))
c1     <- 0.5+log(2)
c2     <- 0.5+log(2)
abstol <- 1e-20 
reltol <- 1e-20
maxit  <- 1000

use.IW        <- TRUE
IW.w          <- 1/(2*log(2))
REPORT        <- 100
lambda        <- 1
boundary.wall <- "absorbing2007"
#######################################

# Setting the user temporal directory as working directory
oldwd <- getwd()      
on.exit(setwd(oldwd), add = TRUE) 
setwd(tempdir())

# Running PSO  and writing the results to text files
set.seed(100)

hydroPSO(fn= fn, method="spso2007", lower=lower, upper=-lower,
         control=list(MinMax="min", maxit=maxit, npart=npart,  
                      c1=c1, c2=c2, 
                      use.IW=use.IW, IW.w=IW.w, 
                      topology="random", lambda=lambda, K=3,
                      Xini.type="random", Vini.type="random2007",
                      best.update="sync",
                      boundary.wall=boundary.wall, 
                      write2disk=TRUE, plot=FALSE, REPORT=REPORT,
                      abstol=abstol, reltol=reltol 
                      )
         )

# Plotting all the results 
plot_results(MinMax="min")

################################################################################
#######################  SPSO-2007 example END   ###############################
################################################################################


################################################################################
############### recommended hydroPSO configuration - START #####################
################################################################################

# Running PSO  and writing the results to text files
set.seed(100)
hydroPSO(fn= fn, method="spso2011", lower=lower, upper=-lower,
         control=list(MinMax="min", maxit=maxit, npart=40, 
                      c1=2.05, c2=2.05, 
                      use.IW=FALSE, use.CF=TRUE, 
                      topology="random", K=11,
                      use.TVlambda=TRUE, TVlambda.rng=c(1, 0.5), 
                      Xini.type="lhs", Vini.type="lhs2011",
                      best.update="sync",
                      boundary.wall="absorbing2011", 
                      write2disk=FALSE, plot=FALSE, REPORT=REPORT,
                      abstol=abstol, reltol=reltol 
                      )
         )

# compare the final optimum value and the number of function calls with those
# obtained in the SPSO-2007 example

################################################################################
################ recommended hydroPSO configuration - END ######################
################################################################################

}) # local END
} # donttest END
}

\author{
Mauricio Zambrano-Bigiarini
}

\seealso{
\code{\link{hydroPSO}}, \code{\link{read_best}}, \code{\link{read_particles}},
\code{\link{read_velocities}}, \code{\link{read_out}},
\code{\link{read_convergence}}, \code{\link{read_GofPerParticle}},
\code{\link{plot_particles}}, \code{\link{plot_out}},
\code{\link{plot_convergence}}, \code{\link{plot_NparOF}},
\code{\link{params2ecdf}}
}

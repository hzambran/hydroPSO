%% file plot_NparOF.Rd
%% Part of the hydroPSO R package, http://www.rforge.net/hydroPSO/ ; 
%%                                 http://cran.r-project.org/web/packages/hydroPSO
%% Copyright 2011-2026 Mauricio Zambrano-Bigiarini & Rodrigo Rojas
%% Distributed under GPL 2 or later

\name{plot_NparOF}
\alias{plot_NparOF}
\title{
Pairwise dotty plots of parameter interactions coloured by objective-function performance
}

\description{
Creates a set of pairwise parameter plots for exploring how the objective function varies across the parameter space. 
For \kbd{n} selected parameters, the function generates \kbd{sum(1:(npar-1))} \code{\link{plot_2parOF}} panels, corresponding to all possible two-parameter combinations.

Each panel displays one pair of parameters on the axes and colours the sampled parameter sets according to their goodness-of-fit values. This provides a compact visual summary of parameter sensitivity, equifinality, behavioural regions, and parameter interactions, which is especially useful in hydrological calibration studies based on Monte Carlo, PSO, GLUE, or other ensemble-based workflows.
}

\usage{
plot_NparOF(params, gofs, param.names=colnames(params), 
         MinMax=c(NULL,"min","max"), beh.thr=NA, nrows="auto", 
         gof.name="GoF", main=paste(gof.name, "Surface"), GOFcuts="auto", 
         colorRamp= colorRampPalette(c("darkred", "red", "orange", "yellow", 
         "green", "darkgreen", "cyan")), points.cex=0.7, alpha=0.65, 
         axis.rot=c(0, 0), verbose=TRUE)
}

\arguments{
  \item{params}{
  matrix or data.frame with one row per parameter set and one column per model parameter.
  }

  \item{gofs}{
  numeric vector with the goodness-of-fit values associated with each row of \code{params}, in the same order. Therefore, \code{length(gofs)} must be equal to \code{nrow(params)}.
  }

  \item{param.names}{
  character vector with the names of the parameters to be displayed. It can be a subset of \code{colnames(params)}, allowing the user to focus on the parameters that are most relevant for a given hydrological analysis.
  }

  \item{MinMax}{
  character string indicating whether better model performance corresponds to minimising or maximising the objective function. Valid values are \code{c(NULL, "min", "max")}. This argument is required when \code{beh.thr} is used.

  If \code{MinMax="min"}, parameter sets with lower goodness-of-fit values are treated as better solutions and are plotted on top of poorer solutions. If \code{MinMax="max"}, the opposite ordering is used. When \code{MinMax=NULL}, no preference for minimisation or maximisation is assumed.
  }

  \item{beh.thr}{
  optional numeric threshold used to retain only \sQuote{behavioural} parameter sets, following the GLUE terminology.

  If \code{MinMax="min"}, only parameter sets with \code{gofs <= beh.thr} are kept. If \code{MinMax="max"}, only parameter sets with \code{gofs >= beh.thr} are kept. This is useful when the goal is to visualise only the acceptable region of the parameter space instead of the full ensemble.
  }

  \item{nrows}{
  numeric value giving the number of rows in the multi-panel plotting layout.
  If \code{nrows="auto"}, the function computes the number of rows automatically from the number of panels to be produced.
  }

  \item{gof.name}{
  character string used as the title of the legend associated with the objective function values. Typical examples in hydrology are \code{"NSE"}, \code{"KGE"}, \code{"RMSE"}, or \code{"PBIAS"}.
  }

  \item{main}{
  character string intended for the main title. It is currently not used by the implementation.
  }

  \item{GOFcuts}{
  numeric vector defining the breakpoints used to convert \code{gofs} into performance classes for colouring the panels.

  If \code{GOFcuts="auto"}, the breakpoints are computed from quantiles of \code{gofs}. The quantile probabilities depend on \code{MinMax}:
  \itemize{
    \item if \code{MinMax=NULL}: \code{probs = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1)}
    \item if \code{MinMax="min"}: \code{probs = c(0, 0.25, 0.5, 0.85, 0.9, 0.97, 1)}
    \item if \code{MinMax="max"}: \code{probs = c(0, 0.03, 0.1, 0.15, 0.5, 0.75, 1)}
  }
  Automatic breakpoints are convenient for highlighting performance gradients within large calibration ensembles.
  }

  \item{colorRamp}{
  function defining the colour ramp used to colour the points according to \code{gofs}, or a character vector with colours from which such a ramp can be built.
  }

  \item{points.cex}{
  numeric value controlling the size of the plotted points.
  }

  \item{alpha}{
  numeric value between 0 and 1 controlling colour transparency. \code{0} means fully transparent and \code{1} means fully opaque.
  }

  \item{axis.rot}{
  numeric vector of length 2 giving the rotation angles, in degrees, for the left or bottom and right or top axis labels, respectively.
  }

  \item{verbose}{
  logical value. If \code{TRUE}, progress messages are printed during filtering, breakpoint computation, and panel generation.
  }
}

\value{
Produces a lattice-based multi-panel figure on the active graphics device. Each panel is generated internally through \code{\link{plot_2parOF}} and shows one pairwise parameter projection coloured by objective-function performance. The final panel contains the legend for the goodness-of-fit classes.

This type of display is particularly useful for diagnosing parameter identifiability, interaction effects, trade-offs, and behavioural regions in hydrological model calibration.
}

\author{
Mauricio Zambrano-Bigiarini, \email{mzb.devel@gmail.com}
}

\seealso{
\code{\link{plot_2parOF}}, \code{\link{read_results}}, \code{\link{plot_results}}, \code{\link{plot_GofPerParticle}}, \code{\link{plot_params}}, \code{\link{plot_ParamsPerIter}}
}

\examples{
# Number of dimensions to be optimised
D <- 5

# Boundaries of the search space (Rosenbrock test function)
lower <- rep(-30, D)
upper <- rep(30, D)

\donttest{
# Setting the user temporal directory as working directory
oldwd <- getwd()
on.exit(setwd(oldwd))
setwd(tempdir())

# Setting the seed
set.seed(100)

# Optimising the Rosenbrock test function and writing the results to text files
hydroPSO(fn=rosenbrock, lower=lower, upper=upper,
         control=list(write2disk=TRUE))

# Reading the particle history generated by hydroPSO
particles <- read_particles(file="./PSO.out/Particles.txt", plot=FALSE)

# Visualising pairwise parameter interactions coloured by model performance
plot_NparOF(params=particles[["part.params"]],
            gofs=particles[["part.gofs"]],
            gof.name="Rosenbrock",
            alpha=0.5)

# Focusing only on behavioural parameter sets can help reveal
# the most plausible regions of the parameter space
thr <- quantile(particles[["part.gofs"]], probs=0.10, na.rm=TRUE)
plot_NparOF(params=particles[["part.params"]],
            gofs=particles[["part.gofs"]],
            MinMax="min", beh.thr=thr,
            gof.name="Rosenbrock",
            alpha=0.6)
}
}

\keyword{math}
\keyword{graph}

%% File wquantile.R
%% Part of the hydroPSO R package, http://www.rforge.net/hydroPSO/ ; 
%%                                 http://cran.r-project.org/web/packages/hydroPSO
%% Copyright 2011-2026 Mauricio Zambrano-Bigiarini & Rodrigo Rojas
%% Distributed under GPL 2 or later

\name{wquantile}
\Rdversion{1.1}
\alias{wquantile}

\title{
Weighted Quantiles for Hydrological Ensembles
}

\description{
Computes weighted quantiles for each column of a matrix or data frame, or for each row when \code{byrow=TRUE}.

This function is a wrapper around \code{\link[Hmisc]{wtd.quantile}} from the \bold{Hmisc} package, adapted to hydrological ensemble analysis. It is particularly useful for summarising ensembles of simulated streamflow, groundwater levels, storage, or model parameters produced by multiple behavioural parameter sets, likelihood weights, or posterior samples.

In hydrology, this function is especially convenient for deriving uncertainty bands such as the 95\% prediction uncertainty interval, weighted medians, or other weighted summary limits from GLUE-style or Bayesian ensembles.
}

\usage{
wquantile(x, weights=NULL, byrow=FALSE, probs=c(.025, .5, .975), 
          normwt=TRUE, verbose=TRUE)
}

\arguments{
  \item{x}{
A numeric matrix or data frame containing the values from which weighted quantiles will be computed.

By default, quantiles are computed for each column of \code{x}. Therefore, rows usually represent ensemble members, behavioural parameter sets, posterior samples, or any other realisations, and columns represent variables, parameters, stations, or time steps.
}

  \item{weights}{
A numeric vector of weights used to compute the weighted quantiles. See \code{\link[Hmisc]{wtd.quantile}} for details. If \code{weights} is omitted, \code{NULL}, or a zero-length vector, the function returns the usual unweighted quantile estimates.

When \code{byrow=FALSE}, \code{length(weights)} must be equal to \code{nrow(x)}.

When \code{byrow=TRUE}, \code{length(weights)} must be equal to \code{ncol(x)}.
}

  \item{byrow}{
Logical. Indicates whether weighted quantiles should be computed by column or by row.

When \code{byrow=FALSE} (default), weighted quantiles are computed for each column of \code{x}. This is the most common setting for hydrological applications in which rows correspond to alternative model runs or parameter sets, and columns correspond to time steps, sites, variables, or model parameters.

When \code{byrow=TRUE}, weighted quantiles are computed for each row of \code{x}. This is useful when the values to be summarised are stored across columns instead of rows.
}

  \item{probs}{
Numeric vector of probabilities defining the quantiles to be computed. See \code{\link[Hmisc]{wtd.quantile}}.

The default is \kbd{c(.025, .5, .975)}, corresponding to the 2.5\%, 50\%, and 97.5\% quantiles. In hydrological uncertainty analysis, these are often used to characterise the lower limit, median, and upper limit of an ensemble.
}

  \item{normwt}{
Logical argument passed to \code{\link[Hmisc]{wtd.quantile}}. When \code{TRUE}, the supplied \code{weights} are normalised so that they sum to the effective sample size after removal of missing values.
}

  \item{verbose}{
Logical. If \code{TRUE}, a progress bar is shown while quantiles are computed.
}
}

\value{
A numeric matrix with one row for each summarised column of \code{x} when \code{byrow=FALSE}, or one row for each summarised row of \code{x} when \code{byrow=TRUE}. The number of columns in the returned matrix is \code{length(probs)}.

Column names correspond to the requested quantiles expressed as percentages, for example \code{"2.5%"}, \code{"50%"}, and \code{"97.5%"}. When available, row names are inherited from \code{colnames(x)} if \code{byrow=FALSE}, or from \code{rownames(x)} if \code{byrow=TRUE}.
}

\references{
Beven, K. and Binley, A. (2014). GLUE: 20 years on. \emph{Hydrological Processes}, \bold{28}(24), 5897--5918. \url{https://doi.org/10.1002/hyp.10082}.

Beven, K. and Binley, A. (1992). The future of distributed models: model calibration and uncertainty prediction. \emph{Hydrological Processes}, \bold{6}(3), 279--298. \url{https://doi.org/10.1002/hyp.3360060305}.
}

\author{
Mauricio Zambrano-Bigiarini, \email{mzb.devel@gmail.com}
}

\seealso{
\code{\link[Hmisc]{wtd.quantile}}, \code{\link{quant2ecdf}}
}

\examples{
#########################################################################
# Example 1: Weighted quantiles of model parameters (Param1, ..., ParamN)
#########################################################################
# Matrix with 100 behavioural parameter sets (rows) and 10 parameters
# (columns). In this layout, quantiles are computed for each parameter.
set.seed(123)
params <- matrix(rnorm(1000), nrow=100, ncol=10)
colnames(params) <- paste0("Param", 1:10)

# Equal weights for all parameter sets
wquantile(params, weights=rep(1, nrow(params)), byrow=FALSE)

# Example with unequal GLUE-style weights
w <- runif(nrow(params))
wquantile(params, weights=w, byrow=FALSE, probs=c(0.05, 0.5, 0.95))

#########################################################################
# Example 2: Weighted quantiles of simulated streamflow ensembles
#########################################################################
# Matrix with 10 behavioural parameter sets (rows) and 100 time steps
# (columns). In this layout, quantiles are computed for each simulated
# time step, yielding an uncertainty envelope through time.
sims <- matrix(rnorm(1000), nrow=10, ncol=100)
colnames(sims) <- paste0("t", 1:100)

# Equal weights for all simulations
wquantile(sims, weights=rep(1, nrow(sims)), byrow=FALSE)

# Weighted uncertainty band using likelihood-based weights, one weight 
# for each one of the 10 behavioural parameter sets
wsim <- c(0.30, 0.15, 0.10, 0.10, 0.08, 0.08, 0.07, 0.05, 0.04, 0.03)
wquantile(sims, weights=wsim, byrow=FALSE, probs=c(0.025, 0.5, 0.975))
}

\keyword{math}

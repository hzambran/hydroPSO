%% File quant2ecdf.Rd
%% Part of the hydroPSO R package, http://www.rforge.net/hydroPSO/ ; 
%%                                 http://cran.r-project.org/web/packages/hydroPSO
%% Copyright 2011-2026 Mauricio Zambrano-Bigiarini & Rodrigo Rojas
%% Distributed under GPL 2 or later

\name{quant2ecdf}
\Rdversion{1.1}
\alias{quant2ecdf}
\alias{quant2ecdf.default}
\alias{quant2ecdf.matrix}
\alias{quant2ecdf.data.frame}
\title{
From simulated ensembles to empirical cumulative distribution functions of selected quantiles
}
\description{
Computes empirical cumulative distribution functions (ECDFs) for user-selected quantiles derived from an ensemble of simulated values, with optional weighting and plotting.

This function is especially useful in hydrology when comparing the distribution of low, median, and high simulated flows across behavioural parameter sets, for example in GLUE-style analyses, ensemble streamflow prediction, or uncertainty assessment. It helps summarise not only the central tendency of simulated quantiles, but also their full empirical distribution across acceptable model runs.
}
\usage{
quant2ecdf(sim, ...)
           
\method{quant2ecdf}{default}(sim, weights=NULL, byrow=TRUE, 
           quantiles.desired= c(0.05, 0.5, 0.95), plot=TRUE, obs=NULL, 
           quantiles.labels= c("Q5", "Q50", "Q95"), main=NULL, 
           ylab="Probability", col="blue", leg.cex=1.2, leg.pos="bottomright", 
           cex.axis=1.2, cex.main=1.2, cex.lab=1.2, verbose=TRUE, ...)
           
\method{quant2ecdf}{matrix}(sim, weights=NULL, byrow=TRUE, 
           quantiles.desired= c(0.05, 0.5, 0.95), plot=TRUE, obs=NULL, 
           quantiles.labels= c("Q5", "Q50", "Q95"), main=NULL, 
           ylab="Probability", col="blue", leg.cex=1.2, leg.pos="bottomright", 
           cex.axis=1.2, cex.main=1.2, cex.lab=1.2, verbose=TRUE, ...)
           
\method{quant2ecdf}{data.frame}(sim, weights=NULL, byrow=TRUE, 
           quantiles.desired= c(0.05, 0.5, 0.95), plot=TRUE, obs=NULL, 
           quantiles.labels= c("Q5", "Q50", "Q95"), main=NULL, 
           ylab="Probability", col="blue", leg.cex=1.2, leg.pos="bottomright", 
           cex.axis=1.2, cex.main=1.2, cex.lab=1.2, verbose=TRUE, ...)
}
\arguments{
  \item{sim}{
A matrix or data.frame with simulated values obtained from different parameter sets, model structures, or ensemble members.

By default, each \emph{row} of \code{sim} is assumed to correspond to one behavioural simulation, and the quantiles are computed across the values stored in that row. This layout is often convenient when each row represents a complete simulated time series or a set of simulated values associated with one parameter set.
}
  \item{weights}{
Optional numeric vector of weights used when constructing the ECDFs of the quantiles. In hydrological applications these weights will often represent likelihoods, goodness-of-fit measures, posterior weights, or other measures of behavioural performance assigned to each simulation.

If \code{weights} is omitted, \code{NULL}, or a zero-length vector, the ECDFs are unweighted.

When \code{byrow=TRUE}, \code{length(weights)} must be equal to \code{nrow(sim)} because one quantile set is computed per row.

When \code{byrow=FALSE}, \code{length(weights)} must be equal to \code{ncol(sim)} because one quantile set is computed per column.
}
  \item{byrow}{
Logical value indicating whether quantiles are computed for each row or for each column of \code{sim}.

When \code{byrow=TRUE} (the default), quantiles are computed for each row of \code{sim}. Use this when each row stores the simulated values associated with one behavioural parameter set.

When \code{byrow=FALSE}, quantiles are computed for each column of \code{sim}. This is often convenient when behavioural simulations are stored in columns, for example when each column is one ensemble member and rows correspond to time steps.
}
  \item{quantiles.desired}{
Numeric vector with the probabilities of the quantiles to be analysed. The default is \kbd{c(0.05, 0.5, 0.95)}, corresponding to the 5\%, 50\%, and 95\% quantiles.

These are commonly used in hydrology to characterise low-flow, median-flow, and high-flow behaviour, but any set of probabilities between 0 and 1 can be supplied.
}
  \item{plot}{
Logical value indicating whether a plot of the resulting ECDFs should be produced.
}
  \item{obs}{
Optional numeric, integer, or \code{zoo} object with observed values. Only used when \code{plot=TRUE}.

When provided, the corresponding observed quantile is added to each panel as a vertical reference line, allowing a direct visual comparison between the simulated ECDF and the observed benchmark.
}
  \item{quantiles.labels}{
Optional character vector with labels for \code{quantiles.desired}. Only used when \code{plot=TRUE}. The default is \kbd{c("Q5", "Q50", "Q95")}.

Using informative labels can improve figure readability, especially when presenting hydrological uncertainty bounds to non-specialist audiences.
}
  \item{main}{
Optional title for the plot. Only used when \code{plot=TRUE}.
}
  \item{ylab}{
Label for the y-axis. Only used when \code{plot=TRUE}. See \code{\link[graphics]{plot}}.
}
  \item{col}{
Colour used for plotting the ECDFs. Only used when \code{plot=TRUE}. See \code{\link[graphics]{par}}.
}
  \item{leg.cex}{
Numeric character expansion factor used in the legend. Only used when \code{plot=TRUE}. The default is \code{1.2}.
}
  \item{leg.pos}{
Keyword indicating the legend position. Only used when \code{plot=TRUE}. See \code{\link[graphics]{legend}}.
}
  \item{cex.axis}{
Numeric magnification used for axis annotations relative to \code{cex}. Only used when \code{plot=TRUE}. See \code{\link[graphics]{par}}.
}
  \item{cex.main}{
Numeric magnification used for the main title relative to the current \code{cex} setting. Only used when \code{plot=TRUE}.
}
  \item{cex.lab}{
Numeric magnification used for x- and y-axis labels relative to the current \code{cex} setting. Only used when \code{plot=TRUE}. See \code{\link[graphics]{par}}.
}
  \item{verbose}{
Logical value indicating whether progress messages should be printed.
}
  \item{\dots}{
Further arguments passed to \code{\link[graphics]{plot}} or from other methods.
}
}
\details{
The function proceeds in two steps:

\enumerate{
  \item For each simulation unit defined by \code{byrow}, it computes the unweighted quantiles specified in \code{quantiles.desired} using \code{\link{wquantile}}.

  \item For each requested quantile level, it computes an empirical cumulative distribution function across the ensemble of previously computed quantile values using \code{\link[Hmisc]{wtd.Ecdf}}. If \code{weights} is supplied, the ECDF is weighted accordingly.
}

This workflow is useful when the analyst wants to assess how uncertain a given summary of the simulations is. For example, instead of reporting only one median low-flow estimate across behavioural runs, the function allows the user to inspect the full ECDF of that low-flow quantile and compare it with the corresponding observed value.
}
\value{
A named list with one element for each value in \code{quantiles.desired}. The element names are taken from \code{quantiles.labels}.

Each list element is the object returned by \code{\link[Hmisc]{wtd.Ecdf}}, typically containing at least:

\item{x}{The sorted unique values of the selected simulated quantile across the ensemble.}

\item{ecdf}{The empirical cumulative probabilities associated with \code{x}. If the first estimated CDF value is greater than zero, an additional point at probability zero may be added at the minimum value.}

If \code{plot=TRUE}, the function also produces a multi-panel figure with one ECDF per requested quantile, optionally including the corresponding observed quantile, the median of the simulated ECDF, and the percentage bias of that median relative to the observed quantile.
}
\author{
Mauricio Zambrano-Bigiarini, \email{mzb.devel@gmail.com}
}
\note{
This function relies on \code{\link[Hmisc]{wtd.Ecdf}} from the \pkg{Hmisc} package.
}

\seealso{
\code{\link[Hmisc]{wtd.Ecdf}}, \code{\link{wquantile}}, \code{\link{params2ecdf}}
}
\examples{
#########################################################################
# Example 1: Unweighted ECDFs of selected quantiles from a streamflow 
#            ensemble
#########################################################################
# Simulated streamflows from 1000 behavioural parameter sets (rows)
# and 100 time steps (columns)
set.seed(123)
sim <- matrix(abs(rnorm(50 * 100, mean=20, sd=8)), nrow=1000, ncol=100)

# Unweighted ECDFs for Q5, Q50, and Q95 across behavioural simulations
q.ecdf <- quant2ecdf(sim, byrow=TRUE, plot=FALSE)
str(q.ecdf, max.level=1)

#########################################################################
# Example 2: Weighted ECDFs using likelihood-type weights
#########################################################################
# In GLUE-type applications, each behavioural simulation can be assigned
# a weight representing its relative performance.
weights <- runif(nrow(sim))
weights <- weights / sum(weights)

q.ecdf.w <- quant2ecdf(sim, weights=weights, byrow=TRUE, plot=FALSE,
                       quantiles.desired=c(0.05, 0.5, 0.95),
                       quantiles.labels=c("Q5", "Q50", "Q95"))

#########################################################################
# Example 3: Visual comparison against observed flows
#########################################################################
obs <- abs(rnorm(100, mean=21, sd=7))
quant2ecdf(sim, weights=weights, byrow=TRUE, obs=obs,
           main="ECDFs of simulated flow quantiles")
}
\keyword{math}
\keyword{manip}
\keyword{graph}

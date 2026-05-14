%% File params2ecdf.Rd
%% Part of the hydroPSO R package, http://www.rforge.net/hydroPSO/ ; 
%%                                 http://cran.r-project.org/web/packages/hydroPSO
%% Copyright 2011-2026 Mauricio Zambrano-Bigiarini & Rodrigo Rojas
%% Distributed under GPL 2 or later

\name{params2ecdf}
\Rdversion{1.1}
\alias{params2ecdf}
\alias{params2ecdf.default}
\alias{params2ecdf.matrix}
\alias{params2ecdf.data.frame}

\title{
Parameter values to empirical cumulative distribution functions
}

\description{
Computes empirical cumulative distribution functions (ECDFs) for model parameters from an ensemble of calibrated or behavioural parameter sets, with optional weighting and plotting.

This function is especially useful in hydrology when analysing the parameter space sampled during calibration or uncertainty analysis with \code{\link{hydroPSO}}. The resulting ECDFs help identify well-constrained parameters, parameter ranges that are consistently supported by behavioural simulations, and the extent to which prior expectations or reference values agree with the calibrated ensemble.
}

\usage{
params2ecdf(params, ...)

\method{params2ecdf}{default}(params, param.names=NULL, gofs=NULL, MinMax=NULL,
        beh.thr=NA, weights=NULL, byrow=FALSE, plot=TRUE, obs=NULL, main=NULL,
        nrows="auto", ylab="Probability", col="blue", leg.cex=1.2,
        leg.pos="topleft", cex.axis=1.2, cex.main=1.2, cex.lab=1.2,
        verbose=TRUE, ..., do.png=FALSE, png.width=1500, png.height=900,
        png.res=90, png.fname="Params_ECDFs.png")

\method{params2ecdf}{matrix}(params, param.names=colnames(params), gofs=NULL,
        MinMax=NULL, beh.thr=NA, weights=NULL, byrow=FALSE, plot=TRUE,
        obs=NULL, main=NULL, nrows="auto", ylab="Probability", col="blue",
        leg.cex=1.2, leg.pos="topleft", cex.axis=1.2, cex.main=1.2,
        cex.lab=1.2, verbose=TRUE, ..., do.png=FALSE, png.width=1500,
        png.height=900, png.res=90, png.fname="Params_ECDFs.png")

\method{params2ecdf}{data.frame}(params, param.names=colnames(params), gofs=NULL,
        MinMax=NULL, beh.thr=NA, weights=NULL, byrow=FALSE, plot=TRUE,
        obs=NULL, main=NULL, nrows="auto", ylab="Probability", col="blue",
        leg.cex=1.2, leg.pos="topleft", cex.axis=1.2, cex.main=1.2,
        cex.lab=1.2, verbose=TRUE, ..., do.png=FALSE, png.width=1500,
        png.height=900, png.res=90, png.fname="Params_ECDFs.png")
}

\arguments{
  \item{params}{
A matrix or data frame with parameter values from multiple model runs. Each row should contain one parameter set, and each column should contain the sampled values of one model parameter.
}

  \item{param.names}{
Character vector with the names to be used for the parameters in \code{params}. By default, the column names of \code{params} are used.
}

  \item{gofs}{
Optional numeric vector with one goodness-of-fit value for each parameter set in \code{params}. This is required when \code{beh.thr} is used to retain only behavioural parameter sets.
}

  \item{MinMax}{
Optional character string indicating whether better model performance corresponds to minimising or maximising the objective function in \code{gofs}. This argument is only used together with \code{beh.thr} to select behavioural parameter sets.

Valid values are \code{"min"} and \code{"max"}.
}

  \item{beh.thr}{
Optional numeric threshold used to retain only behavioural parameter sets. If \code{MinMax="max"}, parameter sets with \code{gofs >= beh.thr} are retained. If \code{MinMax="min"}, parameter sets with \code{gofs <= beh.thr} are retained.

The default \code{beh.thr=NA} means that all parameter sets are used.
}

  \item{weights}{
Optional numeric vector with one weight for each row of \code{params}. These weights are passed to \code{\link[Hmisc]{wtd.Ecdf}} to compute weighted ECDFs.

Omitting \code{weights}, or providing \code{NULL} or a zero-length vector, yields the usual unweighted ECDFs.
}

  \item{byrow}{
Logical value kept for interface compatibility. In the current implementation, the calculations are performed column-wise, so \code{params} should be organised with parameter sets in rows and parameters in columns.
}

  \item{plot}{
Logical value indicating whether a plot of the ECDF for each parameter should be produced.
}

  \item{obs}{
Optional numeric or \code{zoo} object with one reference value for each parameter. When \code{plot=TRUE}, these values are drawn as vertical reference lines to facilitate visual comparison between observed, target, or preferred parameter values and the parameter ensemble.
}

  \item{main}{
Optional overall title for the plot.
}

  \item{nrows}{
Optional number of rows in the plotting layout when \code{plot=TRUE}. If \code{nrows="auto"}, the layout is chosen automatically from the number of parameters.
}

  \item{ylab}{
Optional label for the y axis. See \code{\link[graphics]{plot}}.
}

  \item{col}{
Optional plotting colour used for the ECDF curves. See \code{\link[graphics]{par}}.
}

  \item{leg.cex}{
Optional character expansion factor for legends when \code{plot=TRUE}.
}

  \item{leg.pos}{
Optional keyword controlling the legend position. See \code{\link[graphics]{legend}}.
}

  \item{cex.axis}{
Optional magnification for axis annotation relative to the current \code{cex} setting.
}

  \item{cex.main}{
Optional magnification for main titles relative to the current \code{cex} setting.
}

  \item{cex.lab}{
Optional magnification for x and y labels relative to the current \code{cex} setting.
}

  \item{verbose}{
Logical value. If \code{TRUE}, progress messages are printed.
}

  \item{\dots}{
Further arguments passed to \code{plot} or to other methods.
}

  \item{do.png}{
Logical value indicating whether the figures should be written to PNG files instead of the active graphics device.
}

  \item{png.width}{
Optional width of the PNG device in pixels when \code{do.png=TRUE}. See \code{\link[grDevices]{png}}.
}

  \item{png.height}{
Optional height of the PNG device in pixels when \code{do.png=TRUE}. See \code{\link[grDevices]{png}}.
}

  \item{png.res}{
Optional nominal resolution in ppi for the PNG device when \code{do.png=TRUE}. See \code{\link[grDevices]{png}}.
}

  \item{png.fname}{
Optional file name used to store the PNG output when \code{do.png=TRUE}.
}
}

\value{
A list with one element per parameter, where the number of parameters is the number of columns in \code{params}.

Each list element is the object returned by \code{\link[Hmisc]{wtd.Ecdf}} for the corresponding parameter. These objects contain the information required to inspect, plot, or interpolate the empirical cumulative distribution of each parameter.

If \code{plot=TRUE}, one or more figures are also produced. At most 21 parameters are shown per figure, so the number of figures is \code{ceiling(nparam/21)}.
}

\references{
In hydrological calibration and uncertainty analysis, empirical cumulative distributions are useful for diagnosing parameter identifiability, comparing prior and posterior parameter support, and summarising behavioural ensembles.
}

\author{
Mauricio Zambrano-Bigiarini, \email{mzb.devel@gmail.com}
}

\seealso{
\code{\link[Hmisc]{wtd.Ecdf}}, \code{\link{quant2ecdf}}, \code{\link{hydroPSO}}
}

\examples{

# Example 1: ECDFs of calibrated parameters from an ensemble of behavioural runs
set.seed(123)
params <- matrix(runif(600), nrow=100, ncol=6)
colnames(params) <- c("Kq", "Ks", "Alpha", "Beta", "Kperc", "Smax")

# Unweighted ECDFs for all parameters
pe <- params2ecdf(params, plot=FALSE)
str(pe[[1]])

\donttest{
# Example 2: Weighted ECDFs using likelihood-based weights
# Useful when some parameter sets are more plausible than others
w <- rexp(100)
w <- w / sum(w)
params2ecdf(params, weights=w, plot=TRUE,
            main="Weighted ECDFs of behavioural parameter sets")

# Example 3: Retaining only behavioural parameter sets above a threshold
gofs <- runif(100, min=0.4, max=0.95)
params2ecdf(params, gofs=gofs, MinMax="max", beh.thr=0.75,
            plot=TRUE, main="Parameter ECDFs for behavioural simulations")
}

}

\keyword{math}
\keyword{manip}
\keyword{graph}

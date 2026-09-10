%% File read_verification_results.Rd
%% Part of the hydroPSO R package, https://github.com/hzambran/hydroPSO
%%                                 http://cran.r-project.org/web/packages/hydroPSO
%%                                 http://www.rforge.net/hydroPSO/
%% Copyright 2026 Mauricio Zambrano-Bigiarini & Rodrigo Rojas
%% Distributed under GPL 2 or later

\name{read_verification_results}
\Rdversion{1.1}
\alias{read_verification_results}
\alias{plot_verification_results}

\title{Read and Visualise Verification Result Files}

\description{
\code{read_verification_results} reads the output files produced by \code{\link{verification}} when \code{write2disk=TRUE} and organises them into a single list for post-processing.

\code{plot_verification_results} builds on those outputs to generate diagnostic plots for the verified parameter sets, their goodness-of-fit values, and, when observations are provided, the verification-period model outputs.
}

\usage{
read_verification_results(drty.out = "Verification.out",
                          MinMax = c("min", "max"), beh.thr = NA,
                          modelout.cols = NULL, verbose = TRUE)

plot_verification_results(drty.out = "Verification.out", param.names = NULL,
             gof.name = "GoF", MinMax = c("min", "max"), beh.thr = NA,
             beh.col = "red", beh.lty = 1, beh.lwd = 2, nrows = "auto",
             col = "black", ylab = gof.name, main = NULL, pch = 19,
             cex = 0.5, cex.main = 1.7, cex.axis = 1.3,
             cex.lab = 1.5, breaks = "Scott", freq = TRUE,
             do.pairs = FALSE, weights = NULL, byrow = FALSE,
             leg.cex = 1.2, dp3D.names = "auto", GOFcuts = "auto",
             colorRamp = colorRampPalette(c("darkred", "red", "orange",
             "yellow", "green", "darkgreen", "cyan")), alpha = 0.65,
             points.cex = 0.7, obs = NULL, obs.tzone = NULL,
             modelout.cols = NULL, ftype = "o", FUN = mean,
             quantiles.desired = c(0.05, 0.5, 0.95),
             quantiles.labels = c("Q5", "Q50", "Q95"),
             do.png = FALSE, png.res = 90, png.width = 1500,
             png.height = 900, params.png.width = 1500,
             params.png.height = 900,
             dotty.png.fname = "Verification_Params_DottyPlots.png",
             hist.png.fname = "Verification_Params_Histograms.png",
             bxp.png.fname = "Verification_Params_Boxplots.png",
             ecdf.png.fname = "Verification_Params_ECDFs.png",
             pruns.png.fname = "Verification_Params_ValuesPerRun.png",
             dp3d.png.fname = "Verification_Params_dp3d.png",
             pairs.png.fname = "Verification_Params_Pairs.png",
             modelout.best.png.fname =
               "Verification_ModelOut_BestSim_vs_Obs.png",
             modelout.quant.png.fname =
               "Verification_ModelOut_Quantiles.png",
             verbose = TRUE, skip.incompatible.obs = FALSE)
}

\arguments{
  \item{drty.out}{character string with the path to the directory containing the verification output files. If only a directory name is provided, it is interpreted relative to the current working directory.}

  \item{param.names}{optional character vector with the names of the parameters to be plotted. By default, all parameters available in \sQuote{Verification-ParamValues.txt} are used.}

  \item{gof.name}{character string used to label the goodness-of-fit metric in the plots, for example \code{"NSE"}, \code{"KGE"}, or \code{"RMSE"}.}

  \item{MinMax}{character string indicating whether the best goodness-of-fit value corresponds to the minimum or maximum of the objective function. Valid values are \code{"min"} and \code{"max"}. This must match the value used in \code{\link{verification}}.}

  \item{beh.thr}{optional numeric threshold used to retain only behavioural verification parameter sets. If \code{MinMax="min"}, parameter sets with \code{gofs <= beh.thr} are retained. If \code{MinMax="max"}, parameter sets with \code{gofs >= beh.thr} are retained.}

  \item{modelout.cols}{optional numeric vector indicating which columns of \sQuote{Verification-ModelOut.txt} should be read or plotted, excluding the first two bookkeeping columns (parameter-set number and goodness-of-fit). If \code{NULL}, all model output columns after the first two are used.}

  \item{verbose}{logical value indicating whether progress messages should be printed.}

  \item{beh.col}{colour used to draw the horizontal threshold line that separates behavioural from non-behavioural solutions in relevant plots.}

  \item{beh.lty}{line type used for the behavioural threshold line.}

  \item{beh.lwd}{line width used for the behavioural threshold line.}

  \item{nrows}{number of rows in multi-panel plotting layouts. If \code{"auto"}, the layout is computed automatically from the number of parameters to be displayed.}

  \item{col}{colour used for plotting points or lines in the diagnostic plots.}

  \item{ylab}{label for the y-axis in the dotty plots. The default is \code{gof.name}.}

  \item{main}{optional main title passed to the relevant plotting routines.}

  \item{pch}{plotting symbol used for the dotty plots.}

  \item{cex}{general expansion factor controlling the size of points and text in the plots.}

  \item{cex.main}{expansion factor for main titles.}

  \item{cex.axis}{expansion factor for axis annotations.}

  \item{cex.lab}{expansion factor for axis labels.}

  \item{breaks}{break specification for parameter histograms. Passed to \code{\link[graphics]{hist}}.}

  \item{freq}{logical value indicating whether histograms should display frequencies (\code{TRUE}) or densities (\code{FALSE}).}

  \item{do.pairs}{logical value indicating whether a parameter correlation matrix should also be plotted.}

  \item{weights}{optional numeric vector of weights used when computing empirical cumulative distribution functions of parameter values or simulated outputs.}

  \item{byrow}{logical value passed to ECDF-related routines.}

  \item{leg.cex}{expansion factor for legend text.}

  \item{dp3D.names}{names of the parameters to be used in the pseudo-3D dotty plots. If \code{"auto"}, a subset of parameters is selected automatically.}

  \item{GOFcuts}{numeric vector defining the goodness-of-fit intervals used to colour the pseudo-3D dotty plots. If \code{"auto"}, these intervals are computed automatically.}

  \item{colorRamp}{function or vector defining the colour ramp used in the pseudo-3D dotty plots.}

  \item{alpha}{numeric value between 0 and 1 controlling colour transparency in the pseudo-3D dotty plots.}

  \item{points.cex}{point size used in the pseudo-3D dotty plots.}

  \item{obs}{optional numeric or zoo vector with observations to be compared against the best simulated verification output. If \code{NULL}, \code{plot_verification_results} tries to read \sQuote{Observations.txt} from \code{drty.out}; if the file is not available, observation-dependent plots are skipped.}

  \item{obs.tzone}{optional time zone used when reading sub-daily observations from \sQuote{Observations.txt}.}

  \item{ftype}{graphical type used when plotting observed and simulated time series. Passed to \code{\link{plot_out}} and, when relevant, to \code{\link[hydroGOF]{ggof}}.}

  \item{FUN}{summary function used by time-series plotting methods when aggregation is needed. Passed to \code{\link{plot_out}}.}

  \item{quantiles.desired}{numeric vector with the simulation quantiles to be plotted.}

  \item{quantiles.labels}{character vector with labels for \code{quantiles.desired}.}

  \item{do.png}{logical value indicating whether plots should be saved as PNG files instead of being drawn only on the active graphics device.}

  \item{png.res}{resolution of all output PNG figures, in pixels per inch.}

  \item{png.width}{width of the output PNG figures, in pixels.}

  \item{png.height}{height of the output PNG figures, in pixels.}

  \item{params.png.width}{width of the output parameter PNG figures, in pixels.}

  \item{params.png.height}{height of the output parameter PNG figures, in pixels.}

  \item{dotty.png.fname}{output filename for the parameter dotty plots.}

  \item{hist.png.fname}{output filename for the parameter histograms.}

  \item{bxp.png.fname}{output filename for the parameter boxplots.}

  \item{ecdf.png.fname}{output filename for the parameter ECDFs.}

  \item{pruns.png.fname}{output filename for the parameter trajectories across verification runs.}

  \item{dp3d.png.fname}{output filename for the pseudo-3D parameter plots.}

  \item{pairs.png.fname}{output filename for the parameter correlation matrix.}

  \item{modelout.best.png.fname}{output filename for the observed-versus-best-simulation comparison plot.}

  \item{modelout.quant.png.fname}{output filename for the ECDF or quantile-based model output plot.}

  \item{skip.incompatible.obs}{logical value indicating whether incompatible observed and simulated output lengths should be treated as a warning instead of an error. The default \code{FALSE} preserves the strict check. When \code{TRUE} and \code{length(obs)} differs from the number of simulated values in each row of \sQuote{Verification-ModelOut.txt}, observation-dependent plots are skipped.}
}

\details{
\code{read_verification_results} reads information from the standard verification results directory. In particular, it reads the following output files produced by \code{\link{verification}}:

1) \sQuote{Verification-ModelOut.txt}: model outputs and goodness-of-fit values for all verification parameter sets \cr

2) \sQuote{Verification-ParamValues.txt}: parameter values and goodness-of-fit values for all verification parameter sets \cr

\code{plot_verification_results} reads the same files through \code{read_verification_results} and then creates the subset of \code{\link{plot_results}} diagnostics that can be produced from verification output files: parameter dotty plots, histograms, boxplots, optional pairs plots, ECDFs, parameter trajectories, pseudo-3D parameter plots, and optional model-output plots when observations are available.
}

\value{
\code{read_verification_results} returns a list with the following components:
\itemize{
  \item \code{gofs}: numeric vector with the goodness-of-fit values corresponding to each parameter set,
  \item \code{model.values}: data frame with the model outputs corresponding to each parameter set,
  \item \code{best.gof}: numeric value with the best goodness-of-fit found during verification,
  \item \code{best.param}: numeric vector with the best parameter set found during verification, and
  \item \code{params}: data frame with the verification parameter sets.
}

\code{plot_verification_results} is invoked for its side effects. It produces diagnostic plots on screen or saves them as PNG files when \code{do.png = TRUE}. It does not return a structured object.
}

\author{
Mauricio Zambrano-Bigiarini, \email{mzb.devel@gmail.com}
}

\seealso{
\code{\link{verification}}, \code{\link{read_results}}, \code{\link{plot_results}}, \code{\link{read_out}}, \code{\link{plot_out}}
}

\examples{
local({

drty.out <- tempfile("verification-out-")

params <- matrix(c(1, 2,
                   0, 0,
                   3, 4),
                 ncol=2, byrow=TRUE)
colnames(params) <- c("x", "y")

invisible(
  verification(fn=sphere, par=params,
               control=list(drty.out=drty.out, MinMax="min",
                            write2disk=TRUE, verbose=FALSE))
)

res <- read_verification_results(drty.out, MinMax="min", verbose=FALSE)
res$best.param

}) # local END
}

\keyword{files}
\keyword{optimize}

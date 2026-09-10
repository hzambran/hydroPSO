# Read and Visualise Verification Result Files

`read_verification_results` reads the output files produced by
[`verification`](http://mzb.cl/hydroPSO/reference/verification.md) when
`write2disk=TRUE` and organises them into a single list for
post-processing.

`plot_verification_results` builds on those outputs to generate
diagnostic plots for the verified parameter sets, their goodness-of-fit
values, and, when observations are provided, the verification-period
model outputs.

## Usage

``` r
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
```

## Arguments

- drty.out:

  character string with the path to the directory containing the
  verification output files. If only a directory name is provided, it is
  interpreted relative to the current working directory.

- param.names:

  optional character vector with the names of the parameters to be
  plotted. By default, all parameters available in
  ‘Verification-ParamValues.txt’ are used.

- gof.name:

  character string used to label the goodness-of-fit metric in the
  plots, for example `"NSE"`, `"KGE"`, or `"RMSE"`.

- MinMax:

  character string indicating whether the best goodness-of-fit value
  corresponds to the minimum or maximum of the objective function. Valid
  values are `"min"` and `"max"`. This must match the value used in
  [`verification`](http://mzb.cl/hydroPSO/reference/verification.md).

- beh.thr:

  optional numeric threshold used to retain only behavioural
  verification parameter sets. If `MinMax="min"`, parameter sets with
  `gofs <= beh.thr` are retained. If `MinMax="max"`, parameter sets with
  `gofs >= beh.thr` are retained.

- modelout.cols:

  optional numeric vector indicating which columns of
  ‘Verification-ModelOut.txt’ should be read or plotted, excluding the
  first two bookkeeping columns (parameter-set number and
  goodness-of-fit). If `NULL`, all model output columns after the first
  two are used.

- verbose:

  logical value indicating whether progress messages should be printed.

- beh.col:

  colour used to draw the horizontal threshold line that separates
  behavioural from non-behavioural solutions in relevant plots.

- beh.lty:

  line type used for the behavioural threshold line.

- beh.lwd:

  line width used for the behavioural threshold line.

- nrows:

  number of rows in multi-panel plotting layouts. If `"auto"`, the
  layout is computed automatically from the number of parameters to be
  displayed.

- col:

  colour used for plotting points or lines in the diagnostic plots.

- ylab:

  label for the y-axis in the dotty plots. The default is `gof.name`.

- main:

  optional main title passed to the relevant plotting routines.

- pch:

  plotting symbol used for the dotty plots.

- cex:

  general expansion factor controlling the size of points and text in
  the plots.

- cex.main:

  expansion factor for main titles.

- cex.axis:

  expansion factor for axis annotations.

- cex.lab:

  expansion factor for axis labels.

- breaks:

  break specification for parameter histograms. Passed to
  [`hist`](https://rdrr.io/r/graphics/hist.html).

- freq:

  logical value indicating whether histograms should display frequencies
  (`TRUE`) or densities (`FALSE`).

- do.pairs:

  logical value indicating whether a parameter correlation matrix should
  also be plotted.

- weights:

  optional numeric vector of weights used when computing empirical
  cumulative distribution functions of parameter values or simulated
  outputs.

- byrow:

  logical value passed to ECDF-related routines.

- leg.cex:

  expansion factor for legend text.

- dp3D.names:

  names of the parameters to be used in the pseudo-3D dotty plots. If
  `"auto"`, a subset of parameters is selected automatically.

- GOFcuts:

  numeric vector defining the goodness-of-fit intervals used to colour
  the pseudo-3D dotty plots. If `"auto"`, these intervals are computed
  automatically.

- colorRamp:

  function or vector defining the colour ramp used in the pseudo-3D
  dotty plots.

- alpha:

  numeric value between 0 and 1 controlling colour transparency in the
  pseudo-3D dotty plots.

- points.cex:

  point size used in the pseudo-3D dotty plots.

- obs:

  optional numeric or zoo vector with observations to be compared
  against the best simulated verification output. If `NULL`,
  `plot_verification_results` tries to read ‘Observations.txt’ from
  `drty.out`; if the file is not available, observation-dependent plots
  are skipped.

- obs.tzone:

  optional time zone used when reading sub-daily observations from
  ‘Observations.txt’.

- ftype:

  graphical type used when plotting observed and simulated time series.
  Passed to
  [`plot_out`](http://mzb.cl/hydroPSO/reference/ReadPlot_out.md) and,
  when relevant, to
  [`ggof`](https://hzambran.github.io/hydroGOF/reference/ggof.html).

- FUN:

  summary function used by time-series plotting methods when aggregation
  is needed. Passed to
  [`plot_out`](http://mzb.cl/hydroPSO/reference/ReadPlot_out.md).

- quantiles.desired:

  numeric vector with the simulation quantiles to be plotted.

- quantiles.labels:

  character vector with labels for `quantiles.desired`.

- do.png:

  logical value indicating whether plots should be saved as PNG files
  instead of being drawn only on the active graphics device.

- png.res:

  resolution of all output PNG figures, in pixels per inch.

- png.width:

  width of the output PNG figures, in pixels.

- png.height:

  height of the output PNG figures, in pixels.

- params.png.width:

  width of the output parameter PNG figures, in pixels.

- params.png.height:

  height of the output parameter PNG figures, in pixels.

- dotty.png.fname:

  output filename for the parameter dotty plots.

- hist.png.fname:

  output filename for the parameter histograms.

- bxp.png.fname:

  output filename for the parameter boxplots.

- ecdf.png.fname:

  output filename for the parameter ECDFs.

- pruns.png.fname:

  output filename for the parameter trajectories across verification
  runs.

- dp3d.png.fname:

  output filename for the pseudo-3D parameter plots.

- pairs.png.fname:

  output filename for the parameter correlation matrix.

- modelout.best.png.fname:

  output filename for the observed-versus-best-simulation comparison
  plot.

- modelout.quant.png.fname:

  output filename for the ECDF or quantile-based model output plot.

- skip.incompatible.obs:

  logical value indicating whether incompatible observed and simulated
  output lengths should be treated as a warning instead of an error. The
  default `FALSE` preserves the strict check. When `TRUE` and
  `length(obs)` differs from the number of simulated values in each row
  of ‘Verification-ModelOut.txt’, observation-dependent plots are
  skipped.

## Details

`read_verification_results` reads information from the standard
verification results directory. In particular, it reads the following
output files produced by
[`verification`](http://mzb.cl/hydroPSO/reference/verification.md):

1\) ‘Verification-ModelOut.txt’: model outputs and goodness-of-fit
values for all verification parameter sets  

2\) ‘Verification-ParamValues.txt’: parameter values and goodness-of-fit
values for all verification parameter sets  

`plot_verification_results` reads the same files through
`read_verification_results` and then creates the subset of
[`plot_results`](http://mzb.cl/hydroPSO/reference/ReadPlot_results.md)
diagnostics that can be produced from verification output files:
parameter dotty plots, histograms, boxplots, optional pairs plots,
ECDFs, parameter trajectories, pseudo-3D parameter plots, and optional
model-output plots when observations are available.

## Value

`read_verification_results` returns a list with the following
components:

- `gofs`: numeric vector with the goodness-of-fit values corresponding
  to each parameter set,

- `model.values`: data frame with the model outputs corresponding to
  each parameter set,

- `best.gof`: numeric value with the best goodness-of-fit found during
  verification,

- `best.param`: numeric vector with the best parameter set found during
  verification, and

- `params`: data frame with the verification parameter sets.

`plot_verification_results` is invoked for its side effects. It produces
diagnostic plots on screen or saves them as PNG files when
`do.png = TRUE`. It does not return a structured object.

## Author

Mauricio Zambrano-Bigiarini, <mzb.devel@gmail.com>

## See also

[`verification`](http://mzb.cl/hydroPSO/reference/verification.md),
[`read_results`](http://mzb.cl/hydroPSO/reference/ReadPlot_results.md),
[`plot_results`](http://mzb.cl/hydroPSO/reference/ReadPlot_results.md),
[`read_out`](http://mzb.cl/hydroPSO/reference/ReadPlot_out.md),
[`plot_out`](http://mzb.cl/hydroPSO/reference/ReadPlot_out.md)

## Examples

``` r
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
#> x y 
#> 0 0 
```

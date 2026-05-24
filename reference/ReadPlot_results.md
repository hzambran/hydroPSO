# Read and Visualise hydroPSO Result Files

`read_results` reads the main output files produced by
[`hydroPSO`](http://mzb.cl/hydroPSO/reference/hydroPSO.md) and organises
them into a single list for post-processing. See Details section.

`plot_results` builds on those outputs to generate a comprehensive suite
of diagnostic plots for parameter values, goodness-of-fit evolution,
model simulations, and convergence behaviour. See Details section.

These functions are particularly useful in hydrology because they
streamline the interpretation of calibration results and their
uncertainty. They help identify behavioural parameter sets, inspect
equifinality, assess parameter sensitivity, compare simulated and
observed responses, and summarise optimisation performance across all
iterations.

## Usage

``` r
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
```

## Arguments

- drty.out:

  Character string with the path to the directory containing the
  hydroPSO output files. If only a directory name is provided, it is
  interpreted relative to the current working directory.

- param.names:

  Optional character vector with the names of the parameters to be
  plotted. By default, all parameters available in ‘Particles.txt’ are
  used, which are read from the ‘Particles.txt’ file using the
  [`read_particles`](http://mzb.cl/hydroPSO/reference/ReadPlot_particles.md)
  function. This argument is especially helpful when the user want to
  analyse only a subset of hydrologically relevant parameters, such as
  soil storage, recession coefficients, or routing parameters.

- gof.name:

  Character string used to label the goodness-of-fit metric in the
  plots, for example `"NSE"`, `"KGE"`, or `"RMSE"`.

- MinMax:

  Optional character string indicating whether the objective function is
  to be minimised or maximised. Valid values are `"min"` and `"max"`.
  When omitted, the value is inferred from ‘PSO_logfile.txt’. This
  argument affects how best and behavioural simulations are identified.

- beh.thr:

  Optional numeric threshold used to retain only behavioural solutions,
  that is, simulations with acceptable performance according to the
  selected objective function. This is useful for GLUE-style analyses
  and for focusing on hydrologically plausible parameter sets.

- modelout.cols:

  Optional numeric vector indicating which columns of ‘Model_out.txt’
  should be read or plotted, excluding the first three bookkeeping
  columns (iteration, particle, and goodness-of-fit). If `NULL`, all
  model output columns after the first three are used.

- nsim:

  Currently unused in the implementation, although it remains in the
  function signatures for backward compatibility.

- obs.tzone:

  Optional time zone used when reading sub-daily observations from
  ‘Observations.txt’. This is only relevant when the observed series has
  a time index and the time zone must be interpreted explicitly.

- verbose:

  Logical value indicating whether progress messages should be printed.

- beh.col:

  Colour used to draw the horizontal threshold line that separates
  behavioural from non-behavioural solutions in relevant plots.

- beh.lty:

  Line type used for the behavioural threshold line.

- beh.lwd:

  Line width used for the behavioural threshold line.

- nrows:

  Number of rows in multi-panel plotting layouts. If `"auto"`, the
  layout is computed automatically from the number of parameters to be
  displayed.

- col:

  Colour used for plotting points in the dotty plots.

- ylab:

  Label for the y-axis in the dotty plots. The default is `gof.name`.

- main:

  Optional main title passed to the relevant plotting routines.

- pch:

  Plotting symbol used for the dotty plots.

- cex:

  General expansion factor controlling the size of points and text in
  the plots.

- cex.main:

  Expansion factor for main titles.

- cex.axis:

  Expansion factor for axis annotations.

- cex.lab:

  Expansion factor for axis labels.

- breaks:

  Break specification for parameter histograms. Passed to
  [`hist`](https://rdrr.io/r/graphics/hist.html).

- freq:

  Logical value indicating whether histograms should display frequencies
  (`TRUE`) or densities (`FALSE`).

- do.pairs:

  Logical value indicating whether a parameter correlation matrix should
  also be plotted. This can be computationally expensive for large
  calibration experiments.

- weights:

  Optional numeric vector of weights used when computing empirical
  cumulative distribution functions of parameter values or simulated
  outputs. This is useful when different parameter sets have unequal
  importance, for example when weighting behavioural solutions.

- byrow:

  Logical value passed to ECDF-related routines. In `plot_results`,
  weighted ECDFs of model outputs are computed with `byrow = TRUE`
  internally when required.

- leg.cex:

  Expansion factor for legend text.

- dp3D.names:

  Names of the parameters to be used in the pseudo-3D dotty plots (See
  [`plot_NparOF`](http://mzb.cl/hydroPSO/reference/plot_NparOF.md)
  function). If `"auto"`, a subset of parameters is selected
  automatically.

- GOFcuts:

  Numeric vector defining the goodness-of-fit intervals used to colour
  the pseudo-3D dotty plots. If `"auto"`, these intervals are computed
  automatically.

- colorRamp:

  Function or vector defining the colour ramp used in the pseudo-3D
  dotty plots.

- alpha:

  Numeric value between 0 and 1 controlling colour transparency in the
  pseudo-3D dotty plots.

- points.cex:

  Point size used in the pseudo-3D dotty plots.

- ptype:

  Character string controlling how goodness-of-fit per particle is
  plotted. See
  [`plot_GofPerParticle`](http://mzb.cl/hydroPSO/reference/ReadPlot_GofPerParticle.md).

- ftype:

  Graphical type used when plotting observed and simulated time series.
  Passed to
  [`plot_out`](http://mzb.cl/hydroPSO/reference/ReadPlot_out.md) and,
  when relevant, to
  [`ggof`](https://hzambran.github.io/hydroGOF/reference/ggof.html).

- FUN:

  Summary function used by time-series plotting methods when aggregation
  is needed. Passed to
  [`plot_out`](http://mzb.cl/hydroPSO/reference/ReadPlot_out.md).

- quantiles.desired:

  Numeric vector with the simulation quantiles to be plotted. The
  default is `c(0.05, 0.5, 0.95)`, corresponding to the 5th, 50th, and
  95th percentiles. These are often useful to visualise uncertainty
  envelopes around simulated hydrographs.

- quantiles.labels:

  Character vector with labels for `quantiles.desired`.

- legend.pos:

  Legend position used in the convergence plots. Passed to
  [`plot_convergence`](http://mzb.cl/hydroPSO/reference/ReadPlot_convergence.md).

- do.png:

  Logical value indicating whether the plots created with this function
  should be saved as PNG files instead of being drawn only on the active
  graphics device.

- png.res:

  Resolution of all the output PNG figures, in pixels per inch.

- png.width:

  Width of the output PNG figures, in pixels. Only for used for the
  following figures:

  -) ‘Params_ValuesPerRun.png’ -) ‘Params_dp3d.png’ -)
  ‘Particles_GofPerIter.png’ -) ‘Velocities_ValuePerRun.png’ -)
  ‘ModelOut_BestSim_vs_Obs.png’ -) ‘ModelOut_Quantiles.png’ -)
  ‘ConvergenceMeasures.png’

- png.height:

  Height of the output PNG figures, in pixels. Only for used for the
  following figures:

  -) ‘Params_ValuesPerRun.png’ -) ‘Params_dp3d.png’ -)
  ‘Particles_GofPerIter.png’ -) ‘Velocities_ValuePerRun.png’ -)
  ‘ModelOut_BestSim_vs_Obs.png’ -) ‘ModelOut_Quantiles.png’ -)
  ‘ConvergenceMeasures.png’

- params.png.width:

  Width of the output PNG figures, in pixels. Only for used for the
  following figures:

  -) ‘Params_DottyPlots.png’ -) ‘Params_Histograms.png’ -)
  ‘Params_Boxplots.png’ -) ‘Params_ECDFs.png’ -) ‘Params_Pairs.png’

- params.png.height:

  Height of the output PNG figures, in pixels. Only for used for the
  following figures:

  -) ‘Params_DottyPlots.png’ -) ‘Params_Histograms.png’ -)
  ‘Params_Boxplots.png’ -) ‘Params_ECDFs.png’ -) ‘Params_Pairs.png’

- dotty.png.fname:

  Output filename for the parameter dotty plots.

- hist.png.fname:

  Output filename for the parameter histograms.

- bxp.png.fname:

  Output filename for the parameter boxplots.

- ecdf.png.fname:

  Output filename for the parameter ECDFs.

- pruns.png.fname:

  Output filename for the parameter trajectories across model
  evaluations.

- dp3d.png.fname:

  Output filename for the pseudo-3D parameter plots.

- pairs.png.fname:

  Output filename for the parameter correlation matrix.

- part.png.fname:

  Output filename for the goodness-of-fit by particle plot.

- vruns.png.fname:

  Output filename for the velocity evolution plot.

- modelout.best.png.fname:

  Output filename for the observed-versus-best-simulation comparison
  plot.

- modelout.quant.png.fname:

  Output filename for the ECDF or quantile-based model output plot.

- conv.png.fname:

  Output filename for the convergence diagnostics plot.

## Details

`read_results` reads information from the standard hydroPSO results
directory, including parameter values, particle velocities, model
outputs, convergence metrics, and goodness-of-fit values. In particular,
it reads the following output files of hydroPSO:  

1\) ‘BestParameterSet.txt’: best parameter set and its corresponding
goodness-of-fit found during the optimisation  

2\) ‘Particles.txt’: parameter values and their corresponding
goodness-of-fit value for all particles and iterations  

3\) ‘Velocities.txt’: velocity values and their corresponding
goodness-of-fit value for all particles and iterations  

4\) ‘Model_out.txt’: values of the objective function/model output for
each particle and iteration  

5\) ‘ConvergenceMeasures.txt’: convergence measures summarizing
performance of
[`hydroPSO`](http://mzb.cl/hydroPSO/reference/hydroPSO.md)  

6\) ‘Particles_GofPerIter.txt’: goodness-of-fit only for all the
particles during all the iterations  

`plot_results` is a high-level wrapper that calls several lower-level
plotting utilities in sequence. Depending on the available files and
arguments, it can produce:

The function `plot_results` is a high-level wrapper that calls several
lower-level plotting utilities in sequence. Depending on the available
files and arguments, it can produce: takes the outputs of the
`read_results` function and then produces the following plots:  

1\) Dotty plots of parameter values  

2\) Histograms of parameter values  

3\) Boxplots of parameter values  

4\) Correlation matrix among parameter values (optional)  

5\) Empirical CDFs of parameter values  

6\) Parameter values vs Number of Model Evaluations  

7\) (pseudo) 3D dotty plots of (selected) parameter values  

8\) GoF for each particle against Number of Model Evaluations  

9\) Velocity values vs Number of Model Evaluations  
10a) Scatterplot between Best Simulated values and Observations
(OPTIONAL, only if `MinMax` is provided)  

10b) Empirical CDFs for model's output (only produced if `obs` is NOT a
zoo object)  

10b) ggof (See
[`ggof`](https://hzambran.github.io/hydroGOF/reference/ggof.html))
between Best Simulated values and Observations (OPTIONAL, only if `obs`
is a zoo object)  

10d) Empirical CDFs for selected quantiles of model's output (OPTIONAL,
only if `obs` is a zoo object)  

11\) Convergence Measures (Gbest and normSwarmRadius) vs Iteration
Number.

## Value

`read_results` returns a list with the following components:

- `best.param`: numeric vector with the best parameter set found during
  the optimisation,

- `best.gof`: numeric value of the best objective-function score,

- `params`: data frame with all tested parameter sets,

- `gofs`: numeric vector with the goodness-of-fit values associated with
  `params`,

- `velocities`: matrix or data frame with particle velocities,

- `model.values`: model outputs read from ‘Model_out.txt’,

- `model.best`: best simulated output identified according to `MinMax`,

- `model.obs`: observed values used during calibration,

- `convergence.measures`: convergence diagnostics read from
  ‘ConvergenceMeasures.txt’, and

- `part.GofPerIter`: goodness-of-fit values for all particles across
  iterations.

`plot_results` is invoked for its side effects. It produces a sequence
of diagnostic plots on screen or saves them as PNG files when
`do.png = TRUE`. It does not return a structured object.

## Author

Mauricio Zambrano-Bigiarini

## See also

[`hydroPSO`](http://mzb.cl/hydroPSO/reference/hydroPSO.md),
[`read_best`](http://mzb.cl/hydroPSO/reference/read_best.md),
[`read_particles`](http://mzb.cl/hydroPSO/reference/ReadPlot_particles.md),
[`read_velocities`](http://mzb.cl/hydroPSO/reference/ReadPlot_particles.md),
[`read_out`](http://mzb.cl/hydroPSO/reference/ReadPlot_out.md),
[`read_convergence`](http://mzb.cl/hydroPSO/reference/ReadPlot_convergence.md),
[`read_GofPerParticle`](http://mzb.cl/hydroPSO/reference/ReadPlot_GofPerParticle.md),
[`plot_particles`](http://mzb.cl/hydroPSO/reference/ReadPlot_particles.md),
[`plot_out`](http://mzb.cl/hydroPSO/reference/ReadPlot_out.md),
[`plot_convergence`](http://mzb.cl/hydroPSO/reference/ReadPlot_convergence.md),
[`plot_NparOF`](http://mzb.cl/hydroPSO/reference/plot_NparOF.md),
[`params2ecdf`](http://mzb.cl/hydroPSO/reference/params2ecdf.md)

## Examples

``` r
# \donttest{
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
#>                                                                                 
#> ================================================================================
#> [                                Initialising  ...                             ]
#> ================================================================================
#>                                                                                 
#> [npart=40 ; maxit=1000 ; method=spso2011 ; topology=random ; boundary.wall=absorbing2011]
#>                                                                                 
#> ================================================================================
#> [ Writing the 'PSO_logfile.txt' file ...                                       ]
#> ================================================================================
#>                                                                                 
#> ================================================================================
#> [                                 Running  PSO ...                             ]
#> ================================================================================
#>                                                                                 
#> iter: 100  Gbest: 1.830E-03  Gbest_rate:  0.00%  Iter_best_fit: 2.020E-03  nSwarm_Radius: 1.38E-05  |g-mean(p)|/mean(p): 44.43%
#>                            
#> [ Writing output files... ]
#>                            
#>                                     |                                           
#> ================================================================================
#> [                          Creating the R output ...                           ]
#> ================================================================================
#> [ 'MinMax' was read from the 'PSO_logfile.txt' file, and set to 'min' ]
#> [                                               ]
#> [         Reading output files ...              ]
#> [                                               ]
#>                                                      
#> [ Reading the file 'Particles.txt' ... ]
#> [ Total number of parameter sets: 7840 ]
#>                                                      
#> [ Reading the file 'Velocities.txt' ... ]
#> [ Total number of parameter sets: 7840 ]
#>                                                      
#> [ Reading the file 'Model_out.txt' ... ]
#> [ Total number of parameter sets: 7840 ]
#> [ Number of model outputs for each parameter set ('nsim'): 1 ]
#>                                                      
#> [ Reading the file 'ConvergenceMeasures.txt' ... ]
#> [ Total number of iterations: 196 ]
#>                                                      
#> [ Reading the file 'Particles_GofPerIter.txt' ... ]
#> [ Number of particles : 40 ]
#> [ Number of iterations: 196 ]
#> [                                               ]
#> [         Reading output files ...              ]
#> [                                               ]
#>                                                      
#> [ Reading the file 'Particles.txt' ... ]
#> [ Total number of parameter sets: 7840 ]
#> [ Number of behavioural parameter sets: 6512 ]
#>                                                      
#> [ Reading the file 'Velocities.txt' ... ]
#> [ Total number of parameter sets: 7840 ]
#> [ Number of behavioural parameter sets: 6512 ]
#>                                                      
#> [ Reading the file 'Model_out.txt' ... ]
#> [ Total number of parameter sets: 7840 ]
#> [ Number of model outputs for each parameter set ('nsim'): 1 ]
#> [ Number of behavioural model outputs           :  ]
#>                                                      
#> [ Reading the file 'ConvergenceMeasures.txt' ... ]
#> [ Total number of iterations: 196 ]
#> [ Number of iterations with Gbest <= 5: 174 ]
#>                                                      
#> [ Reading the file 'Particles_GofPerIter.txt' ... ]
#> [ Number of particles : 40 ]
#> [ Number of iterations: 196 ]
#> [                                               ]
#> [                  Plotting ...                 ]
#> [                                               ]
#> [ Plotting convergence measures' ... ]
#>                                                      
#> [ Plotting dotty plots for parameter values' ... ]
#> [ Plotting histograms for parameter values' ... ]
#> [ Plotting boxplots for parameter values' ... ]
#> [ Plotting empirical CDFs for parameter values' ... ]
#> [ Plotting parameter values vs Number of Model Evaluations' ... ]
#> [ Plotting 3D dotty plots for parameter values' ... ]
#> [ Plotting GoF for each particle vs Number of Model Evaluations' ... ]
#> [ Plotting velocity values vs Number of Model Evaluations' ...]
#> [ Plotting ECDFs of simulated quantiles vs observations' ... ]
#> [ Computing the ECDF for 'sim' , 1/1 => 100.00% ]
#> [                                               ]
#> [             Plots are finished !!             ]
#> [                                               ]
# } # donttest END

# \donttest{
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
#>                                                                                 
#> ================================================================================
#> [                                Initialising  ...                             ]
#> ================================================================================
#>                                                                                 
#> [npart=16 ; maxit=1000 ; method=spso2007 ; topology=random ; boundary.wall=absorbing2007]
#>          
#> [ user-definitions in control: MinMax=min ; maxit=1000 ; npart=16 ; c1=1.19314718055995 ; c2=1.19314718055995 ; use.IW=TRUE ; IW.w=0.721347520444482 ; topology=random ; lambda=1 ; K=3 ; Xini.type=random ; Vini.type=random2007 ; best.update=sync ; boundary.wall=absorbing2007 ; write2disk=TRUE ; plot=FALSE ; REPORT=100 ; abstol=1e-20 ; reltol=1e-20 ]
#>          
#>                                                                                 
#> ================================================================================
#> [ Writing the 'PSO_logfile.txt' file ...                                       ]
#> ================================================================================
#>                                                                                 
#> ================================================================================
#> [                                 Running  PSO ...                             ]
#> ================================================================================
#>                                                                                 
#> iter: 100  Gbest: 1.702E-01  Gbest_rate:  8.03%  Iter_best_fit: 1.702E-01  nSwarm_Radius: 7.27E-04  |g-mean(p)|/mean(p): 31.62%
#> iter: 200  Gbest: 5.787E-07  Gbest_rate:  0.00%  Iter_best_fit: 7.553E-07  nSwarm_Radius: 1.99E-06  |g-mean(p)|/mean(p): 53.66%
#> iter: 300  Gbest: 7.541E-12  Gbest_rate:  0.00%  Iter_best_fit: 7.655E-12  nSwarm_Radius: 5.31E-09  |g-mean(p)|/mean(p): 43.01%
#> iter: 400  Gbest: 7.091E-17  Gbest_rate: 46.15%  Iter_best_fit: 7.091E-17  nSwarm_Radius: 1.82E-11  |g-mean(p)|/mean(p): 58.88%
#>                            
#> [ Writing output files... ]
#>                            
#>                                     |                                           
#> ================================================================================
#> [                          Creating the R output ...                           ]
#> ================================================================================
#> [                                               ]
#> [         Reading output files ...              ]
#> [                                               ]
#>                                                      
#> [ Reading the file 'Particles.txt' ... ]
#> [ Total number of parameter sets: 7216 ]
#>                                                      
#> [ Reading the file 'Velocities.txt' ... ]
#> [ Total number of parameter sets: 7216 ]
#>                                                      
#> [ Reading the file 'Model_out.txt' ... ]
#> [ Total number of parameter sets: 7216 ]
#> [ Number of model outputs for each parameter set ('nsim'): 1 ]
#>                                                      
#> [ Reading the file 'ConvergenceMeasures.txt' ... ]
#> [ Total number of iterations: 451 ]
#>                                                      
#> [ Reading the file 'Particles_GofPerIter.txt' ... ]
#> [ Number of particles : 16 ]
#> [ Number of iterations: 451 ]
#> [                                               ]
#> [                  Plotting ...                 ]
#> [                                               ]
#> [ Plotting convergence measures' ... ]
#>                                                      
#> [ Plotting dotty plots for parameter values' ... ]
#> [ Plotting histograms for parameter values' ... ]
#> [ Plotting boxplots for parameter values' ... ]
#> [ Plotting empirical CDFs for parameter values' ... ]
#> [ Plotting parameter values vs Number of Model Evaluations' ... ]
#> [ Plotting 3D dotty plots for parameter values' ... ]
#> [ Plotting GoF for each particle vs Number of Model Evaluations' ... ]
#> [ Plotting velocity values vs Number of Model Evaluations' ...]
#> [ Plotting ECDFs of simulated quantiles vs observations' ... ]
#> [ Computing the ECDF for 'sim' , 1/1 => 100.00% ]
#> [                                               ]
#> [             Plots are finished !!             ]
#> [                                               ]
#>                                                                                 
#> ================================================================================
#> [                                Initialising  ...                             ]
#> ================================================================================
#>                                                                                 
#> [npart=40 ; maxit=1000 ; method=spso2011 ; topology=random ; boundary.wall=absorbing2011]
#>          
#> [ user-definitions in control: MinMax=min ; maxit=1000 ; npart=40 ; c1=2.05 ; c2=2.05 ; use.IW=FALSE ; use.CF=TRUE ; topology=random ; K=11 ; use.TVlambda=TRUE ; TVlambda.rng=c(1, 0.5) ; Xini.type=lhs ; Vini.type=lhs2011 ; best.update=sync ; boundary.wall=absorbing2011 ; write2disk=FALSE ; plot=FALSE ; REPORT=100 ; abstol=1e-20 ; reltol=1e-20 ]
#>          
#>                                                                                 
#> ================================================================================
#> [                                 Running  PSO ...                             ]
#> ================================================================================
#>                                                                                 
#> iter: 100  Gbest: 7.103E-03  Gbest_rate: 19.09%  Iter_best_fit: 7.103E-03  nSwarm_Radius: 2.41E-04  |g-mean(p)|/mean(p): 74.77%
#> iter: 200  Gbest: 3.635E-09  Gbest_rate: 55.82%  Iter_best_fit: 3.635E-09  nSwarm_Radius: 2.28E-07  |g-mean(p)|/mean(p): 81.16%
#> iter: 300  Gbest: 2.164E-15  Gbest_rate:  2.66%  Iter_best_fit: 2.164E-15  nSwarm_Radius: 1.41E-10  |g-mean(p)|/mean(p): 74.83%
#>                                     |                                           
#> ================================================================================
#> [                          Creating the R output ...                           ]
#> ================================================================================
#> $par
#>        Param1        Param2        Param3        Param4        Param5 
#> -7.966961e-11  4.558953e-11  9.212705e-11 -2.043979e-10  5.727084e-11 
#>        Param6        Param7        Param8        Param9       Param10 
#> -9.775772e-11 -3.226021e-10  7.510675e-11  2.872612e-10  1.039172e-10 
#> 
#> $value
#> [1] 2.74559e-19
#> 
#> $best.particle
#> [1] 37
#> 
#> $counts
#> function.calls     iterations    regroupings 
#>          14120            353              0 
#> 
#> $convergence
#> [1] 1
#> 
#> $message
#> [1] "Converged ('reltol' criterion)"
#> 
# } # donttest END
```

# hydromod - Definition and execution of the model to be calibrated/optimised using an executable file that runs in the system console

It runs a user-defined model to be calibrated/optimised and returns a
goodness-of-fit value as measure of model performance, by comparing
observations against simulated equivalents.  

This function was designed to run an executable file from the system
console, not a model implemented as an R function.

## Usage

``` r
hydromod(param.values, param.files="ParamFiles.txt", 
         param.ranges="ParamRanges.txt",
         model.drty = getwd(), 
         exe.fname, exe.args= character(), stdout= FALSE, stderr="", 
         verbose = FALSE, out.FUN, out.FUN.args, gof.FUN, gof.FUN.args=list(), 
         gof.Ini, gof.Fin, date.fmt = "%Y-%m-%d", obs,
         do.png=FALSE, png.fname, width = 1200, height = 600, res=90,
         main, leg.cex=1.2, tick.tstep= "auto", lab.tstep= "auto", 
         lab.fmt=NULL)
```

## Arguments

- param.values:

  numeric vector, a single parameter set used to run the model specified
  in `exe.fname`.

- param.files:

  character, file name (with full path) storing locations and names of
  the files that have to be modified for each parameter. By default
  `param.files="ParamFiles.txt"`.

- param.ranges:

  character, file name (with full path) storing the ranges (maximum and
  minum values) of the parameters to be used in the optimisation. By
  default `param.files="ParamRanges.txt"`.

- model.drty:

  character, path to the executable file of the model specified in
  `exe.fname`. ALL the files required to run the model have to be
  located within this directory (input files for the model may be
  located in a different directory, if properly referenced).

- exe.fname:

  character, model command line arguments to be entered through a
  prompted string to execute the user-defined model.

- exe.args:

  character, with optional arguments to be passed in the command line to
  the user-defined model.

- stdout, stderr:

  where output to ‘stdout’ or ‘stderr’ should be sent. Possible values
  are `FALSE` (discard output, the default), `""`, to the R console. See
  [`system2`](https://rdrr.io/r/base/system2.html).  

  By default `stdout=FALSE` and any message printed by the model code to
  the screen will be omitted. This setting is recommended when
  calibrating the model with
  [`hydroPSO`](http://mzb.cl/hydroPSO/reference/hydroPSO.md). However,
  when trying to run the model code with `hydromod` by the first time,
  it is recommend to set `stdout=""`, in order to detect if the model
  was properly executed or not.  

  By default `stderr=""` and any error message of the model code will be
  printed to the screen.

- verbose:

  logical; if TRUE, progress messages are printed to the screen.  

  If `verbose=TRUE`, the following messages will appear: i) parameter
  values for each particle; (ii) model execution; iii) extraction of
  simulated values; and iv) computation of the goodness-of-fit measure.

- out.FUN:

  character, name of a valid R function to read the model outputs and
  transform them into a (zoo) object to be compared against `obs` (e.g.,
  see [`read.table`](https://rdrr.io/r/utils/read.table.html),
  [`read.csv`](https://rdrr.io/r/utils/read.table.html).

- out.FUN.args:

  list, arguments to be passed to `out.FUN`.

- gof.FUN:

  character, name of a valid (goodness-of-fit) R function to obtain
  model performance (e.g., see
  [`NSE`](https://hzambran.github.io/hydroGOF/reference/NSE.html),
  [`rmse`](https://hzambran.github.io/hydroGOF/reference/rmse.html),
  etc).  

  It MUST HAVE -at least- the following two arguments in its
  definition:  

  -) sim: numeric with the value(s) simulated by the model specified in
  `exe.fname`  
  -) obs: numeric with the observation(s) used to compute model's
  performance by comparison against sim

- gof.FUN.args:

  list, arguments additional to sim and obs that need to be passed to
  `gof.FUN` (e.g., see j argument in
  [`mNSE`](https://hzambran.github.io/hydroGOF/reference/mNSE.html))

- gof.Ini:

  OPTIONAL. Character with the starting date used in the goodness-of-fit
  function.  

  It is used to subset `obs` (if necessary), AND to define the time
  period to compare simulated with observed values

- gof.Fin:

  OPTIONAL. Character with the ending date used in the goodness-of-fit
  function.  

  It is used to subset `obs` (if necessary), AND to define the time
  period to compare simulated with observed values

- date.fmt:

  character, format in which the dates are stored in `Sim.Ini`,
  `Sim.Fin`, `gof.Ini`, `gof.Fin`, e.g. %Y-%m-%d. See `format` in
  [`as.Date`](https://rdrr.io/r/base/as.Date.html)

- obs:

  (zoo) object with the observed values

- do.png:

  logical indicating whether a PNG image with the comparison between
  `obs` and the best simulated values has to be created.  

  If the hydroGOF package is available, the plot is produced with the
  [`ggof`](https://hzambran.github.io/hydroGOF/reference/ggof.html)
  function. A correlation plot is produced otherwise with the
  [`plot_out`](http://mzb.cl/hydroPSO/reference/ReadPlot_out.md)
  function

- png.fname:

  OPTIONAL. Used only when `do.png=TRUE`.  

  Name of the PNG file to be created within the `model.drty` directory.
  The default value is ‘Obs_vs_Sim.png’

- width:

  OPTIONAL. Used only when `do.png=TRUE`.  

  numeric, width of the output PNG image

- height:

  OPTIONAL. Used only when `do.png=TRUE`.  

  numeric, height of the output PNG image

- res:

  OPTIONAL. Used only when `do.png=TRUE`.  

  numeric, resolution of the output PNG image

- main:

  OPTIONAL. Used only when `do.png=TRUE`.  

  character, representing the main title of the plot comparing observed
  and simulated values

- leg.cex:

  See [`ggof`](https://hzambran.github.io/hydroGOF/reference/ggof.html)

- tick.tstep:

  See [`ggof`](https://hzambran.github.io/hydroGOF/reference/ggof.html)

- lab.tstep:

  See [`ggof`](https://hzambran.github.io/hydroGOF/reference/ggof.html)

- lab.fmt:

  See [`ggof`](https://hzambran.github.io/hydroGOF/reference/ggof.html)

## Value

A list of two elements:

- sim:

  numeric, with the simulated values obtained by running the model

.  

- GoF:

  numeric, goodness-of-fit value representing how close each one of the
  simulated values in `sim` are to their observed counterparts, by using
  the USER-DEFINED `gof.FUN` function

.

## Author

Mauricio Zambrano-Bigiarini, <mzb.devel@gmail.com>

## See also

[`hydroPSO`](http://mzb.cl/hydroPSO/reference/hydroPSO.md),
[`hydromodInR`](http://mzb.cl/hydroPSO/reference/hydromodInR.md)

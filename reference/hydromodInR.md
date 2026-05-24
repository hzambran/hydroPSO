# hydromodInR

This function runs an R-based model using a single parameter set taken
from a matrix/data.frame with many parameter sets.

This function is for internal use within the package, and it is only
documented in case it be useful to other packages. See Details section
for more information.

## Usage

``` r
hydromodInR(part, Particles, model.FUN, model.FUN.args )
```

## Arguments

- part:

  numeric, a single integer value indicating the row number in
  `Particles` which is used to select the parameter set used to run the
  model specified in `model.FUN`.

- Particles:

  matrix/data.frame with all the parameter sets that can be used to run
  the model defined in `model.FUN`.

  It has as many columns as parameters used by the model, and as many
  rows as parameter sets provided by the user in `Particles`.

- model.FUN:

  character, valid R function representing the model code to be
  calibrated/optimised.

- model.FUN.args:

  list with the arguments to be passed to `model.FUN`.

## Details

This function takes an R-based user-defined model, a matrix/data.frame
with many parameter sets (e.g., randomly generated or the output of a
previous optimisation/calibration), and index indicating which specific
parameter set must be used from the matrix/data.frame, and then runs the
model and returns a goodness-of-fit value as measure of model
performance, by comparing observations against simulated equivalents.  

This function it was designed to run a model implemented as an R
function, not an executable file that runs in the system console.

## Value

This function MUST return a list with two elements:

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
[`hydromod`](http://mzb.cl/hydroPSO/reference/hydromod.md)

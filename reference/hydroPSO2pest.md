# Export hydroPSO input files to PEST

This function exports the content of the hydroPSO input files
‘ParamRanges.txt’ and ‘ParamFiles.txt’ to PEST, into a single ‘.pst’
files with corresponding ‘.tpl’ and ‘.ins’ files

## Usage

``` r
hydroPSO2pest(param.files="ParamFiles.txt", param.ranges="ParamRanges.txt",
              observations.fname="Observations.txt", exe.fname, 
              drty.model=getwd(), pst.fname="hydroPSO2PEST.pst", verbose=TRUE)
```

## Arguments

- param.files:

  character, name (full path) of the hydroPSO input file storing the
  location and names of the model files that have to be modified for
  each parameter subject to calibration.  
  By default this file is called ‘ParamFiles.txt’ and -if the full path
  it is not specified- it is searched for within the ‘PSO.in’
  subdirectory of `drty.model`

- param.ranges:

  character, name (full path) of the hydroPSO input file defining the
  minimum and maximum boundary values for each one of the parameters to
  be calibrated  
  By default this file is called ‘ParamRanges.txt’ and -if the full path
  it is not specified- it is searched for within the ‘PSO.in’
  subdirectory of `drty.model`

- observations.fname:

  character name (full path) of the hydroPSO output file storing the
  observed values used during the optimisation.  
  By default this file is called ‘Observations.txt’ and -if the full
  path it is not specified- it is searched for within the ‘PSO.out’
  subdirectory of `drty.model`

- exe.fname:

  character, model command line arguments to be entered through a
  prompted string to execute the user-defined model

- drty.model:

  character, path to the executable file of the model specified in
  `exe.fname`. ALL the files required to run the model have to be
  located within this directory (however, input files may be located
  elsewhere)

- pst.fname:

  character, with the name of the output ‘.pst’ file

- verbose:

  logical, indicates if progress messages are to be printed. By default
  `verbose=TRUE`

## Value

A single text textfilet mo be used as input file by PEST. The name of
the output text file is `pst.fname` and it is located within
the`drty.model` directory.

## References

'PEST' refers to a software package and to a number of suites of utility
programs that support it. Collectively, these are essential tools in
decision-support environmental modelling. More information on
<https://pesthomepage.org/>.

## Author

Mauricio Zambrano-Bigiarini, <mzb.devel@gmail.com>

## See also

[`pest2hydroPSO`](http://mzb.cl/hydroPSO/reference/pest2hydroPSO.md),
[`hydroPSO`](http://mzb.cl/hydroPSO/reference/hydroPSO.md)

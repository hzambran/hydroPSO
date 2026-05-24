# Import PEST input files to hydroPSO

This function imports the PEST input files (a master ‘.pst’ and its
corresponding ‘.tpl’ and ‘.ins’) into
[`hydroPSO`](http://mzb.cl/hydroPSO/reference/hydroPSO.md)
(‘ParamRanges.txt’ and ‘ParamFiles.txt’)

## Usage

``` r
pest2hydroPSO(pst.fname, drty.pest=NULL, drty.model=NULL, drty.out="PSO.in",
              param.files="ParamFiles.txt", param.ranges="ParamRanges.txt",
              decimals=5, verbose=TRUE)
```

## Arguments

- pst.fname:

  character, with name of the PEST input file (‘.pst’), which contains
  all the information regarding parameters, observations and template
  files (‘.tpl’ and ‘.ins’) used by PEST.

- drty.pest:

  character, path to the executable file of PEST. ALL the files required
  to run PEST with the model have to be located within this directory
  (‘.tpl’ and ‘.ins’).  

  Default value is `NULL`, which assigns to `drty.pest` the parent
  directory of `pst.fname`.

- drty.model:

  character, path to the executable file of the model specified in
  `exe.fname`. ALL the files required to run the model have to be
  located within this directory  

  Default value is `NULL`, which assigns to `drty.pest` the parent
  directory of `pst.fname`.

- drty.out:

  character, name of the directory that will store all the output files
  produced by this function  

  Default value is ‘PSO.in’, which creteas a directory called ‘PSO.in’
  within the parent directory of `pst.fname`.

- param.files:

  character, name of the output file that will store the location and
  names of the model files that have to be modified for each parameter
  subject to calibration with hydroPSO.  

  By default this file is called ‘ParamFiles.txt’ and -if the full path
  it is not specified- it is searched for within the ‘PSO.in’
  subdirectory of `drty.model`.

- param.ranges:

  character, name of the output file defining the minimum and maximum
  boundary values for each one of the parameters to be calibrated with
  hydroPSO.  
  By default this file is called ‘ParamRanges.txt’ and -if the full path
  it is not specified- it is searched for within the ‘PSO.in’
  subdirectory of `drty.model`.

- decimals:

  character, model command line arguments to be entered through a
  prompted string to execute the user-defined model.

- verbose:

  logical, indicates if progress messages are to be printed. By default
  `verbose=TRUE`.

## Value

Two input files for
[`hydroPSO`](http://mzb.cl/hydroPSO/reference/hydroPSO.md):

- param.files:

  plain text file with the location and names of the model files that
  have to be modified for each parameter subject to calibration with
  hydroPSO

- param.ranges:

  plain text file defining the minimum and maximum boundary values for
  each one of the parameters to be calibrated with hydroPSO

## Author

Mauricio Zambrano-Bigiarini, <mzb.devel@gmail.com>

## See also

[`hydroPSO2pest`](http://mzb.cl/hydroPSO/reference/hydroPSO2pest.md),
[`hydroPSO`](http://mzb.cl/hydroPSO/reference/hydroPSO.md)

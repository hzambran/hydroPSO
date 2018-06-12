# hydroPSO
[![Research software impact](http://depsy.org/api/package/cran/hydroPSO/badge.svg)](http://depsy.org/package/r/hydroPSO) [![Build Status](https://travis-ci.org/hzambran/hydroPSO.svg?branch=master)](https://travis-ci.org/hzambran/hydroPSO)

hydroPSO is a global optimisation R package implementing a state-of-the-art version of the Particle Swarm Optimisation (PSO) algorithm (SPSO-2011 and SPSO-2007 capable), with a special focus on the calibration of environmental models. 

hydroPSO is parallel-capable, to alleviate the computational burden of complex models.

hydroPSO is model-independent, allowing the user to easily interface any model code with the calibration engine (PSO), and includes a series of controlling options and PSO variants to fine-tune the performance of the optimisation engine. An advanced sensitivity analysis function together with user-friendly plotting summaries facilitate the interpretation and assessment of the calibration results. 

Bugs / comments / questions / collaboration of any kind are very welcomed.


## Articles using `hydroPSO`
  
| Year | Journal |  Model(s) / Application | Article |
| ---- | ------- | ----------------------- | --------|
| 2013 | EMS     | SWAT-2005, MODFLOW-2005 | [A model-independent Particle Swarm Optimisation software for model calibration](http://dx.doi.org/10.1016/j.envsoft.2013.01.004) |
| 2013 | IEEE    | Benchmark functions     | [Standard Particle Swarm Optimisation 2011 at CEC-2013: A baseline for future PSO improvements](http://ieeexplore.ieee.org/xpl/articleDetails.jsp?arnumber=6557848) |
| 2013 | JoH     | LISFLOOD                | [Hydrological evaluation of satellite-based rainfall estimates over the Volta and Baro-Akobo Basin](http://www.sciencedirect.com/science/article/pii/S0022169413005295) |
| 2014 | JCH     | MODFLOW2005-MT3DMS      | [Particle Swarm Optimization for inverse modeling of solute transport in fractured gneiss aquifer](http://www.sciencedirect.com/science/article/pii/S0169772214000862) |
| 2014 | JRSE    | SWAT                    | [SWAT model parameter calibration and uncertainty analysis using the hydroPSO R package in Nzoia Basin, Kenya](http://41.204.187.24:8080/bitstream/handle/123456789/2286/SWAT%20model%20parameter%20calibration%20and%20uncertainty.pdf?sequence=1&isAllowed=y) |
| 2014 | GMD     | WALRUS                  | [The Wageningen Lowland Runoff Simulator (WALRUS): a lumped rainfall-runoff model for catchments with shallow groundwater](https://doi.org/10.5194/gmd-7-2313-2014) |
| 2014 | HESS    | WALRUS                  | [The Wageningen Lowland Runoff Simulator (WALRUS): application to the Hupsel Brook catchment and the Cabauw polder](https://doi.org/10.5194/hess-18-4007-2014) |
| 2014 | HP      | Travel time distributions | [Consequences of mixing assumptions for time‐variable travel time distributions](https://doi.org/10.1002/hyp.10372) |
| 2015 | HP      | HBV                     | [A coupled hydrology-biogeochemistry model to simulate dissolved organic carbon exports from a permafrost‐influenced catchment](https://doi.org/10.1002/hyp.10566) |
| 2015 | HESS    | LISFLOOD                | [Global warming increases the frequency of river floods in Europe](https://doi.org/10.5194/hess-19-2247-2015) |
| 2015 | HESS    | LISFLOOD                | [A pan-African medium-range ensemble flood forecast system](https://doi.org/10.5194/hess-19-3365-2015) |
| 2015 | EE      | MARS-based              | [Hybrid PSO-MARS-based model for forecasting a successful growth cycle of the Spirulina platensis from experimental data in open raceway ponds](https://doi.org/10.1016/j.ecoleng.2015.04.064) |
| 2015 | MJ      | Malaria transmission    | [Predicting the impact of border control on malaria transmission: a simulated focal screen and treat campaign](https://doi.org/10.1186/s12936-015-0776-2) |
| 2016 | SC      | Stock Market            | [Natural combination to trade in the stock market](https://doi.org/10.1007/s00500-015-1652-2) |
| 2016 | EMS     | SWAT-VSA                | [Coupling the short-term global forecast system weather data with a variable source area hydrologic model](https://doi.org/10.1016/j.envsoft.2016.09.008) |
| 2016 | JoH-RS  | LISFLOOD                | [Assessing the role of uncertain precipitation estimates on the robustness of hydrological model parameters under highly variable climate conditions](https://doi.org/10.1016/j.ejrh.2016.09.003) |
| 2016 | NHESS   | LISFLOOD                | [Modelling the socio-economic impact of river floods in Europe](https://doi.org/10.5194/nhess-16-1401-2016) |
| 2017 | EP      | WALRUS-paddy+PDP        | [Hydrology and phosphorus transport simulation in a lowland polder by a coupled modeling system](https://doi.org/10.1016/j.envpol.2016.09.093) |
| 2017 | HP      | SWAT                    | [The value of remotely sensed surface soil moisture for model calibration using SWAT](https://doi.org/10.1002/hyp.11219) |
| 2017 | IS:CLS  | Genetics                | [Reconstructing Genetic Regulatory Networks Using Two-Step Algorithms with the Differential Equation Models of Neural Networks](https://doi.org/10.1007/s12539-017-0254-3) |
| 2017 | Bioener.| EPIC                    | [The greenhouse gas intensity and potential biofuel production capacity of maize stover harvest in the US Midwest](https://doi.org/10.1111/gcbb.12473) |
| 2017 | Sustain.| SWAT, GSWAT | [Development of an Evapotranspiration Data Assimilation Technique for Streamflow Estimates: A Case Study in a Semi-Arid Region](https://doi.org/10.3390/su9101658) |
| 2017 | CSR     | Clustering colors       | [Clustering colors](https://doi.org/10.1016/j.cogsys.2017.05.004) |
| 2017 | PLoS ONE| Partitioning of color space | [Does optimal partitioning of color space account for universal color categorization?](https://doi.org/10.1371/journal.pone.0178083) |
| 2017 | HESS    | Isotope analysis        | [Pesticide fate on catchment scale: conceptual modelling of stream CSIA data](https://doi.org/10.5194/hess-21-5243-2017) |
| 2017 | HESS (in review) | Dissolved organic carbon | [Hydrological control of dissolved organic carbon dynamics in a rehabilitated Sphagnum-dominated peatland: a water-table based modelling approach](https://doi.org/10.5194/hess-2017-578) |
| 2018 | Antrop. | WALRUS                  | [Hydrologic impacts of changing land use and climate in the Veneto lowlands of Italy](https://doi.org/10.1016/j.ancene.2018.04.001) |
| 2018 | JoH     | Soil moisture model (in R) | [Can next-generation soil data products improve soil moisture modelling at the continental scale? An assessment using a new microclimate package for the R programming environment](https://doi.org/10.1016/j.jhydrol.2018.04.040) |
| 2018 | AWM     | SWAT                    | [Assessing the impact of the MRBI program in a data limited Arkansas watershed using the SWAT model](https://doi.org/10.1016/j.agwat.2018.02.012) |
| 2018 | EMA     | Air quality             | [Air Quality Modeling Using the PSO-SVM-Based Approach, MLP Neural Network, and M5 Model Tree in the Metropolitan Area of Oviedo (Northern Spain)](https://doi.org/10.1007/s10666-017-9578-y) |



## Installation
Installing the latest stable version from [CRAN](https://CRAN.R-project.org/package=hydroPSO):
```{r}
install.packages("hydroPSO")
```

Alternatively, you can also try the under-development version from [Github](https://github.com/hzambran/hydroPSO):
```{r}
if (!require(devtools)) install.packages("devtools")
library(devtools)
install_github("hzambran/hydroPSO")
```

## Reporting bugs, requesting new features

If you find an error in some function, or want to report a typo in the documentation, or to request a new feature (and wish it be implemented :) you can do it [here](https://github.com/hzambran/hydroPSO/issues)


## Citation 
```{r}
citation("hydroPSO")
```

To cite hydroPSO in publications use:

> Zambrano-Bigiarini, M. and Rojas, R. (2013). [A model-independent Particle Swarm Optimisation software for model calibration](https://doi.org/10.1016/j.envsoft.2013.01.004), Environmental Modelling & Software, 43, 5-25, doi:10.1016/j.envsoft.2013.01.004.

> Zambrano-Bigiarini, M. and Rojas, R. (2018). [hydroPSO: Particle Swarm Optimisation, with Focus on Environmental Models](https://cran.r-project.org/package=hydroPSO). R package version 0.4-1. URL https://cran.r-project.org/package=hydroPSO. DOI:10.5281/zenodo.1287350.


BibTeX entries for LaTeX users are

>  @Article{Zambrano-BigiariniRojas2013-hydroPSO_article,
>      title = {A model-independent Particle Swarm Optimisation software for model calibration},
>      journal = {Environmental Modelling \& Software},
>      author = {{Zambrano-Bigiarini}, {M.} and {Rojas}, {R.}},
>      volume = {43},
>      pages = {5-25},
>      year = {2013},
>      doi = {10.1016/j.envsoft.2013.01.004},
>      url = {https://doi.org/10.1016/j.envsoft.2013.01.004},
>    }


>  @Manual{Zambrano-BigiariniRojas-hydroPSO_pkg,
>      title = {hydroPSO: Particle Swarm Optimisation, with Focus on Environmental Models},
>      author = {Mauricio Zambrano-Bigiarini and Rodrigo Rojas},
>      year = {2018},
>      note = {{R} package version 0.4-0. doi:10.5281/zenodo.1287350},
>      url = {https://CRAN.R-project.org/package=hydroPSO},

## Vignette 
[Here](http://www.rforge.net/hydroPSO/files/hydroPSO_vignette.pdf) you can find a vignette showing how to use hydroPSO to calibrate parameters of SWAT-2005 and MODFLOW-2005.A similar approach can be used to calibrate SWAT-2012 or other models that need to be run from the system console.

* The file [MF2005.zip](http://www.rforge.net/hydroPSO/files/MF2005.zip), with all the necessary files to run the MODFLOW-2005 examples in the vignette, contains **3 Windows binary files**: `mf2005.exe`, `preproc.exe` and `zonbud_hydroPSO.exe`. These binary files are distributed in the hope that they will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  In no event shall the authors be liable for any CLAIM, DAMAGES or other LIABILITY, whether in an action of contract, tort or otherwise, arising from, out of or in connection with the software or the use or other dealings in the software. 

* The file [SWAT2005.zip](http://www.rforge.net/hydroPSO/files/SWAT2005.zip) , with all the necessary files to run the SWAT-2005 examples in the vignette, contains **1 Windows binary file**: `swat2005.exe` and  **1 UNIX binary file**: `swat2005.out`. Those binary files are distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  In no event shall the authors be liable for any CLAIM, DAMAGES or other LIABILITY, whether in an action of contract, tort or otherwise, arising from, out of or in connection with the software or the use or other dealings in the software. 


## Related Material 

* *hydroPSO: A Versatile Particle Swarm Optimisation R Package for Calibration of Environmental Models* (**EGU-2012**) [abstract](http://meetingorganizer.copernicus.org/EGU2012/EGU2012-10950-2.pdf), [poster](http://www.slideshare.net/hzambran/egu2012-10950hydro-pso4web).

* *R: a statistical environment for hydrological analysis* (**EGU-2010**) [abstract](http://meetingorganizer.copernicus.org/EGU2010/EGU2010-13008.pdf), [poster](http://www.slideshare.net/hzambran/egu2010-ra-statisticalenvironmentfordoinghydrologicalanalysis-9095709).



## See Also 

* [hydroGOF: Goodness-of-fit functions for comparison of simulated and observed hydrological time series](https://github.com/hzambran/hydroGOF).

* [hydroTSM: Time Series Management, analysis and interpolation for hydrological modelling](https://github.com/hzambran/hydroTSM).


## Feedback 
[Give us your opinion !](http://www.surveyexpression.com/Survey.aspx?id=dc5b906f-2bc7-400c-b8ea-23a4420a724f).

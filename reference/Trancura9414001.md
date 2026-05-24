# Hydrometeorological time series for Trancura antes de Llafenco basin

Daily time series of precipitation, mean air temperature, potential
evapotranspiration and streamflows for the catchment draining into the
'Trancura antes de Llafenco' streamflow station (Cod.BNA: 9414001,
drainage area= 1415 km2), Araucania Region, Chile (Lat:-39.3333,
Lon:-71.6667), with data from 01/Jan/1979 to 31/Dec/2016 (including some
gaps).

## Usage

``` r
data(Trancura9414001)
```

## Format

data.frame with 5 columns:  
-) `Date`: character with the date (YYYY-MM-DD) for each daily
observation.  

-) `P_mm`: Spatially-averaged mean daily values of precipitation
computed based on the CR2met dataset, \[mm/day\].  

-) `Tmean_degC`: Spatially-averaged mean daily values of air temperature
computed based on the CR2met dataset, \[degree Celsius\].  

-) `PET_mm`: Spatially-averaged mean daily values of precipitation
computed based on the Hargreaves-Samani equation and daily maximum and
minimum air temperatures obtained from the CR2met dataset, \[mm/day\].  

-) `Qobs_m3s`: Daily sreamflows measured at the Trancura antes de
Llafenco (9414001) station.  

## Source

Provided by Center for Climate and Resilience Research, Universidad de
Chile, Santiago, Chile (<https://www.cr2.cl/>, last accessed \[Feb
2020\]).  

These data are intended to be used for research purposes only, being
distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY.

## References

Alvarez-Garreton, C., Mendoza, P. A., Boisier, J. P., Addor, N.,
Galleguillos, M., Zambrano-Bigiarini, M., Lara, A., Puelma, C., Cortes,
G., Garreaud, R., McPhee, J., and Ayala, A (2018). The CAMELS-CL
dataset: catchment attributes and meteorology for large sample
studies-Chile dataset. Hydrology and Earth System Sciences, 22(11),
5817-5846.
[doi:10.5194/hess-22-5817-2018](https://doi.org/10.5194/hess-22-5817-2018)
.

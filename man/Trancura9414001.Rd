%%File Trancura9414001.Rd
%% Part of the hydroPSO R package, http://www.rforge.net/hydroTSM/ ; 
%%                                 http://cran.r-project.org/web/packages/hydroTSM/
%% Copyright 2008-2020 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later

\name{Trancura9414001}
\docType{data}
\alias{Trancura antes de Llafenco (9414001)}
\title{
Hydrometeorological time series for Trancura antes de Llafenco basin
}
\description{
Daily time series of precipitation, mean air temperature, potential evapotranspiration and streamflows at station Trancura antes de Llafenco (Cod.BNA: 9414001), Araucania Region, Chile (Lat:-39.3333, Lon:-71.6667), with data from 01/Jan/1979 to 31/Dec/2016 (including some gaps).
}
\usage{
data(Trancura9414001)
}

%%\details{
%%
%%}
\format{
data.frame with 5 columns: \cr
-) \var{Date}: character with the date (YYYY-MM-DD) for each daily observation. \cr
-) \var{P\_mm}: Spatially-averaged mean daily values of precipitation computed based on the CR2met dataset, [mm/day]. \cr
-) \var{Tmean\_degC}: Spatially-averaged mean daily values of air temperature computed based on the CR2met dataset, [degree Celsius]. \cr
-) \var{PET_mm}: Spatially-averaged mean daily values of precipitation computed based on the Hargreaves-Samani equation and daily maximum and minimum air temperatures obtained from the CR2met dataset, [mm/day]. \cr 
-) \var{Qobs_m3s}: Daily sreamflows measured at the Trancura antes de Llafenco (9414001) station. \cr 
}
\source{
Provided by Center for Climate and Resilience Research, Universidad de Chile, Santiago, Chile (\url{http://www.cr2.cl} at \url{http://camels.cr2.cl/}, last accessed [Feb 2020]). \cr
These data are intended to be used for research purposes only, being distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY.
}

\references{
 \cite{Alvarez-Garreton, C., Mendoza, P. A., Boisier, J. P., Addor, N., Galleguillos, M., Zambrano-Bigiarini, M., Lara, A., Puelma, C., Cortes, G., Garreaud, R., McPhee, J., and Ayala, A (2018). The CAMELS-CL dataset: catchment attributes and meteorology for large sample studies-Chile dataset. Hydrology and Earth System Sciences, 22(11), 5817-5846. doi:10.5194/hess-22-5817-2018.}
}
\keyword{datasets}

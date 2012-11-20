%% File hydroPSO2pest.Rd
%% Part of the hydroPSO R package, http://www.rforge.net/hydroPSO/ ; 
%%                                 http://cran.r-project.org/web/packages/hydroPSO
%% Copyright 2012-2012 Mauricio Zambrano-Bigiarini & Rodrigo Rojas
%% Distributed under GPL 2 or later

\name{hydroPSO2pest}
\alias{hydroPSO2pest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Export hydroPSO input files to PEST
}
\description{
This function exports the content of the hydroPSO input files \sQuote{ParamRanges.txt} and \sQuote{ParamFiles.txt} to PEST, into a single \sQuote{.pst} files with its corresponding \sQuote{.tpl} and \sQuote{.ins} files   
}
\usage{
hydroPSO2pest(param.files="ParamFiles.txt", param.ranges="ParamRanges.txt",
              observations.fname="Observations.txt", exe.fname, 
              drty.model=getwd(), pst.fname="hydroPSO2PEST.pst", verbose=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{param.files}{
character, name (full path) of the hydroPSO input file storing the location and names of the model files that have to be modified for each parameter subject to calibration. \cr
By default this file is called \sQuote{ParamFiles.txt} and -if the full path it is not specified- it is searched for within the \sQuote{PSO.in} subdirectory of \code{drty.model}
}
  \item{param.ranges}{
character, name (full path) of the hydroPSO input file defining the minimum and maximum boundary values for each one of the parameters to be calibrated \cr
By default this file is called \sQuote{ParamRanges.txt} and -if the full path it is not specified- it is searched for within the \sQuote{PSO.in} subdirectory of \code{drty.model}
}
  \item{observations.fname}{
character name (full path) of the hydroPSO output file storing the observed values used during the optimisation. \cr
By default this file is called \sQuote{Observations.txt} and -if the full path it is not specified- it is searched for within the \sQuote{PSO.out} subdirectory of \code{drty.model}
}
  \item{exe.fname}{
character, model command line arguments to be entered through a prompted string to execute the user-defined model 
}
  \item{drty.model}{
character, path to the executable file of the model specified in \code{exe.fname}. ALL the files required to run the model have to be located within this directory (however, input files may be located elsewhere)
}
  \item{pst.fname}{
character, with the name of the output \sQuote{.pst} file
}
  \item{verbose}{
logical, indicates if progress messages are to be printed. By default \code{verbose=TRUE}
}
 

%%  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
%%}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{
A list of two elements: 
\item{sim}{numeric, with the simulated values obtained by running the model}
\item{GoF}{numeric, goodness-of-fit value representing how close each one of the simulated values in \code{sim} are to their observed counterparts, by using the USER-DEFINED \code{gof.FUN} function}
}
%%\references{
%% ~put references to the literature/web site here ~
%%}
\author{
Mauricio Zambrano-Bigiarini, \email{mzb.devel@gmail.com}
}
%%\note{
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{pest2hydroPSO}}, \code{\link{hydroPSO}}
}
%%\examples{
%%setwd("/mnt/netapp1/nahaUsers/rojasro/test_functions/SWAT2005_hydroPSO")
%%hydroPSO2pest(            paramfiles.fname="ParamFiles.txt",
%%                          paramranges.fname="ParamRanges.txt",
%%                          observations.fname="PEST2hydroPSO_OBS.txt",
%%                          exe.fname="./swat2005.out",
%%                          drty.model="/mnt/netapp1/nahaUsers/rojasro/test_functions/SWAT2005_hydroPSO",
%%                          #rscript.fname,
%%                          pst.fname="hydroPSO2PEST_test1.pst"                       
%%                          
%%                          )
%%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{files}
\keyword{optimisation}% __ONLY ONE__ keyword per line

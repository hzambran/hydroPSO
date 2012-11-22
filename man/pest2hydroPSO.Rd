%% File pest2hydroPSO.Rd
%% Part of the hydroPSO R package, http://www.rforge.net/hydroPSO/ ; 
%%                                 http://cran.r-project.org/web/packages/hydroPSO
%% Copyright 2012-2012 Mauricio Zambrano-Bigiarini & Rodrigo Rojas
%% Distributed under GPL 2 or later

\name{pest2hydroPSO}
\alias{pest2hydroPSO}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Import PEST input files to hydroPSO
}
\description{
This function imports the PEST input files (a master \sQuote{.pst} and its corresponding \sQuote{.tpl} and \sQuote{.ins}) into hydroPSO (\sQuote{ParamRanges.txt} and \sQuote{ParamFiles.txt}) 
}
\usage{
pest2hydroPSO(pst.fname, drty.pest=NULL, drty.model=NULL, drty.out="PSO.in",
              param.files="ParamFiles.txt", param.ranges="ParamRanges.txt",
              decimals=5, verbose=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pst.fname}{
character, with name of the PEST input file (\sQuote{.pst}), which contains all the information regarding parameters, observations and template files (\sQuote{.tpl} and \sQuote{.ins}) used by PEST
}
  \item{drty.pest}{
character, path to the executable file of PEST. ALL the files required to run PEST with the model have to be located within this directory (\sQuote{.tpl} and \sQuote{.ins}) \cr
Default value is \code{NULL}, which assigns to \code{drty.pest} the parent directory of \code{pst.fname}
}
  \item{drty.model}{
character, path to the executable file of the model specified in \code{exe.fname}. ALL the files required to run the model have to be located within this directory \cr
Default value is \code{NULL}, which assigns to \code{drty.pest} the parent directory of \code{pst.fname}
}
  \item{drty.out}{
character, name of the directory that will store all the output files produced by this function \cr
Default value is \sQuote{PSO.in}, which creteas a directory called \sQuote{PSO.in} within the parent directory of \code{pst.fname}
}
  \item{param.files}{
character, name of the output file that will store the location and names of the model files that have to be modified for each parameter subject to calibration with hydroPSO. \cr
By default this file is called \sQuote{ParamFiles.txt} and -if the full path it is not specified- it is searched for within the \sQuote{PSO.in} subdirectory of \code{drty.model}
}
  \item{param.ranges}{
character, name of the output file defining the minimum and maximum boundary values for each one of the parameters to be calibrated with hydroPSO. \cr
By default this file is called \sQuote{ParamRanges.txt} and -if the full path it is not specified- it is searched for within the \sQuote{PSO.in} subdirectory of \code{drty.model}
}
  \item{decimals}{
character, model command line arguments to be entered through a prompted string to execute the user-defined model 
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
Two input files for \code{\link{hydroPSO}}:
\item{param.files}{plain text file with the location and names of the model files that have to be modified for each parameter subject to calibration with hydroPSO}
\item{param.ranges}{plain text file defining the minimum and maximum boundary values for each one of the parameters to be calibrated with hydroPSO}

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
\code{\link{hydroPSO2pest}}, \code{\link{hydroPSO}}
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

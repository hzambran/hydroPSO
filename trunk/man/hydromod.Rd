%% File hydromod.Rd
%% Part of the hydroPSO R package, http://www.rforge.net/hydroPSO/ ; 
%%                                 http://cran.r-project.org/web/packages/hydroPSO
%% Copyright 2011-2012 Mauricio Zambrano-Bigiarini & Rodrigo Rojas
%% Distributed under GPL 2 or later

\name{hydromod}
\alias{hydromod}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
hydromod - Definition and execution of the model to be calibrated
}
\description{
Run a user-defined model to be calibrated, obtaining a goodness-of-fit value as measure of model performance by comparing observations against simulated equivalents
}
\usage{
hydromod(param.values, param.files = "ParamFiles.txt", model.drty = getwd(), 
         exe.fname = "./swat2005.out", stdout= FALSE, stderr="", 
         verbose = FALSE, out.FUN, out.FUN.args, gof.FUN, gof.FUN.args=list(), 
         gof.Ini, gof.Fin, date.fmt = "\%Y-\%m-\%d", obs,
         do.png=FALSE, png.fname, width = 1200, height = 600, res=90,
         main, leg.cex=1.2, tick.tstep= "auto", lab.tstep= "auto", lab.fmt=NULL
         )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{param.values}{
numeric vector, parameter values that will be used in the model
}
  \item{param.files}{
character, file name (full path) storing location and names of the files that have to be modified for each parameter 
}
  \item{model.drty}{
character, path storing the executable file of the model and ALL the input files required for the simulation
}
  \item{exe.fname}{
character, name and extension of the file executing the model
}
  \item{stdout, stderr}{
where output to \sQuote{stdout} or \sQuote{stderr} should be sent. Possible values are \code{FALSE} (discard output, the default), \code{""}, to the R console. See \code{\link[base]{system2}}
}
  \item{verbose}{
logical; if TRUE, progress messages are printed to the screen \cr
If \code{verbose=TRUE}, the following messages will appear: i) parameter values for each particle; (ii) model execution; iii) extraction of simulated values; and iv) computation of the goodness-of-fit measure
}
  \item{out.FUN}{
character, name of a valid R function to read the model outputs and transform them into a zoo object
}
  \item{out.FUN.args}{
list, arguments to be passed to \code{out.FUN}
}
  \item{gof.FUN}{
character, name of a valid (goodness-of-fit) R function to obtain model performance
}
  \item{gof.FUN.args}{
list, arguments to be passed to \code{gof.FUN}
}
  \item{gof.Ini}{
OPTIONAL. Character with the starting date used in the goodness-of-fit function \cr
It is used to subset \code{obs} (if necessary), AND to define the time period to compare simulated with observed values
}
  \item{gof.Fin}{
OPTIONAL. Character with the ending date used in the goodness-of-fit function \cr
It is used to subset \code{obs} (if necessary), AND to define the time period to compare simulated with observed values
}
  \item{date.fmt}{
character, format in which the dates are stored in \code{Sim.Ini}, \code{Sim.Fin}, \code{gof.Ini}, \code{gof.Fin}, e.g. \kbd{\%Y-\%m-\%d}. See \code{format} in \code{\link[base]{as.Date}}
}
  \item{obs}{
zoo object with the observed values
}
  \item{do.png}{
logical indicating if a png image with the results of the \code{ggof.FUN} function has to be produced %RRM WHAT IS 'ggof'
}
  \item{png.fname}{
OPTIONAL. Used only when \code{do.png=TRUE} \cr
Name of the PNG file to be produced. The default values is \sQuote{Obs_vs_Sim.png}, within the \code{model.drty} directory
}
  \item{width}{
OPTIONAL. Used only when \code{do.png=TRUE} \cr
numeric, width of the output PNG image
}
  \item{height}{
OPTIONAL. Used only when \code{do.png=TRUE} \cr
numeric, height of the output PNG image
}
  \item{res}{
OPTIONAL. Used only when \code{do.png=TRUE} \cr
numeric, resolution of the output PNG image
}
  \item{main}{
OPTIONAL. Used only when \code{do.png=TRUE} \cr
character, representing the main title of the plot comparing observed and simulated values
}
  \item{leg.cex}{
See \code{\link[hydroGOF]{ggof}}
}
  \item{tick.tstep}{
See \code{\link[hydroGOF]{ggof}}
}
  \item{lab.tstep}{
See \code{\link[hydroGOF]{ggof}}
}
  \item{lab.fmt}{
See \code{\link[hydroGOF]{ggof}}
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
\item{GoF}{numeric, goodness-of-fit value representing how close each onbe of the simulated values in \code{sim} are to their observed counterparts, by using the USER-DEFINED \code{gof.FUN} function}
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
\code{\link{hydroPSO}}
}
%%\examples{
%%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{files}
\keyword{optimisation}% __ONLY ONE__ keyword per line

# Part of the hydroPSO package, http://www.rforge.net/hydroPSO/
# Copyright 2008-2012 Mauricio Zambrano-Bigiarini & Rodrigo Rojas
# Distributed under GPL 2 or later

# All these function were started on 2008, with updates on:                    #
# 13-Dec-2010 ; 20-Dec-2010; 21-Dec-2010                                       #
# 24-Jan-2011 ; 02-Feb-2011                                                    #
# 14-Nov-2011

# MZB, 21-Jun-2011
# 3D sinc function: f(1,..,1)=1. Maximization
sinc <- function(x) {
    n <- length(x)
    return( prod (sin( pi*(x-seq(1:n)) ) / ( pi*(x-seq(1:n)) ), na.rm=TRUE) )
} # 'sinc' END

# MZB, RR, 21-Jun-2011,  14-Nov-2011
# Rosenbrock function: f(1,..,1)=0. Minimization. In [-30, 30]^n. AcceptableError < 100
rosenbrock <- function(x) {  
  n <- length(x)
  return( sum( ( 1- x[1:(n-1)] )^2 + 100*( x[2:n] - x[1:(n-1)]^2 )^2 ) )
} # 'rosenbrock' END


# MZB, RR, 21-Jun-2011
# Sphere function: f(1,..,1)=0. Minimization. In [-100, 100]^n. AcceptableError < 0.01
sphere <- function(x) {
  return(sum(x^2))  
} # 'sphere' END


# MZB, RR, 21-Jun-2011,  14-Nov-2011. Keep only for backward compatibility
# Rastrigrin function: f(0,..,0)=0. Minimization. In [-5.12, 5.12]^n. AcceptableError < 100
rastrigrin <- function(x) { 
  n <- length(x) 
  return( 10*n + sum( x^2 - 10*cos(2*pi*x) ) )
} # 'rastrigrin' END

# MZB, RR, 17-Jul-2012. The correct name of the function is 'Rastrigin' and NOT 'Rastrigrin' !!!
# Rastrigin function: f(0,..,0)=0. Minimization. In [-5.12, 5.12]^n. AcceptableError < 100
rastrigin <- function(x) { 
  n <- length(x) 
  return( 10*n + sum( x^2 - 10*cos(2*pi*x) ) )
} # 'rastrigin' END


# MZB, RR, 21-Jun-2011
# Griewank function: f(0,..,0)=0. Minimization. In [-600, 600]^n. AcceptableError < 0.05
griewank <- function(x) {  
  n <- length(x)
  return( 1 + (1/4000)*sum( x^2 ) - prod( cos( x/sqrt(seq(1:n)) ) ) )
} # 'griewank' END


# MZB, RR, 21-Jun-2011,  14-Nov-2011,  13-Sep-2012
# Schaffer's f6 function: f(0,..,0)=0. Minimization. In [-100, 100]^n. AcceptableError < 0.00001
schafferF6 <- function(x) {  
  return( 0.5 + ( ( sin( sqrt( sum( x^2 ) ) ) )^2 - 0.5) / ( ( 1 + 0.001*sum(x^2) )^2 ) )
} # 'schafferF6' END


# MZB, RR, 14-Nov-2011
# Ackley function: f(0,..,0)=0. Minimization. In [-32.768, 32.768]^n. AcceptableError < 0.01, a=20 ; b=0.2 ; c=2*pi
ackley <- function(x) {  
  n <- length(x)
  return( -20*exp( -0.2*sqrt((1/n)*sum(x^2)) ) - exp( (1/n)*sum(cos(2*pi*x)) ) + 20 + exp(1) )
} # 'schafferF6' END


################################################################################
########################### Shifted Functions ##################################
################################################################################

# MZB, 21-Sep-2012. Unimodal Function
# Shifted Sphere: f(o,..,o)=-450. Minimization. In [-100, 100]^n. AcceptableError < 0.01
ssphere <- function (x, o=-100+200*runif(length(x)), fbias=-450) {
 n <- length(x)
 if (n != length(o)) stop("length(x) != length(o)")
 z <- x - o
 return(sum(z^2) + fbias)
} # 'ssphere'

# MZB, RR, 21-Jun-2011
# Shifted Griewank : f(o,..,o)=-180. Minimization. In [-600, 600]^n. AcceptableError < 0.05
sgriewank <- function (x, o=-600+1200*runif(length(x)), fbias=-180) {
  n <- length(x)
  if (n != length(o)) stop("length(x) != length(o)")
  z <- x - o
  return(1 + (1/4000) * sum(z^2) - prod(cos(z/sqrt(seq(1:n)))) + fbias)
} # 'sgriewank'


# MZB, 21-Sep-2012. Multimodal Basic Function
# Shifted Rosenbrock : f(o,..,o)=390. Minimization. In [-100, 100]^n. AcceptableError < 100
srosenbrock <- function(x, o=-100+200*runif(length(x)), fbias=390) {  
  n <- length(x)
  if (n != length(o)) stop("length(x) != length(o)")
  z <- x - o
  return( sum( ( 1- z[1:(n-1)] )^2 + 100*( z[2:n] - z[1:(n-1)]^2 )^2 ) + fbias )
} # 'srosenbrock' END


# MZB, 21-Sep-2012
# Shifted Ackley: f(o,..,o)=-140. Minimization. In [-32.768, 32.768]^n. AcceptableError < 0.01, a=20 ; b=0.2 ; c=2*pi
sackley <- function (x, o=-32+64*runif(length(x)), fbias=-140) {
  n <- length(x)
  if (n != length(o)) stop("length(x) != length(o)")
  z <- x - o
  return(-20 * exp(-0.2 * sqrt((1/n) * sum(z^2))) - exp((1/n) * sum(cos(2 * pi * z))) + 20 + exp(1) + fbias )
} # 'sackley'


# MZB, 21-Sep-2012
# Shifted Rastrigin: f(o,..,o)=-330. Minimization. In [-5.12, 5.12]^n. AcceptableError < 100
srastrigin <- function(x, o=-5+10*runif(length(x)), fbias=-330) { 
  n <- length(x) 
  if (n != length(o)) stop("length(x) != length(o)")
  z <- x - o
  return( 10*n + sum( z^2 - 10*cos(2*pi*z) ) + fbias )
} # 'srastrigin' END


# MZB, 21-Sep-2012
# Shifted Rotated Rastrigin: f(o,..,o)=-330. Minimization. In [-5.12, 5.12]^n. AcceptableError < 100
srrastrigin <- function(x, o=-5+10*runif(length(x)), fbias=-330) { 
  n <- length(x) 
  if (n != length(o)) stop("length(x) != length(o)")
  z <- x - o
  return( 10*n + sum( z^2 - 10*cos(2*pi*z) ) + fbias )
} # 'srastrigin' END


# File PSO_v2013.R
# Part of the hydroPSO R package, https://github.com/hzambran/hydroPSO
#                                 http://cran.r-project.org/web/packages/hydroPSO
#                                 http://www.rforge.net/hydroPSO/
# Copyright 2010-2020 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
##                          Random.Bounded.Matrix                             ##
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                        ##
################################################################################
# Created: 2008                                                               ##
# Updates: 23-Nov-2010                                                        ##
#          20-Sep-2012 ; 29-Oct-2012                                          ##
################################################################################
# Purpose  : To create a matrix randomly generated, with a bounded uniform distribution

# 'npart'   : number of particles in the swarm
# 'X.MinMax': Matrix of 'n' rows and 2 columns, 
#             the first column has the minimum values for each dimension, and
#             the second column has the maximum values for each dimension
Random.Bounded.Matrix <- function(npart, x.MinMax) {

  # dimension of the solution space (number of parameters )
  n <- nrow(x.MinMax)
	
  lower <- matrix( rep(x.MinMax[,1], npart), nrow=npart, byrow=TRUE)
  upper <- matrix( rep(x.MinMax[,2], npart), nrow=npart, byrow=TRUE)
	
  # random initialization for all the particles, with a value in [0,1]
  X <- matrix(runif(n*npart, min=0, max=1), nrow=npart, ncol=n)

  # Transforming X into the real range defined by the user
  #X <- t( lower +  (upper - lower )*t(X) ) # when using vector instead of matrixes
  X <- lower + (upper-lower)*X  
	
} # 'Random.Bounded.Matrix' end
#set.seed(1)
#Random.Bounded.Matrix(10, x.MinMax)


################################################################################
##                        random Latin-Hypercube Sampling                     ##
################################################################################
# Author: Mauricio Zambrano-Bigiarini                                         ##
# Created: 17-Dec-2010                                                        ##
# Updates: 20-Sep-2012  ; 29-Oct-2012                                         ##
#          07-Feb-2014                                                        ##
################################################################################
# Purpose  : Draws a Latin Hypercube Sample from a set of uniform distributions
#            for use in creating a Latin Hypercube Design
################################################################################
# Output   : An n by ndim Latin Hypercube Sample matrix with values uniformly 
#            distributed on 'ranges'
################################################################################

# 'n'      : number of strata used to divide each parameter range. 
#            For hydroPSO: 'n=npart'
# 'ranges' : Matrix of 'N' rows and 2 columns, (N is the number of parameters)
#            the first column has the minimum values for each dimension, and
#            the second column has the maximum values for each dimension
rLHS <- function(n, ranges) {

  # dimension of the solution space (number of parameters )
  ndim <- nrow(ranges)
  
  # number of particles
  npart <- n
  
  lower <- matrix( rep(ranges[,1], npart), nrow=npart, byrow=TRUE)
  upper <- matrix( rep(ranges[,2], npart), nrow=npart, byrow=TRUE)
	
  # LHS initialization for all the particles, with a value in [0,1]
  X <- lhs::randomLHS(n, ndim) # lhs::randomLHS

  # Transforming X into the real range defined by the user
  #X <- t( lower +  (upper - lower )*t(X) ) # when using vector instead of matrixes
  X <- lower + (upper-lower)*X  
	
} # 'rLHS' end


################################################################################
##                        Quasi-random Sobol Sampling                         ##
################################################################################
# Author: Mauricio Zambrano-Bigiarini & RMR                                   ##
# Created: 26-Dec-2020                                                        ##
# Updates: 22-May-2026                                                        ##
################################################################################
# Purpose  : Draws a Sobol Sample from a set of uniform distributions         ##
#            for use in creating a Sobol Design                               ##
################################################################################
# Output   : An n by ndim Sobol Sample matrix with values uniformly           ##
#            distributed on 'ranges'                                          ##
################################################################################
#                                                                             ##
# 'n'      : number of strata used to divide each parameter range. 
#            For hydroPSO: 'n=npart'
# 'ranges' : Matrix of 'N' rows and 2 columns, (N is the number of parameters)
#            the first column has the minimum values for each dimension, and
#            the second column has the maximum values for each dimension
rSobol <- function(n, ranges) {
  
  # dimension of the solution space (number of parameters)
  
  ndim <- nrow(ranges)
  
  # number of particles
  npart <- n
  
  lower <- matrix( rep(ranges[,1], npart), nrow=npart, byrow=TRUE)
  upper <- matrix( rep(ranges[,2], npart), nrow=npart, byrow=TRUE)
  
  # Sobol initialization for all the particles, with a value in [0,1]
  X <- randtoolbox::sobol(n = n, dim = ndim)
  #X <- randtoolbox::sobol(n=N, dim=2*K, scrambling=scrambling)  
  #if ( scrambling == "none") {
  #  X <- spacefillr::generate_sobol_set(n=n, dim=ndim, seed = 0)
  #} else X <- spacefillr::generate_sobol_owen_set(n=n, dim=ndim, seed = 0) 
  
  # Transforming X into the real range defined by the user
  #X <- t( lower +  (upper - lower )*t(X) ) # when using vector instead of matrixes
  X <- lower + (upper-lower)*X  
  
} # 'rSobol' end


################################################################################
#                                    enorm                                     #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Created: 19-Sep-2012                                                         #
# Updates:                                                                     #
################################################################################
# Purpose  : Computes the Euclidean norm of a vector                           #
################################################################################
# Output   : single numeric value with the euclidean norm of 'x'               #
################################################################################
enorm <- function(x) sqrt( sum(x*x) )


################################################################################
#                                alea.normal                                   #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
#          Based on the Matlab function developed by Maurice Clerc (May 2011), #
#          and available on:                                                   #
#          http://www.particleswarm.info/SPSO2011_matlab.zip                   #
################################################################################
# Created: 19-Sep-2012                                                         #
# Updates: 29-Oct-2012                                                         #
################################################################################
# Purpose  : It uses the polar form of the Box-Muller transformation to obtain #
#            a pseudo-random number from a Gaussian distribution               #
################################################################################
# Output   : single numeric value with a pseudo-random number from a Gaussian  #
#            distribution with mean='mean' and standard deviation ='sd'        #
################################################################################
alea.normal <- function(mean=0, sd=1) {

  w <- 2  
  while (w >= 1) {
     x1 <- 2 * runif(1) - 1
     x2 <- 2 * runif(1) - 1
     w  <- x1*x1 + x2*x2   
  } # 'WHILE' end   
  w  <- sqrt( -2*log(w) / w )
  y1 <- x1*w  
  if ( runif(1) < 0.5 ) y1 <- -y1  
  y1 <- y1 * sd + mean 

} # 'alea.normal' end


################################################################################
#                                alea.sphere                                   #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
#          Based on the Matlab function developed by Maurice Clerc (May 2011), #
#          and available on:                                                   #
#          http://www.particleswarm.info/SPSO2011_matlab.zip                   #
################################################################################
# Created: 19-Sep-2012                                                         #
# Updates:                                                                     #
################################################################################
# Purpose  : It generates a random point inside the hypersphere centered       #
#            around G with radius = r                                          #
################################################################################
# Output   : numeric vector with the location of a random point inside the     #
#            hypersphere around G with radius = r                              #
################################################################################
alea.sphere <- function(G, radius) {

  # dimension of 'G' (number of parameters)
  n <- length(G)
  
  # Step 1. Direction
  l <- 0
  #x <- replicate( n, alea.normal(mean=0, sd=1) )
  x <- rnorm(n, mean=0, sd=1)
  l <- sqrt( sum(x*x) )
  
  # Step 2. Random Radius
  r <- runif(1)
  
  x <- r * radius * x / l
  
  # Centering the random point at 'G'
  return( x + G)

} # 'alea.sphere' end


################################################################################
#                               compute.CF Function                            #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Created: 2008                                                                #
# Updates:                                                                     #
################################################################################
# Computes the Clerc's Constriction Factor
# Clerc, M.; Kennedy, J.; , "The particle swarm - explosion, stability, and 
# convergence in a multidimensional complex space," Evolutionary Computation, 
# IEEE Transactions on , vol.6, no.1, pp.58-73, Feb 2002
# doi: 10.1109/4235.985692

compute.CF <- function(c1, c2) {
    psi <- c1 + c2  # psi >= 4
    CF <- 2 / abs( 2 - psi - sqrt(psi^2 - 4*psi) )
} # 'compute.CF' end


   
# 'x' 	      : vector of 'n' parameters, corresponding to one particle
# 'v'         : vector of 'n' velocities of each parameter, corresponding to the current particle
# 'vmax.perc' : percentage of the range of each dimension that is allowed as 
#               maximum velocity in that dimension
# 'Pbest'     : matrix with the current best parameter values for all the particles
# 'gbest'     : vector with the best parameter values in the swarm
# 'w'	      : Inertial factor.
# 'c1'        : constant that encourages the exploration of the solution space
# 'c2'	      : constant that encourages the exploitation of the current global best
# 'CF'        : constant representing the Clerk's Constriction Factor,to insure convergence of the PSO algorithm
# 'x.MinMax'  : Matrix with the valid range for each parameter of 'X'.
#               Rows = 'n' (number of parameter
#               Columns = 2, 
#               First column has the minimum possible value for each parameter
#               Second column has the maximum possible value for each parameter
# 'topology'  : character, with the topology to be used in PSO. Valid values
#               are in c('gbest', 'lbest')
# 'method'    : character, with the method to be used as PSO algorithm. Valid values
#               are in c('spso2007', 'spso2011', 'spso2011-sa',
#                        'spso2011-de', 'spso2011-cde', 'spso2011-dds', 'spso2011-pdds',
#                        'ipso', 'fips', 'wfips')

# Result      : vector of 'n' velocities, one for each parameter, corresponding to the current particle
################################################################################
##                        compute.veloc Function                              ##
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Created: 2008                                                                #
# Updates: Oct-2011 ; Nov-2011                                                 #
#          19-Sep-2012 ; 20-Sep-2012 ; 28-Oct-2012 ; 31-Oct-2012               #
#          09-May-2016                                                         #
################################################################################
compute.veloc <- function(x, v, w, c1, c2, CF, Pbest, part.index, gbest, 
                          topology, method, MinMax, neighs.index, 
                          localBest, localBest.pos, 
                          ngbest.fit, ngbest, lpbest.fit, DE.F, DE.archive=NULL) {

  pbest <- Pbest[part.index, ]
  
  # dimension of 'x' (number of parameters)
  n <- length(gbest)

  r1 <- runif(n, min=0, max=1)
  r2 <- runif(n, min=0, max=1)
  
  if ( method %in% c("spso2011", "spso2011-sa", "spso2011-de", "spso2011-dds",
                     "spso2011-pdds") ) {

       p <- x + r1*c1 * ( pbest - x )
       l <- x + r2*c2 * ( localBest - x )

      if ( part.index != localBest.pos) {
        Gr <- (x + p + l) / 3
      } else  Gr <- (x + p) / 2
     
      if (method == "spso2011-de") {
        # Mutates the SPSO-2011 centre along a radius-scaled informant-memory direction.
        de.out <- informant.de.mutation(x=x, G=Gr, Pbest=Pbest,
                                        part.index=part.index,
                                        informants=neighs.index,
                                        archive=DE.archive, F=DE.F)
        vn <- CF * (w*v + de.out[["point"]] - x)
        attr(vn, "DE.used") <- de.out[["used"]] # Records whether this candidate used DE mutation.
      } else {
        # Retains the standard spherical draw for the existing SPSO-2011 methods.
        vn <- CF * (w*v + alea.sphere(G=Gr, radius=enorm(Gr-x)) - x)
      } # ELSE method == "spso2011-de" end
  
  } else if ( method == "spso2007" ) {
  
           if( part.index != localBest.pos) {
                  vn <- CF * ( w*v + r1*c1*(pbest-x) + r2*c2*(localBest-x) )
           } else vn <- CF * ( w*v + r1*c1*(pbest-x) ) 
    
         } else if ( method=="ipso" ) {
  
               # number of best particles that have to be considered      
               nngbest <- length(ngbest.fit)
  
               R2 <-  matrix(rep(r2,nngbest), nrow=nngbest, byrow=TRUE)
        
               # computing the c2 values for each one of the best particles,
               # weighted according to their fitness value
               if(MinMax == "min") {
                 c2i <- c2 * ( (1/ngbest.fit)/sum(1/ngbest.fit) )
               } else c2i <- c2 * ( ngbest.fit/sum(ngbest.fit) )
               
               nan.index <- which(is.nan(c2i))
               if (length(nan.index) > 0) c2i[nan.index] <- c2

               # transforming 'x' into a matrix, with the same values in each row, in 
               # order to be able to substract 'x' from 'ngest'
               X <- matrix(rep(x, nngbest), ncol=n, byrow=TRUE)
        
               # computing the velocity
               vn <- CF * ( w*v + r1*c1*(pbest-x) + colSums(R2*c2i*(ngbest-X) ) )      
           
           } else if ( method=="fips" ) {
              
                     neighs.index <- neighs.index[!is.na(neighs.index)] # only for topology=='random' 
                     N   <- length(neighs.index)              
                     X   <- matrix(rep(x,N), nrow=N, byrow=TRUE)
                     P   <- Pbest[neighs.index, ]
                     phi <- c1 + c2
                     r   <- runif(N, min=0, max=phi)
                 
                     vn  <-  CF * ( w*v + (1/N)*colSums( r*(P-X) ) )
              
               
                   } else if ( method=="wfips" ) {
                       
                       neighs.index <- neighs.index[!is.na(neighs.index)] # only for topology=='random' 
                       N    <- length(neighs.index)              
                       X    <- matrix(rep(x,N), nrow=N, byrow=TRUE)
                       P    <- Pbest[neighs.index, ]
                       pfit <- lpbest.fit[neighs.index]
                       phi  <- c1 + c2
                       r    <- runif(N, min=0, max=phi)
                       if(MinMax == "min") {
                         wght <- (1/pfit)/sum(1/pfit)
                       } else wght <- pfit/sum(pfit) 
                   
                       vn  <-  CF * ( w*v + (1/N) * colSums( wght*r*(P-X) ) )	
                         
                     }  else if ( method == "canonical")    
                                       
                           vn <- CF * ( w*v + r1*c1*(pbest-x) + r2*c2*(localBest-x) )
                       
  
  return(vn)
  
} # 'compute.veloc' end


################################################################################
##                    informant.de.mutation Function                          ##
################################################################################
# Mutates the SPSO-2011 centre using current and historical informant memories.
informant.de.mutation <- function(x, G, Pbest, part.index, informants,
                                  archive=NULL, F) {

  radius <- enorm(G-x) # Uses the native SPSO-2011 search radius as mutation scale.
  informants <- unique(informants[!is.na(informants)]) # Removes unused topology cells.
  informants <- setdiff(informants, part.index) # Prevents the target from donating to itself.
  current <- Pbest[informants, , drop=FALSE] # Reads only the current informants' memories.

  if (is.null(archive))
    archive <- matrix(numeric(), nrow=0, ncol=length(G)) # Creates an empty compatible archive.

  if (nrow(current) > 0)
    current <- current[apply(current, 1, function(z) all(is.finite(z))), , drop=FALSE] # Removes invalid memories.
  pool <- unique(current) # Gives the realised current topology first priority.
  if (nrow(pool) < 2 && nrow(archive) > 0) {
    archive <- archive[apply(archive, 1, function(z) all(is.finite(z))), , drop=FALSE] # Removes invalid history.
    pool <- unique(rbind(pool, archive)) # Uses history only to repair an insufficient current pool.
  } # IF current pool needs historical support end

  if (nrow(pool) < 2 || !is.finite(radius) || radius <= 0) {
    fallback.radius <- if (is.finite(radius) && radius > 0) radius else 0 # Keeps fallback finite.
    point <- alea.sphere(G=G, radius=fallback.radius) # Restores SPSO exploration when DE is unavailable.
    return(list(point=point, used=FALSE))
  } # IF invalid pool or radius end

  donors <- sample.int(nrow(pool), size=2, replace=FALSE) # Selects two distinct memory donors.
  difference <- pool[donors[1], ] - pool[donors[2], ] # Retains the correlated donor direction.
  difference.norm <- enorm(difference) # Measures the raw differential displacement.

  if (!is.finite(difference.norm) || difference.norm <= .Machine$double.eps) {
    point <- alea.sphere(G=G, radius=radius) # Restores SPSO exploration after donor collapse.
    return(list(point=point, used=FALSE))
  } # IF invalid difference end

  direction <- difference / difference.norm # Normalises direction independently of donor spacing.
  mutant <- G + F*radius*direction # Keeps DE displacement commensurate with SPSO geometry.

  return(list(point=mutant, used=TRUE))

} # 'informant.de.mutation' end


################################################################################
##                    update.informant.archive Function                       ##
################################################################################
# Retains recent personal-best positions supplied by a particle's informants.
update.informant.archive <- function(archive, Pbest, informants, part.index,
                                     max.size) {

  informants <- unique(informants[!is.na(informants)]) # Removes unused topology cells.
  informants <- setdiff(informants, part.index) # Excludes the target particle's own memory.
  current <- Pbest[informants, , drop=FALSE] # Collects memories from the realised topology only.
  current <- current[apply(current, 1, function(z) all(is.finite(z))), , drop=FALSE] # Removes invalid rows.
  combined <- rbind(archive, current) # Appends the newest informant memories to history.

  if (nrow(combined) > 0)
    combined <- unique(combined) # Stores each position once while preserving first occurrence order.
  if (nrow(combined) > max.size)
    combined <- utils::tail(combined, max.size) # Keeps the most recent bounded set of unique memories.

  return(combined)

} # 'update.informant.archive' end


################################################################################
##                         sample.de.F Function                               ##
################################################################################
# Samples a positive differential weight around the learned success-history mean.
sample.de.F <- function(mu, scale=0.1, upper=2) {

  repeat {
    value <- stats::rcauchy(1, location=mu, scale=scale) # Uses JADE-style heavy-tailed adaptation.
    if (is.finite(value) && value > 0) break # Rejects unusable non-positive draws.
  } # REPEAT end

  return(min(value, upper)) # Bounds rare large Cauchy draws to the supported control range.

} # 'sample.de.F' end


################################################################################
##                    update.de.success.history Function                      ##
################################################################################
# Updates the mutation-factor mean from particles that improved their pbest.
update.de.success.history <- function(mu, F, fitness, pbest.fit, MinMax, c) {

  gain <- if (MinMax == "min") pbest.fit-fitness else fitness-pbest.fit # Makes improvement positive.
  successful <- is.finite(F) & is.finite(gain) & (gain > 0) # Selects successful DE-generated candidates.

  if (!any(successful))
    return(list(mu=mu, n.success=0L)) # Retains the previous mean when no DE step succeeds.

  successful.F <- F[successful] # Extracts mutation factors associated with improvements.
  successful.gain <- gain[successful] # Extracts their positive objective improvements.
  weights <- successful.gain / sum(successful.gain) # Favours factors producing larger improvements.
  lehmer <- sum(weights*successful.F^2) / sum(weights*successful.F) # Computes weighted Lehmer mean.
  updated.mu <- (1-c)*mu + c*lehmer # Smooths the new successful value into memory.

  return(list(mu=updated.mu, n.success=length(successful.F)))

} # 'update.de.success.history' end


################################################################################
##                         cde.is.better Function                             ##
################################################################################
# Compares competitive candidates with their current parents.
cde.is.better <- function(new.fit, old.fit, MinMax) {

  valid.new <- is.finite(new.fit) # Rejects invalid competitive candidates.
  if (MinMax == "min") {
    out <- valid.new & (!is.finite(old.fit) | new.fit < old.fit)
  } else out <- valid.new & (!is.finite(old.fit) | new.fit > old.fit)

  return(out)

} # 'cde.is.better' end


################################################################################
##                         cde.fitness.gain Function                          ##
################################################################################
# Computes a non-negative, scale-resistant reward for operator adaptation.
cde.fitness.gain <- function(new.fit, old.fit, MinMax) {

  better <- cde.is.better(new.fit=new.fit, old.fit=old.fit, MinMax=MinMax)
  gain <- rep(0, length(new.fit)) # Gives failed proposals zero operator credit.
  finite.parent <- better & is.finite(old.fit) # Uses relative improvement when possible.
  if (any(finite.parent)) {
    difference <- if (MinMax == "min") {
      old.fit[finite.parent] - new.fit[finite.parent]
    } else new.fit[finite.parent] - old.fit[finite.parent]
    gain[finite.parent] <- difference / pmax(abs(old.fit[finite.parent]), 1)
  } # IF finite parent end
  gain[better & !is.finite(old.fit)] <- 1 # Rewards recovery from an invalid parent.

  return(gain)

} # 'cde.fitness.gain' end


################################################################################
##                      cde.sample.parameters Function                       ##
################################################################################
# Samples one SHADE memory cell and its associated F and CR values.
cde.sample.parameters <- function(M.F, M.CR) {

  memory.index <- sample.int(length(M.F), 1) # Selects one circular success memory.
  F <- sample.de.F(mu=M.F[memory.index], scale=0.1, upper=1) # Uses positive Cauchy sampling.
  CR <- stats::rnorm(1, mean=M.CR[memory.index], sd=0.1) # Uses SHADE-style normal sampling.
  CR <- min(1, max(0, CR)) # Restricts crossover probability to its valid interval.

  return(list(F=F, CR=CR, memory.index=memory.index))

} # 'cde.sample.parameters' end


################################################################################
##                      cde.coordinate.basis Function                        ##
################################################################################
# Builds an eigen coordinate system from the personal-best population.
cde.coordinate.basis <- function(Pbest) {

  n <- ncol(Pbest) # Reads the search-space dimension.
  valid <- apply(Pbest, 1, function(z) all(is.finite(z))) # Removes invalid memories.
  population <- Pbest[valid, , drop=FALSE] # Retains finite population information only.
  center <- if (nrow(population) > 0) colMeans(population) else rep(0, n)
  vectors <- diag(n) # Falls back to the original coordinate system.
  usable <- FALSE # Records whether a data-derived rotation was obtained.

  if (n > 1 && nrow(population) > 1) {
    covariance <- stats::cov(population) # Estimates the population orientation.
    decomposition <- try(eigen(covariance, symmetric=TRUE), silent=TRUE)
    if (!inherits(decomposition, "try-error") && all(is.finite(decomposition$vectors))) {
      vectors <- decomposition$vectors # Uses orthonormal covariance eigenvectors.
      usable <- TRUE
    } # IF valid eigendecomposition end
  } # IF multivariate population end

  return(list(center=center, vectors=vectors, usable=usable))

} # 'cde.coordinate.basis' end


################################################################################
##                         cde.reflect Function                              ##
################################################################################
# Reflects a competitive DE trial into the finite parameter bounds.
cde.reflect <- function(x, lower, upper) {

  width <- upper-lower # Computes each parameter's feasible width.
  reflected <- lower # Keeps fixed parameters at their only feasible value.
  varying <- width > 0 # Avoids modulo operations for fixed parameters.
  phase <- (x[varying]-lower[varying]) %% (2*width[varying])
  reflected[varying] <- ifelse(phase <= width[varying], lower[varying]+phase,
                               upper[varying]-(phase-width[varying]))

  return(reflected)

} # 'cde.reflect' end


################################################################################
##                       cde.trial.vector Function                           ##
################################################################################
# Generates a complete DE mutant, crossover trial, and reflected candidate.
cde.trial.vector <- function(part.index, X, Pbest, pbest.fit, archive, F, CR,
                             p, MinMax, lower, upper,
                             strategy=c("current-to-pbest", "rand1"),
                             basis=NULL, use.eigen=FALSE) {

  strategy <- match.arg(strategy) # Checks the requested DE mutation strategy.
  npart <- nrow(X) # Reads the common competitive population size.
  n <- ncol(X) # Reads the search-space dimension.
  target <- X[part.index, ] # Uses the current particle as the crossover parent.

  if (strategy == "current-to-pbest") {
    decreasing <- MinMax == "max" # Orders personal bests in the optimisation direction.
    ranked <- order(pbest.fit, decreasing=decreasing, na.last=TRUE)
    elite.size <- min(npart, max(2L, ceiling(p*npart))) # Forms the top-p elite pool.
    elite.pool <- ranked[seq_len(elite.size)] # Builds the current top-p index set.
    elite.index <- elite.pool[sample.int(length(elite.pool), 1)] # Avoids a deterministic global best.
    r1.pool <- setdiff(seq_len(npart), c(part.index, elite.index))
    if (length(r1.pool) == 0)
      r1.pool <- setdiff(seq_len(npart), part.index) # Repairs very small elite pools.
    r1 <- r1.pool[sample.int(length(r1.pool), 1)] # Selects the first population donor.

    r2.index <- setdiff(seq_len(npart), c(part.index, elite.index, r1))
    r2.pool <- X[r2.index, , drop=FALSE] # Starts with distinct population donors.
    if (nrow(archive) > 0)
      r2.pool <- rbind(r2.pool, archive) # Adds historical replaced parents as donors.
    r2.pool <- unique(r2.pool[apply(r2.pool, 1, function(z) all(is.finite(z))), , drop=FALSE])
    if (nrow(r2.pool) == 0)
      stop("Internal error: 'spso2011-cde' could not form a second DE donor")
    r2 <- r2.pool[sample.int(nrow(r2.pool), 1), ] # Selects one current or archived donor.
    mutant <- target + F*(Pbest[elite.index, ]-target) + F*(X[r1, ]-r2)
  } else {
    donors <- sample(setdiff(seq_len(npart), part.index), 3, replace=FALSE)
    mutant <- X[donors[1], ] + F*(X[donors[2], ]-X[donors[3], ])
  } # ELSE exploratory rand/1 end

  mask <- stats::runif(n) <= CR # Selects mutant coordinates through binomial crossover.
  mask[sample.int(n, 1)] <- TRUE # Guarantees at least one mutant coordinate.
  coordinate <- "original" # Records the crossover coordinate system.

  if (use.eigen && !is.null(basis) && isTRUE(basis$usable)) {
    target.rotated <- drop(crossprod(basis$vectors, target-basis$center))
    mutant.rotated <- drop(crossprod(basis$vectors, mutant-basis$center))
    trial.rotated <- target.rotated # Retains target coordinates outside the mask.
    trial.rotated[mask] <- mutant.rotated[mask] # Crosses in the learned eigen system.
    trial <- drop(basis$center + basis$vectors %*% trial.rotated)
    coordinate <- "eigen"
  } else {
    trial <- target # Retains target parameters outside the crossover mask.
    trial[mask] <- mutant[mask] # Crosses in the original parameter system.
  } # ELSE original-coordinate crossover end

  trial <- cde.reflect(trial, lower=lower, upper=upper) # Enforces finite bounds by reflection.

  return(list(point=trial, strategy=strategy, coordinate=coordinate))

} # 'cde.trial.vector' end


################################################################################
##                       cde.update.archive Function                         ##
################################################################################
# Adds successfully replaced parents to a bounded global DE archive.
cde.update.archive <- function(archive, parents, max.size) {

  if (nrow(parents) > 0)
    archive <- unique(rbind(archive, parents)) # Retains unique replaced search states.
  if (nrow(archive) > max.size)
    archive <- archive[sample.int(nrow(archive), max.size), , drop=FALSE] # Prunes without age bias.

  return(archive)

} # 'cde.update.archive' end


################################################################################
##                       cde.update.memories Function                        ##
################################################################################
# Updates one circular SHADE cell from successful DE factors and crossovers.
cde.update.memories <- function(M.F, M.CR, pointer, F, CR, gain, successful) {

  successful <- successful & is.finite(F) & is.finite(CR) & is.finite(gain) & gain > 0
  if (!any(successful))
    return(list(M.F=M.F, M.CR=M.CR, pointer=pointer, n.success=0L))

  successful.gain <- gain[successful] # Extracts positive DE improvements.
  weights <- successful.gain/sum(successful.gain) # Favours more productive parameters.
  successful.F <- F[successful] # Extracts successful mutation factors.
  successful.CR <- CR[successful] # Extracts successful crossover rates.
  M.F[pointer] <- sum(weights*successful.F^2)/sum(weights*successful.F)
  M.CR[pointer] <- sum(weights*successful.CR) # Uses the weighted arithmetic crossover mean.
  pointer <- if (pointer == length(M.F)) 1L else pointer+1L # Advances the circular memory.

  return(list(M.F=M.F, M.CR=M.CR, pointer=pointer,
              n.success=sum(successful)))

} # 'cde.update.memories' end


################################################################################
##                        cde.update.scores Function                         ##
################################################################################
# Updates recent operator or coordinate rewards from evaluated proposals.
cde.update.scores <- function(scores, labels, gain, learning.rate=0.2) {

  for (label in names(scores)) {
    selected <- !is.na(labels) & labels == label & is.finite(gain) # Locates proposals from this source.
    if (any(selected)) {
      reward <- mean(gain[selected]) # Includes failed proposals as zero improvement.
      scores[label] <- (1-learning.rate)*scores[label] + learning.rate*reward
    } # IF source was evaluated end
  } # FOR label end
  scores <- pmax(scores, .Machine$double.eps) # Keeps probability ratios well defined.

  return(scores)

} # 'cde.update.scores' end


################################################################################
##                      dds.dimension.mask Function                           ##
################################################################################
# Selects the incumbent dimensions perturbed at the current DDS evaluation.
dds.dimension.mask <- function(n, t, tmax) {

  t <- min(max(t, 1), tmax) # Keeps the logarithmic schedule inside its budget.
  probability <- if (tmax <= 1) 0 else 1 - log(t) / log(tmax) # Computes the DDS probability.
  mask <- stats::runif(n) < probability # Selects each dimension independently.

  if (!any(mask))
    mask[sample.int(n, size=1)] <- TRUE # Guarantees that at least one dimension moves.

  return(mask)

} # 'dds.dimension.mask' end


################################################################################
##                         dds.reflect Function                               ##
################################################################################
# Reflects a DDS proposal into finite lower and upper parameter bounds.
dds.reflect <- function(x, lower, upper) {

  width <- upper - lower # Computes each dimension's finite search width.
  reflected <- lower # Keeps fixed dimensions at their only feasible value.
  varying <- width > 0 # Avoids undefined modulo operations for fixed dimensions.
  phase <- (x[varying] - lower[varying]) %% (2 * width[varying])
  reflected[varying] <- ifelse(phase <= width[varying], lower[varying] + phase,
                               upper[varying] - (phase - width[varying]))

  return(reflected)

} # 'dds.reflect' end


################################################################################
##                        pdds.is.better Function                              ##
################################################################################
# Compares candidate and incumbent fitness values using the requested direction.
pdds.is.better <- function(candidate, incumbent, MinMax) {

  if (MinMax == "min") {
    out <- candidate < incumbent
  } else out <- candidate > incumbent

  return(out)

} # 'pdds.is.better' end


################################################################################
##                     pdds.update.archive Function                           ##
################################################################################
# Updates a bounded quality-diversity archive in normalised parameter space.
pdds.update.archive <- function(archive.X, archive.fit, candidates, fitness,
                                MinMax, max.size, min.distance, x.MinMax) {

  candidates <- as.matrix(candidates) # Preserves one row for every candidate solution.
  valid <- is.finite(fitness) & apply(candidates, 1, function(x) all(is.finite(x)))
  candidates <- candidates[valid, , drop=FALSE]
  fitness <- fitness[valid]

  if (is.null(archive.X)) {
    archive.X <- matrix(numeric(), nrow=0, ncol=ncol(candidates))
    archive.fit <- numeric()
  } # IF archive.X is NULL end

  combined.X <- rbind(archive.X, candidates) # Makes new candidates compete with retained elites.
  combined.fit <- c(archive.fit, fitness)
  if (length(combined.fit) == 0)
    return(list(X=combined.X, fit=combined.fit))

  ranking <- order(combined.fit, decreasing=(MinMax == "max"))
  combined.X <- combined.X[ranking, , drop=FALSE]
  combined.fit <- combined.fit[ranking]
  duplicated.position <- duplicated(as.data.frame(combined.X))
  combined.X <- combined.X[!duplicated.position, , drop=FALSE]
  combined.fit <- combined.fit[!duplicated.position]

  width <- x.MinMax[,2] - x.MinMax[,1] # Normalises mixed parameter ranges.
  width[width == 0] <- 1 # Keeps fixed dimensions numerically harmless.
  scaled <- sweep(combined.X, 2, x.MinMax[,1], FUN="-")
  scaled <- sweep(scaled, 2, width, FUN="/")
  selected <- integer()
  deferred <- integer()

  for (i in seq_len(nrow(combined.X))) {
    if (length(selected) == 0) {
      selected <- i # Always retains the best available solution.
    } else {
      difference <- sweep(scaled[selected, , drop=FALSE], 2, scaled[i, ], FUN="-")
      distance <- sqrt(rowMeans(difference^2))
      if (all(distance >= min.distance)) {
        selected <- c(selected, i)
      } else deferred <- c(deferred, i)
    } # ELSE selected archive is non-empty end
    if (length(selected) >= max.size) break
  } # FOR i end

  if (length(selected) < max.size && length(deferred) > 0) {
    fill <- deferred[seq_len(min(max.size - length(selected), length(deferred)))]
    selected <- c(selected, fill) # Ensures enough chain seeds when elites are close.
  } # IF archive needs filling end

  selected <- selected[seq_len(min(length(selected), max.size))]
  out <- list(X=combined.X[selected, , drop=FALSE], fit=combined.fit[selected])

  return(out)

} # 'pdds.update.archive' end


################################################################################
##                     pdds.generate.batch Function                           ##
################################################################################
# Generates one full parallel DDS batch from persistent multi-scale chains.
pdds.generate.batch <- function(chain.X, chain.evals, chain.r, npart, epoch,
                                x.MinMax) {

  nchains <- nrow(chain.X)
  n <- ncol(chain.X)
  chain.id <- rep(seq_len(nchains), length.out=npart)
  occurrence <- stats::ave(seq_len(npart), chain.id, FUN=seq_along)
  candidates <- chain.X[chain.id, , drop=FALSE]
  width <- x.MinMax[,2] - x.MinMax[,1]
  varying <- which(width > 0)

  if (length(varying) > 0) {
    for (i in seq_len(npart)) {
      k <- chain.id[i]
      proposal.index <- ((chain.evals[k] + occurrence[i] - 1L) %% epoch) + 1L
      local.mask <- dds.dimension.mask(n=length(varying), t=proposal.index,
                                       tmax=epoch)
      dimensions <- varying[local.mask]
      candidates[i, dimensions] <- candidates[i, dimensions] +
                                    stats::rnorm(length(dimensions), mean=0,
                                                 sd=chain.r[k]*width[dimensions])
      candidates[i, ] <- dds.reflect(candidates[i, ], lower=x.MinMax[,1],
                                     upper=x.MinMax[,2])
    } # FOR i end
  } # IF varying dimensions exist end

  evaluations <- tabulate(chain.id, nbins=nchains)
  next.evals <- (chain.evals + evaluations) %% epoch
  out <- list(X=candidates, chain=chain.id, evals=next.evals)

  return(out)

} # 'pdds.generate.batch' end


################################################################################
##                             roll.vector Function                           ##
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Created: 2009                                                                #
# Updates:                                                                     #
################################################################################
# 'old.vector': vector of length 'L' that will be rolled
# 'new.value' : real that will be put at the end of 'old.vector'

# Result: a vector with elements x.new[i] = x.old[i+1], i=1,L-1, x.new[L]= 'new.value'
roll.vector <- function(old.vector, new.value) {
  tmp	<- old.vector
  L		<- length(old.vector)  
  tmp[1:(L-1)]	<- old.vector[2:L]
  tmp[L]        <- new.value
  
  return(tmp)

} # 'roll.vector' end


################################################################################
##                       compute.value.with.iter Function                     ##
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Created: 2008                                                                #
# Updates: 29-Oct-2012                                                         #
################################################################################
# 'iter'    : the current iteration number
# 'niter'   : maximum number of iteration that can be done within a single run
# 'nexp'    : nonlinear modulation index
# 'val.ini' : initial inertial weight at the start of a given run
# 'val.fin' : fiinal inertial weight at the end of a given run

# Result    : Nonlinear modulated inertial weight
compute.value.with.iter <- function(iter, niter, nexp, val.ini, val.fin) {

  w <- ( ( (niter - iter) / niter )^nexp ) * ( val.ini - val.fin) + val.fin

} # 'compute.value.with.iter' end


################################################################################
##                         compute.w.aiwf Function                            ##
##                 Adaptive inertial weight factor (AIWF)                     ##
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                        ##
################################################################################
# Started: 2008                                                               ##
# Updates: 24-Nov-2011                                                        ##
#          22-Oct-2012 ; 28-Oct-2012                                          ##
################################################################################
# Reference:
# According to Liu et al., 2005 ("Improved Particle Swarm Combined with Caos")
# B. Liu, et al., "An improved particle swarm optimization combined with chaos," 
# Chaos, Solitons and Fractals, vol. 25, pp. 1261-1271, 2005.

# 'iter.fit'    : vector with 'n.particles' values corresponding to the fitness
#                 obtained in the current iteration for each one of the particles
# 'particle.pos': position of the particle, for identifying it
# 'gbest.fit'   : number with the best fitness found so far, considering all the particles
# 'w.max'       : initial inertial weight at the start of a given run (maximum value)
# 'w.min'       : final inertial weight at the end of a given run (minimum value)
# 'MinMax'      : string indicating if PSO have to find a minimum or a maximum for the fitness function.
#                 valid values are: "min", "max"

# Result        : Adaptive inertial weight factor (AIWF)
  
compute.w.aiwf <- function(iter.fit, particle.pos, gbest.fit, w.max, w.min, MinMax) {
  # 'f' : fitness value of the particle in the current iteration
  f <- iter.fit[particle.pos]
  
  # 'f.avg': mean fitness value of all the particles in the current iteration
  f.avg <- mean(iter.fit, na.rm=TRUE) 
  
  # 'f.min': best fitness value of all the particles in the current iteration
  if(MinMax == "min") {
    f.min <- min(iter.fit, na.rm=TRUE)
   } else f.min <- max(iter.fit, na.rm=TRUE)
  
  if(MinMax == "min") {
    to.apply <- f <= f.avg
  } else to.apply <- f >= f.avg
  
  if ( (to.apply) & (abs(f.avg - f.min)!=0) ) {
    w <- w.min + ( ( (w.max - w.min) * abs(f - f.min) ) / abs(f.avg - f.min) )
  } else w <- w.max
  
  return(w)

} # 'compute.w.aiwf' end



################################################################################
##                      compute.w.with.GLratio Function                       ##
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 23-Dec-2010                                                         #
# Updates: 28-Oct-2012                                                         #
################################################################################
compute.w.with.GLratio <- function(MinMax, gbest.fit, pbest.fit) {
  
  # If we are Minimizing, the ratio 'gbest/pbest' have to be less than 1,
  # and the closer to 1, the closer the particle to 'gbest'
  if(MinMax == "min") { 
    w <- 1.1 - ( gbest.fit / mean(pbest.fit) )
  } else w <- 1.1 - ( mean(pbest.fit) / gbest.fit )
  
  return(w)

} # 'compute.w.with.GLratio' END


################################################################################
##                       compute.c1.with.GLratio Function                     ##
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 27-Dec-2010                                                         #
# Updates: 28-Oct-2012                                                         #
################################################################################
# Based on M. Senthil Arumugam and M.V.C. Rao; 2008. Applied Soft Computing
# "On the improved Performances of the particle swarm optimization algorithms
#  with adaptive parameters, cross-over operators and root mean square (RMS) 
#  variants for computing optimal control of a class of hybrid systems"

# 'gbest.fit': global best objective function
# 'pbest.fit': best objective function of the current particle
compute.c1.with.GLratio <- function(MinMax, gbest.fit, pbest.fit) {
  
  # If we are Minimizing, the ratio 'gbest/pbest' have to be less than 1,
  # and the closer to 1, the closer the particle to 'gbest'
  if(MinMax == "min") { 
     c1 <- 1.0 + ( gbest.fit / pbest.fit )
  } else c1 <- 1.0 + ( pbest.fit / gbest.fit )
  
  return(c1)

} # 'compute.c1.with.GLratio' END


################################################################################
##                    velocity.boundary.treatment Function                    ##
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 2008                                                                #
# Updates:                                                                     #
################################################################################
# 'x' 		  : vector of 'n' parameters, corresponding to one particle
# 'n'             : dimension of the solution space (number of parameters)
# 'X.MinMax'      : string indicating if PSO have to find a minimum or a maximum 
#                   for the fitness function.
#                   valid values are: "min", "max"
# 'v'             : vector of 'n' velocities of each parameter, corresponding to 
#                   the current particle
# 'boundary.wall' : boundary treatment that is used to limit the sea
#                   the limits given by 'X.MinMax'.
#                   Valid values are: 'absorbing', 'reflecting' and 'invisible'

# Result          : vector of 'n' velocities, one for each parameter, 
#                   corresponding to the current particle
velocity.boundary.treatment <- function(v, vmax ) {	
  
  byd.vmax.pos <- which( abs(v) > vmax )
  if ( length(byd.vmax.pos) > 0 ) 
       v[byd.vmax.pos] <- sign(v[byd.vmax.pos])*abs(vmax[byd.vmax.pos])
  
  return(v)
	
} # 'velocity.boundary.treatment' end


################################################################################
##                  position.update.and.boundary.treatment Function           ##
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 2008                                                                #
# Updates: Nov-2011                                                            #
#          23-Sep-2012 ; 29-Oct-2012                                           #
################################################################################
# 'x'             : vector of 'n' parameters, corresponding to one particle
# 'X.MinMax'      : string indicating if PSO have to find a minimum or a maximum 
#                   for the fitness function.
#                   valid values are: "min", "max"
# 'v'             : vector of 'n' velocities of each parameter, corresponding to 
#                   the current particle
# 'boundary.wall' : boundary treatment that is used to limit the sea
#                   the limits given by 'X.MinMax'.
#                   Valid values are: 'absorbing', 'reflecting', 'invisible', 'damping'

# Result          : vector of 'n' velocities, one for each parameter, 
#                   corresponding to the current particle

# References:
# Robinson, J.; Rahmat-Samii, Y.; Particle swarm optimization in electromagnetics. 
# Antennas and Propagation, IEEE Transactions on , vol.52, no.2, pp. 397-407, 
# Feb. 2004. doi: 10.1109/TAP.2004.823969

# Huang, T.; Mohan, A.S.; , A hybrid boundary condition for robust particle
# swarm optimization. Antennas and Wireless Propagation Letters, IEEE , vol.4, 
# no., pp. 112-117, 2005. doi: 10.1109/LAWP.2005.846166
position.update.and.boundary.treatment <- function(x, v, x.MinMax, boundary.wall) {
 
 # Vector with the new positions of the current particle
 x.new <- x + v
 
 # By default the new velocity is assumed not to be limited
 v.new <- v
 
 # Minimum and maximum values for each dimension
 x.min <- x.MinMax[,1]
 x.max <- x.MinMax[,2]
 
 byd.min.pos <- which(x.new < x.min)
 if ( length(byd.min.pos) > 0) { 
    if ( boundary.wall == "absorbing2011") {     
       x.new[byd.min.pos] <- x.min[byd.min.pos]
       v.new[byd.min.pos] <- -0.5*v[byd.min.pos]      
    } else if ( boundary.wall == "absorbing2007") {     
         x.new[byd.min.pos] <- x.min[byd.min.pos]
         v.new[byd.min.pos] <- 0*v[byd.min.pos]      
      } else if ( boundary.wall == "reflecting") {    
           x.new[byd.min.pos] <- 2*x.min[byd.min.pos] - x.new[byd.min.pos] 
           v.new[byd.min.pos] <- -v[byd.min.pos]
      } else if ( boundary.wall == "invisible") {
             x.new[byd.min.pos] <- x[byd.min.pos]
             v.new[byd.min.pos] <- v[byd.min.pos]
        } else if ( boundary.wall == "damping") {
             L                  <- abs( x.min[byd.min.pos] - x.new[byd.min.pos] )
             x.new[byd.min.pos] <- x.min[byd.min.pos] + runif(1)*L
             v.new[byd.min.pos] <- -v[byd.min.pos]
        }# ELSE end
 } # IF end
      
 byd.max.pos <- which( x.new > x.max )
 if ( length(byd.max.pos) > 0 ) {	 
    if ( boundary.wall == "absorbing2011") { 
       x.new[byd.max.pos] <- x.max[byd.max.pos]
       v.new[byd.max.pos] <- -0.5*v[byd.max.pos] 
    } else if ( boundary.wall == "absorbing2007") { 
        x.new[byd.max.pos] <- x.max[byd.max.pos]
        v.new[byd.max.pos] <- 0*v[byd.max.pos] 
      } else if ( boundary.wall == "reflecting") {
           x.new[byd.max.pos] <- 2*x.max[byd.max.pos] - x.new[byd.max.pos] 
           v.new[byd.max.pos] <- -v[byd.max.pos]
        } else if ( boundary.wall == "invisible") {
             x.new[byd.max.pos] <- x[byd.max.pos]
             v.new[byd.max.pos] <- v[byd.max.pos]
          } else if ( boundary.wall == "damping") {
              L                  <- abs( x.new[byd.max.pos] - x.max[byd.max.pos])
              x.new[byd.max.pos] <- x.max[byd.max.pos] - runif(1)*L
              v.new[byd.max.pos] <- -v[byd.max.pos]
            }# ELSE end
 } # IF end
 
 out <- list(x.new=x.new, v.new=v.new)

} # 'position.update.and.boundary.treatment' end


################################################################################
##                    async.update.pgbests Function                           ##
################################################################################
## Author : Mauricio Zambrano-Bigiarini                                       ## 
################################################################################
## Started: 2008                                                              ##
## Updated: 26-Jan-2012 ; 28-Oct-2012                                         ##
################################################################################
# Function for updating the values of 'pbest', 'x.best', 'gbest.fit' and 'gbest.pos'
# for ONLY 1 particle !!

# 'x'           : vector with the 'n' parameters of a single particle
# 'x.pos'       : position of the current particle
# 'xt.fitness'  : particle's fitness
# 'MinMax'      : string indicating if PSO have to find a minimum or a maximum 
#                 for the fitness function.
#                 valid values are: "min", "max"
# 'of.name'     : function that will be used for computing the fitness.
# 'l.pbest.fit' : best fitness value found by the particle so far.
# 'gbest.fit'   : best fitness value found by the swarm so far.
# 'gbest.pos'   : position of the particle corresponding to 'gbest.fit'.
# 'x.best'      : 'n' values corresponding to the parameters of the particle 
#                 that achieve the best fitness value for the particle

# Result        : a list of 4 components (this values are different of the input ones,
#                 only if the new values provide a better fitness value )
#               : 1: pbest      : real with the new best fitness value of the particle
#               : 2: x.best     : vector with the new 'n' best parameters for the particle
#               : 3: gbest.fit  : real with the new best fitness value for the swarm
#               : 4: gbest.pos  : integer with the location of the particle that has
#                                 the best fitness value in the swarm

async.update.pgbests <- function(x, 
                                 x.pos, 
                                 xt.fitness,
                                 MinMax, 
                                 l.pbest.fit, 
                                 gbest.fit, 
                                 gbest.pos, 
                                 x.best
                                 ) {
  
  if(MinMax == "max") {
    l.update <- which(xt.fitness > l.pbest.fit )
  } else l.update <- which(xt.fitness < l.pbest.fit )
  
  # Updating 'pbest.fit', 'gbest.fit', 'gbest.pos' and 'x.best.part'
  if ( length(l.update>0) ) {
  
    # 'pbest.fit': updating the value of the best fit of the particles
    l.pbest.fit <- xt.fitness
    
    # 'X.best' updating
    x.best <- x  
    
    # 'gbest.fit': updating the value of the best global fit
    # 'gbest.pos': updating the position of the particle with the best global fit
    if ( MinMax=="max" ) {
      if ( l.pbest.fit > gbest.fit ) { 
          gbest.fit <- l.pbest.fit 
          gbest.pos <- x.pos 
      } # IF end 
    } else if ( MinMax=="min" ) {
        if ( l.pbest.fit < gbest.fit ) { 
           gbest.fit <- l.pbest.fit
           gbest.pos <- x.pos 
        } # IF end 
      } # ELSE end 
  
  } # IF end  
                
  tmp      <- list(4)
  tmp[[1]] <- l.pbest.fit
  tmp[[2]] <- x.best
  tmp[[3]] <- gbest.fit
  tmp[[4]] <- gbest.pos 
  names(tmp)  <- c("pbest", "x.best", "gbest.fit", "gbest.pos") 
                
  return(tmp)
                 
} # 'async.update.pgbests' end


################################################################################
##                        sync.update.pgbests Function                        ##
################################################################################
## Author : Mauricio Zambrano-Bigiarini                                       ## 
################################################################################
## Started: 2008                                                              ##
## Updated: 27-Jan-2012 ; 28-Oct-2012                                         ##
################################################################################
# Function for updating the values of 'pbest', 'x.best', 'gbest.fit' and 'gbest.pos'
# for the ALL the SWARM !!

# 'x'                : Matrix with the 'n' parameters of all the particles
# 'xt.fitness'       : numeric vector of 'n.particles', with the current fitness 
#                      obtained by all the particles in the swarm
# 'MinMax'           : Character indicating if PSO have to find a minimum or a 
#                      maximum for the fitness function.
#                      Valid values are in: c('min', 'max')
# 'pbest.fit'        : numeric vector with length equal to 'n.particles', with the best 
#                      fitness value found by each particle in the swarm so far.
# 'gbest.fit'        : numeric, with the best fitness value found by the swarm so far.
# 'gbest.pos'        : numeric, with the position of the particle corresponding 
#                      to 'gbest.fit'.
# 'x.best'           : Matrix of 'n*n.particles' values, corresponding to the
#                      'n' parameters of each one of the particles in the swarm 
#                      that achieve the best fitness value for the particle

# Result: a list of 4 components (this values are different of the input ones,
#         only if the new values provide a better fitness value )
#       : 1: pbest.fit  : numeric vector with the new best fitness value of each 
#                         particle
#       : 2: x.best     : matrix with the new 'n' best parameters for each particle
#       : 3: gbest.fit  : numeric (single value) with the new best fitness value 
#                         for the swarm
#       : 4: gbest.pos  : integer with the location of the particle that has the 
#                         best fitness value in the swarm   

sync.update.pgbests <- function(x, 
                                xt.fitness, 
                                MinMax, 
                                pbest.fit, 
                                gbest.fit, 
                                gbest.pos, 
                                x.best
                                ) {
  
  # index of all the particles which current fit is better than their last 'pbest'
  if(MinMax == "max") {
    better.index <- which( xt.fitness > pbest.fit )
  } else better.index <- which( xt.fitness < pbest.fit )
                          
  # if it exists some particles that have a better fitness value
  if (length(better.index) > 0) {
  
    # 'pbest.fit': updating the value of the best fit for each particle
    pbest.fit[ better.index ] <- xt.fitness[better.index ]
    
    # 'X.best.part': updating the value of the best parameters for the particles 
    #                with better fitness
    x.best[better.index, ] <- x[better.index, ]  
    
    # 'gbest.fit': updating the value of the best global fit
    # 'gbest.pos': updating the position of the particle with the best global fit
    if ( MinMax == "max" ) {    
      if ( max(pbest.fit, na.rm=TRUE) > gbest.fit ) { 
          gbest.pos <- which.max(pbest.fit) 
          #gbest.fit <- max(xt.fitness[better.index ])          
          gbest.fit <- pbest.fit[gbest.pos]
      } # IF end          
    } else if ( MinMax == "min" ) {    
          if ( min(pbest.fit, na.rm=TRUE) < gbest.fit ) { 
              gbest.pos <- which.min(pbest.fit) 
              #gbest.fit <- min(xt.fitness[better.index ])
              gbest.fit <- pbest.fit[gbest.pos]
          } # IF end          
      } # ELSE end 
                                                   
  } # IF end   
  
  # Creating the output
  tmp      <- list(4) 
  tmp[[1]] <- pbest.fit
  tmp[[2]] <- x.best
  tmp[[3]] <- gbest.fit
  tmp[[4]] <- gbest.pos 
  names(tmp)  <- c("pbest", "x.best", "gbest.fit", "gbest.pos") 
                
  return(tmp)
  
} # 'sync.update.pgbests' end


################################################################################
#                          computeCurrentXmaxMin                               #
################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
# Started: 22-Dec-2010                                                         #
# Updates: 28-Oct-2012                                                         #
################################################################################
# Purpose: To compute the minimum parameter range currently embraced for the best
#          positions found so far in the swarm
################################################################################
# 'x.best.part': matrix with the best parameter values for each particle.
#                nrows = number of particles
#                ncols = number of parameters = n 
computeCurrentXmaxMin <- function(x.best.part) {

  # number of parameters
  n <- ncol(x.best.part)

  x.min <- sapply(1:n, function(i,y) { min(y[,i], na.rm=TRUE) }, y = x.best.part)
  x.max <- sapply(1:n, function(i,y) { max(y[,i], na.rm=TRUE) }, y = x.best.part)
	
  return (cbind(x.min, x.max))
                              
}  # 'computeCurrentXmaxMin' END


################################################################################
#                      decrease.search.space Function                          #
################################################################################
## Author : Mauricio Zambrano-Bigiarini                                       ## 
################################################################################
## Started: 2010                                                              ##
## Updated:                                                                   ##
################################################################################
# Decrease the search space according to the methodology proposed in the CPSO #

# 'Lmin.min'        : vector of 'n' elements, with the minimum value that each dimension
#                     take within the minimum searching space defined by the user
# 'Lmin.max'        : vector of 'n' elements, with the maximum value that each dimension
#                     take within the minimum searching space defined by the user
# 'Lmin'            : vector of 'n' elements, with the minimum length that each dimension
#                     can take in the searching space defined by the user
# 'x.MinMaxCurrent' : Matrix with the minimum and maximum values for each dimension 
#                     during the current iteration
#                     Rows = 'n' (number of parameters)
#                     Columns = 2, 
#                     First column has the minimum possible value for each parameter
#                     Second column has the maximum possible value for each parameter
# 'x.MinMaxRange'   : Matrix with the valid range for each parameter of 'X', as 
#                     defined by the user
#                     Rows = 'n' (number of parameters)
#                     Columns = 2, 
#                     First column has the minimum possible value for each parameter
#                     Second column has the maximum possible value for each parameter
# 'x.best'          : vector of 'n' elements with the parameters of the particle 
#                     with the best fitness value
# 'r'               : real between 0 and 1.  Only required when 'use.CLS' =TRUE
#                     represents the rate that is used for decreasing the 
#                     searching space in the chaotic implementation 

# Result            : vector with the 'n' parameters of only 1 particle
decrease.search.space <- function(Lmin, x.MinMaxCurrent, x.MinMaxRange, x.best, r) {
  
  # name of each parameter  
  param.IDs <- row.names(x.MinMaxRange)
  
  # number of dimensions
  n <- length(param.IDs)

  # Removing possible attributes
  x.best     <- as.numeric( x.best ) 
  x.min.curr <- as.numeric( x.MinMaxCurrent[ ,1] )
  x.max.curr <- as.numeric( x.MinMaxCurrent[ ,2] )
  x.min.rng  <- as.numeric( x.MinMaxRange[ ,1] )
  x.max.rng  <- as.numeric( x.MinMaxRange[ ,2] )

  # Current lenght of the parameter space in each dimension
  Lcurr <- x.max.curr - x.min.curr 

  # New desired lenght of the parameter space in each dimension
  Lnew  <- r*Lcurr  

  # Creating the new minimum and maximum boundaries
  x.min.new <- x.min.curr
  x.max.new <- x.max.curr

  # Updating the boundaries when the new length is larger than Lmin
  for (i in 1:n) {
    if (Lnew[i] >= Lmin[i]) {
      x.min.new[i] <- max( x.min.curr[i], x.best[i] - 0.5*Lnew[i] )
      x.max.new[i] <- min( x.max.curr[i], x.best[i] + 0.5*Lnew[i] )
      #x.min.new[i] <- max( x.min.rng[i], x.best[i] - 0.5*Lnew[i] )
      #x.max.new[i] <- min( x.max.rng[i], x.best[i] + 0.5*Lnew[i] )
    } # IF end      
  } # FOR end

  # New lenght of the parameter space in each dimension
  Lnew  <- x.max.new - x.min.new 

  #x.min.new <- pmax(x.min.rng, x.best - 0.5*Lnew )
  #x.max.new <- pmin(x.max.rng, x.best + 0.5*Lnew )
  
  x.MinMaxCurrent[ ,1] <- x.min.new
  x.MinMaxCurrent[ ,2] <- x.max.new
  
  # Relative change achieved in each dimension
  rel.change        <- (Lnew-Lcurr)/Lcurr
  names(rel.change) <- param.IDs 
  
  return(x.MinMaxCurrent) 

} # 'decrease.search.space' end 
#decrease.search.space(Lmin, x.MinMaxCurrent, x.MinMaxRange, x.best, r)


################################################################################
#                            InitializateX                                     #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
# Started: 23-Dec-2010                                                         #
# Updates: 24-Dec-2010                                                         #
#          28-Oct-2012                                                         #
#          15-Nov-2020                                                         #
#          22-May-2026 ; 25-May-2026                                           #
################################################################################
# Purpose: Function for the initialization of the position and the velocities  # 
#          of all the particles in the swarm                                   #
################################################################################
# -) npart     : number of particles
# -) X.MinMax  : Matrix with the minimum and maximum values for each dimension 
#                during the current iteration
#              -) Rows   : 'n' (number of parameters)
#              -) Columns: 2 
#                 First column has the minimum possible value for each parameter
#                 Second column has the maximum possible value for each parameter
# 'x.ini.type' : character, indicating how to carry out the initialization 
#                of the position of all the particles in the swarm
#                valid values are in c('random', 'lhs', 'sobol') 
InitializateX <- function(npart, x.MinMax, x.ini.type=c('random', 'lhs', 'sobol')) {
 
  # Checking 'x.ini.type'
  x.ini.type <- match.arg(x.ini.type)

  # 'X'              : Output matrix with parameter sets. 
  # Number of Rows   : 'npart' (number of particles in the swarm)
  # Number of Columns: 'n'     (dimension of the Solution Space)
  if ( x.ini.type=="random" ) {
      X <- Random.Bounded.Matrix(npart, x.MinMax)
  } else if ( x.ini.type=="lhs" ) {
      X <- rLHS(npart, x.MinMax)
    } else X <- rSobol(npart, x.MinMax)   

  # ensuring that the output object X is always a matrix, 
  # even for a single parameter being optimised
  if (nrow(x.MinMax)==1) 
    X <- matrix(X, nrow=npart)

  colnames(X) <- rownames(x.MinMax)  

  return(X)

} # InitializateX


################################################################################
#                            InitializateV                                     #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
# Started: 24-Dec-2010                                                         #
# Updates: 24-Nov-2011                                                         #
#          17-Sep-2012 ; 28-Oct-2012                                           #
################################################################################
# Purpose: Function for the initialization of the position and the velocities  #
#          of all the particles in the swarm                                   #
################################################################################
# -) npart     : number of particles
# -) X.MinMax  : Matrix with the minimum and maximum values for each dimension 
#                during the current iteration
#              -) Rows = 'n' (number of parameters)
#              -) Columns = 2, 
#                 First column has the minimum possible value for each parameter
#                 Second column has the maximum possible value for each parameter
# 'v.ini'      : character, indicating how to carry out the initialization 
#                of the velocitites of all the particles in the swarm
#                valid values are in c('zero', 'random2007', 'lhs2007', 'random2011', 'lhs2011') 
InitializateV <- function(npart, x.MinMax, v.ini.type, Xini) {

  # Number of parameters
  n <- nrow(x.MinMax)
 
  # 'V' #
  # Matrix of velocities for each particle and iteration. 
  # Rows    = 'npart'; 
  # Columns = 'n' (Dimension of the solution space)
  # Random bounded values are assigned to each dimension
  
  if (v.ini.type %in% c("random2011", "lhs2011") ) {
    lower <- matrix( rep(x.MinMax[,1], npart), nrow=npart, byrow=TRUE)
    upper <- matrix( rep(x.MinMax[,2], npart), nrow=npart, byrow=TRUE)
    
    if ( v.ini.type=="random2011" ) {
      V <- matrix(runif(n*npart, min=as.vector(lower-Xini), max=as.vector(upper-Xini)), nrow=npart)
    } else if ( v.ini.type=="lhs2011" ) {
        # LHS initialization for all the particles, with a value in [0,1]
        V <- randomLHS(npart, n) 

        # Transforming V into the real range defined by SPSO-2011
        lower <- lower - Xini
        upper <- upper - Xini

        V <- lower + (upper-lower)*V  
      } # ELSE end 
  } else if ( v.ini.type=="random2007" ) {
        V <- ( Random.Bounded.Matrix(npart, x.MinMax) - Xini ) / 2
      } else if ( v.ini.type=="lhs2007" ) {
          V <- ( rLHS(npart, x.MinMax) - Xini ) / 2
        } else if ( v.ini.type=="zero" ) {
            V <- matrix(0, ncol=n, nrow=npart, byrow=TRUE)    
          } # ELSE end

  return(V)

} # InitializateV



################################################################################
##                            UpdateLocalBest                                 ##
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                        ##
# Started: 24-Dec-2010                                                        ##
# Updates: 29-Dec-2010 ;                                                      ##
#          14-Nov-2011 ; 27-Jan-2011                                          ##
#          28-Oct-2012 ; 29-Oct-2012                                          ##
################################################################################
# Purpose: Function for computing the best value in the neighbourhood of each 
#          particle
################################################################################
# pbest.fit    : numeric, with best fitness in the history of each particle
# LocalBest.fit: numeric, best fitness in the history of the neighbourhood of 
#                each particle
# x.neighbours : numeric matrix, with the index/position of the particles that 
#                are considered "informants" of each particle.
#                rows   = number of particles
#                columns= K+1 ; K: nmbr of informants provided by the user
# MinMax       : character, indicating if PSO have to find a minimum or a 
#                maximum for the fitness function.
#                Valid values are in: c('min', 'max')
 
UpdateLocalBest <- function(pbest.fit, localBest.pos, localBest.fit, x.neighbours, MinMax) {

  # Number of particles
  npart <- nrow(x.neighbours)  
  
  for ( i in 1:npart ) {
  
    # index of all the particles that are "neighbours" to the particle 'i'
    neighs.index <- x.neighbours[i,]
    
    # if one or more of the "neighbours" have a better fitness than the current Local Best
    if(MinMax == "max") { 
      best.neigh.index <- which.max( pbest.fit[neighs.index] )
      if (pbest.fit[best.neigh.index] > localBest.fit[i] ) {
        localBest.pos[i] <- neighs.index[best.neigh.index ]
        localBest.fit[i] <- pbest.fit[localBest.pos[i]]
      } # IF end
    } else {
        best.neigh.index <- which.min( pbest.fit[neighs.index] )
        if ( pbest.fit[neighs.index][best.neigh.index] < localBest.fit[i] ) {
          localBest.pos[i] <- neighs.index[best.neigh.index]
          localBest.fit[i] <- pbest.fit[localBest.pos[i]]
        } # IF end
      } # ELSE end
    
  } # FOR end 
  
  out <- list(localBest.pos=localBest.pos, localBest.fit=localBest.fit)

} # UpdateLocalBest


################################################################################
#                            UpdateNgbest                                      #
################################################################################
# Author : Mauricio Zambrano-Bigiarini
# Started: 24-Dec-2010
# Updates: 29-Dec-2010
#          28-Oct-2012
################################################################################
# Purpose: Function for computing the 'n.neighbours' best values in the swarm, 
#          and its positions
################################################################################
# pbest.fit : numeric, with best fitness in the history of each particle
# ngbest    : Number of local best that have to be considered
# MinMax    : character, indicating if PSO have to find a minimum or a 
#             maximum for the fitness function.
#             Valid values are in: c('min', 'max')
 
UpdateNgbest <- function(pbest.fit, ngbest, MinMax) {
  
  if(MinMax=="max") {
    sorted.fit <- sort(pbest.fit, decreasing= TRUE)
  } else sorted.fit <- sort(pbest.fit, decreasing= FALSE)
                            
  # Ordered index of all the particles, 
  # MinMax=="max" => Decreasing order
  # MinMax=="min" => Increasing order
  sorted.index <- pmatch(sorted.fit, pbest.fit)
  
  ngbest.fit <- pbest.fit[sorted.index][1:ngbest]  
  ngbest.pos <- sorted.index[1:ngbest]    

  # Creating the output
  out <- list(2)
  out[[1]] <- ngbest.fit
  out[[2]] <- ngbest.pos
  names(out) <- c("ngbest.fit", "ngbest.pos")
  
  return(out)

} # UpdateNgbest


################################################################################
#                    ComputeSwarmRadiusAndDiameter                             #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
# Started: 12-Jan-2011                                                         #
# Updates: 12-Jan-2011 ; 28-Oct-2011                                           #
#          06-Nov-2012 ; 07-Nov-2012                                           #
################################################################################
# Purpose: Function for computing the swarm radius, for detecting premature    #
#          convergence, in order to avoid stagnation                           #
################################################################################
# X      : matrix, with the current parameter values for all the particles
# gbest  : numeric, with the parameter values for the best particle in the swarm
#          Valid values are in: c('min', 'max')
# Lmax   : vector with the range of the search space in each dimension
 
ComputeSwarmRadiusAndDiameter <- function(x, gbest, Lmax) {
  
  # number of parameters
  n <- ncol(x)
  
  # number of particles
  npart <- nrow(x)
  
  # Euclidean distance, in the n-dimensional space for all the particles
  radius <- numeric(npart)
  
  gbest  <- matrix(rep(gbest, npart), nrow=npart, byrow=TRUE)
  radius <- sqrt( rowSums( (x-gbest)^2, na.rm=TRUE) )
  
  #swarm.radius   <- max(radius, na.rm=TRUE)
  swarm.radius   <- median(radius, na.rm=TRUE)
  swarm.diameter <- sqrt(sum(Lmax^2))

  out        <- list(2)
  out[[1]]   <- swarm.radius
  out[[2]]   <- swarm.diameter
  names(out) <- c("swarm.radius", "swarm.diameter")
  
  return( out )

} # ComputeSwarmRadiusAndDiameter


################################################################################
#                           RegroupingSwarm                                    #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
# Started: 13-Jan-2011                                                         #
# Updates: 18-Nov-2011                                                         #
#          06-Nov-2012 ; 07-Nov-2012 ; 08-Nov-2012                             #
################################################################################
# Purpose: Function for regrouping the swarm in a search space centered around #
#          the global best, which is hoped to be both, small enough for        #
#          efficient search and large enough to allow the swarm to escape from #
#          the current local best.                                             #
#          There are 4 differences wrt Evers and Ghalia 2009:                  #
#          -) swarm radius: median is used instead of max                      #
#          -) computation of the new range of parameter space, which           #
#             corresponds to the maximum boundary of all the swarm, instead of #
#             abs(x-Gbest)                                                     #
#          -) regrouping factor: user-defined instead of 6/(5*ro)              #
#          -) velocity is re-initialized using Vini.type instead of using the  #
#             formula proposed by Evers and Ghalia 2009                        #
################################################################################
# Reference: Evers, G.I.; Ben Ghalia, M. 2009. Regrouping particle swarm       #
#            optimization: A new global optimization algorithm with improved   #
#            performance consistency across benchmarks.                        #
#            Systems, Man and Cybernetics, 2009. SMC 2009.                     #
#            IEEE International Conference on, vol., no., pp.3901-3908,        #
#            DOI: 10.1109/ICSMC.2009.5346625                                   #
################################################################################
RegroupingSwarm <- function(x, 
                            xini.type, 
                            v, 
                            vini.type,
                            gbest, 
                            x.Range,
                            Lmax,
                            RG.thr,
                            RG.r) {

  # number of parameters
  n <- ncol(x)
  
  # number of particles
  npart <- nrow(x)

  # Regrouping factor 
  rf <- RG.r          # user-defined
  #rf <- 6/(5*RG.thr) # Evers & Ghalia
  
  # name of each parameter  
  param.IDs <- row.names(x.Range)

  # Removing possible attributes
  x.min.rng  <- as.numeric( x.Range[ ,1] )
  x.max.rng  <- as.numeric( x.Range[ ,2] )
  
  # Computing current boundaries for the whole swarm
  xmin    <- apply(x, MARGIN=2, FUN=min) 
  xmax    <- apply(x, MARGIN=2, FUN=max) 
  xMinMaxO <- cbind(xmin, xmax)  
  #message("Boundaries0  :")
  #print(xMinMaxO)
  
  # Maximum length of the parameter space in each dimension
  RangeO <- xmax - xmin   
  #message("RangeO  :")
  #print(RangeO)

  # Transforming the 'gbest' into a matrix, in order to make easier some 
  # further computations
  Gbest <- matrix(rep(gbest, npart), nrow=npart, byrow=TRUE)

  # New desired length of the parameter space in each dimension
  # Is equal to the product of the regrouping factor with the maximum distance of 
  # each particle to the global best, for each dimension
  #RangeNew <- rf * apply( abs(x-Gbest), MARGIN=2, FUN=max) ## Evers & Ghalia
  RangeNew <- rf * abs(xmax - xmin)                        ## MZB
  
  # Making sure that the new range for each dimension is no larger than the original one
  RangeNew <- pmin(abs(x.max.rng - x.min.rng), RangeNew)  
  #message("RangeNew:")
  #print(RangeNew)

  # Re-initializing particle's positions around gbest
  for (part in 1:npart) {
    r3        <- runif(n, 0, 1) 
    x[part, ] <- gbest + r3*RangeNew - 0.5*RangeNew
    # If needed, Clamping the particle positions to the maximum value
    x[part, ] <- pmin(x[part,], x.max.rng)
    # If needed, Clamping the particle positions to the minimum value 
    x[part, ] <- pmax(x[part,], x.min.rng)
  } # FOR end
  
  # Defining the new boundaries
  #xmin <- gbest - 0.5*RangeNew           ## Evers & Ghalia
  #xmax <- gbest + 0.5*RangeNew           ## Evers & Ghalia  
  xmin    <- apply(x, MARGIN=2, FUN=min)  ## MZB
  xmax    <- apply(x, MARGIN=2, FUN=max)  ## MZB
  xMinMax <- cbind(xmin, xmax)            ## MZB  
  #message("Gbest:")
  #print(gbest)
  #message("BoundariesNew:")
  #print(xMinMax)   
  
  # Printing old velocities
  #vmin    <- apply(v, MARGIN=2, FUN=min) 
  #vmax    <- apply(v, MARGIN=2, FUN=max) 
  #vMinMax <- cbind(vmin, vmax)
  #message("OldBoundariesV:")
  #print(vMinMax)
  
  # Re-initializing velocities
  v <- InitializateV(npart=npart, x.MinMax=xMinMax, v.ini.type=vini.type, Xini=x)
  
  # Printing new velocities
  #vmin    <- apply(v, MARGIN=2, FUN=min) 
  #vmax    <- apply(v, MARGIN=2, FUN=max) 
  #vMinMax <- cbind(vmin, vmax)  
  #message("NewBoundariesV:")
  #print(vMinMax)
  
  # Relative change achieved in each dimension
  #rel.change        <- (RangeNew-RangeO)/RangeO
  #names(rel.change) <- param.IDs 
  
  out      <- list(3)
  out[[1]] <- x
  out[[2]] <- v
  out[[3]] <- RangeNew
  names(out)  <- c("X", "V", "RangeNew") 
  
  return(out) 
  
} # 'RegroupingSwarm' end


################################################################################
#                      Random.Topology.Generation                              #
################################################################################
# Author : Mauricio Zambrano-Bigiarini
# Started: 23-Nov-2011
# Updates: 
################################################################################
# Purpose: Function for creating a random topology, as implemented on the 
#          Standard PSO 2007
################################################################################
Random.Topology.Generation <- function(npart, K, 
                                       psoout.drty, iter # only needed during testing phase
                                       ) { 

  p.avg <- 1 - (1 - 1/npart)^K
  tmp   <- matrix(runif(npart*npart, 0, 1) <= p.avg, nrow=npart, ncol=npart)
  diag(tmp) <- TRUE
  X.neighbours <- matrix(rep(NA, npart*npart), ncol=npart, nrow=npart, byrow=TRUE)
  for (i in 1:npart) {
       l <- length(which(tmp[,i]))
       X.neighbours[i, 1:l] <- which(tmp[,i])
  } # FOR end
  
  return(X.neighbours)

} # ELSE end


################################################################################
###  Function for evaluating the hydrological model for a single particle  #####
################################################################################
### Started: 21-Jun-2011                                                     ###
### Updates: 28-Jun-2011                                                     ###
###          19-Jun-2012 ; 03-Jul-2012 ; 09-Jul-2012 ; 04-Dec-2012           ###
################################################################################
hydromod.eval <- function(part, Particles, iter, npart, maxit, 
                          REPORT, verbose, digits, 
                          model.FUN, model.FUN.args, 
                          parallel, ncores, part.dirs) {

  if ( iter/REPORT == floor(iter/REPORT) ) {
    if (verbose) message("================================================================================")
    if (verbose) message( "[Iter: ", format( iter, width=4, justify="left" ), "/", maxit, 
			  ". Particle: ", format( part, width=4, justify="left" ), "/", npart, 
			  ": Starting...]" )
    if (verbose) message("================================================================================")
  } # IF end
  if (parallel!="none")         
     model.FUN.args <- modifyList(model.FUN.args, list(model.drty=part.dirs[part]) ) 
    
  # Creating the R output
  nelements <- 2        
  out       <- vector("list", nelements)

  # Evaluating the hydrological model
  model.FUN.args <- modifyList(model.FUN.args, list(param.values=Particles[part,]) ) 
  hydromod.out   <- do.call(model.FUN, as.list(model.FUN.args)) 
   
  out[[1]] <- as.numeric(hydromod.out[["GoF"]])
  out[[2]] <- hydromod.out[["sim"]]
  
  # meaningful names
  names(out)[1:nelements] <- c("GoF", "sim") 

  if ( iter/REPORT == floor(iter/REPORT) ) {
    if (verbose) message("================================================================================")
    if (verbose) message( "[Iter: ", format( iter, width=4, justify="left" ), "/", maxit,  
                          ". Particle: ", format( part, width=4, justify="left" ), "/", npart,  
                          ". Finished !.   GoF: ", format(hydromod.out[["GoF"]], scientific=TRUE, digits=digits), 
                          "]" )
    if (verbose) message("================================================================================")
    if (verbose) message("                                    |                                           ")  
    if (verbose) message("                                    |                                           ")    
  } # IF end
        
  return(out)
  
} # 'hydromod.eval' END


#####################################################################################
### Function for evaluating an R-based hydrological model for a single particle #####
#####################################################################################
### Started: 19-Nov-2020                                                          ###
### Updates:                                                                      ###
#####################################################################################
hydromodInR.eval <- function(part, 
                             Particles, 
                             model.FUN, 
                             model.FUN.args 
                             ) {

  # Creating the R output
  nelements <- 2        
  out       <- vector("list", nelements)

  # Evaluating the hydrological model
  model.FUN.args <- modifyList(model.FUN.args, list(param.values=Particles[part,]) ) 
  hydromod.out   <- do.call(model.FUN, as.list(model.FUN.args)) 
   
  out[[1]] <- as.numeric(hydromod.out[["GoF"]])
  out[[2]] <- hydromod.out[["sim"]]
  
  # meaningful names
  names(out)[1:nelements] <- c("GoF", "sim") 
        
  return(out)
  
} # 'hydromodInR.eval' END


################################################################################
#                                    P.S.O.                                    #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 2008                                                                #
# Updates: Dec-2010                                                            #
#          May-2011    ; 28-Oct-2011 ; 14-Nov-2011 ; 23-Nov-2011 ;             #
#          15-Jan-2012 ; 23-Jan-2012 ; 30-Jan-2012 ; 23-Feb-2012 ; 23-Mar-2012 #
#          14-Jun-2012 ; 15-Jun-2012 ; 03-Jul-2012 ; 06-Jul-2012               #
#          11-Jul-2012 ; 17-Jul-2012 ; 18-Jul-2012 ; 13-Sep-2012 ; 14-Sep-2012 #
#          17-Sep-2012 ; 23-Sep-2012 ; 15-Oct-2012 ; 25-Oct-2012 ; 28-Oct-2012 #
#          08-Nov-2012 ; 26-Nov-2012 ; 27-Nov-2012 ; 28-Nov-2012 ; 29-Nov-2012 #
#          19-Dec-2012                                                         #
#          07-May-2013 ; 10-May-2013 ; 28-May-2013 ; 29-May-2013               #
#          07-Feb-2014 ; 09-Abr-2014                                           #
#          29-Jan-2016 ; 09-May-2016                                           #
#          10-Jun-2018                                                         #
#          27-Feb-2020 ; 28-Feb-2020 ; 06-Mar-2020 ; 09-Mar-2020 ; 12-Mar-2020 #
#          13-Mar-2020 ; 24-Apr-2020                                           #
#          27-Jan-2022                                                         #
#          10-Jul-2024 ; 30-Nov-2024                                           #
#          18-May-2026 ; 22-May-2026 ; 23-May-2026 ; 25-May-2026 ; 27-May-2026 #
#          30-May-2026 ; 31-May-2026                                           #
################################################################################
# 'lower'           : minimum possible value for each parameter
# 'upper'           : maximum possible value for each parameter
# 'of.name'         : String with the test function that will be used for computing the fitness.
#                     Valid values are in: c('sinc', 'rosenbrock', 'sphere', 
#                     'rastrigin', 'griewank', 'schafferF6', 'hydromod')
# 'MinMax'          : character, indicating if PSO have to find a minimum or a 
#                     maximum for the fitness function.
#                     Valid values are in: c('min', 'max')
# 'npart'           : number of particles that makes the swarm. 
#                     Must be divisible by 5 (requirement for the chaotic part)
# 'maxit'           : numeric, with the maximum number of iterations
# 'c1'              : numeric, representing the cognition constant. 
#                     Encourages the exploitation of the solution space.
#                     How much the particle is influenced by the memory of his best location
# 'c2'              : numeric, representing the social constant. 
#                     Encourages the exploration of the current global best
#                     How much the particle is influenced by the rest of the swarm	
# 'use.CF'          : logical, indicating if the Clerc's Constriction Factor have to be used
#                     to ensure the convergence of the PSO algorithm
# 'lambda'          : numeric in [0,1] representing a percentage to limit
#                     the maximum velocity for each dimension, which is computed
#                     as 'vmax = Vmax.perc*(Xmax-Xmin)'
# 'par'             : OPTIONAL. numeric of length 'n' (number of parameters), 
#                     providing a first guess for the parameters to be optimised. 
#                     If it is not present, all the particles are randomly initialized,
#                     according to the value of \code{Xini.type}
# 'Xini.type'       : character, indicating how to initialise the position of all
#                     the particles in the swarm, within the ranges defined by 
#                     \code{X.Boundaries}. Valid values are: \cr
#                     -) "random": random initialisation
#                     -) "lhs"   : Latin Hypercube initialisation. \bold{it 
#                                  requires the \pkg{lhs} package}
# 'Vini.type        : character, indicating how to initialise the velocity of all
#                     the particles in the swarm, within the ranges defined by 
#                     \code{X.Boundaries}. Valid values are: \cr
#                     -) 0       : all the particles are initialised with zero velocity
#                     -) "random": random initialisation
#                     -) "lhs"   : Latin Hypercube initialisation. \bold{it 
#                                  requires the \pkg{lhs} package}
# 'best.update'     : character, indicating when to update the global and local best
#                     Valid values are: \cr
#                     -) "sync" : the update is made in a synchronous way, i.e., 
#                                 after computing the position and fitness of 
#                                 ALL the particles in the swarm
#                     -) "async": the update is made in an asynchronous way, i.e., 
#                                 after computing the position and fitness of 
#                                 EACH particle in the swarm
# 'boundary.wall'   : Boundary treatment that is used to limit the sea
#                     the limits given by 'X.Boundaries'.
#                     Valid values are: 'absorbing', 'reflecting' and 'invisible'
#                     See  \citet{RobinsonRahmatSamii2004}
# 'topology'        : character, with the neighbourhood topology to be used in PSO. 
#                     Valid values are in \code{c('gbest', 'lbest', 'ipso')}:
#                      \kbd{gbest}: every particle is connected to every other individual,
#                                   and in particular to the best one. This is 
#                                   also termed \textit{star} topology, and it is 
#                                   generally assumed to have a fast convergence. 
#                                   However, if the global optimum is not close 
#                                   to the attraction zone of the best particle, 
#                                   the swarm can get trapped into local optima.
#                                   see Kennedy 1999; Kennedy and Mendes 2002
#                      \kbd{lbest}: each particle is connected to its \textit{K} 
#                                   immediate neighbours only. This is also termed 
#                                   \textit{circles} or \textit{ring} topology, 
#                                   and generally the swarm will converge slower 
#                                   than with the \kbd{gbest} topology,
#                                   but it has a greater chance to locate the
#                                   global optimum. see Kennedy 1999; Kennedy and Mendes 2002
#                      \kbd{ipso}:  At each iteration, al the particles in the swarm
#                                   are rearranged in descending order according 
#                                   to  their fitness value and the best \kbd{ngbest}
#                                   particles to modify particle's position and velocity. 
#                                   See Zhao 2006.
# 'K'               : OPTIONAL. Only used when \code{topology=='lbest'}
#                     numeric, with the total amount of neighbours of each particle
#                     to be considered in the computation of their personal best.
#                     It MUST BE an even number, in order to consider the same 
#                     amount of neighbours to the left and the right of each particle.
#                     By default \code{K=3}. 
# 'iter.ini'        : OPTIONAL. Only used when \code{topology=='lbest'}
#                     numeric, with the amount of iterations in which the topology 
#                     \kbd{gbest} will be used before starting to use the \kbd{lbest}
#                     topology for the computation of the personal best of each particle.
#                     This argument has not been found in literature, and it aims at
#                     making faster the localization of the global zone of attraction. 
#                     By default \code{iter.ini=0}.
# 'ngbest'          : OPTIONAL. Only used when \code{topology=='ipso'}
#                     numeric, with the amount of particles that will be considered 
#                     in the computation of the global best. \cr
#                     By default \code{ngbest=4}. See Zhao 2006.
# 'use.IW'          : logical, indicating if an inertia weight (\env{w}) will be used
#                     to avoid particles fly around their best position without 
#                     converging into it. See Shi and Eberhart 1998 and Zheng et al. 2003.
# 'IW.type'         : OPTIONAL, only required when \code{use.IW= TRUE}.
#                     character, that defined how to vary the inertia weight 
#                     \env{w} along the iterations. Valid values are: \cr
#                     Valid values are: \cr
#                     -) "linear"    : \env{w} varies linearly between the initial 
#                                       and final values specified by the user 
#                                       in \code{IW.w}. 
#                                       See Shi and Eberhart, 1998. and Zheng et al., 2003
#                     -) "non-linear": \env{w} varies non-linearly between the initial 
#                                       and final values specified by the user 
#                                       in \code{IW.w}. 
#                                       See Chatterjee and Siarry, 2006. \cr
#                     -) "runif"     : \env{w} is a uniform random variable in
#                                       the range [w.min, w.max] specified by the user 
#                                       in \code{IW.w}. It is a generalization of 
#                                       the variation proposed in Eberhart and Shi, 2001b
#                     -) "aiwf"      : Adaptive inertia weight factor, where the 
#                                      inertia weight is varied adaptively 
#                                      depending of the objective values of the particles \cr
#                                      See Liu et al., 2005 \cr
#                     -) "GLratio"   : \env{w} varies according to the ration between 
#                                      the global best and the average of the particle's 
#                                      local best. See Arumugam and Rao, 2008 \cr
# 'IW.w'            : OPTIONAL, only required when \code{use.IW= TRUE & IW.type!='GLratio'}
#                     numeric with the inertia weight(s) (\env{w} or \env{[w.ini, w.fin]}). 
#                     It can be one single number which is then used during all the algorithm, 
#                     or it can be a vector of length 2, with the initial and final values 
#                     (in that order) that \env{w} will take along the iterations
# 'IW.exp'          : OPTIONAL, only required when \code{use.IW= TRUE} AND \code{IW.type= 'non-linear'}
#                     non-linear modulation index. See Chatterjee and Siarry, 2006. \cr
#                     When \code{IW.type='nparamsetslinear'}, \code{IW.exp} is set to 1.
# 'use.TVc1'        : logical, indicating if the cognitive constant (c1) will have
#                     a time-varying value instead of a constant one provided by the user.
#                     See Ratnaweera et al., 2004
# 'TVc1.type'       : character, required only when \code{use.TVc1 = TRUE}.
#                     Valid values are in: \code{c('linear', 'non-linear', 'GLratio')}
# 'TVc1.rng'        : OPTIONAL, only required when \code{use.TVc1= TRUE & TVc1.type!='GLratio'}
#                     numeric with the initial and final values for the cognitive 
#                     constant \env{[c1.ini, c1.fin]} (in that order) along the iterations
# 'TVc1.exp'        : OPTIONAL, only required when \code{use.TVc1= TRUE} AND ('TVc1.type'= "non-linear"). \cr
#                     non-linear modulation index. When  \code{TVc1.type= 'linear} 
#                     \code{TVc1.exp} is set to 1.   
#                     When \code{TVc1.exp} is equal to 1, \code{TVc1} corresponds 
#                     to the improvement proposed by Ratnaweera et al., 2004,
#                     whereas when \code{TVc1.exp} is different from one, no reference 
#                     has been found in literature by the authors, but it was included 
#                     as an option taking into account the work of Chatterjee and 
#                     Siarry 2006 for the inertia weight.\cr                      
# 'use.TVc2'        : logical, indicating if the social constant (c2) will have
#                     a time-varying value instead of a constant one provided by the user.
#                     See Ratnaweera et al., 2004
# 'TVc2.type'       : character, required only when \code{use.TVc2 = TRUE}.
#                     Valid values are in: \code{c('linear', 'non-linear')}
# 'TVc2.rng'        : OPTIONAL, only required when \code{use.TVc2= TRUE}
#                     numeric with the initial and final values for the social 
#                     constant \env{[c2.ini, c2.fin]} (in that order) 
#                     for \env{c1} along the iterations
# 'TVc2.exp'        : OPTIONAL, only required when \code{use.TVc1= TRUE} AND ('TVc1.type'= "non-linear"). \cr
#                     non-linear modulation index. When  \code{TVc1.type= 'linear} 
#                     \code{TVc1.exp} is set to 1. 
#                     When \code{TVc2.exp} is equal to 1, \code{TVc2} corresponds 
#                     to the improvement proposed by Ratnaweera et al., 2004,
#                     whereas when \code{TVc2.exp} is different from one, no reference 
#                     has been found in literature by the authors, but it was included 
#                     as an option taking into account the work of Chatterjee and 
#                     Siarry 2006 for the inertia weight.\cr  
# 'use.RG'          : logical, indicating if the swarm should be regrouped when 
#                     premature convergence is detected. When \code{use.RG=TRUE} 
#                     the swarm is regrouped in a search space centred about gbest,
#                     which is hoped to be both small enough for efficient search 
#                     and large enough to allow the swarm to escape from stagnation. 
#                     See Evers and Ghalia 2009
# 'RG.thr'          : ONLY required when \code{use.RG=TRUE}. \cr
#                     numeric with a positive value representing the 
#                     \kbd{stagnation threshold}, which is used to decide if the 
#                     swarm has to be regrouped or not. See Evers and Galia 2009 
#                     for further details \cr
#                     Regrouping occurs when the \kbd{normalised swarm radius} 
#                     is less than \code{RG.thr}.                   
# 'RG.r'            : ONLY required when \code{use.RG=TRUE}. \cr
#                     numeric with a positive value representing the 
#                     \kbd{regrouping factor}, which is used to regroup the swarm 
#                     in a search space centred about the global best, which is 
#                     hoped to be both small enough for efficient search and 
#                     large enough to allow the swarm escape from the current well. 
#                     See Evers and Galia 2009 for further details 
# 'RG.miniter'      : ONLY required when \code{use.RG=TRUE}. \cr
#                     numeric with the minimum number of iterations needed before 
#                     regrouping
# 'psoout.drty'     : character, with the name of the directory where the output 
#                     files will be written
# 'use.DS'          : Boolean. It indicates if the solution space will be decreased 
#                     when the particles are stagned around gbest
# 'DS.dmin'         : Real between 0 and 1, indicating the minimum length of 
#                     each dimension accepted by the user. 
#                     Only required when 'use.DS' = TRUE 
# 'DS.tol'          : Real. Tolerance that will be used for deciding when
#                     to decrease the solution space 
#                     It starts when 'gbest.fit.rate' < DS.tol, 
#                     gbest.fit.rate = abs( [gbest.fit(n)-gbest.fit(n-1)/gbest.fit(n) )
#                     Only required when 'use.DS' = TRUE 
# 'DS.r'            : Real between 0 and 1. 
#                     represents the rate that is used for decreasing the 
#                     searching space 
#                     Only required when 'use.DS' = TRUE 
# 'out.with.pbest'  : Logical, indicating if the best parameter values for each 
#                     particle and their fitness has to be included in the output 
#                     of the algorithm 
# 'out.with.fit.iter: Logical,indicating if the fitness of each particle and in 
#                     each iteration has to be included in the output of the algorithm
# 'write2disk'      : Logical, indicating if output files have to be written to
#                     the disk or not  
# 'param.ranges'    : character, with the name of the file that stores the desired 
#                     range of variation for each
# 'verbose'         : logical, should progress messages be printed ?
# 'REPORT'          : OPTIONAL, only used when \code{verbose=TRUE}.
#                     The frequency of report messages printed to the screen. Default
#                     to every 10 iterations

hydroPSO <- function(
                    par, 
                    fn="hydromod",  
                    ...,
                    method=c("spso2011", "spso2007", "ipso", "fips", "wfips",
                             "canonical", "spso2011-sa", "spso2011-de",
                             "spso2011-cde", "spso2011-dds", "spso2011-pdds"),
                    lower=-Inf,
                    upper=Inf,                
                    control=list(),
                    model.FUN=NULL,
                    model.FUN.args=list(),
                    change.type="repl",                    # Type(s) of change applied to R-based model parameters before model evaluation.
                    refValue=NULL                           # Reference value used when 'change.type' is 'addi' or 'mult'.
                    ) {

    ########################################################################
    # 0) Checkings and Basic computations - Start                          #
    ########################################################################

    method <- match.arg(method) # Checks the requested optimisation method before processing any other argument.

    caller.env <- parent.frame()

    if (!missing(par)) { 
      if (is(par, "numeric")) {
	    n <- length(par)
      } else if ( (is(par, "matrix")) | (is(par, "data.frame")) ) {
	      n <- ncol(par)
	    } # ELSE IF end
    } else n <- NULL

    if (missing(fn)) {
      stop("Missing argument: 'fn' must be provided")
    } else 
      if ( is.character(fn) | is.function(fn) )  {
        if (is.character(fn)) {
          if (fn=="hydromod") {
            fn.name <- fn
	        fn      <- match.fun(fn)
          } else if (fn=="hydromodInR") {
              if (is.null(model.FUN)) {
                stop("Missing argument: 'model.FUN' must be provided when 'fn=hydromodInR' !!")
              } else {              
                  fn.name <- fn
	              fn      <- match.fun(model.FUN)
                } # ELSE end
            } else stop("Invalid argument: valid character values for 'fn' are only: c('hydromod', 'hydromodInR')")
	    } else if (is.function(fn)) {
	        fn.name <- as.character(substitute(fn))
	        fn      <- fn
	      } # ELSE end
    } else stop("Missing argument: 'class(fn)' must be in c('function', 'character')")   

    if (length(lower) != length(upper) )
      stop( "Invalid argument: 'length(lower) != length(upper) (", length(lower), "!=", length(upper), ")'" )

    if (!is.null(n)) {
       if (length(lower) != n)
	 stop( "Invalid argument: 'length(lower) != nparam (", length(lower), "!=", n, ")'" )             
       if (length(upper) != n)
	 stop( "Invalid argument: 'length(upper) != nparam (", length(lower), "!=", n, ")'" )
    } else n <- length(lower)      

    ############################################################################

    con <- list(
	    drty.in="PSO.in",
	    drty.out=NULL,
	    #param.ranges="ParamRanges.txt",    
	    digits=7,

	    MinMax=c("min", "max"), 
	    npart=NA, 
	    maxit=1000, 
	    maxfn=Inf,
	    c1= 0.5+log(2), 
	    c2= 0.5+log(2), 
	    use.IW= TRUE, IW.w=1/(2*log(2)), IW.type=c("linear", "non-linear", "runif", "aiwf", "GLratio"), IW.exp= 1, 
	    use.CF= FALSE, 
	    lambda= 1,

	    abstol= NULL,    
	    reltol=sqrt(.Machine$double.eps),             
	    Xini.type=c("random", "lhs", "sobol"),  
	    Vini.type=c(NA, "random2011", "lhs2011", "random2007", "lhs2007",  "zero"), 
	    best.update=c("sync", "async"),
	    random.update=TRUE,
	    boundary.wall=c(NA, "absorbing2011", "absorbing2007", "reflecting", "damping", "invisible"),
	    topology=c("random", "gbest", "lbest", "vonNeumann"), K=3, 
	    iter.ini=0, # only used when 'topology=lbest'   
	    ngbest=4,   # only used when 'method=ipso'   
	    SA.T0=NA_real_, SA.alpha=0.98, SA.scale=0.05, # only used when 'method=spso2011-sa'
	    SA.nsteps=4, SA.ndim=1, SA.stagnation=5, SA.reheat=50,
	    SA.target.accept=0.50, SA.replicas=NA_integer_, SA.swap.every=2,
	    SA.scout.fraction=0.10, SA.scout.generations=3,
	    SA.radius.threshold=0.03, SA.budget.fraction=0.10,
	    # only used when 'method=spso2011-sa'
	    DE.F=0.1, DE.c=0.1, DE.archive.size=20, # only used when 'method=spso2011-de'
	    CDE.H=10, CDE.p=0.10, CDE.qmin=0.20, CDE.qmax=0.80,
	    CDE.stag.iter=5, CDE.diversity=0.05, CDE.eigen=TRUE,
	    CDE.eigen.freq=5, # only used when 'method=spso2011-cde'
	    DDS.r=0.20, DDS.stag.iter=5, # only used when 'method=spso2011-dds'
	    PDDS.warmup=3, PDDS.batch.every=3, PDDS.nchains=NA_integer_,
	    PDDS.archive.size=NA_integer_, PDDS.r=c(0.05, 0.20, 0.50),
	    PDDS.epoch=40, PDDS.migration.frac=0.20, PDDS.min.distance=0.05,
	    # only used when 'method=spso2011-pdds'
	    
	    normalise=FALSE,

	    use.TVc1= FALSE, TVc1.rng= c(1.28, 1.05), TVc1.type=c("linear", "non-linear", "GLratio"), TVc1.exp= 1, 
	    use.TVc2= FALSE, TVc2.rng= c(1.05, 1.28), TVc2.type=c("linear", "non-linear"), TVc2.exp= 1, 
	    use.TVlambda=FALSE, TVlambda.rng= c(1, 0.25), TVlambda.type=c("linear", "non-linear"), TVlambda.exp= 1, 
	    use.RG = FALSE, RG.thr= 1.1e-4, RG.r= 0.8, RG.miniter= 5, # RG.r not used in reagrouping
	    
	    plot=FALSE,                
	    out.with.pbest=FALSE,
	    out.with.fit.iter=FALSE,
	    write2disk=FALSE,

	    verbose=TRUE,
	    REPORT=100, 
	    parallel=c("none", "multicore", "parallel", "parallelWin"),
	    par.nnodes=NA,
	    par.pkgs= list(),
	    par.env=NULL,
	    par.export=NULL
	       )

    MinMax        <- match.arg(control[["MinMax"]], con[["MinMax"]])    
    Xini.type     <- match.arg(control[["Xini.type"]], con[["Xini.type"]])     
    Vini.type     <- match.arg(control[["Vini.type"]], con[["Vini.type"]])     
    Vini.type     <- if (is.na(Vini.type)) { 
                            ifelse(method=="spso2007", "random2007", "random2011")
                     } else Vini.type
    best.update   <- match.arg(control[["best.update"]], con[["best.update"]]) 
    boundary.wall <- match.arg(control[["boundary.wall"]], con[["boundary.wall"]]) 
    boundary.wall <- ifelse(is.na(boundary.wall), 
                            ifelse(method %in% c("spso2007", "canonical"), 
                                   "absorbing2007", "absorbing2011"),
                            boundary.wall)
    topology      <- match.arg(control[["topology"]], con[["topology"]]) 
    IW.type       <- match.arg(control[["IW.type"]], con[["IW.type"]])
    TVc1.type     <- match.arg(control[["TVc1.type"]], con[["TVc1.type"]]) 
    TVc2.type     <- match.arg(control[["TVc2.type"]], con[["TVc2.type"]]) 
    TVlambda.type <- match.arg(control[["TVlambda.type"]], con[["TVlambda.type"]])
    parallel      <- match.arg(control[["parallel"]], con[["parallel"]])
    parallel      <- .resolveParallelBackend(parallel)
        	       
    nmsC <- names(con)
    con[(namc <- names(control))] <- control
    if (length(noNms <- namc[!namc %in% nmsC])) 
      warning("[Unknown names in control: ", paste(noNms, collapse = ", "), " (not used) !]")	       

    drty.in           <- con[["drty.in"]]
    drty.out          <- con[["drty.out"]]
    #param.ranges      <- con[["param.ranges"]]         
    digits            <- con[["digits"]]                    
    npart             <- ifelse(is.na(con[["npart"]]), 
                                ifelse(method=="spso2007", floor(10+2*sqrt(n)), 40),
                                con[["npart"]] )                                 
    maxit             <- con[["maxit"]] 
    maxfn             <- con[["maxfn"]] 
    c1                <- ifelse(method=="canonical", 2.05, con[["c1"]])
    c2                <- ifelse(method=="canonical", 2.05, con[["c2"]])
    use.IW            <- ifelse(method=="canonical", FALSE, as.logical(con[["use.IW"]]))
    IW.w              <- con[["IW.w"]]
    IW.exp            <- con[["IW.exp"]]
    use.CF            <- ifelse(method=="canonical", TRUE, as.logical(con[["use.CF"]]))
    lambda            <- con[["lambda"]]  
    abstol            <- con[["abstol"]]     
    reltol            <- con[["reltol"]]             
    random.update     <- as.logical(con[["random.update"]])
    K                 <- con[["K"]]      
    iter.ini          <- con[["iter.ini"]]
    ngbest            <- con[["ngbest"]]
    SA.T0              <- con[["SA.T0"]]
    SA.alpha           <- con[["SA.alpha"]]
    SA.scale           <- con[["SA.scale"]]
    SA.nsteps          <- con[["SA.nsteps"]]
    SA.ndim            <- con[["SA.ndim"]]
    SA.stagnation      <- con[["SA.stagnation"]]
    SA.reheat          <- con[["SA.reheat"]]
    SA.target.accept   <- con[["SA.target.accept"]]
    SA.replicas        <- con[["SA.replicas"]]
    SA.swap.every      <- con[["SA.swap.every"]]
    SA.scout.fraction  <- con[["SA.scout.fraction"]]
    SA.scout.generations <- con[["SA.scout.generations"]]
    SA.radius.threshold <- con[["SA.radius.threshold"]]
    SA.budget.fraction <- con[["SA.budget.fraction"]]
    DE.F               <- con[["DE.F"]]
    DE.c               <- con[["DE.c"]]
    DE.archive.size    <- con[["DE.archive.size"]]
    CDE.H              <- con[["CDE.H"]]
    CDE.p              <- con[["CDE.p"]]
    CDE.qmin           <- con[["CDE.qmin"]]
    CDE.qmax           <- con[["CDE.qmax"]]
    CDE.stag.iter      <- con[["CDE.stag.iter"]]
    CDE.diversity      <- con[["CDE.diversity"]]
    CDE.eigen          <- con[["CDE.eigen"]]
    CDE.eigen.freq     <- con[["CDE.eigen.freq"]]
    DDS.r              <- con[["DDS.r"]]
    DDS.stag.iter      <- con[["DDS.stag.iter"]]
    PDDS.warmup        <- con[["PDDS.warmup"]]
    PDDS.batch.every   <- con[["PDDS.batch.every"]]
    PDDS.nchains       <- con[["PDDS.nchains"]]
    PDDS.archive.size  <- con[["PDDS.archive.size"]]
    PDDS.r             <- con[["PDDS.r"]]
    PDDS.epoch         <- con[["PDDS.epoch"]]
    PDDS.migration.frac <- con[["PDDS.migration.frac"]]
    PDDS.min.distance  <- con[["PDDS.min.distance"]]
    normalise         <- as.logical(con[["normalise"]])             
    use.TVc1          <- as.logical(con[["use.TVc1"]])
    TVc1.rng          <- con[["TVc1.rng"]]
    TVc1.exp          <- con[["TVc1.exp"]]
    use.TVc2          <- as.logical(con[["use.TVc2"]])
    TVc2.rng          <- con[["TVc2.rng"]]
    TVc2.exp          <- con[["TVc2.exp"]]
    use.TVlambda      <- as.logical(con[["use.TVlambda"]])
    TVlambda.rng      <- con[["TVlambda.rng"]]
    TVlambda.exp      <- con[["TVlambda.exp"]]
    use.RG            <- as.logical(con[["use.RG"]])
    RG.thr            <- con[["RG.thr"]]
    RG.r              <- con[["RG.r"]]
    RG.miniter        <- con[["RG.miniter"]]
    plot              <- as.logical(con[["plot"]])            
    out.with.pbest    <- as.logical(con[["out.with.pbest"]])
    out.with.fit.iter <- as.logical(con[["out.with.fit.iter"]])
    write2disk        <- as.logical(con[["write2disk"]])
    verbose           <- as.logical(con[["verbose"]])
    REPORT            <- con[["REPORT"]] 
    par.nnodes        <- con[["par.nnodes"]]
    par.pkgs          <- con[["par.pkgs"]] 
    par.env           <- con[["par.env"]]
    if (is.null(par.env)) par.env <- caller.env
    if (!is.environment(par.env))
      stop("Invalid argument: 'control$par.env' must be an environment")
    par.export        <- con[["par.export"]]
    if (!is.null(par.export) && !is.character(par.export))
      stop("Invalid argument: 'control$par.export' must be a character vector")

    if (method == "spso2011-sa") {
      # NA requests automatic temperature scaling from the first uphill chain transition.
      if (!is.numeric(SA.T0) || length(SA.T0) != 1 || is.nan(SA.T0) ||
          (!is.na(SA.T0) && (!is.finite(SA.T0) || SA.T0 <= 0)))
        stop("Invalid argument: 'control$SA.T0' must be NA or a finite number greater than zero")
      # A cooling factor strictly between zero and one reduces the temperature each iteration.
      if (!is.numeric(SA.alpha) || length(SA.alpha) != 1 || !is.finite(SA.alpha) ||
          SA.alpha <= 0 || SA.alpha >= 1)
        stop("Invalid argument: 'control$SA.alpha' must be a finite number in (0, 1)")
      # The perturbation scale is expressed as a positive fraction of each parameter range.
      if (!is.numeric(SA.scale) || length(SA.scale) != 1 || !is.finite(SA.scale) || SA.scale <= 0)
        stop("Invalid argument: 'control$SA.scale' must be a finite number greater than zero")
      # Each activation must perform a positive integer number of local-search steps.
      if (!is.numeric(SA.nsteps) || length(SA.nsteps) != 1 || !is.finite(SA.nsteps) ||
          SA.nsteps < 1 || SA.nsteps != as.integer(SA.nsteps))
        stop("Invalid argument: 'control$SA.nsteps' must be a positive integer")
      # Sparse proposals may perturb between one and all search dimensions.
      if (!is.numeric(SA.ndim) || length(SA.ndim) != 1 || !is.finite(SA.ndim) ||
          SA.ndim < 1 || SA.ndim > n || SA.ndim != as.integer(SA.ndim))
        stop("Invalid argument: 'control$SA.ndim' must be an integer in [1, length(lower)]")
      # Stagnation is measured in completed SPSO generations without a new global best.
      if (!is.numeric(SA.stagnation) || length(SA.stagnation) != 1 ||
          !is.finite(SA.stagnation) || SA.stagnation < 0 ||
          SA.stagnation != as.integer(SA.stagnation))
        stop("Invalid argument: 'control$SA.stagnation' must be a non-negative integer")
      # Reheating restarts an unproductive chain after this many SA proposals.
      if (!is.numeric(SA.reheat) || length(SA.reheat) != 1 || !is.finite(SA.reheat) ||
          SA.reheat < 1 || SA.reheat != as.integer(SA.reheat))
        stop("Invalid argument: 'control$SA.reheat' must be a positive integer")
      # The adaptive proposal scale targets a valid acceptance probability.
      if (!is.numeric(SA.target.accept) || length(SA.target.accept) != 1 ||
          !is.finite(SA.target.accept) || SA.target.accept <= 0 || SA.target.accept >= 1)
        stop("Invalid argument: 'control$SA.target.accept' must be a finite number in (0, 1)")
      # NA selects a worker-aware replica count after the parallel backend is initialised.
      if (npart < 2)
        stop("Invalid argument: 'method=spso2011-sa' requires 'control$npart' to be at least 2")
      if (!is.numeric(SA.replicas) || length(SA.replicas) != 1 || is.nan(SA.replicas) ||
          (!is.na(SA.replicas) && (!is.finite(SA.replicas) || SA.replicas < 2 ||
                                   SA.replicas > npart || SA.replicas != as.integer(SA.replicas))))
        stop("Invalid argument: 'control$SA.replicas' must be NA or an integer in [2, npart]")
      # Adjacent replicas exchange configurations at a positive integer sweep interval.
      if (!is.numeric(SA.swap.every) || length(SA.swap.every) != 1 ||
          !is.finite(SA.swap.every) || SA.swap.every < 1 ||
          SA.swap.every != as.integer(SA.swap.every))
        stop("Invalid argument: 'control$SA.swap.every' must be a positive integer")
      # Only a bounded fraction of the swarm is repopulated after each SA event.
      if (!is.numeric(SA.scout.fraction) || length(SA.scout.fraction) != 1 ||
          !is.finite(SA.scout.fraction) || SA.scout.fraction <= 0 ||
          SA.scout.fraction >= 1)
        stop("Invalid argument: 'control$SA.scout.fraction' must be a finite number in (0, 1)")
      # Protected scouts must explore their new basins for at least one generation.
      if (!is.numeric(SA.scout.generations) || length(SA.scout.generations) != 1 ||
          !is.finite(SA.scout.generations) || SA.scout.generations < 1 ||
          SA.scout.generations != as.integer(SA.scout.generations))
        stop("Invalid argument: 'control$SA.scout.generations' must be a positive integer")
      # The collapse threshold is a positive fraction of the normalised search diameter.
      if (!is.numeric(SA.radius.threshold) || length(SA.radius.threshold) != 1 ||
          !is.finite(SA.radius.threshold) || SA.radius.threshold <= 0 ||
          SA.radius.threshold > 1)
        stop("Invalid argument: 'control$SA.radius.threshold' must be a finite number in (0, 1]")
      # The SA engine may consume only this fraction of the declared evaluation budget.
      if (!is.numeric(SA.budget.fraction) || length(SA.budget.fraction) != 1 ||
          !is.finite(SA.budget.fraction) || SA.budget.fraction <= 0 ||
          SA.budget.fraction > 1)
        stop("Invalid argument: 'control$SA.budget.fraction' must be a finite number in (0, 1]")
    } # IF method == "spso2011-sa" end

    if (method == "spso2011-de") {
      # The initial differential weight centres the adaptive Cauchy sampling distribution.
      if (!is.numeric(DE.F) || length(DE.F) != 1 || !is.finite(DE.F) || DE.F <= 0 || DE.F > 2)
        stop("Invalid argument: 'control$DE.F' must be a finite number in (0, 2]")
      # The learning rate controls how quickly successful factors replace earlier history.
      if (!is.numeric(DE.c) || length(DE.c) != 1 || !is.finite(DE.c) || DE.c <= 0 || DE.c > 1)
        stop("Invalid argument: 'control$DE.c' must be a finite number in (0, 1]")
      # The bounded archive must contain at least one differential pair.
      if (!is.numeric(DE.archive.size) || length(DE.archive.size) != 1 ||
          !is.finite(DE.archive.size) || DE.archive.size < 2 ||
          DE.archive.size != as.integer(DE.archive.size))
        stop("Invalid argument: 'control$DE.archive.size' must be an integer greater than or equal to 2")
      DE.archive.size <- as.integer(DE.archive.size) # Uses an integer row limit internally.
    } # IF method == "spso2011-de" end

    if (method == "spso2011-cde") {
      # Competitive DE requires enough particles for three distinct rand/1 donors.
      if (!is.numeric(npart) || length(npart) != 1 || !is.finite(npart) || npart < 4)
        stop("Invalid argument: 'method=spso2011-cde' requires 'control$npart' greater than or equal to 4")
      # Generational selection keeps every competitive proposal on one common parent population.
      if (best.update != "sync")
        stop("Invalid argument: 'method=spso2011-cde' requires 'control$best.update=sync'")
      # SHADE uses a positive integer number of circular success-memory cells.
      if (!is.numeric(CDE.H) || length(CDE.H) != 1 || !is.finite(CDE.H) ||
          CDE.H < 1 || CDE.H != as.integer(CDE.H))
        stop("Invalid argument: 'control$CDE.H' must be a positive integer")
      CDE.H <- as.integer(CDE.H) # Uses an integer circular-memory size internally.
      # The current-to-pbest elite fraction must select a non-empty population subset.
      if (!is.numeric(CDE.p) || length(CDE.p) != 1 || !is.finite(CDE.p) ||
          CDE.p <= 0 || CDE.p > 1)
        stop("Invalid argument: 'control$CDE.p' must be a finite number in (0, 1]")
      # Both operators retain a strictly positive share of every population batch.
      if (!is.numeric(CDE.qmin) || length(CDE.qmin) != 1 || !is.finite(CDE.qmin) ||
          !is.numeric(CDE.qmax) || length(CDE.qmax) != 1 || !is.finite(CDE.qmax) ||
          CDE.qmin <= 0 || CDE.qmin >= CDE.qmax || CDE.qmax >= 1)
        stop("Invalid arguments: 'control$CDE.qmin' and 'control$CDE.qmax' must satisfy 0 < qmin < qmax < 1")
      # Emergency exploration starts after this many generations without a new global best.
      if (!is.numeric(CDE.stag.iter) || length(CDE.stag.iter) != 1 ||
          !is.finite(CDE.stag.iter) || CDE.stag.iter < 1 ||
          CDE.stag.iter != as.integer(CDE.stag.iter))
        stop("Invalid argument: 'control$CDE.stag.iter' must be a positive integer")
      # Diversity is measured as mean relative coordinate spread.
      if (!is.numeric(CDE.diversity) || length(CDE.diversity) != 1 ||
          !is.finite(CDE.diversity) || CDE.diversity < 0 || CDE.diversity > 1)
        stop("Invalid argument: 'control$CDE.diversity' must be a finite number in [0, 1]")
      # Eigen crossover can be disabled without changing the competitive architecture.
      if (!is.logical(CDE.eigen) || length(CDE.eigen) != 1 || is.na(CDE.eigen))
        stop("Invalid argument: 'control$CDE.eigen' must be TRUE or FALSE")
      # Covariance eigendecomposition is refreshed at a positive integer interval.
      if (!is.numeric(CDE.eigen.freq) || length(CDE.eigen.freq) != 1 ||
          !is.finite(CDE.eigen.freq) || CDE.eigen.freq < 1 ||
          CDE.eigen.freq != as.integer(CDE.eigen.freq))
        stop("Invalid argument: 'control$CDE.eigen.freq' must be a positive integer")
      CDE.eigen.freq <- as.integer(CDE.eigen.freq) # Uses integer generation intervals.
    } # IF method == "spso2011-cde" end

    if (method == "spso2011-dds") {
      # The DDS perturbation scale is a positive fraction of each parameter range.
      if (!is.numeric(DDS.r) || length(DDS.r) != 1 || !is.finite(DDS.r) || DDS.r <= 0)
        stop("Invalid argument: 'control$DDS.r' must be a finite number greater than zero")
      # Stagnation is counted in complete swarm generations without gbest improvement.
      if (!is.numeric(DDS.stag.iter) || length(DDS.stag.iter) != 1 ||
          !is.finite(DDS.stag.iter) || DDS.stag.iter < 1 ||
          DDS.stag.iter != floor(DDS.stag.iter))
        stop("Invalid argument: 'control$DDS.stag.iter' must be a positive integer")
    } # IF method == "spso2011-dds" end

    if (method == "spso2011-pdds") {
      # At least one SPSO generation is required to seed the elite archive.
      if (!is.numeric(PDDS.warmup) || length(PDDS.warmup) != 1 ||
          !is.finite(PDDS.warmup) || PDDS.warmup < 1 ||
          PDDS.warmup != as.integer(PDDS.warmup))
        stop("Invalid argument: 'control$PDDS.warmup' must be a positive integer")
      # A cycle must retain at least one SPSO batch between pDDS batches.
      if (!is.numeric(PDDS.batch.every) || length(PDDS.batch.every) != 1 ||
          !is.finite(PDDS.batch.every) || PDDS.batch.every < 2 ||
          PDDS.batch.every != as.integer(PDDS.batch.every))
        stop("Invalid argument: 'control$PDDS.batch.every' must be an integer greater than or equal to 2")
      if (length(PDDS.nchains) == 1 && is.na(PDDS.nchains))
        PDDS.nchains <- min(npart, 5L, max(2L, floor(npart/8))) # Avoids fragmenting small batches.
      if (!is.numeric(PDDS.nchains) || length(PDDS.nchains) != 1 ||
          !is.finite(PDDS.nchains) || PDDS.nchains < 1 || PDDS.nchains > npart ||
          PDDS.nchains != as.integer(PDDS.nchains))
        stop("Invalid argument: 'control$PDDS.nchains' must be an integer in [1, npart]")
      PDDS.nchains <- as.integer(PDDS.nchains)
      if (length(PDDS.archive.size) == 1 && is.na(PDDS.archive.size))
        PDDS.archive.size <- min(20L, max(5L, 2L*n)) # Bounds archive memory while retaining multiple basins.
      if (!is.numeric(PDDS.archive.size) || length(PDDS.archive.size) != 1 ||
          !is.finite(PDDS.archive.size) || PDDS.archive.size < PDDS.nchains ||
          PDDS.archive.size != as.integer(PDDS.archive.size))
        stop("Invalid argument: 'control$PDDS.archive.size' must be an integer greater than or equal to 'PDDS.nchains'")
      PDDS.archive.size <- as.integer(PDDS.archive.size)
      if (!is.numeric(PDDS.r) || length(PDDS.r) < 1 ||
          any(!is.finite(PDDS.r)) || any(PDDS.r <= 0))
        stop("Invalid argument: 'control$PDDS.r' must contain finite numbers greater than zero")
      if (!is.numeric(PDDS.epoch) || length(PDDS.epoch) != 1 ||
          !is.finite(PDDS.epoch) || PDDS.epoch < 2 ||
          PDDS.epoch != as.integer(PDDS.epoch))
        stop("Invalid argument: 'control$PDDS.epoch' must be an integer greater than or equal to 2")
      if (!is.numeric(PDDS.migration.frac) || length(PDDS.migration.frac) != 1 ||
          !is.finite(PDDS.migration.frac) || PDDS.migration.frac <= 0 ||
          PDDS.migration.frac > 1)
        stop("Invalid argument: 'control$PDDS.migration.frac' must be a finite number in (0, 1]")
      if (!is.numeric(PDDS.min.distance) || length(PDDS.min.distance) != 1 ||
          !is.finite(PDDS.min.distance) || PDDS.min.distance < 0 ||
          PDDS.min.distance > 1)
        stop("Invalid argument: 'control$PDDS.min.distance' must be a finite number in [0, 1]")
    } # IF method == "spso2011-pdds" end

    ############################################################################
    ######################### Dummy checkings ##################################

    if (maxit < REPORT) {
      REPORT <- maxit
      warning("[ 'REPORT' is greater than 'maxit' => 'REPORT=maxit' ]")
    } # IF end

    if ( (lambda < 0) | (lambda >1) )
      stop("Invalid argument: 'lambda' has to be in [0, 1] !!")

    if ( K > npart ) {
      K <- npart
      warning("[ 'K' is greater than 'npart' => 'K=npart' ]")
    } # IF end

    if ( (K < 1) | (floor(K) != K) ) {
      K <- npart
      stop("'K' must be a positive integer (> 0) !!'")
    } # IF end
    
    if ( ("gof.Ini" %in% names(model.FUN.args)) ) {
      gof.Ini.exists <- TRUE
    } else gof.Ini.exists <- FALSE
    if ( ("gof.Fin" %in% names(model.FUN.args)) ) {
      gof.Fin.exists <- TRUE 
    } else gof.Fin.exists <- FALSE
    if ( ("date.fmt" %in% names(model.FUN.args)) ) {
      date.fmt.exists <- TRUE
    } else date.fmt.exists <- FALSE

    ############################################################################  
    # 1)                              Initialisation                           #
    ############################################################################  
    if (verbose) message("                                                                                ")          
    if (verbose) message("================================================================================")
    if (verbose) message("[                                Initialising  ...                             ]")
    if (verbose) message("================================================================================")
    if (verbose) message("                                                                                ")        

    tmp.stg <- c("npart", "maxit", "method", "topology", "boundary.wall")
    tmp.val <- c(npart, maxit, method, topology, boundary.wall)
    message("[", paste(tmp.stg, tmp.val, collapse = " ; ", sep="="), "]")

    if (length(yesNms <- namc[namc %in% nmsC])) {          
      yesVals <- con[pmatch(namc[namc %in% nmsC], nmsC)][c(yesNms)]
      message("         ")
      message("[ user-definitions in control: ", paste(yesNms, yesVals, collapse = " ; ", sep="="), " ]")
      message("         ")          
    } # IF end

    if (fn.name=="hydromod") {

      # Checking 'drty.in', the directory where 'ParamRanges.txt' and 'ParamFiles.txt' must be stored
      if ( !file.exists( drty.in) )
        stop( "Invalid value for 'drty.in': The directory '", path.expand(drty.in), "' does not exist !" )

      if (write2disk && (is.null(drty.out) || !is.character(drty.out) ||
                         length(drty.out) != 1 || !nzchar(drty.out)))
        stop("Invalid argument: 'control$drty.out' must be provided when 'control$write2disk=TRUE'")

      # Checking 'model.FUN'
      if ( is.null(model.FUN) ) {
        stop( "'model.FUN' has to be defined !" )
      } else  {
          model.FUN.name <- model.FUN
          model.FUN      <- match.fun(model.FUN)
        } # ELSE end

      # Checking 'model.FUN.args'
      if ( length(model.FUN.args)==0 ) {
        warning( "['model.FUN.args' is an empty list. Are you sure your model does not have any argument(s) ?]" )
      } else {
          model.FUN.argsDefaults <- formals(model.FUN)
          model.FUN.args         <- modifyList(model.FUN.argsDefaults, model.FUN.args) 
        } # ELSE end

      # Checking 'param.ranges' argument
      param.ranges <- model.FUN.args[["param.ranges"]]
      if (param.ranges == basename(param.ranges) ) 
        param.ranges <- file.path(drty.in, param.ranges)
      if ( !file.exists( param.ranges ) )
        stop( "Invalid argument: The file '", param.ranges, "' does not exist !" ) 

      # Checking 'param.files' argument
      param.files <- model.FUN.args[["param.files"]]
      if (param.files == basename(param.files) ) 
        param.files <- file.path(drty.in, param.files)
      if ( !file.exists( param.files ) )
        stop( "Invalid argument: The file '", param.files, "' does not exist !" ) 

    } # IF (fn.name=="hydromod") end   


    if (fn.name=="hydromodInR") {					
      if ( is.null(model.FUN) ) {
        stop( "'model.FUN' has to be defined !" )
      } else  {
          model.FUN.name <- as.character(substitute(model.FUN))
          model.FUN      <- match.fun(model.FUN)   
        } # ELSE end

      if (!("param.values" %in% names(formals(model.FUN)) ))
        stop("[ Invalid argument: 'param.values' must be the first argument of the 'model.FUN' function! ]")

      if (!("obs" %in% names(formals(model.FUN)) )) 
        stop("[ Invalid argument: 'obs' must be an argument of the 'model.FUN' function! ]")
   
      model.FUN.argsDefaults <- formals(model.FUN)
      if ( length(model.FUN.args) > 0 ) {
        model.FUN.args <- modifyList(model.FUN.argsDefaults, model.FUN.args) 
      } else model.FUN.args <- model.FUN.argsDefaults

    } # IF end   


    # checking 'X.Boundaries' 
    if (fn.name=="hydromod") {

        if (verbose) message("================================================================================")
        if (verbose) message("[                          Reading 'param.ranges' ...                          ]")
        if (verbose) message("================================================================================") 

        X.Boundaries <- read.ParameterRanges(ParamRanges.fname=param.ranges, verbose=verbose) 

        lower <- X.Boundaries[,1]
        upper <- X.Boundaries[,2]

    } else {
        if ( (lower[1L] == -Inf) || (upper[1L] == Inf) ) {
          stop( "Invalid argument: 'lower' and 'upper' boundaries must be finite !!'" )
        } else X.Boundaries <- cbind(lower, upper)              
      } # ELSE end

    n <- nrow(X.Boundaries)
    
    if (is.null(rownames(X.Boundaries))) {
      param.IDs <- paste("Param", 1:n, sep="")
    } else param.IDs <- rownames(X.Boundaries)
    
    if (normalise) {
      # Backing up the original boundaries
      lower.ini <- lower
      upper.ini <- upper
      X.Boundaries.ini <- X.Boundaries
      LOWER.ini <- matrix( rep(lower.ini, npart), nrow=npart, byrow=TRUE)
      UPPER.ini <- matrix( rep(upper.ini, npart), nrow=npart, byrow=TRUE)
      
      # normalising
      lower <- rep(0, n)
      upper <- rep(1, n)
      X.Boundaries <- cbind(lower, upper)
      rownames(X.Boundaries) <- param.IDs
    } # IF end

    if (write2disk) {
      if (is.null(drty.out) || !is.character(drty.out) ||
          length(drty.out) != 1 || !nzchar(drty.out))
        stop("Invalid argument: 'control$drty.out' must be provided when 'control$write2disk=TRUE'")
      if (drty.out == basename(drty.out) )
        drty.out <- paste( getwd(), "/", drty.out, sep="")

      if (!file.exists(file.path(drty.out))) {
	      dir.create(file.path(drty.out))
	      if (verbose) message("                                            ")
	      if (verbose) message("[ Output directory '", basename(drty.out), "' was created on: '", dirname(drty.out), "' ]") 
	      if (verbose) message("                                            ")
      } # IF end
    } # IF end  

    if (is.null(abstol)) 
      if (MinMax == "max") {
        abstol <- +Inf
      } else abstol <- -Inf

    if (Xini.type=="lhs") { 
	    if ( length(find.package("lhs", quiet=TRUE)) == 0 ) {
	      warning("[ Package 'lhs' is not installed =>  Xini.type='random' ]")
	      Xini.type <- "random"
	    }  # IF end  
    } # IF end

    if (Vini.type %in% c("lhs2011", "lhs2007")) { 
	    if ( length(find.package("lhs", quiet=TRUE)) == 0 ) {
	      warning("[ Package 'lhs' is not installed =>  Vini.type='random2011' ]")
	      Vini.type <- "random2011"
	    }  # IF end  
    } # IF end

    if (use.IW) {
      w <- IW.w   

      if (length(w) == 2) {    
	      w.ini <- w[1] #initial inertial weight at the start of a given run
	      w.fin <- w[2] #final inertial weight at the end of a given run
      } else if (length(w) == 1) {
	        w.ini <- w
	        w.fin <- w
	      } else stop("Invalid argument: 'length(w)' must be 1 or 2 !!")

      if  (IW.type == "linear") {
	      if (IW.exp != 1) {
	        warning("[ IW.type == 'linear' => 'IW.exp=1' ]")
	        IW.exp= 1 
	      } # IF end
      } # IF end                
  } # IF end

  if (use.CF) {
    if (c1+c2 < 4) stop("Invalid argument: 'c1+c2' must be >= 4 when 'use.CF=TRUE'")
      CF <- compute.CF(c1, c2)

    if ( use.TVc1 ) stop("Invalid argument: You can not use 'TVc1' when 'use.CF=TRUE'")
    if ( use.TVc2 ) stop("Invalid argument: You can not use 'TVc2' when 'use.CF=TRUE'")
  } else CF <- 1  

    if ( use.IW & use.CF ) 
      stop("Invalid argument: Inertia Weight and Constriction Factor can not be used simultaneously !!")       

    if (use.TVc1) {         
       c1.ini <- TVc1.rng[1]
       c1.fin <- TVc1.rng[2]                
       if  (TVc1.type == "linear") {
	       if (TVc1.exp != 1) {
	         warning("[ TVc1.type == 'linear' => 'TVc1.exp=1' ]")
	         TVc1.exp= 1 
	       } # IF end
       } # IF end              
    } # IF end

    if (use.TVc2) {      
       c2.ini <- TVc2.rng[1]
       c2.fin <- TVc2.rng[2]                  
       if  (TVc2.type == "linear") {
	       if (TVc2.exp != 1) {
	         warning("[ TVc2.type == 'linear' => 'TVc2.exp=1' ]")
	         TVc2.exp= 1 
	       } # IF end
       } # IF end             
    } # IF end

    if (use.TVlambda) {            
      # Computing ('vmax.ini', 'vmax.fin') 
      vmax.ini <- TVlambda.rng[1]
      vmax.fin <- TVlambda.rng[2]                  
      if  (TVlambda.type == "linear") {
	      if (TVlambda.exp != 1) {
	        warning("[ TVlambda.type == 'linear' => 'TVlambda.exp=1' ]")
	        TVlambda.exp= 1 
	      } # IF end
      } # IF end             
    } # IF end

    if ( (topology == "lbest") | (topology == "random") ) {

      if (topology == "lbest") {
	      if (K != npart) {
	        if ( (trunc((K-1)/2) -(K-1)/2) != 0 )
	          stop("Invalid argument: 'K' must be odd" )
	      } # IF end
      } # IF end
    } else if (topology=="vonNeumann") {
	     topology <- "lbest"
	     if (npart < 5) stop("Invalid argument: for 'vonNeumann' topology 'npart' should be >= 5 !!")
	     if (K != 5) K <- 5
	    } else if (topology == "gbest") {
	        K <- npart  
	      } # ELSE end

    if ( method == "ipso" ) {
       if ( (ngbest < 1) | (ngbest > npart) )
	       stop("Invalid argument: 'ngbest' must be in [1, 'npart]'" )

       if ( topology!="gbest") {
	       if (verbose) warning("[ Note: 'method=ipso' => 'topology' was changed to 'gbest' !]" )
	         topology <- "gbest"
       } # IF end
    } # IF end

    if ( (method == "spso2011-de") && (topology != "random") )
      stop("Invalid argument: 'method=spso2011-de' requires 'control$topology=random'")
    if ( (method == "spso2011-cde") && (topology != "random") )
      stop("Invalid argument: 'method=spso2011-cde' requires 'control$topology=random'")

    Lmax <- (X.Boundaries[ ,2] - X.Boundaries[ ,1])        
    ############################################################################
    ##                                parallel                                 #
    ############################################################################
    if (parallel != "none") {
    
    #  if ( ( (parallel=="multicore") | (parallel=="parallel") ) & 
    #     ( (R.version$os=="mingw32") | (R.version$os=="mingw64") ) )
    #     stop("[ Fork clusters are not supported on Windows =>  'parallel' can not be set to '", parallel, "' ]")

    ifelse(parallel=="parallelWin", parallel.pkg <- "parallel",  parallel.pkg <- parallel) 
    if ( length(find.package(parallel.pkg, quiet=TRUE)) == 0 ) {               
      warning("[ Package '", parallel.pkg, "' is not installed =>  parallel='none' ]")
      parallel <- "none"
    }  else { 
      
         if (verbose) message("                               ")
         if (verbose) message("[ Parallel initialization ... ]")
      
         fn1 <- function(i, x) fn(x[i,])

         #require(parallel)           
         nnodes.pc <- parallel::detectCores()
         if (is.na(nnodes.pc)) {
           if (is.na(par.nnodes)) nnodes.pc <- 1 else nnodes.pc <- par.nnodes
         } # IF end
         if (verbose) message("[ Number of cores/nodes detected: ", nnodes.pc, " ]")
           
         if ( write2disk && ((parallel=="parallel") | (parallel=="parallelWin")) ) {             
            logfile.fname <- paste(file.path(drty.out), "/", "parallel_logfile.txt", sep="") 
            if (file.exists(logfile.fname)) file.remove(logfile.fname)
         } # IF end
             
         if (is.na(par.nnodes)) {
           par.nnodes <- nnodes.pc
         } else if (par.nnodes > nnodes.pc) {
             warning("[ 'nnodes' > number of detected cores (", par.nnodes, ">", nnodes.pc, ") =>  par.nnodes=", nnodes.pc, " ] !",)
             par.nnodes <- nnodes.pc
           } # ELSE end
         if (par.nnodes > npart) {
           warning("[ 'par.nnodes' > npart (", par.nnodes, ">", npart, ") =>  par.nnodes=", npart, " ] !")
           par.nnodes <- npart
         } # ELSE end  
         if (verbose) message("[ Number of cores/nodes used    : ", par.nnodes, " ]")                 
               
         if (parallel=="parallel") {
             ifelse(write2disk, 
                    cl <- parallel::makeForkCluster(nnodes = par.nnodes, outfile=logfile.fname),
                    cl <- parallel::makeForkCluster(nnodes = par.nnodes) )         
         } else if (parallel=="parallelWin") {      
             ifelse(write2disk,
                    cl <- parallel::makeCluster(par.nnodes, outfile=logfile.fname),
                    cl <- parallel::makeCluster(par.nnodes) )
             pckgFn <- function(packages) {
               for(i in packages) library(i, character.only = TRUE)
             } # 'packFn' END
             parallel::clusterCall(cl, pckgFn, par.pkgs)
           } # ELSE end                   

           if (parallel=="parallelWin") {
             worker.exports <- par.export
             if (is.null(worker.exports))
               worker.exports <- ls.str(mode="function", envir=par.env)

             if ( (fn.name=="hydromod") | (fn.name == "hydromodInR") ) {
               fun.args <- character()
               for (fun.arg in c("out.FUN", "gof.FUN")) {
                 fun.name <- model.FUN.args[[fun.arg]]
                 if (is.character(fun.name) && length(fun.name) == 1)
                   fun.args <- c(fun.args, fun.name)
               } # FOR end
               worker.exports <- c(worker.exports, fun.args)
             } # IF end

             if (fn.name == "hydromodInR") {
               fn.default.vars <- as.character(formals(model.FUN))
               worker.exports <- c(worker.exports, fn.default.vars)
             } # IF end

             worker.exports <- unique(worker.exports)
             worker.exports <- worker.exports[nzchar(worker.exports)]
             worker.exports <- worker.exports[worker.exports %in% ls(envir=par.env, all.names=TRUE)]

             if (length(worker.exports) > 0)
               parallel::clusterExport(cl, worker.exports, envir=par.env)
           } # IF end
                            
           if (fn.name=="hydromod") {
             if (!("model.drty" %in% names(formals(hydromod)) )) {
                stop("[ Invalid argument: 'model.drty' has to be an argument of the 'hydromod' function! ]")
             } else { # Copying the files in 'model.drty' as many times as the number of cores
             
                 model.drty <- path.expand(model.FUN.args$model.drty)
                 
                 files <- list.files(model.drty, full.names=TRUE, include.dirs=TRUE) 
                 tmp <- which(basename(files)=="parallel")
                 if (length(tmp) > 0) files <- files[-tmp]
                 parallel.drty <- paste(file.path(model.drty), "/parallel", sep="")

                 if (file.exists(parallel.drty)) {                      
                   if (verbose) message("[ Removing the 'parallel' directory ... ]")    
                   try(unlink(parallel.drty, recursive=TRUE, force=TRUE))
                 } # IF end 
                 dir.create(parallel.drty)

                 mc.dirs <- character(par.nnodes)
                 if (verbose) message("                                                     ")
                 for (i in 1:par.nnodes) {
                   mc.dirs[i] <- paste(parallel.drty, "/", i, "/", sep="")
                   dir.create(mc.dirs[i])
                   if (verbose) message("[ Copying model input files to directory '", mc.dirs[i], "' ... ]")
                   file.copy(from=files, to=mc.dirs[i], overwrite=TRUE, recursive=TRUE)
                 } # FOR end
                 
                 tmp       <- ceiling(npart/par.nnodes)        
                 part.dirs <- rep(mc.dirs, tmp)[1:npart]  
               } # ELSE end                 
           } # IF end
           
         } # ELSE end  
  
    }  # IF end    
    ############################################################################     

    if (method == "spso2011-sa" && is.na(SA.replicas)) {
      # Uses fewer replicas in serial runs while filling the realised parallel workers.
      if (parallel == "none") {
        SA.replicas <- min(4L, npart)
      } else SA.replicas <- min(6L, npart, max(2L, as.integer(par.nnodes)))
    } # IF automatic replica count end
    if (method == "spso2011-sa") SA.replicas <- as.integer(SA.replicas)

    ############################################################################
    # 2) Initialization of Swarm location and velocities                       #
    ############################################################################

    X.Boundaries.current <- X.Boundaries

    Vmax  <- lambda*Lmax

    X <- InitializateX(npart=npart, x.MinMax=X.Boundaries, x.ini.type=Xini.type)
    V <- InitializateV(npart=npart, x.MinMax=X.Boundaries, v.ini.type=Vini.type, 
                       Xini=X)
    V <- matrix( apply(V, MARGIN=1, FUN=velocity.boundary.treatment, vmax=Vmax), nrow=npart,byrow=TRUE)

    if (!missing(par)) {
      if (!any(is.na(par)) && all(t(par)>=lower) && all(t(par)<=upper)) { 
	      if (is(par, "numeric")) {
	        X[1,] <- par 
	      } else if ( (is(par, "matrix")) | (is(par, "data.frame")) ) {
	          tmp <- ncol(par)
	          if ( tmp != n )
	            stop( "Invalid argument: 'ncol(par) != n' (",tmp, "!=", n, ")" )
	          tmp <- nrow(par)
	          X[1:tmp,] <- par 
	        } # ELSE end
      } # IF end  
    } # IF end

    X.best.part <- X

    # Worst possible value defined for the objective function
    if(MinMax == "max") { 
      fn.worst.value <- -.Machine$double.xmax/2
    } else fn.worst.value <- +.Machine$double.xmax/2
                            
    pbest.fit            <- rep(fn.worst.value, npart)     
    pbest.fit.iter       <- fn.worst.value
    pbest.fit.iter.prior <- fn.worst.value*2
			    
    gbest.fit       <- fn.worst.value
    gbest.fit.iter  <- rep(gbest.fit, maxit)
    gbest.fit.prior <- gbest.fit
    gbest.pos       <- 1

    Xt.fitness <- matrix(rep(NA, maxit*npart), ncol=npart, nrow=maxit, byrow=TRUE)       

    if (topology != "random") {
      nc <- K  
      if (trunc(K/2) != ceiling(K/2)) {
        N   <- (K-1)/2
      } else N  <- K/2
      if (trunc(K/2) != ceiling(K/2)) {
        NN  <- 1
      } else NN  <- 0

      X.neighbours <- matrix(rep(-NA, nc*npart), ncol=nc, nrow=npart, byrow=TRUE)
      for ( i in 1:npart) {
	      for ( j in -N:N ) {
	        neigh.index <- i + j
	        if ( neigh.index  < 1 ) neigh.index <- npart + neigh.index
	        if ( neigh.index  > npart ) neigh.index <- neigh.index - npart

	        X.neighbours[i,j+N+NN] <- neigh.index
	      } # FOR end
      } # FOR end                      
    } # IF end 

    LocalBest.fit <- rep(fn.worst.value, npart)

    LocalBest.pos <- 1:npart

    if ( topology == "ipso") { 
      ngbest.fit <- rep(fn.worst.value, ngbest)

      ngbest.pos <- rep(1, ngbest)
    } else {
	      ngbest.fit <- NA           
	      ngbest.pos <- NA
      } # ELSE end

    ############################################################################  
    #                          Text Files initialization                       #
    ############################################################################  
    if (write2disk) {

      if (verbose) message("                                                                                ")
      if (verbose) message("================================================================================")
      if (verbose) message("[ Writing the 'PSO_logfile.txt' file ...                                       ]")
      if (verbose) message("================================================================================") 


      PSOparam.fname <- paste(file.path(drty.out), "/", "PSO_logfile.txt", sep="")
      PSOparam.TextFile  <- file(PSOparam.fname , "w+")
      
      writeLines("================================================================================", PSOparam.TextFile)  
      writeLines(c("hydroPSO version  :", sessionInfo()$otherPkgs$hydroPSO$Version), PSOparam.TextFile, sep="  ")
      writeLines("", PSOparam.TextFile) 
      writeLines(c("hydroPSO Built    :", sessionInfo()$otherPkgs$hydroPSO$Built), PSOparam.TextFile, sep="  ")
      writeLines("", PSOparam.TextFile) 
      writeLines(c("R version         :", sessionInfo()[[1]]$version.string), PSOparam.TextFile, sep="  ")
      writeLines("", PSOparam.TextFile) 
      writeLines(c("Platform          :", sessionInfo()[[1]]$platform), PSOparam.TextFile, sep="  ")
      writeLines("", PSOparam.TextFile) 
      writeLines("================================================================================", PSOparam.TextFile)  
      Time.Ini <- Sys.time()
      writeLines(c("Starting Time     :", date()), PSOparam.TextFile, sep=" ")
      writeLines("", PSOparam.TextFile) 
      writeLines("================================================================================", PSOparam.TextFile)  
      writeLines(c("Objective Function:", fn.name), PSOparam.TextFile, sep=" ") 
      writeLines("", PSOparam.TextFile) 
      writeLines(c("MinMax            :", MinMax), PSOparam.TextFile, sep=" ") 
      writeLines("", PSOparam.TextFile) 
      writeLines(c("Dimension         :", n), PSOparam.TextFile, sep=" ") 
      writeLines("", PSOparam.TextFile) 
      writeLines(c("Nmbr of Particles :", npart), PSOparam.TextFile, sep=" ") 
      writeLines("", PSOparam.TextFile) 
      writeLines(c("Max Iterations    :", maxit), PSOparam.TextFile, sep=" ") 
      writeLines("", PSOparam.TextFile) 
      writeLines(c("Method            :", method), PSOparam.TextFile, sep=" ") 
      writeLines("", PSOparam.TextFile) 
      if ( method == "ipso" ) {
	    writeLines(c("ngbest           :", ngbest), PSOparam.TextFile, sep=" ") 
	    writeLines("", PSOparam.TextFile)  
      } # IF end
      if ( method == "spso2011-sa" ) {
        writeLines(c("SA.T0            :", SA.T0), PSOparam.TextFile, sep=" ")
        writeLines("", PSOparam.TextFile)
        writeLines(c("SA.alpha         :", SA.alpha), PSOparam.TextFile, sep=" ")
        writeLines("", PSOparam.TextFile)
        writeLines(c("SA.scale         :", SA.scale), PSOparam.TextFile, sep=" ")
        writeLines("", PSOparam.TextFile)
        writeLines(c("SA.nsteps        :", SA.nsteps), PSOparam.TextFile, sep=" ")
        writeLines("", PSOparam.TextFile)
        writeLines(c("SA.ndim          :", SA.ndim), PSOparam.TextFile, sep=" ")
        writeLines("", PSOparam.TextFile)
        writeLines(c("SA.stagnation    :", SA.stagnation), PSOparam.TextFile, sep=" ")
        writeLines("", PSOparam.TextFile)
        writeLines(c("SA.reheat        :", SA.reheat), PSOparam.TextFile, sep=" ")
        writeLines("", PSOparam.TextFile)
        writeLines(c("SA.target.accept :", SA.target.accept), PSOparam.TextFile, sep=" ")
        writeLines("", PSOparam.TextFile)
        writeLines(c("SA.replicas      :", SA.replicas), PSOparam.TextFile, sep=" ")
        writeLines("", PSOparam.TextFile)
        writeLines(c("SA.swap.every    :", SA.swap.every), PSOparam.TextFile, sep=" ")
        writeLines("", PSOparam.TextFile)
        writeLines(c("SA.scout.fraction:", SA.scout.fraction), PSOparam.TextFile, sep=" ")
        writeLines("", PSOparam.TextFile)
        writeLines(c("SA.scout.gens    :", SA.scout.generations), PSOparam.TextFile, sep=" ")
        writeLines("", PSOparam.TextFile)
        writeLines(c("SA.radius.thr    :", SA.radius.threshold), PSOparam.TextFile, sep=" ")
        writeLines("", PSOparam.TextFile)
        writeLines(c("SA.budget.frac   :", SA.budget.fraction), PSOparam.TextFile, sep=" ")
        writeLines("", PSOparam.TextFile)
      } # IF method == "spso2011-sa" end
      if ( method == "spso2011-de" ) {
        writeLines(c("DE.F initial     :", DE.F), PSOparam.TextFile, sep=" ")
        writeLines("", PSOparam.TextFile)
        writeLines(c("DE.c             :", DE.c), PSOparam.TextFile, sep=" ")
        writeLines("", PSOparam.TextFile)
        writeLines(c("DE archive size  :", DE.archive.size), PSOparam.TextFile, sep=" ")
        writeLines("", PSOparam.TextFile)
      } # IF method == "spso2011-de" end
      if ( method == "spso2011-cde" ) {
        writeLines(c("CDE.H            :", CDE.H), PSOparam.TextFile, sep=" ")
        writeLines("", PSOparam.TextFile)
        writeLines(c("CDE.p            :", CDE.p), PSOparam.TextFile, sep=" ")
        writeLines("", PSOparam.TextFile)
        writeLines(c("CDE q range      :", CDE.qmin, CDE.qmax), PSOparam.TextFile, sep=" ")
        writeLines("", PSOparam.TextFile)
        writeLines(c("CDE.stag.iter    :", CDE.stag.iter), PSOparam.TextFile, sep=" ")
        writeLines("", PSOparam.TextFile)
        writeLines(c("CDE.diversity    :", CDE.diversity), PSOparam.TextFile, sep=" ")
        writeLines("", PSOparam.TextFile)
        writeLines(c("CDE.eigen        :", CDE.eigen), PSOparam.TextFile, sep=" ")
        writeLines("", PSOparam.TextFile)
        writeLines(c("CDE.eigen.freq   :", CDE.eigen.freq), PSOparam.TextFile, sep=" ")
        writeLines("", PSOparam.TextFile)
      } # IF method == "spso2011-cde" end
      if ( method == "spso2011-dds" ) {
        writeLines(c("DDS.r            :", DDS.r), PSOparam.TextFile, sep=" ")
        writeLines("", PSOparam.TextFile)
        writeLines(c("DDS.stag.iter    :", DDS.stag.iter), PSOparam.TextFile, sep=" ")
        writeLines("", PSOparam.TextFile)
      } # IF method == "spso2011-dds" end
      if ( method == "spso2011-pdds" ) {
        writeLines(c("PDDS.warmup       :", PDDS.warmup), PSOparam.TextFile, sep=" ")
        writeLines("", PSOparam.TextFile)
        writeLines(c("PDDS.batch.every  :", PDDS.batch.every), PSOparam.TextFile, sep=" ")
        writeLines("", PSOparam.TextFile)
        writeLines(c("PDDS.nchains      :", PDDS.nchains), PSOparam.TextFile, sep=" ")
        writeLines("", PSOparam.TextFile)
        writeLines(c("PDDS.archive.size :", PDDS.archive.size), PSOparam.TextFile, sep=" ")
        writeLines("", PSOparam.TextFile)
        writeLines(c("PDDS.r            :", PDDS.r), PSOparam.TextFile, sep=" ")
        writeLines("", PSOparam.TextFile)
        writeLines(c("PDDS.epoch        :", PDDS.epoch), PSOparam.TextFile, sep=" ")
        writeLines("", PSOparam.TextFile)
        writeLines(c("PDDS migration    :", PDDS.migration.frac), PSOparam.TextFile, sep=" ")
        writeLines("", PSOparam.TextFile)
        writeLines(c("PDDS min distance :", PDDS.min.distance), PSOparam.TextFile, sep=" ")
        writeLines("", PSOparam.TextFile)
      } # IF method == "spso2011-pdds" end
      writeLines(c("Topology          :", topology), PSOparam.TextFile, sep=" ") 
      writeLines("", PSOparam.TextFile)  
      if ( (topology == "lbest") | (topology == "random") ) {
	    writeLines(c("K                 :", K), PSOparam.TextFile, sep=" ") 
	    writeLines("", PSOparam.TextFile)  
	    writeLines(c("iter.ini          :", iter.ini), PSOparam.TextFile, sep=" ") 
	    writeLines("", PSOparam.TextFile)  
      } # IF end
      writeLines(c("Boundary wall     :", boundary.wall), PSOparam.TextFile, sep=" ") 
      writeLines("", PSOparam.TextFile) 
      writeLines(c("normalise         :", normalise), PSOparam.TextFile, sep=" ") 
      writeLines("", PSOparam.TextFile) 
      writeLines(c("Xini.type         :", Xini.type), PSOparam.TextFile, sep=" ") 
      writeLines("", PSOparam.TextFile) 
      writeLines(c("Vini.type         :", Vini.type), PSOparam.TextFile, sep=" ")
      writeLines("", PSOparam.TextFile)  
      writeLines(c("Best update method:", best.update), PSOparam.TextFile, sep=" ") 
      writeLines("", PSOparam.TextFile) 
      writeLines(c("Random update     :", random.update), PSOparam.TextFile, sep=" ") 
      writeLines("", PSOparam.TextFile) 
      if (use.TVc1) {
	    writeLines(c("use.TVc1          :", use.TVc1), PSOparam.TextFile, sep=" ") 
	    writeLines("", PSOparam.TextFile) 
	    writeLines(c("TVc1.rng          :", TVc1.rng), PSOparam.TextFile, sep=" ") 
	    writeLines("", PSOparam.TextFile) 
	    writeLines(c("TVc1.type         :", TVc1.type), PSOparam.TextFile, sep=" ") 
        writeLines("", PSOparam.TextFile) 
	    writeLines(c("TVc1.exp          :", TVc1.exp), PSOparam.TextFile, sep=" ") 
	    writeLines("", PSOparam.TextFile) 
      } else {
        writeLines(c("c1                :", c1), PSOparam.TextFile, sep=" ") 
        writeLines("", PSOparam.TextFile) 
      } # ELSE end
      if (use.TVc2) {
	    writeLines(c("use.TVc2          :", use.TVc2), PSOparam.TextFile, sep=" ") 
	    writeLines("", PSOparam.TextFile) 
	    writeLines(c("TVc2.rng          :", TVc2.rng), PSOparam.TextFile, sep=" ") 
	    writeLines("", PSOparam.TextFile) 
	    writeLines(c("TVc2.type         :", TVc2.type), PSOparam.TextFile, sep=" ") 
	    writeLines("", PSOparam.TextFile) 
	    writeLines(c("TVc2.exp          :", TVc2.exp), PSOparam.TextFile, sep=" ") 
	    writeLines("", PSOparam.TextFile) 
      } else {
          writeLines(c("c2                :", c2), PSOparam.TextFile, sep=" ") 
          writeLines("", PSOparam.TextFile) 
        } # ELSE end 
      writeLines(c("use.IW            :", use.IW), PSOparam.TextFile, sep=" ") 
      writeLines("", PSOparam.TextFile) 
      if (use.IW) {
	    writeLines(c("IW.w              :", IW.w), PSOparam.TextFile, sep=" ") 
	    writeLines("", PSOparam.TextFile) 
	    if ( length(IW.w) > 1 ) {
  	      writeLines(c("IW.type           :", IW.type), PSOparam.TextFile, sep=" ") 
	      writeLines("", PSOparam.TextFile) 
	      writeLines(c("IW.exp            :", IW.exp), PSOparam.TextFile, sep=" ") 
	      writeLines("", PSOparam.TextFile) 
	    } # IF end
      }  # IF end
      if (use.TVlambda) {
	    writeLines(c("use.TVlambda      :", use.TVlambda), PSOparam.TextFile, sep=" ") 
	    writeLines("", PSOparam.TextFile) 
	    writeLines(c("TVlambda.rng      :", TVlambda.rng), PSOparam.TextFile, sep=" ") 
	    writeLines("", PSOparam.TextFile) 
	    writeLines(c("TVlambda.type     :", TVlambda.type), PSOparam.TextFile, sep=" ") 
	    writeLines("", PSOparam.TextFile) 
	    writeLines(c("TVlambda.exp      :", TVlambda.exp), PSOparam.TextFile, sep=" ") 
	    writeLines("", PSOparam.TextFile) 
      } else {
        writeLines(c("lambda            :", lambda), PSOparam.TextFile, sep=" ") 
        writeLines("", PSOparam.TextFile)   
      }  # ELSE end
      writeLines(c("use.RG            :", use.RG), PSOparam.TextFile, sep=" ") 
      writeLines("", PSOparam.TextFile) 
      if (use.RG) {
        writeLines(c("RG.thr            :", RG.thr), PSOparam.TextFile, sep=" ") 
	    writeLines("", PSOparam.TextFile) 
	    writeLines(c("RG.r              :", RG.r), PSOparam.TextFile, sep=" ") 
	    writeLines("", PSOparam.TextFile) 
	    writeLines(c("RG.miniter        :", RG.miniter), PSOparam.TextFile, sep=" ") 
	    writeLines("", PSOparam.TextFile) 	
      }  # IF end
      writeLines(c("maxfn             :", maxfn), PSOparam.TextFile, sep=" ")  
      writeLines("", PSOparam.TextFile) 
      writeLines(c("abstol            :", abstol), PSOparam.TextFile, sep=" ")  
      writeLines("", PSOparam.TextFile) 
      writeLines(c("reltol            :", reltol), PSOparam.TextFile, sep=" ")  
      writeLines("", PSOparam.TextFile)       
      writeLines(c("parallel          :", parallel), PSOparam.TextFile, sep=" ")  
      writeLines("", PSOparam.TextFile)  
      if (parallel!="none") {
        writeLines(c("par.nnodes        :", par.nnodes), PSOparam.TextFile, sep=" ") 
	    writeLines("", PSOparam.TextFile)
	    writeLines(c("par.pkgs          :", as.character(unlist(par.pkgs, use.names=FALSE)) ),
                                          PSOparam.TextFile, sep=" ")
	    writeLines("", PSOparam.TextFile)     
      } # IF end
      close(PSOparam.TextFile) 

      # File 'Model_out.txt' #
      OFout.Text.fname <- paste(file.path(drty.out), "/", "Model_out.txt", sep="")
      OFout.Text.file  <- file(OFout.Text.fname, "w+")
      
      writeLines(c("Iter", "Part", "GoF", "Model_Output"), OFout.Text.file, sep="  ") 
      writeLines("", OFout.Text.file) 
      close(OFout.Text.file) 

      # File 'Particles.txt' #
      Particles.Textfname <- paste(file.path(drty.out), "/", "Particles.txt", sep="")
      Particles.TextFile  <- file(Particles.Textfname, "w+")
      
      writeLines(c("Iter", "Part", "GoF", param.IDs), Particles.TextFile, sep=" ") 
      writeLines("", Particles.TextFile) 
      close(Particles.TextFile) 

      # File 'Velocities.txt' #
      Velocities.Textfname <- paste(file.path(drty.out), "/", "Velocities.txt", sep="")
      Velocities.TextFile  <- file(Velocities.Textfname, "w+")
      
      writeLines(c("Iter", "Part", "GoF", param.IDs), Velocities.TextFile, sep=" ") 
      writeLines("", Velocities.TextFile) 
      close(Velocities.TextFile) 

      # File 'ConvergenceMeasures.txt' #
      ConvergenceMeasures.Textfname <- paste(file.path(drty.out), "/", "ConvergenceMeasures.txt", sep="")
      ConvergenceMeasures.TextFile  <- file(ConvergenceMeasures.Textfname, "w+")
      
      writeLines(c("Iter", "Gbest", "GbestRate[%]", "IterBestFit", "normSwarmRadius", "|gbest-mean(pbest)|/mean(pbest)[%]"), ConvergenceMeasures.TextFile, sep=" ") 
      writeLines("", ConvergenceMeasures.TextFile) 
      close(ConvergenceMeasures.TextFile)   

      # File 'BestParamPerIter.txt' #
      BestParamPerIter.Textfname <- paste(file.path(drty.out), "/", "BestParamPerIter.txt", sep="")
      BestParamPerIter.TextFile  <- file(BestParamPerIter.Textfname, "w+")
      
      writeLines(c("Iter", "GoF", param.IDs), BestParamPerIter.TextFile, sep="  ") 
      writeLines("", BestParamPerIter.TextFile) 
      close(BestParamPerIter.TextFile) 
      
      # File 'PbestPerIter.txt' #
      PbestPerIter.Textfname <- paste(file.path(drty.out), "/", "PbestPerIter.txt", sep="")
      PbestPerIter.TextFile  <- file(PbestPerIter.Textfname, "w+")
      
      writeLines(c("Iter", paste("Part", 1:npart, sep="") ), PbestPerIter.TextFile, sep="  ") 
      writeLines("", PbestPerIter.TextFile) 
      close(PbestPerIter.TextFile) 
      
      # File 'LocalBestPerIter.txt' #
      LocalBestPerIter.Textfname <- paste(file.path(drty.out), "/", "LocalBestPerIter.txt", sep="")
      LocalBestPerIter.TextFile  <- file(LocalBestPerIter.Textfname, "w+")
      
      writeLines(c("Iter", paste("Part", 1:npart, sep="") ), LocalBestPerIter.TextFile, sep="  ") 
      writeLines("", LocalBestPerIter.TextFile) 
      close(LocalBestPerIter.TextFile) 

      if (use.RG) {
	      # File 'Xmin.txt' #
	      Xmin.Text.fname <- paste(file.path(drty.out), "/", "Xmin.txt", sep="")
	      Xmin.Text.file  <- file(Xmin.Text.fname, "w+")
	
        writeLines(c("Iter", param.IDs), Xmin.Text.file, sep="  ") 
	      writeLines("", Xmin.Text.file) 
	      writeLines(as.character(c(1, X.Boundaries[,1])), Xmin.Text.file, sep=" ")
	      writeLines("", Xmin.Text.file) 
	      close(Xmin.Text.file)     

        # File 'Xmax.txt' #
	      Xmax.Text.fname <- paste(file.path(drty.out), "/", "Xmax.txt", sep="")
	      Xmax.Text.file  <- file(Xmax.Text.fname, "w+")
	
	      writeLines(c("Iter", param.IDs ), Xmax.Text.file, sep="  ")
	      writeLines("", Xmax.Text.file)  
	      writeLines(as.character(c(1, X.Boundaries[,2])), Xmax.Text.file, sep=" ")
	      writeLines("", Xmax.Text.file) 
	      close(Xmax.Text.file)      
      } # IF end  

      if ( (fn.name=="hydromod") | (fn.name=="hydromodInR" ) ) {
	      ##############################################################################
	      # 2)                           Writing Info File
	      ##############################################################################  

	      if (verbose) message("================================================================================")
	      if (verbose) message("[ Writing the 'hydroPSO_logfile.txt' file ...                                  ]")
	      if (verbose) message("================================================================================") 


	      # File 'hydroPSO_logfile.txt' #        
	      hydroPSOparam.fname <- paste(file.path(drty.out), "/", "hydroPSO_logfile.txt", sep="")
	      hydroPSOparam.TextFile  <- file(hydroPSOparam.fname , "w+")
	
	      writeLines("================================================================================", hydroPSOparam.TextFile) 
	      writeLines(c("Platform               :", sessionInfo()[[1]]$platform), hydroPSOparam.TextFile, sep="  ")
	      writeLines("", hydroPSOparam.TextFile) 
	      writeLines(c("R version              :", sessionInfo()[[1]]$version.string), hydroPSOparam.TextFile, sep="  ")
	      writeLines("", hydroPSOparam.TextFile) 
	      writeLines(c("hydroPSO version       :", sessionInfo()$otherPkgs$hydroPSO$Version), hydroPSOparam.TextFile, sep="  ")
	      writeLines("", hydroPSOparam.TextFile) 
	      writeLines(c("hydroPSO Built         :", sessionInfo()$otherPkgs$hydroPSO$Built), hydroPSOparam.TextFile, sep="  ")
	      writeLines("", hydroPSOparam.TextFile) 
	      writeLines("================================================================================", hydroPSOparam.TextFile) 
	      Time.Ini <- Sys.time()
	      writeLines(c("Starting Time          :", date()), hydroPSOparam.TextFile, sep="  ")
	      writeLines("", hydroPSOparam.TextFile) 
	      writeLines("================================================================================", hydroPSOparam.TextFile) 
	      if (fn.name=="hydromod") {
          writeLines(c("PSO Input Directory    :", drty.in), hydroPSOparam.TextFile, sep=" ") 
	        writeLines("", hydroPSOparam.TextFile) 
	        writeLines(c("PSO Output Directory   :", drty.out), hydroPSOparam.TextFile, sep=" ") 
	        writeLines("", hydroPSOparam.TextFile) 
	        writeLines(c("Parameter Ranges       :", basename(param.ranges)), hydroPSOparam.TextFile, sep=" ") 
	        writeLines("", hydroPSOparam.TextFile) 
        } # IF end  
        try(writeLines(c("hydromod function      :", model.FUN.name), hydroPSOparam.TextFile, sep=" ") , TRUE)
	      writeLines("", hydroPSOparam.TextFile) 
	      if ( (fn.name=="hydromod") | (fn.name=="hydromodInR") ) {
          writeLines(c("hydromod args          :"), hydroPSOparam.TextFile, sep=" ") 
	        writeLines("", hydroPSOparam.TextFile) 
	        for ( i in 1:length(model.FUN.args) ) {
	          arg.name1  <- names(model.FUN.args)[i]
	          arg.name  <- format(paste("  ", arg.name1, sep=""), width=22, justify="left" )
	          arg.value <- ""
            if (arg.name1 != "param.values") 
              arg.value <- try( as.character( eval( model.FUN.args[[i]]) ), TRUE)
	          writeLines(c(arg.name, ":", arg.value), hydroPSOparam.TextFile, sep=" ") 
	          writeLines("", hydroPSOparam.TextFile) 
          } # FOR end
        } # IF end
        # Closing the text file
        close(hydroPSOparam.TextFile) 

      } # IF 'fn.name' END

    } # IF 'write2disk' end

    ########################################################################
    GPbest.fit.rate <- Inf

    iter     <- 1
    nfn      <- 1
    nfn.eff  <- 1
    iter.rg  <- 1
    nregroup <- 0

    iter.tv  <- iter
    niter.tv <- maxit

    if (method == "spso2011-sa") {
      SA.range <- X.Boundaries[,2] - X.Boundaries[,1] # Scales perturbations to each search dimension.
      SA.scale.current <- matrix(SA.scale, nrow=SA.replicas, ncol=n) # Maintains adaptive scales by replica and dimension.
      SA.scale.lower <- min(1E-4, SA.scale) # Preserves deliberately smaller user-supplied scales.
      SA.scale.upper <- max(0.50, SA.scale) # Preserves deliberately larger user-supplied scales.
      SA.dimension.attempts <- matrix(0L, nrow=SA.replicas, ncol=n) # Counts scale updates independently.
      SA.current <- matrix(NA_real_, nrow=SA.replicas, ncol=n) # Stores replica states in PSO coordinates.
      SA.current.fit <- rep(NA_real_, SA.replicas) # Stores the current fitness of each replica.
      SA.temperature <- rep(NA_real_, SA.replicas) # Holds the cold-to-hot temperature ladder.
      SA.initial.temperature <- rep(NA_real_, SA.replicas) # Retains the first calibrated ladder.
      SA.archive.X <- matrix(numeric(), nrow=0, ncol=n) # Retains quality-diverse basins across events.
      SA.archive.fit <- numeric()
      SA.archive.size <- max(2L*SA.replicas, min(50L, 4L*SA.replicas))
      SA.scout.remaining <- integer(npart) # Protects repopulated particles from the incumbent attractor.
      SA.stagnation.count <- 0L # Counts SPSO generations without a new global best.
      SA.trigger.stagnation <- as.integer(SA.stagnation) # Delays retries after unproductive SA events.
      SA.proposals.since.best <- 0L # Triggers ladder reheating during an unproductive event.
      SA.budget.max <- floor(SA.budget.fraction *
                             if (is.finite(maxfn)) maxfn else npart*maxit)
      SA.diagnostics <- c(attempted=0L, accepted=0L, uphill.accepted=0L,
                          global.improvements=0L, invalid=0L, activations=0L,
                          reheats=0L, swap.attempted=0L, swap.accepted=0L,
                          scouts.injected=0L, collapsed.events=0L,
                          emergency.events=0L, productive.events=0L,
                          unproductive.events=0L) # Accumulates auditable SA outcomes.

      better.sa <- function(new.fit, old.fit) {
        # Compares two finite fitness values using the requested optimisation direction.
        if (MinMax == "min") {
          out <- new.fit < old.fit
        } else out <- new.fit > old.fit

        return(out)
      } # 'better.sa' end

      reflect.sa <- function(x) {
        # Reflects arbitrary overshoots into the bounded search interval without point masses.
        lower.sa <- X.Boundaries[,1]
        out <- lower.sa # Keeps fixed dimensions at their only feasible value.
        varying.sa <- SA.range > 0 # Avoids undefined modulo operations for fixed dimensions.
        period.sa <- 2 * SA.range[varying.sa]
        shifted.sa <- (x[varying.sa] - lower.sa[varying.sa]) %% period.sa
        out[varying.sa] <- lower.sa[varying.sa] +
                           ifelse(shifted.sa <= SA.range[varying.sa], shifted.sa,
                                  period.sa - shifted.sa)

        return(out)
      } # 'reflect.sa' end

      update.archive.sa <- function(archive.X, archive.fit, candidates, fitness) {
        # Retains the best solution and then greedily preserves distinct normalised basins.
        candidates <- as.matrix(candidates)
        valid <- is.finite(fitness) & apply(candidates, 1, function(x) all(is.finite(x)))
        combined.X <- rbind(archive.X, candidates[valid, , drop=FALSE])
        combined.fit <- c(archive.fit, fitness[valid])
        if (length(combined.fit) == 0)
          return(list(X=combined.X, fit=combined.fit))

        ranking <- order(combined.fit, decreasing=(MinMax == "max"))
        combined.X <- combined.X[ranking, , drop=FALSE]
        combined.fit <- combined.fit[ranking]
        unique.position <- !duplicated(as.data.frame(signif(combined.X, digits=14)))
        combined.X <- combined.X[unique.position, , drop=FALSE]
        combined.fit <- combined.fit[unique.position]

        width.sa <- SA.range
        width.sa[width.sa == 0] <- 1
        scaled.sa <- sweep(combined.X, 2, X.Boundaries[,1], FUN="-")
        scaled.sa <- sweep(scaled.sa, 2, width.sa, FUN="/")
        selected.sa <- 1L
        deferred.sa <- integer()
        min.distance.sa <- max(1E-8, SA.radius.threshold/2)

        if (nrow(combined.X) > 1) {
          for (i.sa in 2:nrow(combined.X)) {
            difference.sa <- sweep(scaled.sa[selected.sa, , drop=FALSE], 2,
                                   scaled.sa[i.sa, ], FUN="-")
            distance.sa <- sqrt(rowMeans(difference.sa^2))
            if (all(distance.sa >= min.distance.sa)) {
              selected.sa <- c(selected.sa, i.sa)
            } else deferred.sa <- c(deferred.sa, i.sa)
            if (length(selected.sa) >= SA.archive.size) break
          } # FOR archive candidate end
        } # IF multiple archive candidates end

        if (length(selected.sa) < SA.archive.size && length(deferred.sa) > 0) {
          fill.sa <- deferred.sa[seq_len(min(SA.archive.size - length(selected.sa),
                                             length(deferred.sa)))]
          selected.sa <- c(selected.sa, fill.sa)
        } # IF archive needs filling end
        selected.sa <- selected.sa[seq_len(min(length(selected.sa), SA.archive.size))]
        out <- list(X=combined.X[selected.sa, , drop=FALSE],
                    fit=combined.fit[selected.sa])

        return(out)
      } # 'update.archive.sa' end

      select.diverse.sa <- function(candidates, fitness, number) {
        # Selects a good first state followed by farthest-first basin representatives.
        candidates <- as.matrix(candidates)
        number <- min(number, nrow(candidates))
        if (number < 1) return(integer())
        ranking.sa <- order(fitness, decreasing=(MinMax == "max"))
        width.sa <- SA.range
        width.sa[width.sa == 0] <- 1
        scaled.sa <- sweep(candidates, 2, X.Boundaries[,1], FUN="-")
        scaled.sa <- sweep(scaled.sa, 2, width.sa, FUN="/")
        selected.sa <- ranking.sa[1]

        while (length(selected.sa) < number) {
          available.sa <- setdiff(ranking.sa, selected.sa)
          min.distance.sa <- vapply(available.sa, function(i.sa) {
            difference.sa <- sweep(scaled.sa[selected.sa, , drop=FALSE], 2,
                                   scaled.sa[i.sa, ], FUN="-")
            min(sqrt(rowMeans(difference.sa^2)))
          }, numeric(1))
          selected.sa <- c(selected.sa, available.sa[which.max(min.distance.sa)])
        } # WHILE selecting diverse states end

        return(selected.sa)
      } # 'select.diverse.sa' end

      evaluate.batch.sa <- function(X.sa) {
        # Evaluates one replica sweep with the same backend used by swarm batches.
        X.sa <- as.matrix(X.sa)
        n.sa <- nrow(X.sa)
        if (normalise) {
          X.eval.sa <- sweep(X.sa, 2, upper.ini - lower.ini, FUN="*")
          X.eval.sa <- sweep(X.eval.sa, 2, lower.ini, FUN="+")
        } else X.eval.sa <- X.sa

        if ( (fn.name != "hydromod") & (fn.name != "hydromodInR") ) {
          if (parallel == "none") {
            fit.sa <- apply(X.eval.sa, 1, fn, ...)
          } else if (parallel %in% c("parallel", "parallelWin")) {
            fit.sa <- parallel::parRapply(cl=cl, x=X.eval.sa, FUN=fn, ...)
          } else {
            fit.sa <- unlist(parallel::mclapply(seq_len(n.sa), FUN=fn1, x=X.eval.sa,
                                                ..., mc.cores=min(par.nnodes, n.sa),
                                                mc.silent=TRUE))
          } # ELSE multicore ordinary objective end
          out.sa <- lapply(as.numeric(fit.sa), function(value.sa)
            list(GoF=value.sa, sim=NULL))
        } else if (fn.name == "hydromod") {
          verbose.sa <- if ("verbose" %in% names(model.FUN.args))
                          model.FUN.args[["verbose"]] else FALSE
          dirs.sa <- if (parallel == "none") character() else part.dirs[seq_len(n.sa)]
          args.sa <- list(Particles=X.eval.sa, iter=iter, npart=n.sa, maxit=maxit,
                          REPORT=REPORT, verbose=verbose.sa, digits=digits,
                          model.FUN=model.FUN, model.FUN.args=model.FUN.args,
                          parallel=parallel, ncores=par.nnodes, part.dirs=dirs.sa)
          if (parallel == "none") {
            out.sa <- do.call(lapply, c(list(X=seq_len(n.sa), FUN=hydromod.eval), args.sa))
          } else if (parallel %in% c("parallel", "parallelWin")) {
            out.sa <- do.call(parallel::clusterApply,
                              c(list(cl=cl, x=seq_len(n.sa), fun=hydromod.eval), args.sa))
          } else {
            out.sa <- do.call(parallel::mclapply,
                              c(list(X=seq_len(n.sa), FUN=hydromod.eval), args.sa,
                                list(mc.cores=min(par.nnodes, n.sa), mc.silent=TRUE,
                                     mc.cleanup=TRUE)))
          } # ELSE multicore external model end
        } else {
          X.model.sa <- .changeParamValues(X.eval.sa, change.type=change.type,
                                           refValue=refValue)
          args.sa <- list(Particles=X.model.sa, model.FUN=model.FUN,
                          model.FUN.args=model.FUN.args)
          if (parallel == "none") {
            out.sa <- do.call(lapply,
                              c(list(X=seq_len(n.sa), FUN=hydromodInR.eval), args.sa))
          } else if (parallel %in% c("parallel", "parallelWin")) {
            out.sa <- do.call(parallel::clusterApply,
                              c(list(cl=cl, x=seq_len(n.sa), fun=hydromodInR.eval), args.sa))
          } else {
            out.sa <- do.call(parallel::mclapply,
                              c(list(X=seq_len(n.sa), FUN=hydromodInR.eval), args.sa,
                                list(mc.cores=min(par.nnodes, n.sa), mc.silent=TRUE,
                                     mc.cleanup=TRUE)))
          } # ELSE multicore R-internal model end
        } # ELSE R-internal model end

        fit.sa <- vapply(out.sa, function(value.sa) {
          if (length(value.sa[["GoF"]]) != 1)
            stop("Invalid objective output: each SA replica must return one fitness value")
          as.numeric(value.sa[["GoF"]])
        }, numeric(1))
        out <- list(fit=fit.sa, model=out.sa)

        return(out)
      } # 'evaluate.batch.sa' end
    } # IF method == "spso2011-sa" end

    if (method == "spso2011-dds") {
      evaluate.local <- function(x, part, operator) {
        # Converts internal normalised coordinates before evaluating the objective function.
        if (normalise) {
          x.eval <- x * (upper.ini - lower.ini) + lower.ini
        } else x.eval <- x

        if ( (fn.name != "hydromod") & (fn.name != "hydromodInR") ) {
          out <- list(GoF=as.numeric(fn(x.eval, ...)), sim=NULL) # Evaluates one ordinary R objective sequentially.
        } else if (fn.name == "hydromod") {
          # Reuses one model directory after the parallel swarm batch has completed.
          local.part.dirs <- if (parallel == "none") character() else part.dirs[part]
          out <- hydromod.eval(part=1, Particles=matrix(x.eval, nrow=1), iter=iter,
                               npart=1, maxit=maxit, REPORT=REPORT, verbose=FALSE,
                               digits=digits, model.FUN=model.FUN,
                               model.FUN.args=model.FUN.args, parallel=parallel,
                               ncores=par.nnodes, part.dirs=local.part.dirs)
        } else {
          # Applies the requested parameter transformation before one R-model evaluation.
          x.model <- .changeParamValues(matrix(x.eval, nrow=1),
                                        change.type=change.type, refValue=refValue)
          out <- hydromodInR.eval(part=1, Particles=x.model, model.FUN=model.FUN,
                                  model.FUN.args=model.FUN.args)
        } # ELSE end

        if (length(out[["GoF"]]) != 1)
          stop("Invalid objective output: the ", operator,
               " evaluation must return one fitness value")

        return(out)
      } # 'evaluate.local' end
    } # IF method uses a sequential local proposal end

    if (method == "spso2011-de") {
      DE.mu.F <- DE.F # Starts success-history adaptation from the user-supplied value.
      DE.F.previous <- rep(NA_real_, npart) # Initial particles were not generated by DE mutation.
      DE.archive <- replicate(npart, matrix(numeric(), nrow=0, ncol=n),
                              simplify=FALSE) # Creates one informant-only archive per particle.
    } # IF method == "spso2011-de" end

    if (method == "spso2011-cde") {
      CDE.M.F <- rep(0.5, CDE.H) # Initialises circular SHADE mutation memories.
      CDE.M.CR <- rep(0.5, CDE.H) # Initialises circular SHADE crossover memories.
      CDE.memory.pointer <- 1L # Selects the first memory cell to replace after success.
      CDE.archive <- matrix(numeric(), nrow=0, ncol=n) # Stores successfully replaced parents.
      CDE.archive.max <- npart # Uses the standard one-population-size archive bound.
      CDE.operator.previous <- rep(NA_character_, npart) # Initial positions have no generating operator.
      CDE.parent.X.previous <- matrix(NA_real_, nrow=npart, ncol=n)
      CDE.parent.fit.previous <- rep(NA_real_, npart) # Initial positions have no evaluated parents.
      CDE.F.previous <- rep(NA_real_, npart) # Records DE parameters by evaluated particle.
      CDE.CR.previous <- rep(NA_real_, npart)
      CDE.coordinate.previous <- rep(NA_character_, npart)
      CDE.strategy.previous <- rep(NA_character_, npart)
      CDE.operator.score <- c(pso=.Machine$double.eps, de=.Machine$double.eps)
      CDE.coordinate.score <- c(original=.Machine$double.eps,
                                eigen=.Machine$double.eps) # Starts coordinate competition evenly.
      CDE.particle.stagnation <- integer(npart) # Prioritises unproductive particles for DE.
      CDE.stagnation <- 0L # Counts generations without a new global best.
      CDE.gbest.previous <- gbest.fit # Tracks competitive progress independently.
      CDE.basis <- cde.coordinate.basis(X.best.part) # Starts with an identity-compatible basis.
      CDE.q.de <- 0.5 # Allocates half of the first proposal batch to each operator.
      CDE.diversity.current <- NA_real_ # Receives relative population spread after evaluation.
      CDE.diagnostics <- c(pso.evaluated=0L, de.evaluated=0L, de.accepted=0L,
                           emergency.evaluated=0L, original.evaluated=0L,
                           eigen.evaluated=0L) # Accumulates auditable operator outcomes.
    } # IF method == "spso2011-cde" end

    if (method == "spso2011-dds") {
      dds.range <- X.Boundaries[,2] - X.Boundaries[,1] # Scales DDS perturbations by dimension.
      dds.stagnation <- 0L # Counts completed generations without gbest improvement.
      dds.active <- FALSE # Delays DDS until SPSO-2011 stops improving.
      dds.iter <- 0L # Counts DDS proposals after activation.
      dds.maxiter <- 1L # Receives the remaining proposal budget at activation.
      dds.gbest.prior <- gbest.fit # Tracks improvements independently of topology updates.
    } # IF method == "spso2011-dds" end

    if (method == "spso2011-pdds") {
      pdds.archive.X <- NULL # Receives finite, diverse candidates after the first SPSO batch.
      pdds.archive.fit <- numeric()
      pdds.chain.X <- matrix(numeric(), nrow=0, ncol=n)
      pdds.chain.fit <- numeric()
      pdds.chain.evals <- integer()
      pdds.chain.r <- numeric() # Receives one scale per chain at the first pDDS batch.
      pdds.diagnostics <- c(swarm.batches=0L, dds.batches=0L,
                            dds.evaluations=0L, chain.improvements=0L,
                            migrations=0L) # Accumulates portfolio outcomes.
    } # IF method == "spso2011-pdds" end

    if (write2disk) {
      OFout.Text.file              <- file(OFout.Text.fname, "a")           
      Particles.TextFile           <- file(Particles.Textfname, "a")  
      Velocities.TextFile          <- file(Velocities.Textfname, "a") 
      ConvergenceMeasures.TextFile <- file(ConvergenceMeasures.Textfname, "a")   
      BestParamPerIter.TextFile    <- file(BestParamPerIter.Textfname, "a")
      PbestPerIter.TextFile        <- file(PbestPerIter.Textfname, "a") 
      LocalBestPerIter.TextFile    <- file(LocalBestPerIter.Textfname, "a") 
      if (use.RG) {
	      Xmin.Text.file <- file(Xmin.Text.fname, "a")        
	      Xmax.Text.file <- file(Xmax.Text.fname, "a")
      } # IF end
    } # IF end      

    ######################### START Main Iteration Loop ########################
    abstol.conv <- FALSE
    reltol.conv <- FALSE
    improvement <- FALSE

    end.type.stg  <- "Unknown"
    end.type.code <- "-999"

    ############################################################################  
    # 3)                              Main Algorithm                           #
    ############################################################################  
    if (verbose) message("                                                                                ")          
    if (verbose) message("================================================================================")
    if (verbose) message("[                                 Running  PSO ...                             ]")
    if (verbose) message("================================================================================")
    if (verbose) message("                                                                                ")        

    while ( (iter <= maxit)  && (!abstol.conv) && (!reltol.conv) && (nfn.eff <= maxfn) ) { 

      if (method == "spso2011-sa")
        SA.gbest.before.swarm <- gbest.fit # Separates SPSO improvements from later SA improvements.

      if ( (topology=="random") & (!improvement) ) 
        X.neighbours <- Random.Topology.Generation(npart, K, drty.out, iter)
	
      ModelOut <- vector("list", npart)

      pdds.batch <- FALSE # Distinguishes ordinary swarm and portfolio-DDS batches.
      X.eval <- X # Ordinary methods evaluate the current swarm positions.
      if (method == "spso2011-pdds") {
        scheduled.pdds <- iter > PDDS.warmup &&
                          ((iter - PDDS.warmup) %% PDDS.batch.every == 0)
        pdds.batch <- scheduled.pdds && length(pdds.archive.fit) > 0

        if (pdds.batch) {
          target.chains <- min(PDDS.nchains, nrow(pdds.archive.X))
          current.chains <- nrow(pdds.chain.X)

          if (current.chains < target.chains) {
            add <- seq.int(current.chains + 1L, target.chains)
            pdds.chain.X <- rbind(pdds.chain.X,
                                  pdds.archive.X[add, , drop=FALSE])
            pdds.chain.fit <- c(pdds.chain.fit, pdds.archive.fit[add])
            pdds.chain.evals <- c(pdds.chain.evals, integer(length(add)))
          } # IF more pDDS chains can be seeded end

          for (k in seq_len(target.chains)) {
            if (!is.finite(pdds.chain.fit[k]) ||
                pdds.is.better(pdds.archive.fit[k], pdds.chain.fit[k], MinMax)) {
              pdds.chain.X[k, ] <- pdds.archive.X[k, ]
              pdds.chain.fit[k] <- pdds.archive.fit[k]
              pdds.chain.evals[k] <- 0L # Restarts the local dimension schedule from a new elite.
            } # IF archive elite improves its corresponding chain end
          } # FOR k end

          pdds.chain.r <- rep(PDDS.r, length.out=target.chains)
          pdds.generated <- pdds.generate.batch(chain.X=pdds.chain.X,
                                                 chain.evals=pdds.chain.evals,
                                                 chain.r=pdds.chain.r,
                                                 npart=npart, epoch=PDDS.epoch,
                                                 x.MinMax=X.Boundaries)
          X.eval <- pdds.generated[["X"]]
          pdds.chain.evals <- pdds.generated[["evals"]]
          pdds.diagnostics["dds.batches"] <- pdds.diagnostics["dds.batches"] + 1L
        } else {
          pdds.diagnostics["swarm.batches"] <- pdds.diagnostics["swarm.batches"] + 1L
        } # ELSE ordinary SPSO batch end
      } # IF method == "spso2011-pdds" end
      
      # IW: linear, non-linear, runif
      if (!use.IW) {
         w <- 1   
      } else {                   
	        if ( (IW.type == "linear") | (IW.type == "non-linear") ) {
	          w <- compute.value.with.iter(iter=iter.tv, niter=niter.tv, 
					                               nexp=IW.exp, val.ini=w.ini, 
					                               val.fin=w.fin)                   
	        } else if (IW.type == "runif") {
	            w <- runif(1, min=w.ini, max=w.fin)
            } # ELSE end
	      } # ELSE end 

      # TVc1: linear, non-linear
	    if ( (use.TVc1) & (TVc1.type != "GLratio") ) {
	      if ( (TVc1.type == "linear") | (TVc1.type == "non-linear") )
	        c1 <- compute.value.with.iter(iter=iter.tv, niter=niter.tv, 
			                                  nexp=TVc1.exp, val.ini=c1.ini, 
				                                val.fin=c1.fin)  
	    } # If end  

      # TVc2
	    if (use.TVc2) 
	      c2 <- compute.value.with.iter(iter=iter.tv, niter=niter.tv, 
				                              nexp=TVc2.exp, val.ini=c2.ini, 
					                            val.fin=c2.fin)  
					
      # lambda
	    if (use.TVlambda) {
	      lambda <- compute.value.with.iter(iter=iter.tv, niter=niter.tv, 
				                                  nexp=TVlambda.exp, val.ini=vmax.ini, 
					                                val.fin=vmax.fin)  
	      Vmax   <- lambda*Lmax
	    } # IF end  

      ##########################################################################  
      
      if (normalise) {
        Xn <- X.eval * (UPPER.ini - LOWER.ini) + LOWER.ini
        if (pdds.batch) {
          Vn <- matrix(0, nrow=npart, ncol=n) # pDDS candidates have no particle velocity.
        } else Vn <- V * (UPPER.ini - LOWER.ini) + LOWER.ini
      } else {
          Xn <- X.eval
          if (pdds.batch) {
            Vn <- matrix(0, nrow=npart, ncol=n) # Keeps disk output explicit for pDDS batches.
          } else Vn <- V
        } # ELSE end
      if (fn.name == "hydromodInR") # R-based models can use replacement, additive, or multiplicative parameter changes.
        Xmodel <- .changeParamValues(Xn, change.type=change.type, refValue=refValue) # Converts PSO coordinates to model parameter values.

      # 3.a) Evaluate the particles fitness
      if ( (fn.name != "hydromod") & (fn.name != "hydromodInR") ) {
         
         # Evaluating an R Function 
         if (parallel=="none") {
           GoF <- apply(Xn, fn, MARGIN=1, ...)
         } else             
            if ( (parallel=="parallel") | (parallel=="parallelWin") ) {
                GoF <- parallel::parRapply(cl= cl, x=Xn, FUN=fn, ...)
            } else if (parallel=="multicore")
                GoF <- unlist(parallel::mclapply(1:npart, FUN=fn1, x=Xn, ..., mc.cores=par.nnodes, mc.silent=TRUE)) 
	 
         Xt.fitness[iter, 1:npart] <- GoF
         ModelOut[1:npart]         <- GoF  ###

	     nfn     <- nfn + npart
	     nfn.eff <- nfn.eff + npart

      } else if ( fn.name == "hydromod" ) { # fn.name = "hydromod"       

	     if ("verbose" %in% names(model.FUN.args)) {
	          verbose.FUN <- model.FUN.args[["verbose"]] 
	     } else verbose.FUN <- verbose
	     
	     if (parallel=="none") {
	        out <- lapply(1:npart, hydromod.eval,       
                          Particles=Xn, 
                          iter=iter, 
                          npart=npart, 
                          maxit=maxit, 
                          REPORT=REPORT, 
                          verbose=verbose.FUN, 
                          digits=digits, 
                          model.FUN=model.FUN, 
                          model.FUN.args=model.FUN.args, 
                          parallel=parallel, 
                          ncores=par.nnodes, 
                          part.dirs=mc.dirs  
                          )
                   
         } else if ( (parallel=="parallel") | (parallel=="parallelWin") ) {
                 
             out <- parallel::clusterApply(cl=cl, x=1:npart, fun= hydromod.eval,                                  
                                           Particles=Xn, 
                                           iter=iter, 
                                           npart=npart, 
                                           maxit=maxit, 
                                           REPORT=REPORT, 
                                           verbose=verbose.FUN, 
                                           digits=digits, 
                                           model.FUN=model.FUN, 
                                           model.FUN.args=model.FUN.args, 
                                           parallel=parallel, 
                                           ncores=par.nnodes, 
                                           part.dirs=part.dirs                          
                                           ) # sapply END
                                                   
#                      out <- parallel::clusterMap(cl=cl, 
#                                                  fun= hydromod.eval.perRow,  
#                                                  part=1:npart,                                
#                                                  part.dir=part.dirs,
#                                                  MoreArgs=list(
#                                                           Particles=Xn, 
#                                                           iter=iter, 
#                                                           npart=npart, 
#                                                           maxit=maxit, 
#                                                           REPORT=REPORT, 
#                                                           verbose=verbose.FUN, 
#                                                           digits=digits, 
#                                                           model.FUN=model.FUN, 
#                                                           model.FUN.args=model.FUN.args, 
#                                                           parallel=parallel, 
#                                                           ncores=par.nnodes
#                                                           )                                                                             
#                                                   ) # sapply END
                                  
           } else if (parallel=="multicore") {
                   
                       out <- parallel::mclapply(1:npart, hydromod.eval,       
                                                  Particles=Xn, 
                                                  iter=iter, 
                                                  npart=npart, 
                                                  maxit=maxit, 
                                                  REPORT=REPORT, 
                                                  verbose=verbose.FUN, 
                                                  digits=digits, 
                                                  model.FUN=model.FUN, 
                                                  model.FUN.args=model.FUN.args, 
                                                  parallel=parallel, 
                                                  ncores=par.nnodes, 
                                                  part.dirs=part.dirs,
                                                  mc.cores=par.nnodes,
                                                  mc.silent=TRUE,
                                                  mc.cleanup=TRUE                     
                                                  ) # mclapply END
                                      
             } # ELSE end
                                        
             for (part in 1:npart){         
                   GoF                    <- out[[part]][["GoF"]] 
                   Xt.fitness[iter, part] <- GoF            
                   ModelOut[[part]]       <- out[[part]][["sim"]]  
                   nfn <- nfn + 1 
                   if(is.finite(GoF)) nfn.eff <- nfn.eff + 1                     
             } #FOR part end               

	} else if (fn.name == "hydromodInR") {

      if ("verbose" %in% names(model.FUN.args)) {
	          verbose.FUN <- model.FUN.args[["verbose"]] 
	     } else verbose.FUN <- verbose
	     
	     if (parallel=="none") {
	        out <- lapply(1:npart, hydromodInR.eval,       
                        Particles=Xmodel, # Uses transformed parameter values when running an R-based model.
                        model.FUN=model.FUN, 
                        model.FUN.args=model.FUN.args 
                        )
                   
         } else if ( (parallel=="parallel") | (parallel=="parallelWin") ) {
                 
             out <- parallel::clusterApply(cl=cl, x=1:npart, fun= hydromodInR.eval,                                  
                                           Particles=Xmodel, # Uses transformed parameter values when running an R-based model.
                                           model.FUN=model.FUN, 
                                           model.FUN.args=model.FUN.args 
                                           ) # sapply END
         
          } # ELSE IF end

       for (part in 1:npart){         
          GoF                    <- out[[part]][["GoF"]] 
          Xt.fitness[iter, part] <- GoF            
          ModelOut[[part]]       <- out[[part]][["sim"]]  
          nfn <- nfn + 1 
          if(is.finite(GoF)) nfn.eff <- nfn.eff + 1                     
       } #FOR part end  

      } # else if (fn.name == "hydromodInR") END

      current.fitness <- Xt.fitness[iter, ] # Uses evaluated fitness unless competitive selection restores a parent.

      if (method == "spso2011-cde") {
        CDE.gain <- rep(0, npart) # Gives the initial, ungenerated population no operator reward.
        CDE.accepted.de <- rep(FALSE, npart) # Records strictly improving DE trials.
        CDE.evaluated <- !is.na(CDE.operator.previous) # Excludes the initial random population.

        if (any(CDE.evaluated)) {
          CDE.gain[CDE.evaluated] <- cde.fitness.gain(
            new.fit=Xt.fitness[iter, CDE.evaluated],
            old.fit=CDE.parent.fit.previous[CDE.evaluated], MinMax=MinMax)
          CDE.is.de <- CDE.operator.previous == "de" & CDE.evaluated
          CDE.accepted.de[CDE.is.de] <- cde.is.better(
            new.fit=Xt.fitness[iter, CDE.is.de],
            old.fit=CDE.parent.fit.previous[CDE.is.de], MinMax=MinMax)
          CDE.rejected.de <- CDE.is.de & !CDE.accepted.de

          if (any(CDE.rejected.de)) {
            X[CDE.rejected.de, ] <- CDE.parent.X.previous[CDE.rejected.de, , drop=FALSE]
            current.fitness[CDE.rejected.de] <- CDE.parent.fit.previous[CDE.rejected.de]
            V[CDE.rejected.de, ] <- 0 # Removes displacement from rejected DE trials.
          } # IF rejected DE trials end

          if (any(CDE.accepted.de)) {
            CDE.archive <- cde.update.archive(
              archive=CDE.archive,
              parents=CDE.parent.X.previous[CDE.accepted.de, , drop=FALSE],
              max.size=CDE.archive.max)
          } # IF successful DE trials end

          CDE.memory <- cde.update.memories(
            M.F=CDE.M.F, M.CR=CDE.M.CR, pointer=CDE.memory.pointer,
            F=CDE.F.previous, CR=CDE.CR.previous, gain=CDE.gain,
            successful=CDE.accepted.de)
          CDE.M.F <- CDE.memory[["M.F"]]
          CDE.M.CR <- CDE.memory[["M.CR"]]
          CDE.memory.pointer <- CDE.memory[["pointer"]]
          CDE.operator.score <- cde.update.scores(
            scores=CDE.operator.score, labels=CDE.operator.previous,
            gain=CDE.gain)
          CDE.coordinate.score <- cde.update.scores(
            scores=CDE.coordinate.score, labels=CDE.coordinate.previous,
            gain=CDE.gain)

          CDE.diagnostics["pso.evaluated"] <- CDE.diagnostics["pso.evaluated"] +
            sum(CDE.operator.previous == "pso", na.rm=TRUE)
          CDE.diagnostics["de.evaluated"] <- CDE.diagnostics["de.evaluated"] +
            sum(CDE.is.de)
          CDE.diagnostics["de.accepted"] <- CDE.diagnostics["de.accepted"] +
            sum(CDE.accepted.de)
          CDE.diagnostics["emergency.evaluated"] <-
            CDE.diagnostics["emergency.evaluated"] +
            sum(CDE.strategy.previous == "rand1", na.rm=TRUE)
          CDE.diagnostics["original.evaluated"] <-
            CDE.diagnostics["original.evaluated"] +
            sum(CDE.coordinate.previous == "original", na.rm=TRUE)
          CDE.diagnostics["eigen.evaluated"] <- CDE.diagnostics["eigen.evaluated"] +
            sum(CDE.coordinate.previous == "eigen", na.rm=TRUE)
        } # IF competitive candidates were evaluated end

        CDE.improved.pbest <- cde.is.better(new.fit=current.fitness,
                                             old.fit=pbest.fit, MinMax=MinMax)
        CDE.particle.stagnation[CDE.improved.pbest] <- 0L
        CDE.particle.stagnation[!CDE.improved.pbest] <-
          CDE.particle.stagnation[!CDE.improved.pbest] + 1L
      } # IF method == "spso2011-cde" end

      if (method == "spso2011-de") {
        # Learns from the mutation factors that generated successful current particles.
        DE.history <- update.de.success.history(mu=DE.mu.F, F=DE.F.previous,
                                                fitness=Xt.fitness[iter, ],
                                                pbest.fit=pbest.fit,
                                                MinMax=MinMax, c=DE.c)
        DE.mu.F <- DE.history[["mu"]]
        DE.F.current <- rep(NA_real_, npart) # Records factors used to generate the next batch.
      } # IF method == "spso2011-de" end

      if ( (best.update == "sync") && !pdds.batch ) {
	    tmp <- sync.update.pgbests(x=X,
				       xt.fitness= current.fitness,
				       MinMax= MinMax, 
				       pbest.fit= pbest.fit, 
				       gbest.fit= gbest.fit, 
				       gbest.pos= gbest.pos, 
				       x.best= X.best.part
				       )                               

	    pbest.fit   <- tmp[["pbest"]]
	    X.best.part <- tmp[["x.best"]]
	    gbest.fit   <- tmp[["gbest.fit"]]
	    gbest.pos   <- tmp[["gbest.pos"]]
	    
	    tmp <- UpdateLocalBest(pbest.fit=pbest.fit, 
			     localBest.pos=LocalBest.pos,
			     localBest.fit=LocalBest.fit, 
			     x.neighbours=X.neighbours, 
			     MinMax=MinMax) 
        LocalBest.fit <- tmp[["localBest.fit"]]
        LocalBest.pos <- tmp[["localBest.pos"]]

         if ( method == "ipso" ) {
               tmp <- UpdateNgbest(pbest.fit=pbest.fit, 
        	                   ngbest=ngbest, 
        			   MinMax=MinMax) 
               ngbest.fit <- tmp[["ngbest.fit"]]
               ngbest.pos <- tmp[["ngbest.pos"]]
            } # IF end

      } # IF end 

      if (method == "spso2011-cde") {
        CDE.gbest.improved <- cde.is.better(new.fit=gbest.fit,
                                            old.fit=CDE.gbest.previous,
                                            MinMax=MinMax)
        if (CDE.gbest.improved) {
          CDE.stagnation <- 0L
        } else CDE.stagnation <- CDE.stagnation + 1L
        CDE.gbest.previous <- gbest.fit # Stores the accepted global best for the next batch.

        CDE.range <- X.Boundaries[,2]-X.Boundaries[,1] # Normalises population spread by bounds.
        CDE.varying <- CDE.range > 0 # Excludes fixed parameters from diversity.
        if (any(CDE.varying)) {
          CDE.spread <- apply(X[, CDE.varying, drop=FALSE], 2, stats::sd)
          CDE.diversity.current <- mean(CDE.spread/CDE.range[CDE.varying])
        } else CDE.diversity.current <- 0
        if (!is.finite(CDE.diversity.current)) CDE.diversity.current <- 0
        CDE.emergency <- CDE.stagnation >= CDE.stag.iter &&
                         CDE.diversity.current <= CDE.diversity

        CDE.reward.total <- sum(CDE.operator.score) # Converts recent rewards to an operator share.
        if (is.finite(CDE.reward.total) && CDE.reward.total > 0) {
          CDE.q.de <- CDE.operator.score["de"]/CDE.reward.total
        } else CDE.q.de <- 0.5
        CDE.q.de <- min(CDE.qmax, max(CDE.qmin, CDE.q.de))
        if (CDE.emergency) CDE.q.de <- CDE.qmax # Expands DE/rand/1 during collapse.

        CDE.min.de <- max(1L, ceiling(CDE.qmin*npart))
        CDE.max.de <- min(npart-1L, floor(CDE.qmax*npart))
        CDE.n.de <- min(CDE.max.de,
                        max(CDE.min.de, as.integer(round(CDE.q.de*npart))))
        CDE.priority <- CDE.particle.stagnation + stats::runif(npart, 0, 0.01)
        CDE.de.index <- order(CDE.priority, decreasing=TRUE)[seq_len(CDE.n.de)]
        CDE.operator.current <- rep("pso", npart) # Preserves a native SPSO subpopulation.
        CDE.operator.current[CDE.de.index] <- "de" # Assigns DE to the least productive particles.

        CDE.population <- X # Freezes a common parent population for every DE donor.
        CDE.pbest.population <- X.best.part # Freezes the personal-best donor population.
        CDE.pbest.fit.population <- pbest.fit # Freezes elite ranking for current-to-pbest.
        CDE.parent.X.current <- X # Stores parents for next generation's greedy selection.
        CDE.parent.fit.current <- current.fitness
        CDE.F.current <- rep(NA_real_, npart) # Records sampled SHADE parameters by trial.
        CDE.CR.current <- rep(NA_real_, npart)
        CDE.coordinate.current <- rep(NA_character_, npart)
        CDE.strategy.current <- rep(NA_character_, npart)

        if (CDE.eigen && (iter == 1L || (iter-1L) %% CDE.eigen.freq == 0L))
          CDE.basis <- cde.coordinate.basis(CDE.pbest.population) # Refreshes landscape orientation.
        CDE.coordinate.total <- sum(CDE.coordinate.score)
        if (CDE.eigen && isTRUE(CDE.basis$usable) &&
            is.finite(CDE.coordinate.total) && CDE.coordinate.total > 0) {
          CDE.eigen.probability <- CDE.coordinate.score["eigen"]/CDE.coordinate.total
          CDE.eigen.probability <- min(0.9, max(0.1, CDE.eigen.probability))
        } else CDE.eigen.probability <- 0
      } # IF method == "spso2011-cde" end
      
      # 'X.bak' is only needed to correctly compute the Normalised Swarm Radius
      # for the current iteration
      X.bak <- X           
      if (method == "spso2011-sa")
        SA.scouts.active <- which(SA.scout.remaining > 0L) # Freezes the protected group for this generation.

      ##########################################################################  
      ###################   Particles Loop (j) - Start  ########################
      ##########################################################################  
      
      if ( (best.update == "async") & random.update) { 
	      index.part.upd <- sample(npart)
      } else index.part.upd <- 1:npart
        
      for (j in index.part.upd) {
      
        if (write2disk) {
        
          GoF <- Xt.fitness[iter, j]
        
          # File 'Model_Out.txt'          
          if(is.finite(GoF)) {
             suppressWarnings(
             writeLines(as.character(c(iter, j, 
				       formatC(GoF, format="E", digits=digits, flag=" "), 
				       formatC(ModelOut[[j]], format="E", digits=digits, flag=" ") ) ), 
			         OFout.Text.file, sep="  ")
             ) 
          } else writeLines(as.character(c(iter, j, "NA", "NA" ) ), OFout.Text.file, sep="  ")
	      writeLines("", OFout.Text.file) 
	      flush(OFout.Text.file)
          
          # File 'Particles.txt' #
	      if(is.finite(GoF)) {
            suppressWarnings(
	                 writeLines(as.character( c(iter, j, 
				                      formatC(GoF, format="E", digits=digits, flag=" "), #GoF
				                      formatC(Xn[j, ], format="E", digits=digits, flag=" ") 
				                      ) ), Particles.TextFile, sep="  ") 
                            )
	      } else suppressWarnings( writeLines(as.character( c(iter, j, "NA",
					  formatC(Xn[j, ], format="E", digits=digits, flag=" ") 
				      ) ), Particles.TextFile, sep="  ") 
                                 )
	      writeLines("", Particles.TextFile)
	      flush(Particles.TextFile)
        
	      # File 'Velocities.txt' #
	      if(is.finite(GoF)) {
            suppressWarnings(
	            writeLines( as.character( c(iter, j, 
					  formatC(GoF, format="E", digits=digits, flag=" "), # GoF
					  formatC(Vn[j, ], format="E", digits=digits, flag=" ")                                            
					  ) ), Velocities.TextFile, sep="  ") 
            )
	      } else suppressWarnings( writeLines( as.character( c(iter, j, "NA",
					formatC(Vn[j, ], format="E", digits=digits, flag=" ")                                            
					) ), Velocities.TextFile, sep="  ")
                                 )
	      writeLines("", Velocities.TextFile) 
	      flush(Velocities.TextFile)
	  
        } # IF end

        if (pdds.batch) next # pDDS candidates are processed jointly after disk output.
	    

	if ( best.update == "async" ) {
	   tmp <- async.update.pgbests(x=X[j,], 
	                               x.pos=j, 
                                 xt.fitness= current.fitness[j],
                                 MinMax= MinMax, 
                                 l.pbest.fit= pbest.fit[j], 
                                 gbest.fit= gbest.fit, 
                                 gbest.pos= gbest.pos,
                                 x.best= X.best.part[j, ]
	                               )                                    

	   pbest.fit[j]    <- tmp[["pbest"]]
	   X.best.part[j,] <- tmp[["x.best"]]       
	   gbest.pos       <- tmp[["gbest.pos"]] 
	   gbest.fit       <- tmp[["gbest.fit"]] 
	   
	   tmp <- UpdateLocalBest(pbest.fit=pbest.fit, 
			                      localBest.pos=LocalBest.pos,
			                      localBest.fit=LocalBest.fit, 
			                      x.neighbours=X.neighbours, 
			                      MinMax=MinMax) 
     LocalBest.fit <- tmp[["localBest.fit"]]
     LocalBest.pos <- tmp[["localBest.pos"]]

     if ( method == "ipso" ) {
        tmp <- UpdateNgbest(pbest.fit=pbest.fit, 
                            ngbest=ngbest, 
                            MinMax=MinMax) 
        ngbest.fit <- tmp[["ngbest.fit"]]
        ngbest.pos <- tmp[["ngbest.pos"]]
     } # IF end

	} # IF end  
	
	### IW, TVc1, Tv2, lambda
	
	# IW: aiwf, GLratio
	if (use.IW) { 
	  if (IW.type == "aiwf") { 
	        w <- compute.w.aiwf(iter.fit=current.fitness,
                              particle.pos =j, 
                              gbest.fit=gbest.fit, 
                              w.max=max(w.ini, w.fin), 
                              w.min=min(w.ini, w.fin),
                              MinMax=MinMax
                              )   

	  } else if (IW.type == "GLratio") {
		w <- compute.w.with.GLratio(MinMax, gbest.fit, pbest.fit)   
	    }  # ELSE end
	} # IF end
	
	
	# TVc1: GLratio
	if ( (use.TVc1) & (TVc1.type == "GLratio") ) 
           c1 <- compute.c1.with.GLratio(MinMax, gbest.fit, pbest.fit[j])   

	######################################################################## 
	# 3.b) Updating the velocity of all the particles
	if ( (topology=="lbest") & (iter <= iter.ini) ) {
          ltopology <- "gbest"
    } else ltopology <- topology

    if (method == "spso2011-sa" && j %in% SA.scouts.active) {
      # Protected scouts learn only from the best scout basin, not from the incumbent swarm attractor.
      if (MinMax == "min") {
        sa.local.pos <- SA.scouts.active[which.min(pbest.fit[SA.scouts.active])]
      } else sa.local.pos <- SA.scouts.active[which.max(pbest.fit[SA.scouts.active])]
      sa.local.best <- X.best.part[sa.local.pos, ]
    } else {
      sa.local.pos <- LocalBest.pos[j]
      sa.local.best <- X.best.part[LocalBest.pos[j], ]
    } # ELSE ordinary SPSO informant end

    if (method == "spso2011-cde" && CDE.operator.current[j] == "de") {
      CDE.parameters <- cde.sample.parameters(M.F=CDE.M.F, M.CR=CDE.M.CR)
      CDE.use.eigen <- stats::runif(1) < CDE.eigen.probability
      CDE.strategy <- if (CDE.emergency) "rand1" else "current-to-pbest"
      CDE.trial <- cde.trial.vector(
        part.index=j, X=CDE.population, Pbest=CDE.pbest.population,
        pbest.fit=CDE.pbest.fit.population, archive=CDE.archive,
        F=CDE.parameters[["F"]], CR=CDE.parameters[["CR"]], p=CDE.p,
        MinMax=MinMax, lower=X.Boundaries[,1], upper=X.Boundaries[,2],
        strategy=CDE.strategy, basis=CDE.basis, use.eigen=CDE.use.eigen)
      X[j, ] <- CDE.trial[["point"]] # Installs the DE trial for the next common batch.
      V[j, ] <- X[j, ]-CDE.parent.X.current[j, ] # Defines momentum if the particle returns to SPSO.
      CDE.F.current[j] <- CDE.parameters[["F"]]
      CDE.CR.current[j] <- CDE.parameters[["CR"]]
      CDE.coordinate.current[j] <- CDE.trial[["coordinate"]]
      CDE.strategy.current[j] <- CDE.trial[["strategy"]]
    } else {
      if (method == "spso2011-de") {
        # Adds only personal-best positions received through this particle's realised topology.
        DE.archive[[j]] <- update.informant.archive(archive=DE.archive[[j]],
                                                    Pbest=X.best.part,
                                                    informants=X.neighbours[j, ],
                                                    part.index=j,
                                                    max.size=DE.archive.size)
        DE.F.j <- sample.de.F(mu=DE.mu.F) # Samples this particle's adaptive mutation factor.
      } else DE.F.j <- DE.F

      velocity.method <- if (method == "spso2011-cde") "spso2011" else method
	  velocity <- compute.veloc(
				  x= X[j, ],
				  v= V[j, ],
				  w= w,
				  c1= c1,
				  c2= c2,
				  CF= CF,
				  Pbest= X.best.part,
				  part.index=j,
				  gbest= X.best.part[gbest.pos, ],
				  topology=ltopology,
				  method=velocity.method,
				  MinMax=MinMax,                             # topology="ipso" | method="wfips"
				  neighs.index=X.neighbours[j, ],            # method in c("fips", "wfips")
				  localBest=sa.local.best,                    # protected SA scouts or ordinary local best
				  localBest.pos=sa.local.pos,                 # protected SA scouts or ordinary local-best position
				  ngbest.fit=ngbest.fit,                     # topology="ipso"
				  ngbest=X.best.part[ngbest.pos, ],          # topology="ipso"
				  lpbest.fit= pbest.fit,                     # method="wfips"
				  DE.F=DE.F.j,                               # method="spso2011-de"
				  DE.archive=if (method == "spso2011-de") DE.archive[[j]] else NULL
				  )

      if (method == "spso2011-de" && isTRUE(attr(velocity, "DE.used")))
        DE.F.current[j] <- DE.F.j # Associates this factor with the next evaluated candidate.
	  V[j,] <- velocity
      V[j,] <- velocity.boundary.treatment(v=V[j,], vmax=Vmax)

      ########################################################################
	  # 4.c) Moving the particles: X[j,] <- X[j,] + V[j,]
	  out <- position.update.and.boundary.treatment(x=X[j,], v=V[j,],
	                                                x.MinMax=X.Boundaries,
	                                                boundary.wall=boundary.wall)
	  X[j,] <- out[["x.new"]]
	  V[j,] <- out[["v.new"]]
    } # ELSE SPSO candidate end

    } # FOR j end: Particles Loop
    ##########################################################################  
    ###################   Particles Loop (j) - End  ##########################
    ########################################################################## 

    if (method == "spso2011-cde") {
      CDE.operator.previous <- CDE.operator.current # Carries proposal provenance to evaluation.
      CDE.parent.X.previous <- CDE.parent.X.current
      CDE.parent.fit.previous <- CDE.parent.fit.current
      CDE.F.previous <- CDE.F.current
      CDE.CR.previous <- CDE.CR.current
      CDE.coordinate.previous <- CDE.coordinate.current
      CDE.strategy.previous <- CDE.strategy.current
    } # IF method == "spso2011-cde" end

    if (method == "spso2011-pdds") {
      # Every evaluated batch contributes finite, quality-diverse archive entries.
      pdds.archive <- pdds.update.archive(archive.X=pdds.archive.X,
                                          archive.fit=pdds.archive.fit,
                                          candidates=X.eval,
                                          fitness=Xt.fitness[iter, ],
                                          MinMax=MinMax,
                                          max.size=PDDS.archive.size,
                                          min.distance=PDDS.min.distance,
                                          x.MinMax=X.Boundaries)
      pdds.archive.X <- pdds.archive[["X"]]
      pdds.archive.fit <- pdds.archive[["fit"]]

      if (pdds.batch) {
        pdds.diagnostics["dds.evaluations"] <-
          pdds.diagnostics["dds.evaluations"] + npart
        improved.X <- matrix(numeric(), nrow=0, ncol=n)
        improved.fit <- numeric()

        for (k in seq_len(nrow(pdds.chain.X))) {
          candidate.index <- which(pdds.generated[["chain"]] == k &
                                   is.finite(Xt.fitness[iter, ]))
          if (length(candidate.index) > 0) {
            if (MinMax == "min") {
              best.index <- candidate.index[which.min(Xt.fitness[iter, candidate.index])]
            } else best.index <- candidate.index[which.max(Xt.fitness[iter, candidate.index])]
            best.fit <- Xt.fitness[iter, best.index]

            if (pdds.is.better(best.fit, pdds.chain.fit[k], MinMax)) {
              pdds.chain.X[k, ] <- X.eval[best.index, ]
              pdds.chain.fit[k] <- best.fit
              improved.X <- rbind(improved.X, X.eval[best.index, , drop=FALSE])
              improved.fit <- c(improved.fit, best.fit)
            } # IF chain improved end
          } # IF chain has a finite candidate end
        } # FOR k end

        pdds.diagnostics["chain.improvements"] <-
          pdds.diagnostics["chain.improvements"] + length(improved.fit)

        if (length(improved.fit) > 0) {
          ranking <- order(improved.fit, decreasing=(MinMax == "max"))
          improved.X <- improved.X[ranking, , drop=FALSE]
          improved.fit <- improved.fit[ranking]
          migration.slots <- setdiff(seq_len(npart), gbest.pos)
          if (length(migration.slots) == 0) migration.slots <- gbest.pos
          if (MinMax == "min") {
            migration.slots <- migration.slots[order(pbest.fit[migration.slots],
                                                       decreasing=TRUE)]
          } else {
            migration.slots <- migration.slots[order(pbest.fit[migration.slots],
                                                       decreasing=FALSE)]
          } # ELSE maximisation end
          migration.limit <- min(ceiling(PDDS.migration.frac*npart),
                                 length(improved.fit), length(migration.slots))
          migrations <- 0L

          for (i in seq_len(migration.limit)) {
            slot <- migration.slots[i]
            if (pdds.is.better(improved.fit[i], pbest.fit[slot], MinMax)) {
              X[slot, ] <- improved.X[i, ]
              V[slot, ] <- 0 # Removes velocity inherited from the replaced particle.
              X.best.part[slot, ] <- improved.X[i, ]
              pbest.fit[slot] <- improved.fit[i]
              migrations <- migrations + 1L
            } # IF successful chain is better than migration target end
          } # FOR i end

          if (MinMax == "min") {
            gbest.pos <- which.min(pbest.fit)
          } else gbest.pos <- which.max(pbest.fit)
          gbest.fit <- pbest.fit[gbest.pos]
          pdds.diagnostics["migrations"] <-
            pdds.diagnostics["migrations"] + migrations

          tmp <- UpdateLocalBest(pbest.fit=pbest.fit,
                                 localBest.pos=LocalBest.pos,
                                 localBest.fit=LocalBest.fit,
                                 x.neighbours=X.neighbours,
                                 MinMax=MinMax)
          LocalBest.fit <- tmp[["localBest.fit"]]
          LocalBest.pos <- tmp[["localBest.pos"]]
        } # IF at least one pDDS chain improved end

        X.bak <- X # Uses the post-migration swarm for convergence diagnostics.
      } # IF pdds.batch end
    } # IF method == "spso2011-pdds" end

    if (method == "spso2011-de")
      DE.F.previous <- DE.F.current # Carries generation provenance to the next evaluation.

    if (method == "spso2011-sa" && length(SA.scouts.active) > 0)
      SA.scout.remaining[SA.scouts.active] <-
        pmax(0L, SA.scout.remaining[SA.scouts.active] - 1L) # Ends protection after complete scout generations.

    if (method == "spso2011-sa") {
      # Keeps the best personal memories as quality-diverse seeds for later SA events.
      sa.archive <- update.archive.sa(SA.archive.X, SA.archive.fit,
                                      X.best.part, pbest.fit)
      SA.archive.X <- sa.archive[["X"]]
      SA.archive.fit <- sa.archive[["fit"]]

      # Counts stagnation only when the SPSO batch did not improve the global best.
      sa.swarm.improved <- is.finite(gbest.fit) &&
                           (!is.finite(SA.gbest.before.swarm) ||
                            better.sa(gbest.fit, SA.gbest.before.swarm))
      if (sa.swarm.improved) {
        SA.stagnation.count <- 0L
        SA.trigger.stagnation <- as.integer(SA.stagnation) # A new SPSO basin restores the normal event interval.
      } else SA.stagnation.count <- SA.stagnation.count + 1L

      # Triggers on collapse, while a longer stagnation remains an emergency escape route.
      sa.radius <- ComputeSwarmRadiusAndDiameter(x=X.bak,
                                                 gbest=X.best.part[gbest.pos, ],
                                                 Lmax=Lmax)
      sa.normalised.radius <- sa.radius[["swarm.radius"]] /
                              sa.radius[["swarm.diameter"]]
      sa.collapsed <- is.finite(sa.normalised.radius) &&
                      sa.normalised.radius <= SA.radius.threshold
      sa.emergency.threshold <- max(as.integer(SA.reheat),
                                    4L*max(1L, as.integer(SA.stagnation)))
      sa.emergency <- SA.stagnation.count >= sa.emergency.threshold
      sa.event.ready <- SA.stagnation == 0L || sa.collapsed || sa.emergency
      sa.budget.remaining <- SA.budget.max - SA.diagnostics["attempted"]
      sa.fn.remaining <- if (is.finite(maxfn))
                           max(0L, floor(maxfn - nfn.eff + 1L)) else Inf
      sa.run <- SA.stagnation.count >= SA.trigger.stagnation && sa.event.ready &&
                sa.budget.remaining > 0L && sa.fn.remaining > 0L &&
                nrow(SA.archive.X) > 0L && any(SA.range > 0)

      if (sa.run) {
        # Seeds cold-to-hot replicas from distinct basins rather than cloning gbest.
        sa.nreplicas <- min(SA.replicas, nrow(SA.archive.X))
        sa.seed.ids <- select.diverse.sa(SA.archive.X, SA.archive.fit,
                                         sa.nreplicas)
        sa.replica.ids <- seq_len(sa.nreplicas)
        SA.current[,] <- NA_real_
        SA.current.fit[] <- NA_real_
        SA.current[sa.replica.ids, ] <- SA.archive.X[sa.seed.ids, , drop=FALSE]
        SA.current.fit[sa.replica.ids] <- SA.archive.fit[sa.seed.ids]
        SA.temperature[] <- NA_real_
        SA.initial.temperature[] <- NA_real_
        sa.spread <- apply(X.bak, 2, stats::sd, na.rm=TRUE) / SA.range
        sa.spread[!is.finite(sa.spread)] <- SA.scale
        sa.initial.scale <- pmin(SA.scale, pmax(SA.scale.lower, sa.spread))
        SA.scale.current[,] <- matrix(rep(sa.initial.scale, each=SA.replicas),
                                      nrow=SA.replicas)
        SA.dimension.attempts[,] <- 0L
        SA.proposals.since.best <- 0L
        SA.diagnostics["activations"] <- SA.diagnostics["activations"] + 1L
        if (sa.collapsed) SA.diagnostics["collapsed.events"] <-
          SA.diagnostics["collapsed.events"] + 1L
        if (!sa.collapsed && sa.emergency) SA.diagnostics["emergency.events"] <-
          SA.diagnostics["emergency.events"] + 1L
        sa.varying <- which(SA.range > 0) # Excludes fixed dimensions from all proposal kernels.
        sa.event.X <- matrix(numeric(), nrow=0, ncol=n)
        sa.event.fit <- numeric()
        sa.event.best <- gbest.fit
        sa.gbest.before.event <- gbest.fit

        for (sa.step in seq_len(SA.nsteps)) {
          sa.budget.remaining <- SA.budget.max - SA.diagnostics["attempted"]
          sa.fn.remaining <- if (is.finite(maxfn))
                               max(0L, floor(maxfn - nfn.eff + 1L)) else Inf
          sa.batch.size <- min(sa.nreplicas, sa.budget.remaining, sa.fn.remaining)
          if (sa.batch.size < 1L) break
          sa.chain.ids <- sa.replica.ids[seq_len(sa.batch.size)]

          # Reheats the full ladder while retaining every replica's current basin.
          if (SA.proposals.since.best >= SA.reheat) {
            SA.temperature[sa.replica.ids] <- SA.initial.temperature[sa.replica.ids]
            SA.scale.current[sa.replica.ids, ] <-
              matrix(rep(sa.initial.scale, each=sa.nreplicas), nrow=sa.nreplicas)
            SA.dimension.attempts[sa.replica.ids, ] <- 0L
            SA.proposals.since.best <- 0L
            SA.diagnostics["reheats"] <- SA.diagnostics["reheats"] + 1L
          } # IF SA reheat end

          # Mixes differential, adaptive coordinate, and heavy-tailed global proposals.
          sa.candidates <- SA.current[sa.chain.ids, , drop=FALSE]
          sa.dimensions <- vector("list", sa.batch.size)
          for (i.sa in seq_len(sa.batch.size)) {
            k.sa <- sa.chain.ids[i.sa]
            kernel.sa <- stats::runif(1)
            if (kernel.sa < 0.50 && nrow(SA.archive.X) >= 2L) {
              donor.sa <- sample(seq_len(nrow(SA.archive.X)), size=2L, replace=FALSE)
              sa.dimensions[[i.sa]] <- sa.varying
              sa.candidates[i.sa, sa.varying] <-
                sa.candidates[i.sa, sa.varying] +
                0.50 * (SA.archive.X[donor.sa[1], sa.varying] -
                        SA.archive.X[donor.sa[2], sa.varying]) +
                stats::rnorm(length(sa.varying), mean=0,
                             sd=0.10*SA.scale.current[k.sa, sa.varying]*
                                SA.range[sa.varying])
            } else if (kernel.sa < 0.80) {
              sa.dimensions[[i.sa]] <- sample(sa.varying,
                                               size=min(SA.ndim, length(sa.varying)),
                                               replace=FALSE)
              dims.sa <- sa.dimensions[[i.sa]]
              sa.candidates[i.sa, dims.sa] <- sa.candidates[i.sa, dims.sa] +
                stats::rnorm(length(dims.sa), mean=0,
                             sd=SA.scale.current[k.sa, dims.sa]*SA.range[dims.sa])
            } else {
              sa.dimensions[[i.sa]] <- sa.varying
              sa.candidates[i.sa, sa.varying] <-
                sa.candidates[i.sa, sa.varying] +
                tan(pi*(stats::runif(length(sa.varying)) - 0.50)) *
                SA.scale.current[k.sa, sa.varying]*SA.range[sa.varying]
            } # ELSE heavy-tailed proposal end
            sa.candidates[i.sa, ] <- reflect.sa(sa.candidates[i.sa, ])
          } # FOR replica proposal end

          # All candidates in a sweep use the requested serial or parallel model backend.
          sa.out <- evaluate.batch.sa(sa.candidates)
          sa.fit <- sa.out[["fit"]]
          nfn <- nfn + sa.batch.size
          SA.diagnostics["attempted"] <-
            SA.diagnostics["attempted"] + sa.batch.size
          if ( (fn.name != "hydromod") & (fn.name != "hydromodInR") ) {
            nfn.eff <- nfn.eff + sa.batch.size
          } else nfn.eff <- nfn.eff + sum(is.finite(sa.fit))

          # Positive energy changes are worse relative to each replica's current state.
          if (MinMax == "min") {
            sa.delta <- sa.fit - SA.current.fit[sa.chain.ids]
          } else sa.delta <- SA.current.fit[sa.chain.ids] - sa.fit
          sa.valid <- is.finite(sa.fit) & is.finite(sa.delta)

          # The first actual proposal batch calibrates cold and hot acceptance targets.
          if (any(!is.finite(SA.temperature[sa.replica.ids]))) {
            uphill.sa <- sa.delta[sa.valid & sa.delta > 0]
            if (length(uphill.sa) == 0) {
              gaps.sa <- abs(SA.current.fit[sa.replica.ids] -
                             SA.current.fit[sa.replica.ids[1]])
              uphill.sa <- gaps.sa[is.finite(gaps.sa) & gaps.sa > 0]
            } # IF no uphill proposals end
            energy.scale.sa <- if (length(uphill.sa) > 0) stats::median(uphill.sa) else
                                 max(1, abs(gbest.fit))*1E-3
            cold.sa <- if (is.na(SA.T0)) {
              -energy.scale.sa/log(0.20)
            } else SA.T0
            hot.sa <- max(cold.sa, -energy.scale.sa/log(0.80))
            if (sa.nreplicas == 1L) {
              SA.temperature[sa.replica.ids] <- cold.sa
            } else SA.temperature[sa.replica.ids] <-
              exp(seq(log(cold.sa), log(hot.sa), length.out=sa.nreplicas))
            SA.initial.temperature[sa.replica.ids] <-
              SA.temperature[sa.replica.ids]
          } # IF temperature calibration end

          sa.accept <- sa.valid &
                       (sa.delta <= 0 |
                        stats::runif(sa.batch.size) <
                          exp(-sa.delta/SA.temperature[sa.chain.ids]))
          SA.diagnostics["invalid"] <-
            SA.diagnostics["invalid"] + sum(!sa.valid)
          SA.diagnostics["accepted"] <-
            SA.diagnostics["accepted"] + sum(sa.accept)
          SA.diagnostics["uphill.accepted"] <-
            SA.diagnostics["uphill.accepted"] +
            sum(sa.accept & sa.delta > 0)

          for (i.sa in seq_len(sa.batch.size)) {
            k.sa <- sa.chain.ids[i.sa]
            dims.sa <- sa.dimensions[[i.sa]]
            SA.dimension.attempts[k.sa, dims.sa] <-
              SA.dimension.attempts[k.sa, dims.sa] + 1L
            gain.sa <- 0.25/sqrt(SA.dimension.attempts[k.sa, dims.sa])
            SA.scale.current[k.sa, dims.sa] <-
              SA.scale.current[k.sa, dims.sa] *
              exp(gain.sa*(as.numeric(sa.accept[i.sa]) - SA.target.accept))
            SA.scale.current[k.sa, dims.sa] <-
              pmin(SA.scale.upper,
                   pmax(SA.scale.lower, SA.scale.current[k.sa, dims.sa]))

            if (sa.accept[i.sa]) {
              SA.current[k.sa, ] <- sa.candidates[i.sa, ]
              SA.current.fit[k.sa] <- sa.fit[i.sa]
              sa.event.X <- rbind(sa.event.X, sa.candidates[i.sa, ])
              sa.event.fit <- c(sa.event.fit, sa.fit[i.sa])
            } # IF accepted replica transition end
          } # FOR replica acceptance end

          sa.improved.event <- which(vapply(sa.fit, better.sa, logical(1),
                                             old.fit=sa.event.best))
          if (length(sa.improved.event) > 0) {
            if (MinMax == "min") {
              sa.best.index <- sa.improved.event[which.min(sa.fit[sa.improved.event])]
            } else sa.best.index <- sa.improved.event[which.max(sa.fit[sa.improved.event])]
            sa.event.best <- sa.fit[sa.best.index]
            SA.proposals.since.best <- 0L
            SA.diagnostics["global.improvements"] <-
              SA.diagnostics["global.improvements"] + 1L
          } else SA.proposals.since.best <-
            SA.proposals.since.best + sa.batch.size

          # Alternating adjacent swaps move hot discoveries towards cold replicas.
          if (sa.nreplicas > 1L && sa.step %% SA.swap.every == 0L) {
            swap.round.sa <- sa.step %/% SA.swap.every
            first.sa <- if (swap.round.sa %% 2L == 1L) 1L else 2L
            left.sa <- if (first.sa <= sa.nreplicas - 1L) {
              seq.int(first.sa, sa.nreplicas - 1L, by=2L)
            } else integer()
            for (left.index.sa in left.sa) {
              right.index.sa <- left.index.sa + 1L
              energy.left.sa <- if (MinMax == "min")
                                  SA.current.fit[left.index.sa] else
                                  -SA.current.fit[left.index.sa]
              energy.right.sa <- if (MinMax == "min")
                                   SA.current.fit[right.index.sa] else
                                   -SA.current.fit[right.index.sa]
              log.swap.sa <- (1/SA.temperature[left.index.sa] -
                              1/SA.temperature[right.index.sa]) *
                             (energy.left.sa - energy.right.sa)
              SA.diagnostics["swap.attempted"] <-
                SA.diagnostics["swap.attempted"] + 1L
              if (is.finite(log.swap.sa) &&
                  log(stats::runif(1)) < min(0, log.swap.sa)) {
                state.sa <- SA.current[left.index.sa, ]
                fit.swap.sa <- SA.current.fit[left.index.sa]
                SA.current[left.index.sa, ] <- SA.current[right.index.sa, ]
                SA.current.fit[left.index.sa] <- SA.current.fit[right.index.sa]
                SA.current[right.index.sa, ] <- state.sa
                SA.current.fit[right.index.sa] <- fit.swap.sa
                SA.diagnostics["swap.accepted"] <-
                  SA.diagnostics["swap.accepted"] + 1L
              } # IF replica swap accepted end
            } # FOR adjacent replica pair end
          } # IF swap sweep end

          SA.temperature[sa.replica.ids] <-
            pmax(.Machine$double.xmin,
                 SA.alpha*SA.temperature[sa.replica.ids]) # Cools the ladder without changing its ratios.
        } # FOR sa.step end

        # Replaces weak particles with several distinct accepted basins and protects them as scouts.
        sa.nscouts <- 0L
        if (length(sa.event.fit) > 0) {
          sa.event.archive <- update.archive.sa(matrix(numeric(), nrow=0, ncol=n),
                                                numeric(), sa.event.X, sa.event.fit)
          sa.updated.archive <- update.archive.sa(SA.archive.X, SA.archive.fit,
                                                  sa.event.archive[["X"]],
                                                  sa.event.archive[["fit"]])
          SA.archive.X <- sa.updated.archive[["X"]]
          SA.archive.fit <- sa.updated.archive[["fit"]]

          sa.nscouts.max <- min(nrow(sa.event.archive[["X"]]), npart - 1L,
                                max(1L, floor(SA.scout.fraction*npart)))
          sa.inject.ids <- select.diverse.sa(sa.event.archive[["X"]],
                                              sa.event.archive[["fit"]],
                                              sa.nscouts.max)
          sa.slots <- setdiff(seq_len(npart), gbest.pos)
          sa.slots <- if (MinMax == "min") {
            sa.slots[order(pbest.fit[sa.slots], decreasing=TRUE)]
          } else sa.slots[order(pbest.fit[sa.slots], decreasing=FALSE)]
          sa.slots <- sa.slots[seq_len(sa.nscouts.max)]
          eligible.sa <- vapply(seq_len(sa.nscouts.max), function(i.sa) {
            better.sa(sa.event.archive[["fit"]][sa.inject.ids[i.sa]],
                      pbest.fit[sa.slots[i.sa]])
          }, logical(1))
          sa.inject.ids <- sa.inject.ids[eligible.sa]
          sa.slots <- sa.slots[eligible.sa]
          sa.nscouts <- length(sa.slots)

          if (sa.nscouts > 0L) {
            # Preserves displaced personal bests before resetting scouts to superior new basins.
            displaced.sa <- update.archive.sa(SA.archive.X, SA.archive.fit,
                                               X.best.part[sa.slots, , drop=FALSE],
                                               pbest.fit[sa.slots])
            SA.archive.X <- displaced.sa[["X"]]
            SA.archive.fit <- displaced.sa[["fit"]]

            for (i.sa in seq_len(sa.nscouts)) {
              slot.sa <- sa.slots[i.sa]
              candidate.sa <- sa.inject.ids[i.sa]
              X[slot.sa, ] <- sa.event.archive[["X"]][candidate.sa, ]
              X.best.part[slot.sa, ] <- X[slot.sa, ]
              pbest.fit[slot.sa] <- sa.event.archive[["fit"]][candidate.sa]
              V[slot.sa, ] <- stats::runif(n, min=X.Boundaries[,1] - X[slot.sa, ],
                                           max=X.Boundaries[,2] - X[slot.sa, ])
              V[slot.sa, ] <- velocity.boundary.treatment(V[slot.sa, ], vmax=Vmax)
              SA.scout.remaining[slot.sa] <- as.integer(SA.scout.generations)
            } # FOR scout injection end
            SA.diagnostics["scouts.injected"] <-
              SA.diagnostics["scouts.injected"] + sa.nscouts

            if (MinMax == "min") {
              gbest.pos <- which.min(pbest.fit)
            } else gbest.pos <- which.max(pbest.fit)
            gbest.fit <- pbest.fit[gbest.pos]
            LocalBest.fit <- rep(fn.worst.value, npart)
            LocalBest.pos <- seq_len(npart)
            tmp <- UpdateLocalBest(pbest.fit=pbest.fit, localBest.pos=LocalBest.pos,
                                   localBest.fit=LocalBest.fit,
                                   x.neighbours=X.neighbours, MinMax=MinMax)
            LocalBest.fit <- tmp[["localBest.fit"]]
            LocalBest.pos <- tmp[["localBest.pos"]]
            X.bak <- X # Makes convergence diagnostics see the repopulated swarm.
          } # IF competitive SA states exist end
        } # IF accepted SA states exist end

        if (sa.nscouts > 0L) {
          SA.diagnostics["productive.events"] <-
            SA.diagnostics["productive.events"] + 1L
          SA.trigger.stagnation <- as.integer(SA.stagnation)
        } else {
          SA.diagnostics["unproductive.events"] <-
            SA.diagnostics["unproductive.events"] + 1L
          if (SA.stagnation > 0L)
            SA.trigger.stagnation <- min(as.integer(SA.reheat),
                                          max(as.integer(SA.stagnation) + 1L,
                                              2L*max(1L, SA.trigger.stagnation)))
        } # ELSE unproductive SA event end
        SA.stagnation.count <- 0L # Requires a fresh stagnation event before another burst.
      } # IF sa.run end
    } # IF method == "spso2011-sa" end

    if (method == "spso2011-dds") {
      # Activates DDS only after the requested number of non-improving SPSO generations.
      if (gbest.fit == dds.gbest.prior) {
        dds.stagnation <- dds.stagnation + 1L
      } else dds.stagnation <- 0L
      dds.gbest.prior <- gbest.fit

      if (!dds.active && dds.stagnation >= DDS.stag.iter) {
        dds.active <- TRUE # Keeps the local refinement active after the first stagnation event.
        dds.maxiter <- max(1L, maxit - iter + 1L) # Defines the remaining DDS schedule.
      } # IF DDS activation end

      if (dds.active && (nfn.eff <= maxfn)) {
        dds.iter <- dds.iter + 1L
        dds.pos <- gbest.pos # Perturbs the current best-so-far parameter vector.
        dds.mask <- dds.dimension.mask(n=n, t=dds.iter, tmax=dds.maxiter)
        dds.candidate <- X.best.part[dds.pos, ]
        dds.candidate[dds.mask] <- dds.candidate[dds.mask] +
                                   stats::rnorm(sum(dds.mask), mean=0,
                                                sd=DDS.r*dds.range[dds.mask])
        dds.candidate <- dds.reflect(dds.candidate, lower=X.Boundaries[,1],
                                    upper=X.Boundaries[,2])

        # Evaluates the single DDS proposal after the parallel swarm batch has completed.
        dds.out <- evaluate.local(dds.candidate, dds.pos, operator="DDS")
        dds.fit <- dds.out[["GoF"]]
        nfn <- nfn + 1
        if ( (fn.name != "hydromod") & (fn.name != "hydromodInR") ) {
          nfn.eff <- nfn.eff + 1
        } else if (is.finite(dds.fit)) nfn.eff <- nfn.eff + 1

        # Greedy acceptance protects both personal and global best-so-far records.
        dds.improved <- is.finite(dds.fit) &&
                        ((MinMax == "min" && dds.fit < gbest.fit) ||
                         (MinMax == "max" && dds.fit > gbest.fit))
        if (dds.improved) {
          X.best.part[dds.pos, ] <- dds.candidate
          pbest.fit[dds.pos] <- dds.fit
          gbest.fit <- dds.fit
          dds.gbest.prior <- dds.fit # Prevents recounting the accepted DDS gain as an SPSO gain.

          tmp <- UpdateLocalBest(pbest.fit=pbest.fit, localBest.pos=LocalBest.pos,
                                 localBest.fit=LocalBest.fit, x.neighbours=X.neighbours,
                                 MinMax=MinMax)
          LocalBest.fit <- tmp[["localBest.fit"]]
          LocalBest.pos <- tmp[["localBest.pos"]]
        } # IF dds.improved end
      } # IF DDS proposal end
    } # IF method == "spso2011-dds" end
       
    if ( plot ) {
  	  if (MinMax == "max") {
        lgof <- max(GoF, na.rm=TRUE)
      } else lgof <- min(GoF, na.rm=TRUE)

  	  colorRamp             <- colorRampPalette(c("darkred", "red", "orange", "yellow", "green", "darkgreen", "cyan"))
  	  XX.Boundaries.current <- computeCurrentXmaxMin(X) 

  	  xlim <- range(XX.Boundaries.current)
  	  ylim <- range(XX.Boundaries.current)

  	  if (ncol(X) > 1) {
        graphics::plot(X[,1], X[,2], xlim=X.Boundaries[1,], ylim=X.Boundaries[2,], 
                       main=paste("Iter= ", iter, ". GoF= ", 
                       format(lgof, scientific=TRUE, digits=digits), sep=""), 
                       col=colorRamp(npart), cex=0.5 )
        graphics::grid()
      } else if (iter==1) {
          graphics::plot(rep(iter, npart), X[,1], xlim=c(1, maxit), ylim=X.Boundaries[1,], 
                         main=paste("GoF= ",  format(lgof, scientific=TRUE, digits=digits), sep=""), 
                         col=colorRamp(npart), cex=0.5 )
          graphics::grid()
        } else graphics::points(rep(iter, npart), X[,1], xlim=c(1, maxit), ylim=X.Boundaries[1,], 
                                main=paste("GoF= ", format(lgof, scientific=TRUE, digits=digits), sep=""), 
                                col=colorRamp(npart), cex=0.5)
    } # IF end 
      
    gbest.fit.iter[iter] <- gbest.fit
      
    suppressWarnings(if (MinMax=="max") {
                           pbest.fit.iter <- max(current.fitness, na.rm=TRUE)
                       } else pbest.fit.iter <- min(current.fitness, na.rm=TRUE)
                      )  

    GPbest.fit.rate <- mean(pbest.fit, na.rm=TRUE)
    if ( (is.finite(GPbest.fit.rate) ) & ( GPbest.fit.rate !=0 ) ) { 
	  GPbest.fit.rate <- abs( ( gbest.fit - GPbest.fit.rate ) / GPbest.fit.rate )
    } else GPbest.fit.rate <- +Inf

    if ( (gbest.fit.prior != 0) & (is.finite(gbest.fit.prior) ) ) { 
	  gbest.fit.rate <- abs( ( gbest.fit - gbest.fit.prior ) / gbest.fit.prior )
    } else gbest.fit.rate <- +Inf

    out <- ComputeSwarmRadiusAndDiameter(x=X.bak, gbest= X.best.part[gbest.pos, ], Lmax=Lmax) 
    swarm.radius    <- out[["swarm.radius"]] 
    swarm.diameter  <- out[["swarm.diameter"]]
    NormSwarmRadius <- swarm.radius/swarm.diameter

    if ( (verbose) & ( iter/REPORT == floor(iter/REPORT) ) ) 
      suppressWarnings(
	     message( "iter:", format(iter, width=nchar(maxit), justify="right"), 
		          "  Gbest:", formatC( gbest.fit, format="E", digits=3, flag=" "), 
		          "  Gbest_rate:", format( round(gbest.fit.rate*100, 2), width=6, nsmall=2, justify="left"), "%",
                  "  Iter_best_fit:", formatC(pbest.fit.iter, format="E", digits=3, flag=" "),               
                  "  nSwarm_Radius:", formatC(NormSwarmRadius, format="E", digits=2, flag=" "),
                  "  |g-mean(p)|/mean(p):", format( round(GPbest.fit.rate*100, 2), width=6, nsmall=2, justify="left"), "%" )
                )

      ##########################################################################  
      # Random Generation around gbest, if requested                           #
      ##########################################################################  
      do.RandomGeneration <- ( use.RG && (NormSwarmRadius < RG.thr)  
				      && (iter.rg >= RG.miniter) )

      if (do.RandomGeneration)  {        

	  if (topology!="ipso") {
	    x.bak         <- X[gbest.pos,]
	    v.bak         <- V[gbest.pos,]
	    gbest.fit.bak <- gbest.fit
	    gbest.pos.bak <- gbest.pos	                    
	  } # IF end

	  if (topology == "ipso") {
	   x.bak          <- X[ngbest.pos,]
	   v.bak          <- V[ngbest.pos,]
	   gbest.fit.bak  <- gbest.fit
     gbest.pos.bak  <- gbest.pos	
	   ngbest.fit.bak <- ngbest.fit
	   ngbest.pos.bak <- ngbest.pos	  
	  } # IF end

	  if (verbose) message("[ Re-grouping particles in the swarm (iter: ", iter, ") ... ]")

	  tmp <- RegroupingSwarm(x=X, 
				                   xini.type=Xini.type, 
                           v=V, 
                           vini.type=Vini.type,                            
	                         gbest= X.best.part[gbest.pos, ], 
				                   x.Range=X.Boundaries,
                           #x.Range=X.Boundaries.current,
                           Lmax=Lmax,
                           RG.thr=RG.thr,
                           RG.r=RG.r) 

	  X <- tmp[["X"]]
	  V <- tmp[["V"]]

      if (method == "spso2011-sa") {
        SA.stagnation.count <- 0L
	        SA.scout.remaining[] <- 0L # Ends basin protection after an explicit swarm regrouping.
	        SA.temperature[] <- NA_real_
      } # IF method == "spso2011-sa" end
	  
	  Lmax <- tmp[["RangeNew"]]

	  if (topology == "ipso") {
	    X[ngbest.pos,] <- x.bak
	    gbest.fit      <- gbest.fit.bak
	    gbest.pos      <- gbest.pos.bak
	  } # IF end

          pbest.fit            <- rep(fn.worst.value, npart)     
          pbest.fit.iter       <- fn.worst.value
          pbest.fit.iter.prior <- fn.worst.value*2
			    
          gbest.fit       <- fn.worst.value
          gbest.fit.iter  <- rep(gbest.fit, maxit)
          gbest.fit.prior <- gbest.fit
          gbest.pos       <- 1
                  
          gbest.fit     <- gbest.fit.bak
          gbest.pos     <- gbest.pos.bak
          X[gbest.pos,] <- x.bak

          if (method == "spso2011-de") {
            DE.mu.F <- DE.F # Restarts adaptation after the swarm search region changes.
            DE.F.previous <- rep(NA_real_, npart) # Discards provenance from pre-regrouping moves.
            DE.archive <- replicate(npart, matrix(numeric(), nrow=0, ncol=n),
                                    simplify=FALSE) # Removes obsolete informant memories.
          } # IF method == "spso2011-de" end

          if (method == "spso2011-cde") {
            CDE.M.F[] <- 0.5 # Restarts success memory in the new search region.
            CDE.M.CR[] <- 0.5
            CDE.memory.pointer <- 1L
            CDE.archive <- matrix(numeric(), nrow=0, ncol=n) # Removes obsolete parent states.
            CDE.operator.previous[] <- NA_character_ # Discards pre-regrouping proposal provenance.
            CDE.parent.X.previous[,] <- NA_real_
            CDE.parent.fit.previous[] <- NA_real_
            CDE.F.previous[] <- NA_real_
            CDE.CR.previous[] <- NA_real_
            CDE.coordinate.previous[] <- NA_character_
            CDE.strategy.previous[] <- NA_character_
            CDE.operator.score[] <- .Machine$double.eps
            CDE.coordinate.score[] <- .Machine$double.eps
            CDE.particle.stagnation[] <- 0L
            CDE.stagnation <- 0L
            CDE.gbest.previous <- gbest.fit
            CDE.q.de <- 0.5
            CDE.basis <- cde.coordinate.basis(X)
          } # IF method == "spso2011-cde" end

	  GPbest.fit.rate <- +Inf              
	  if (MinMax=="max") {
         gbest.fit.prior <- +Inf
      } else gbest.fit.prior <- 0

	  niter.tv <- maxit - iter
	  iter.tv  <- 1   
	  iter.rg  <- 1   
	  nregroup <- nregroup + 1

      } # IF end

      ##########################################################################  
      # Updates required before the next iteration
      ##########################################################################  

      if (MinMax=="max") {
        abstol.conv <- gbest.fit >= abstol
      } else abstol.conv <- gbest.fit <= abstol
                     
      if (reltol==0) {
        reltol.conv <- FALSE
      } else {
        tmp <- abs(pbest.fit.iter.prior - pbest.fit.iter)
        if (tmp==0) {
          reltol.conv <- FALSE
        } else reltol.conv <- tmp <= abs(reltol)
      } # ELSE end
                     
      pbest.fit.iter.prior <- pbest.fit.iter

      # Gbest was improved ?
      if (gbest.fit.prior==gbest.fit) {
        improvement <- FALSE
      } else improvement <- TRUE

      gbest.fit.prior <- gbest.fit
            

      if (abstol.conv ) {
	    end.type.stg  <- "Converged ('abstol' criterion)"
	    end.type.code <- 0
      } else if (reltol.conv) {
	      end.type.stg <- "Converged ('reltol' criterion)"
	      end.type.code <- 1
      } else if (nfn.eff >= maxfn) {
	      end.type.stg <- "Maximum number of function evaluations reached"
	      end.type.code <- 2
      } else if (iter >= maxit) {
	      end.type.stg <- "Maximum number of iterations reached"
	      end.type.code <- 3
      } # ELSE end

      if (write2disk) {
      
        # File 'ConvergenceMeasures.txt'
        suppressWarnings(
	writeLines(as.character( c(iter, 
				   formatC(gbest.fit, format="E", digits=digits, flag=" "), 
				   format( round(gbest.fit.rate*100, 3), nsmall=3, width=7, justify="right"),
				   formatC(pbest.fit.iter, format="E", digits=digits, flag=" "),
				   formatC(NormSwarmRadius, format="E", digits=digits, flag=" "),
				   format( round(GPbest.fit.rate*100, 3), nsmall=3, width=7, justify="right")
				  ) ), ConvergenceMeasures.TextFile, sep="  ")
        )
	writeLines("", ConvergenceMeasures.TextFile)
	flush(ConvergenceMeasures.TextFile) 
        
        # File 'BestParamPerIter.txt' #
        if (normalise) {
          temp <- X.best.part[gbest.pos, ] * (upper.ini - lower.ini) + lower.ini
        } else temp <- X.best.part[gbest.pos, ]
        GoF <- gbest.fit
	if(is.finite(GoF)) {	                    
	  suppressWarnings( writeLines( as.character( c(iter,
	                              formatC(GoF, format="E", digits=digits, flag=" "), 
	                              formatC(temp, format="E", digits=digits, flag=" ")	                                                            
	                          ) ), BestParamPerIter.TextFile, sep="  ") 
                           )
	} else suppressWarnings( writeLines( as.character( c(iter,
	                                   "NA",
	                                   formatC(temp, format="E", digits=digits, flag=" ")                                                                                  
	                               ) ), BestParamPerIter.TextFile, sep="  ")
                               )
	writeLines("", BestParamPerIter.TextFile)  
	flush(BestParamPerIter.TextFile)
	
	# File 'PbestPerIter.txt' #
        GoF <- pbest.fit
        suppressWarnings(
	writeLines( as.character( c(iter,
	                            formatC(GoF, format="E", digits=digits, flag=" ") 
	                           ) ), PbestPerIter.TextFile, sep="  ")
        )
	writeLines("", PbestPerIter.TextFile)  
	flush(PbestPerIter.TextFile)
	
	# File 'LocalBestPerIter.txt' #
        GoF <- LocalBest.fit
        suppressWarnings(
	writeLines( as.character( c(iter,
	                            formatC(GoF, format="E", digits=digits, flag=" ") 
	                           ) ), LocalBestPerIter.TextFile, sep="  ")
        )
	writeLines("", LocalBestPerIter.TextFile)  
	flush(LocalBestPerIter.TextFile)
	
      } # IF end

      iter    <- iter + 1
      iter.tv <- iter.tv + 1
      iter.rg <- iter.rg + 1

    } # WHILE end
    ########################## END Main Iteration Loop #########################
    
    if (normalise) X.best.part <- X.best.part * (UPPER.ini - LOWER.ini) + LOWER.ini

    if (write2disk) {
      close(OFout.Text.file)        
      close(Particles.TextFile)
      close(Velocities.TextFile)
      close(ConvergenceMeasures.TextFile)
      close(BestParamPerIter.TextFile)
      close(PbestPerIter.TextFile) 
      close(LocalBestPerIter.TextFile)
      if (use.RG) {
	close(Xmin.Text.file)        
	close(Xmax.Text.file)
      } # IF end
    } # IF end

    ############################################################################  
    # Sorting the particles according to their best fit
    ############################################################################  
    if (MinMax=="max") {
      sorted.fit <- sort(pbest.fit, decreasing= TRUE)
    } else sorted.fit <- sort(pbest.fit, decreasing= FALSE)

    sorted.index <- pmatch(sorted.fit, pbest.fit)

    ###################   START WRITING OUTPUT FILES     ###################
    if (write2disk) {

      if (verbose) message("                           ")
      if (verbose) message("[ Writing output files... ]")
      if (verbose) message("                           ")

      niter.real <- iter - 1 

      PSOparam.TextFile <- file(PSOparam.fname, "a")    
      
      writeLines("================================================================================", PSOparam.TextFile) 
      writeLines(c("Total fn calls    :", nfn-1), PSOparam.TextFile, sep="  ")
      writeLines("", PSOparam.TextFile) 
      writeLines(c("Nmbr of Iterations:", iter-1), PSOparam.TextFile, sep="  ")
      writeLines("", PSOparam.TextFile) 
      writeLines(c("Regroupings       :", nregroup), PSOparam.TextFile, sep="  ")
      writeLines("", PSOparam.TextFile) 
      if (method == "spso2011-sa") {
        writeLines(c("SA proposals      :", SA.diagnostics["attempted"]),
                   PSOparam.TextFile, sep="  ")
        writeLines("", PSOparam.TextFile)
        writeLines(c("SA accepted       :", SA.diagnostics["accepted"]),
                   PSOparam.TextFile, sep="  ")
        writeLines("", PSOparam.TextFile)
        writeLines(c("SA improvements   :", SA.diagnostics["global.improvements"]),
                   PSOparam.TextFile, sep="  ")
        writeLines("", PSOparam.TextFile)
        writeLines(c("SA reheats        :", SA.diagnostics["reheats"]),
                   PSOparam.TextFile, sep="  ")
        writeLines("", PSOparam.TextFile)
        writeLines(c("SA swaps accepted :", SA.diagnostics["swap.accepted"]),
                   PSOparam.TextFile, sep="  ")
        writeLines("", PSOparam.TextFile)
        writeLines(c("SA scouts injected:", SA.diagnostics["scouts.injected"]),
                   PSOparam.TextFile, sep="  ")
        writeLines("", PSOparam.TextFile)
      } # IF method == "spso2011-sa" end
      writeLines("================================================================================", PSOparam.TextFile) 
      writeLines(c("Ending Time       :", date()), PSOparam.TextFile, sep="  ")
      writeLines("", PSOparam.TextFile) 
      Time.Fin <- Sys.time()
      writeLines("================================================================================", PSOparam.TextFile) 
      writeLines(c("Elapsed Time      :", format(round(Time.Fin - Time.Ini, 2))), PSOparam.TextFile, sep="  ")
      writeLines("", PSOparam.TextFile) 
      writeLines("================================================================================", PSOparam.TextFile) 
      close(PSOparam.TextFile)

      # Writing the file 'BestParameterSet.txt'
      tmp.fname <- paste(file.path(drty.out), "/", "BestParameterSet.txt", sep="") 
      tmp.TextFile  <- file(tmp.fname , "w+")
      writeLines(c("BestParticle", "GoF   ", param.IDs), tmp.TextFile, sep="  ") 
      writeLines("", tmp.TextFile)  
      suppressWarnings( tmp <- formatC(c(gbest.fit, X.best.part[gbest.pos,]), format="E", digits=digits, flag=" ") )
      writeLines(as.character(c(gbest.pos, tmp)), tmp.TextFile, sep="  ") 
      writeLines("", tmp.TextFile)  
      close(tmp.TextFile) 

      # Writing the file 'XMinMax.txt' with the parameter ranges used during PSO
      fname <- paste(file.path(drty.out), "/", "XMinMax.txt", sep="") 	
      ifelse(normalise, tmp <- X.Boundaries.ini, tmp <- X.Boundaries)				
      write.table(format(tmp, scientific=TRUE, digits=digits), file=fname, col.names=TRUE, row.names=TRUE, sep="  ", quote=FALSE) 

      # Writing the file 'Particles_GofPerIter.txt', with the GoF for each particle in each iteration
      tmp.fname <- paste(file.path(drty.out), "/", "Particles_GofPerIter.txt", sep="") 
      tmp.TextFile  <- file(tmp.fname , "w+")
      writeLines(paste("Iter", paste("Part", 1:npart, collapse="    ", sep=""), sep="    "), tmp.TextFile, sep="  ") 
      writeLines("", tmp.TextFile)  
      for ( i in (1:niter.real) ) {               
	      suppressWarnings( tmp <- formatC(Xt.fitness[i, ], format="E", digits=digits, flag=" ") )
	      writeLines(as.character(c(i, tmp)), tmp.TextFile, sep="  ") 
	      writeLines("", tmp.TextFile)    
      } # FOR end 
      close(tmp.TextFile) 

      # Writing the file 'BestParamPerParticle.txt', with ...
      fname <- paste(file.path(drty.out), "/", "BestParamPerParticle.txt", sep="") 
      tmp <- cbind(pbest.fit, X.best.part)
      colnames(tmp) <- c("GoF", param.IDs)
      write.table(format(tmp, scientific=TRUE, digits=digits), file=fname, col.names=TRUE, row.names=FALSE, sep="  ", quote=FALSE)

      # Writing the file 'X.neighbours.txt' 
      fname <- paste(file.path(drty.out), "/", "Particles_Neighbours.txt", sep="") 
      ifelse(topology == "lbest", nc <- K, nc <- npart)
      write.table(X.neighbours, file=fname, col.names=paste("Neigh", 1:nc, sep=""), row.names=paste("Part", 1:npart, sep=""), sep="  ", na="", quote=FALSE) 

      # Writing the file 'LocalBest.txt' 
      fname <- paste(file.path(drty.out), "/", "LocalBest.txt", sep="") 	
      write.table(format(LocalBest.fit, scientific=TRUE, digits=digits), file=fname, col.names=TRUE, row.names=FALSE, sep="  ", quote=FALSE)

      if ( (fn.name=="hydromod") | (fn.name=="hydromodInR") ) {

	      hydroPSOparam.TextFile <- file(hydroPSOparam.fname, "a")    
	
	      writeLines("================================================================================", PSOparam.TextFile) 
        writeLines(c("Total model calls      :", nfn-1), PSOparam.TextFile, sep="  ")
        writeLines("", PSOparam.TextFile) 
        writeLines(c("Effective model calls  :", nfn.eff-1), PSOparam.TextFile, sep="  ")
        writeLines("", PSOparam.TextFile) 
        writeLines("================================================================================", hydroPSOparam.TextFile) 
	      writeLines(c("Ending Time            :", date()), hydroPSOparam.TextFile, sep=" ")
	      writeLines("", hydroPSOparam.TextFile) 
	      writeLines("================================================================================", hydroPSOparam.TextFile) 
	      Time.Fin <- Sys.time()
	      writeLines(c("Elapsed Time           :", format(round(Time.Fin - Time.Ini, 2))), hydroPSOparam.TextFile, sep=" ")
	      writeLines("", hydroPSOparam.TextFile) 
	      writeLines("================================================================================", hydroPSOparam.TextFile) 
	      close(hydroPSOparam.TextFile)

      } # IF 'fn.name' END           

    } # IF end

    #####################     END WRITING OUTPUT FILES     #####################

    ########################################################################
    ##                                parallel                             #
    ########################################################################
    if (parallel!="none") {
      if ( (parallel=="parallel") | (parallel=="parallelWin") )   
           parallel::stopCluster(cl)   
      if (fn.name=="hydromod") {
        if (verbose) message("                                         ")
        if (verbose) message("[ Removing the 'parallel' directory ... ]")    
        unlink(dirname(mc.dirs[1]), recursive=TRUE)
      } # IF end
         
    } # IF end
    ############################################################################
    if (verbose) message("                                    |                                           ")  
    if (verbose) message("================================================================================")
    if (verbose) message("[                          Creating the R output ...                           ]")
    if (verbose) message("================================================================================")

    # Creating the R output
    nelements <- 6 
    out       <- vector("list", nelements)

    out[[1]]        <- X.best.part[gbest.pos,]
    names(out[[1]]) <- param.IDs
    out[[2]] <- gbest.fit
    out[[3]] <- gbest.pos
    out[[4]] <- c("function.calls"=nfn-1, "iterations"=iter-1, "regroupings"=nregroup)
    out[[5]] <- end.type.code
    out[[6]] <- end.type.stg

    names(out)[1:nelements] <- c("par",           # "Best.Parameter.Values", 
				 "value",         # "Global.Best.Value", 
				 "best.particle", # "Global.Best.Position",
				 "counts", 
				 "convergence", 
				 "message") 

    if (method == "spso2011-sa") {
      # Returns replica, exchange, and repopulation diagnostics for auditing the hybrid.
      SA.current.out <- SA.current
      SA.archive.out <- SA.archive.X
      if (normalise) {
        SA.current.out <- sweep(SA.current.out, 2, upper.ini - lower.ini, FUN="*")
        SA.current.out <- sweep(SA.current.out, 2, lower.ini, FUN="+")
        SA.archive.out <- sweep(SA.archive.out, 2, upper.ini - lower.ini, FUN="*")
        SA.archive.out <- sweep(SA.archive.out, 2, lower.ini, FUN="+")
      } # IF normalised SA states end
      colnames(SA.current.out) <- param.IDs
      colnames(SA.archive.out) <- param.IDs
      colnames(SA.scale.current) <- param.IDs
      rownames(SA.current.out) <- paste0("replica", seq_len(nrow(SA.current.out)))
      rownames(SA.scale.current) <- rownames(SA.current.out)
      SA.cold.par <- SA.current.out[1, ]
      names(SA.cold.par) <- param.IDs
      SA.cold.scale <- SA.scale.current[1, ]
      names(SA.cold.scale) <- param.IDs
      out[[nelements+1]] <- list(attempted=unname(SA.diagnostics["attempted"]),
                                 accepted=unname(SA.diagnostics["accepted"]),
                                 uphill.accepted=unname(SA.diagnostics["uphill.accepted"]),
                                 global.improvements=unname(SA.diagnostics["global.improvements"]),
                                 invalid=unname(SA.diagnostics["invalid"]),
                                 activations=unname(SA.diagnostics["activations"]),
                                 reheats=unname(SA.diagnostics["reheats"]),
                                 swap.attempted=unname(SA.diagnostics["swap.attempted"]),
                                 swap.accepted=unname(SA.diagnostics["swap.accepted"]),
                                 scouts.injected=unname(SA.diagnostics["scouts.injected"]),
                                 collapsed.events=unname(SA.diagnostics["collapsed.events"]),
                                 emergency.events=unname(SA.diagnostics["emergency.events"]),
                                 productive.events=unname(SA.diagnostics["productive.events"]),
                                 unproductive.events=unname(SA.diagnostics["unproductive.events"]),
                                 initial.temperature=SA.initial.temperature[1],
                                 final.temperature=SA.temperature[1],
                                 final.scale=SA.cold.scale,
                                 current.par=SA.cold.par,
                                 current.value=SA.current.fit[1],
                                 temperature.ladder=SA.temperature,
                                 replica.par=SA.current.out,
                                 replica.value=SA.current.fit,
                                 replica.scale=SA.scale.current,
                                 archive.par=SA.archive.out,
                                 archive.value=SA.archive.fit,
                                 scout.remaining=SA.scout.remaining,
                                 evaluation.budget=SA.budget.max)
      names(out)[nelements+1] <- "SA.diagnostics"
      nelements <- nelements + 1
    } # IF method == "spso2011-sa" end

    if (method == "spso2011-cde") {
      # Returns allocation, selection, archive, and success-memory diagnostics.
      CDE.archive.out <- CDE.archive
      if (normalise && nrow(CDE.archive.out) > 0) {
        CDE.archive.out <- sweep(CDE.archive.out, 2, upper.ini-lower.ini, FUN="*")
        CDE.archive.out <- sweep(CDE.archive.out, 2, lower.ini, FUN="+")
      } # IF normalised CDE archive end
      colnames(CDE.archive.out) <- param.IDs
      out[[nelements+1]] <- list(
        pso.evaluated=unname(CDE.diagnostics["pso.evaluated"]),
        de.evaluated=unname(CDE.diagnostics["de.evaluated"]),
        de.accepted=unname(CDE.diagnostics["de.accepted"]),
        emergency.evaluated=unname(CDE.diagnostics["emergency.evaluated"]),
        original.evaluated=unname(CDE.diagnostics["original.evaluated"]),
        eigen.evaluated=unname(CDE.diagnostics["eigen.evaluated"]),
        final.q.de=unname(CDE.q.de), final.diversity=CDE.diversity.current,
        stagnation=CDE.stagnation, operator.score=CDE.operator.score,
        coordinate.score=CDE.coordinate.score, F.memory=CDE.M.F,
        CR.memory=CDE.M.CR, archive.par=CDE.archive.out)
      names(out)[nelements+1] <- "CDE.diagnostics"
      nelements <- nelements + 1
    } # IF method == "spso2011-cde" end

    if (method == "spso2011-pdds") {
      # Returns portfolio diagnostics for auditing evaluation allocation and migration.
      pdds.chain.out <- pdds.chain.X
      if (normalise && nrow(pdds.chain.out) > 0) {
        pdds.chain.out <- pdds.chain.out *
                          matrix(upper.ini - lower.ini, nrow=nrow(pdds.chain.out),
                                 ncol=n, byrow=TRUE) +
                          matrix(lower.ini, nrow=nrow(pdds.chain.out),
                                 ncol=n, byrow=TRUE)
      } # IF normalised pDDS chains end
      colnames(pdds.chain.out) <- param.IDs
      out[[nelements+1]] <- list(swarm.batches=unname(pdds.diagnostics["swarm.batches"]),
                                 dds.batches=unname(pdds.diagnostics["dds.batches"]),
                                 dds.evaluations=unname(pdds.diagnostics["dds.evaluations"]),
                                 chain.improvements=unname(pdds.diagnostics["chain.improvements"]),
                                 migrations=unname(pdds.diagnostics["migrations"]),
                                 archive.size=length(pdds.archive.fit),
                                 chain.r=pdds.chain.r,
                                 chain.par=pdds.chain.out,
                                 chain.value=pdds.chain.fit)
      names(out)[nelements+1] <- "PDDS.diagnostics"
      nelements <- nelements + 1
    } # IF method == "spso2011-pdds" end

    if (out.with.pbest) {            
      out[[nelements+1]] <- X.best.part

      out[[nelements+2]] <- pbest.fit

      names(out)[(nelements+1):(nelements+2)] <- c("pbest.Parameter.Values", "pbest.fit") 

      nelements <- nelements + 2                
    } # IF end

    if (out.with.fit.iter) {  
      Xt.fitness <- Xt.fitness[1:(iter-1), ]
      out[[nelements+1]] <- Xt.fitness          
      names(out)[nelements+1] <- c("part.fit.per.iter")  
    } # IF end

    ############################################################################  
    if  ( (fn.name=="hydromod") | (fn.name=="hydromodInR") ) {

     
      if (verbose) message("                                                                                ")  
      if (verbose) message("                                    |                                           ")  
      if (verbose) message("================================================================================")
      if (verbose) message("[         Running one last time the model with the 'best' parameter set ...    ]")
      if (verbose) message("================================================================================")  
      if (verbose) message("                                    |                                           ")  
      if (verbose) message("                                                                                ")  

      
      param.values <- out[["par"]] # Starts from the best PSO coordinate vector.
      if (fn.name == "hydromodInR") # R-based models need the same parameter-change rule in the final best-model run.
        param.values <- .changeParamValues(param.values, change.type=change.type, refValue=refValue) # Converts the best PSO coordinates to model parameter values.
      model.FUN.args <- modifyList(model.FUN.args, # Updates the model arguments for the final best-model run.
                                   list(param.values=param.values) # Sends transformed parameter values to R-based models and raw values to external models.
                                  ) 
      hydromod.out   <- do.call(model.FUN, as.list(model.FUN.args), ...)       
      
      # Writing observations and best model output
      if (write2disk && ("obs" %in% names(model.FUN.args))) {      
         if (date.fmt.exists) {
           date.fmt <- model.FUN.args[["date.fmt"]]
         } else date.fmt <- "%Y-%m-%d"        
         if ( gof.Ini.exists | gof.Fin.exists ) 
             if ( grepl("%H", date.fmt, fixed=TRUE) | grepl("%M", date.fmt, fixed=TRUE) |
                     grepl("%S", date.fmt, fixed=TRUE) | grepl("%I", date.fmt, fixed=TRUE) |
                     grepl("%p", date.fmt, fixed=TRUE) | grepl("%X", date.fmt, fixed=TRUE)
                 ) { subdaily.date.fmt <- TRUE
                   } else subdaily.date.fmt <- FALSE
                     
        obs <- eval( model.FUN.args[["obs"]] )
        sim <- eval( hydromod.out[["sim"]] )
        
        obs.fname <- paste(file.path(drty.out), "/", "Observations.txt", sep="") 
        sim.fname <- paste(file.path(drty.out), "/", "BestModel_out.txt", sep="") 	
	 
        if (is.zoo(obs)) { # zoo::is.zoo
          if (gof.Ini.exists) {
            if (subdaily.date.fmt) {
               gof.Ini <- as.POSIXct(model.FUN.args[["gof.Ini"]], format=date.fmt)
            } else gof.Ini <- as.Date(model.FUN.args[["gof.Ini"]], format=date.fmt)
            obs <- window(obs, start=gof.Ini)
            sim <- window(sim, start=gof.Ini)
          } # IF end
          if (gof.Fin.exists) {
            if (subdaily.date.fmt) {
              gof.Fin <- as.POSIXct(model.FUN.args[["gof.Fin"]], format=date.fmt)
            } else gof.Fin <- as.Date(model.FUN.args[["gof.Fin"]], format=date.fmt)
            obs <- window(obs, end=gof.Fin)
            sim <- window(sim, end=gof.Fin)
          } # IF end
          if (verbose) message("[ Writing Observations.txt (zoo) ... ]")
          write.zoo(x=obs, file=obs.fname) # zoo::write.zoo
          if (verbose) message("[ Writing BestModel_out.txt ... ]")
          write.zoo(x=sim, file=sim.fname) # zoo::write.zoo
        } else {
            if (verbose) message("[ Writing Observations.txt (numeric) ... ]")
            obs <- cbind(1:length(obs), obs)
            write.table(obs, file=obs.fname, col.names=FALSE, row.names=FALSE, sep="  ", quote=FALSE)
            
            if (verbose) message("[ Writing BestModel_out.txt ... ]")
            sim <- cbind(1:length(sim), sim)
            write.table(obs, file=sim.fname, col.names=FALSE, row.names=FALSE, sep="  ", quote=FALSE)
          } # ELSE end
          
      } # IF end

    } # IF end    

    ############################################################################
    # 7)                                 Output                                #
    ############################################################################

    return(out)
        
} # 'hydroPSO' end

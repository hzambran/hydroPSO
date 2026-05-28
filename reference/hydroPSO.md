# Enhanced Particle Swarm Optimisation algorithm

State-of-the-art version of the Particle Swarm Optimisation (PSO)
algorithm (SPSO-2011 and SPSO-2007 capable). hydroPSO can be used as a
replacement for [`optim`](https://rdrr.io/r/stats/optim.html), but its
main focus is the calibration of environmental and other real-world
model codes. Several fine-tuning options and PSO variants are available
to customise the PSO engine to different calibration problems.

## Usage

``` r
hydroPSO(par, fn= "hydromod", ..., 
         method=c("spso2011", "spso2007", "ipso", "fips", "wfips", "canonical"),
         lower=-Inf, upper=Inf, control=list(), 
         model.FUN=NULL, model.FUN.args=list(),
         change.type="repl", refValue=NULL )
```

## Arguments

- par:

  OPTIONAL. numeric with a first guess for the parameters to be
  optimised, with length equal to the dimension of the solution space  

  All the particles are randomly initialised according to the value of
  `Xini.type`. If the user provides `m` parameter sets for `par`, they
  are used to overwrite the first `m` parameter sets randomly defined
  according to the value of `Xini.type`. If some elements in `par` are
  non finite (lower than `lower` or larger than `upper`) they are
  ignored.

- fn:

  function or character with the name of a valid R function to be
  optimised (minimized or maximized). The character value ‘hydromod’ is
  used to specify that an R-external model code (i.e., an executable
  file that needs to be run from the system console) will be analised
  instead of an R function  

  -) When `fn!='hydromod'`, the first argument of `fn` has to be a
  vector of parameters over which optimisation is going to take place.
  It should return a scalar result. When `fn!='hydromod'` the algorithm
  uses the value(s) returned by `fn` as both model output and its
  corresponding goodness-of-fit measure  

  -) When `fn=='hydromod'` the algorithm will optimise the model defined
  by `model.FUN` and `model.args`, which are used to extract the values
  simulated by the model and to compute its corresponding
  goodness-of-fit measure

- ...:

  OPTIONAL. Only used when `fn!='hydromod' & fn!='hydromodInR'`.  

  further arguments to be passed to `fn`.

- method:

  character, variant of the PSO algorithm to be used. By default
  `method='spso2011'`, while valid values are ‘spso2011’, ‘spso2007’,
  ‘ipso’, ‘fips’, ‘wfips’, ‘canonical’:  

  spso2011: At each iteration particles are attracted to its own
  best-known ‘personal’ and to the best-known position in its ‘local’
  neighbourhood, which depens on the value of `topology`. In addition,
  values of the PSO engine are set to the values defined in the Standard
  PSO 2011 (SPSO 2011, see Clerc 2012)  

  spso2007: As in `method='spso2011'`, but with values of the PSO engine
  set to the values defined in the Standard PSO 2007 (SPSO 2007, see
  Clerc 2012)  

  ipso: at each iteration particles in the swarm are rearranged in
  descending order according to their goodness-of-fit and the best
  `ngbest` particles are used to modify particles' position and velocity
  (see Zhao, 2006). Each particle is connected to a neighbourhood of
  particles depending on the `topology` value  

  fips: at each iteration ALL particles contribute to modify the
  particles' position and velocity (see Mendes et al., 2004). Each
  particle is connected to a neighbourhood of particles depending on the
  `topology` value  

  wfips: same implementation as fips method, but the contribution of
  each particle is weighted according to their goodness-of-fit value
  (see Mendes et al., 2004)  

  canonical: It corresponds to the first formulation of the PSO
  algorithm, and it is included here for educational and comparative
  purposes only, due to several limitations described in literature (see
  Kennedy, 2006).

  At each iteration, particles are attracted to its own best-known
  ‘personal’ and to the best-known position in all the swarm (‘global’).
  The following `control` arguments are set when this method is
  selected:

  \(i\) `npart=40`,

  \(ii\) `topology='gbest'`,

  \(iii\) `Xini.type='random'`,

  \(iv\) `Vini.type='random2007'`,

  \(v\) `use.CF=TRUE`,

  \(vi\) `c1=2.05`,

  \(vii\) `c2=2.05`,

  \(viii\) `boundary.wall='absorbing2007'`,

  \(ix\) `lambda=1.0`

- lower:

  numeric, lower boundary for each parameter  

  Note for [`optim`](https://rdrr.io/r/stats/optim.html) users: in
  hydroPSO the length of `lower` and `upper` are used to defined the
  dimension of the solution space

- upper:

  numeric, upper boundary for each parameter  

  Note for [`optim`](https://rdrr.io/r/stats/optim.html) users: in
  hydroPSO the length of `lower` and `upper` are used to defined the
  dimension of the solution space

- control:

  a list of control parameters. See ‘Details’

- model.FUN:

  OPTIONAL. Used only when `fn='hydromod'`  

  character, valid R function representing the model code to be
  calibrated/optimised

- model.FUN.args:

  OPTIONAL. Used only when `fn='hydromod'`  

  list with the arguments to be passed to `model.FUN`

- change.type:

  OPTIONAL. Used only when `fn='hydromodInR'`. Character of length 1 or
  length equal to the number of parameters, indicating how the PSO
  coordinates are converted to the parameter values passed to
  `model.FUN`. Valid values are `"repl"` for replacement, `"addi"` for
  additive changes, and `"mult"` for multiplicative changes. A single
  value is recycled to all parameters. The default `"repl"` preserves
  the previous behaviour.

- refValue:

  OPTIONAL. Used only when `fn='hydromodInR'` and any element of
  `change.type` is `"addi"` or `"mult"`. Numeric reference value of
  length 1 or length equal to the number of parameters, used to compute
  the parameter values passed to `model.FUN`. Only parameters using
  `"addi"` or `"mult"` require and use their corresponding reference
  value.

## Details

By default the hydroPSO function performs minimization of `fn`, but it
will maximize `fn` if `MinMax='max'`  

The default control arguments in hydroPSO implements the Standard PSO
2011 - SPSO2011 (see Clerc 2012; Clerc et al., 2010). At the same time,
hydroPSO function provides options for clamping the maximal velocity,
regrouping strategy when premature convergence is detected, time-variant
acceleration coefficients, time-varying maximum velocity, (non-)linear /
random / adaptive / best-ratio inertia weight definitions, random or LHS
initialization of positions and velocities, synchronous or asynchronous
update, 4 alternative neighbourhood topologies among others.

The `control` argument is a list that can supply any of the following
components:

- drty.in:

  OPTIONAL. Used only when `fn='hydromod'`  
  character, name of the directory storing the input files required for
  PSO, i.e. ‘ParamRanges.txt’ and ‘ParamFiles.txt’

- drty.out:

  character, path to the directory storing the output files generated by
  hydroPSO

- param.ranges:

  OPTIONAL. Used only when `fn='hydromod'`  

  character, name of the file defining the minimum and maximum boundary
  values for each one of the parameters to be calibrated

- digits:

  OPTIONAL. Used only when `write2disk=TRUE`  

  numeric, number of significant digits used for writing the output
  files with scientific notation

- MinMax:

  character, indicates whether a maximization or minimization problem
  needs to be solved. Valid values are in: `c('min', 'max')`. Default
  value is min

- npart:

  numeric, number of particles in the swarm. By default `npart=NA`,
  which means that the swarm size depends on the value of `method`:  

  when `method='spso2007'` `npart=ceiling(10+2*sqrt(n))`, or `npart=40`
  otherwise

- maxit:

  numeric, maximum number of iterations. By default `maxit=1000`

- maxfn:

  numeric, maximum number of function evaluations. Default value is
  `+Inf`  

  When `fn=='hydromod'`, this stopping criterion uses the number of
  *effective* function calls, i.e. those function calls with a finite
  output value

- c1:

  numeric, cognitive acceleration coefficient. Encourages the
  exploitation of the solution space and reflects how much the particle
  is influenced by its own best-known position  

  By default `c1= 0.5 + log(2)`

- c2:

  numeric, social acceleration coefficient. Encourages the exploration
  of the current global best and reflects how much the particle is
  influenced by the best-known optimum of the swarm  

  By default `c2= 0.5 + log(2)`

- use.IW:

  logical, indicates if an inertia weight (`w`) will be used to avoid
  swarm explosion, i.e. particles flying around their best position
  without converging into it (see Shi and Eberhart, 1998)  

  By default `use.IW=TRUE`

- IW.w:

  OPTIONAL. Used only when `use.IW=TRUE` and `IW.type!='GLratio'`  

  numeric, value of the inertia weight(s) (`w` or `[w.ini, w.fin]`). It
  can be a single number which is used for all iterations, or it can be
  a vector of length 2 with the initial and final values (in that order)
  that `w` will take along the iterations  

  By default `IW.w=1/(2*log(2))`

- use.CF:

  logical, indicates if the Clerc's Constriction Factor (see Clerc,
  1999; Eberhart and Shi, 2000; Clerc and Kennedy, 2002) is used to
  avoid swarm explosion  

  By default `use.CF=FALSE`

- lambda:

  numeric in \[0,1\], represents a percentage to limit the maximum
  velocity (Vmax) for each dimension, which is computed as
  `vmax = lambda*(Xmax-Xmin)`  

  By default `lambda=1`

- abstol:

  numeric, absolute convergence tolerance. The algorithm stops if
  `gbest <= abstol` (minimisation problems) OR when `gbest >= abstol`
  (maximisation problems)  

  By default it is set to `-Inf` or `+Inf` for minimisation or
  maximisation problems, respectively

- reltol:

  numeric, relative convergence tolerance. The algorithm stops if the
  absolute difference between the best ‘personal best’ in the current
  iteration and the best ‘personal best’ in the previous iteration is
  less or equal to `reltol`. Defaults to `sqrt(.Machine$double.eps)`,
  typically, about 1e-8  

  If `reltol` is set to `0`, this stopping criterion is not used

- Xini.type:

  character, indicates how to initialise the particles' positions in the
  swarm within the ranges defined by `lower` and `upper`. Valid values
  are:  

  -) random: random initialisation of positions within `lower` and
  `upper`  

  -) lhs: Latin Hypercube initialisation of particle's positions, using
  `npart` number of strata to divide each parameter range. **It requires
  the lhs package**. See
  [`randomLHS`](https://rdrr.io/pkg/lhs/man/randomLHS.html).  

  -) sobol: Quasi-random initialisation of particle's positions using
  uniform Sobol low discrepancy numbers within `lower` and `upper` with
  `npart` as the number of observations to extract. **It requires the
  randtoolbox package**. See
  [`sobol`](https://rdrr.io/pkg/randtoolbox/man/quasiRNG.html).  

  By default `Xini.type='random'`

- Vini.type:

  character, indicates how to initialise the particles' velocities in
  the swarm. Valid values are:  

  -) random2011: random initialisation of velocities within `lower-Xini`
  and `upper-Xini`, as defined in SPSO 2011
  (`Vini=U(lower-Xini, upper-Xini)`) (see Clerc, 2012, 2010)  

  -) lhs2011: same as in random2011, but using a Latin Hypercube
  initialisation with `npart` number of strata instead of a random
  uniform distribution for each parameter. **It requires the lhs
  package**  

  -) random2007: random initialisation of velocities within `lower` and
  `upper` using the ‘half-diff’ method defined in SPSO 2007
  (`Vini=[U(lower, upper)-Xini]/2`) (see Clerc, 2012, 2010)  

  -) lhs2007: same as in random2007, but using a Latin Hypercube
  initialisation with `npart` number of strata instead of a random
  uniform distribution for each parameter. **It requires the lhs
  package**  

  -) zero: all the particles are initialised with zero velocity  

  By default `Vini.type=NA`, which means that `Vini.type` depends on the
  value of `method`: when method='spso2007' `Vini.type='random2007'`, or
  `Vini.type='random2011'` otherwise

- best.update:

  character, indicates how (when) to update the global/neighbourhood and
  personal best. Valid values are:  

  -)sync: the update is made synchronously, i.e. after computing the
  position and goodness-of-fit for ALL the particles in the swarm. This
  is the DEFAULT option  

  -)async: the update is made asynchronously, i.e. after computing the
  position and goodness-of-fit for EACH individual particle in the swarm

- random.update:

  OPTIONAL. Only used when `best.update='async'`  

  logical, if `TRUE` the particles are processed in random order to
  update their personal best and the global/neighbourhood best  
  By default `random.update=TRUE`

- boundary.wall:

  character, indicates the type of boundary condition to be applied
  during optimisation. Valid values are: `NA`, ‘absorbing2011’,
  ‘absorbing2007’, ‘reflecting’, ‘damping’, ‘invisible’:  

  -) NA: The value of `boundary.wall` depends on the value defined for
  the `method`: when method='spso2007' `boundary.wall='absorbing2007'`,
  or `boundary.wall='absorbing2011'` otherwise. By default
  `boundary.wall=NA`.  

  -) absorbing2011: This condition clamps a particle's position to the
  edge of the search space if it tries to leave, but it handles velocity
  flexibly to match its hypersphere trajectory updates. Instead of
  completely killing the particle's momentum, it often allows the
  internal velocity vector to persist or undergo geometric modification,
  enabling the particle to smoothly "slide" along the boundary during
  subsequent iterations. See more details in Clerc (2012).  

  -) absorbing2007: Acting like a perfectly sticky wall, this method
  strictly clamps the particle's position to the violated boundary and
  forces its velocity in that specific dimension to exactly zero. The
  particle loses all momentum in that direction and will remain stuck at
  the edge until the gravitational pull of its personal or global best
  draws it back into the valid search space. See more details in Clerc
  (2012).  

  -) reflecting: This approach treats the boundary as a perfectly
  elastic surface. When a particle overshoots the search space, its
  position is mirrored back inside by the exact distance it traveled out
  of bounds, and its velocity in that dimension is perfectly reversed.
  This conserves the particle's kinetic energy, encouraging aggressive
  and continuous exploration of the boundary regions without losing
  swarm momentum. See more details in Robinson and Rahmat-Samii
  (2004).  

  -) damping: Functioning as an inelastic collision, the damping
  condition reflects the particle back into the search space but applies
  a random reduction factor to its reversed velocity. This hybrid
  approach allows the particle to bounce off the wall and continue
  exploring, but strategically bleeds off its kinetic energy to prevent
  the swarm from endlessly oscillating back and forth across the entire
  search space, thereby aiding convergence. See more details in Huang
  and Mohan (2005).  

  -) invisible: Under this condition, the boundaries mathematically do
  not restrict movement, allowing particles to fly freely outside the
  search space with their position and velocity completely unchanged.
  However, while a particle is out of bounds, its fitness is not
  evaluated; it relies entirely on the algorithmic attraction of its
  previously established valid personal and global bests to eventually
  pull its trajectory back into the valid optimization space. See more
  details in Robinson and Rahmat-Samii (2004).  

  Experience has shown that Clerc's constriction factor and the inertia
  weights do not always confine the particles within the solution space.
  To address this problem, Robinson and Rahmat-Samii (2004) and Huang
  and Mohan (2005) propose different boundary conditions, namely,
  reflecting, damping, absorbing and invisible to define how particles
  are treated when reaching the boundary of the searching space (see
  Robinson and Rahmat-Samii (2004) and Huang and Mohan (2005) for
  further details)

- topology:

  character, indicates the neighbourhood topology used in hydroPSO.
  Valid values are in `c('random', 'gbest', 'lbest', 'vonNeumann')`:  

  -) random: the random topology is a special case of `lbest` where
  connections among particles are randomly modified after an iteration
  showing no improvement in the global best (see Clerc, 2005; Clerc,
  2010)  

  -) gbest: every particle is connected to each other and, hence the
  global best influences all particles in the swarm. This is also termed
  `star` topology, and it is generally assumed to have a fast
  convergence but is more vulnerable to the attraction to sub-optimal
  solutions (see Kennedy, 1999; Kennedy and Mendes, 2002, Schor et al.,
  2010)  

  -) lbest: each particle is connected to its `K` immediate neighbours
  only. This is also termed `circles` or `ring` topology, and generally
  the swarm will converge slower than the gbest topology but it is less
  vulnerable to sub-optimal solutions (see Kennedy, 1999; Kennedy and
  Mendes, 2002)  

  -) vonNeumann: each particle is connected to its `K=4` immediate
  neighbours only. This topology is more densely connected than `lbest`
  but less densely than `gbest`, thus, showing some parallelism with
  `lbest` but benefiting from a bigger neighbourhood (see Kennedy and
  Mendes, 2003)  

  By default `topology='random'`.

- K:

  OPTIONAL. Only used when `topology` is in
  `c(random, lbest, vonNeumann)`  

  numeric, neighbourhood size, i.e. the number of informants for each
  particle (including the particle itself) to be considered in the
  computation of their personal best  

  When `topology=lbest` `K` MUST BE an even number in order to consider
  the same amount of neighbours to the left and the right of each
  particle  

  As special case, `K` could be equal to `npart`. By default `K=3`

- iter.ini:

  OPTIONAL. Only used when `topology=='lbest'`. By default
  `iter.ini=0`.  

  numeric, number of iterations for which the gbest topology will be
  used before using the lbest topology for the computation of the
  personal best of each particle  

  This option aims at making faster the identification of the global
  zone of attraction.

- ngbest:

  OPTIONAL. Only used when `method=='ipso'`  

  numeric, number of particles considered in the computation of the
  global best  

  By default `ngbest=4` (see Zhao, 2006)

- normalise:

  logical, indicates whether the parameter values have to be normalised
  to the \[0,1\] interval during the optimisation or not.  

  This option appears in the C and Matlab version of SPSO-2011 (See
  https://www.particleswarm.info/standard_pso_2011_c.zip) and there it
  is recommended to use this option when the search space is not an
  hypercube. If the search space is an hypercube, it is better not
  normalise (there is a small difference between the position without
  any normalisation and the de-normalised one). By default
  `normalise=FALSE`

- IW.type:

  OPTIONAL. Used only when `use.IW= TRUE` AND `length(IW.w)>1`  

  character, defines how the inertia weight `w` will vary along
  iterations. Valid values are:  

  -)linear: `w` varies linearly between the initial and final values
  specified in `IW.w` (see Shi and Eberhart, 1998; Zheng et al., 2003).
  This is the DEFAULT option  

  -)non-linear: `w` varies non-linearly between the initial and final
  values specified in `IW.w` with exponential factor IW.exp (see
  Chatterjee and Siarry, 2006)  

  -)runif: `w` is a uniform random variable in the range
  `[w.min, w.max]` specified in `IW.w`. It is a generalisation of the
  weight proposed in Eberhart and Shi (2001b)  

  -)aiwf: adaptive inertia weight factor, where the inertia weight is
  varied adaptively depending on the goodness-of-fit values of the
  particles (see Liu et al., 2005)  

  -)GLratio: `w` varies according to the ratio between the global best
  and the average of the particle's local best (see Arumugam and Rao,
  2008)  

  By default `IW.type='linear'`

- IW.exp:

  OPTIONAL. Used only when `use.IW=TRUE` AND `IW.type='non-linear'`  

  numeric, non-linear modulation index (see Chatterjee and Siarry,
  2006)  

  When `IW.type='linear'`, `IW.exp` is set to 1. By default `IW.exp=1`

- use.TVc1:

  logical, indicates if the cognitive acceleration coefficient `c1` will
  have a time-varying value instead of a constant one provided by the
  user (see Ratnaweera et al. 2004). By default `use.TVc1=FALSE`

- TVc1.type:

  character, required only when `use.TVc1 = TRUE`. Valid values are:  

  -)linear: `c1` varies linearly between the initial and final values
  specified in `TVc1.rng` (see Ratnaweera et al., 2004)  

  -)non-linear: `c1` varies non-linearly between the initial and final
  values specified in `TVc1.rng`. Proposed by the authors of hydroPSO
  taking into account the work of Chatterjee and Siarry (2006) for the
  inertia weight  

  -)GLratio: `c1` varies according to the ratio between the global best
  and the average of the particle's local best (see Arumugam and Rao,
  2008)  

  By default `TVc1.type='linear'`

- TVc1.rng:

  OPTIONAL. Used only when `use.TVc1=TRUE` AND `TVc1.type!='GLratio'`  

  numeric, initial and final values for the cognitive acceleration
  coefficient `[c1.ini, c1.fin]` (in that order) along the iterations  

  By default `TVc1.rng=c(1.28, 1.05)`

- TVc1.exp:

  OPTIONAL. Used only when `use.TVc1= TRUE` AND
  `TVc1.type= 'non-linear'`  

  numeric, non-linear modulation index  

  When `TVc1.exp` is equal to 1, `TVc1` corresponds to the improvement
  proposed by Ratnaweera et al., (2004), whereas when `TVc1.exp` is
  different from one, no reference has been found in literature by the
  authors, but it was included as an option based on the work of
  Chatterjee and Siarry (2006) for the inertia weight  

  When `TVc1.type='linear'`, `TVc1.exp` is automatically set to 1. By
  default `TVc1.exp=1`

- use.TVc2:

  logical, indicates whether the social acceleration coefficient `c2`
  will have a time-varying value or a constant one provided by the user
  (see Ratnaweera et al. 2004). By default `use.TVc2=FALSE`

- TVc2.type:

  character, required only when `use.TVc2=TRUE`. Valid values are:  

  -)linear: `c2` varies linearly between the initial and final values
  specified in `TVc2.rng` (see Ratnaweera et al. 2004)  

  -)non-linear: `c2` varies non-linearly between the initial and final
  values specified in `TVc2.rng`. Proposed by the authors of hydroPSO
  taking into account the work of Chatterjee and Siarry (2006) for the
  inertia weight  

  By default `TVc2.type='linear'`

- TVc2.rng:

  OPTIONAL. Used only when `use.TVc2=TRUE`  

  numeric, initial and final values for the social acceleration
  coefficient `[c2.ini, c2.fin]` (in that order) along the iterations  

  By default `TVc2.rng=c(1.05, 1.28)`

- TVc2.exp:

  OPTIONAL. Used only when `use.TVc2= TRUE` AND
  `TVc2.type='non-linear'`  

  numeric, non-linear modulation index  

  When `TVc2.exp` is equal to 1, `TVc2` corresponds to the improvement
  proposed by Ratnaweera et al., 2004, whereas when `TVc2.exp` is
  different from one, no reference has been found in literature by the
  authors, but it was included as an option based on the work of
  Chatterjee and Siarry (2006) for the inertia weight  

  When `TVc2.type= linear`, `TVc2.exp` is automatically set to 1. By
  default `TVc2.exp=1`

- use.TVlambda:

  logical, indicates whether the percentage to limit the maximum
  velocity `lambda` will have a time-varying value or a constant value
  provided by the user. Proposed by the authors of hydroPSO based on the
  work of Chatterjee and Siarry (2006) for the inertia weight  

  By default `use.TVlambda=FALSE`

- TVlambda.type:

  character, required only when `use.TVlambda=TRUE`. Valid values are:  

  -)linear: `TVvmax` varies linearly between the initial and final
  values specified in `TVlambda.rng`  

  -)non-linear: `TVvmax` varies non-linearly between the initial and
  final values specified in `TVlambda.rng`  

  By default `TVlambda.type='linear'`

- TVlambda.rng:

  OPTIONAL. Used only when `use.TVlambda=TRUE`  

  numeric, initial and final values for the percentage to limit the
  maximum velocity `[TVlambda.ini, TVlambda.fin]` (in that order) along
  the iterations  

  By default `TVlambda.rng=c(1, 0.25)`

- TVlambda.exp:

  OPTIONAL. only required when `use.TVlambda= TRUE` AND
  `TVlambda.type='non-linear'`  

  numeric, non-linear modulation index  

  When `TVlambda.type='linear'`, `TVlambda.exp` is automatically set
  to 1. By default `TVlambda.exp=1`

- use.RG:

  logical, indicates if the swarm should be regrouped when premature
  convergence is detected. By default `use.RG=FALSE`  

  When `use.RG=TRUE` the swarm is regrouped in a search space centred
  around the current global best. This updated search space is hoped to
  be both small enough for efficient search and large enough to allow
  the swarm to escape from stagnation (see Evers and Ghalia, 2009)  

  There are 4 differences wrt Evers and Ghalia (2009):  

  -) swarm radius: median is used instead of max  

  -) computation of the new range of parameter space, which corresponds
  to the boundaries of the whole swarm at a given iteration, instead of
  the maximum values of ‘abs(x-Gbest)’  

  -) regrouping factor: `RG.r` instead of ‘6/(5\*ro)’  

  -) velocity is re-initialized using `Vini.type` instead of using the
  formula proposed by Evers and Ghalia (2009)

- RG.thr:

  ONLY required when `use.RG=TRUE`  

  numeric, positive number representing the stagnation threshold used to
  decide whether the swarm has to be regrouped or not. See Evers and
  Galia (2009) for further details  

  Regrouping occurs when the *normalised swarm radius* is less than
  `RG.thr`. By default `RG.thr=1E-5`

- RG.r:

  ONLY required when `use.RG=TRUE`.  

  numeric, positive number representing the regrouping factor, which is
  used to regroup the swarm in a search space centred around the current
  global best (see Evers and Galia, 2009 for further details). By
  default `RG.thr=2`

- RG.miniter:

  ONLY required when `use.RG=TRUE`  

  numeric, minimum number of iterations needed before each new
  regrouping. By default `RG.miniter=100`

- plot:

  logical, indicates if a two-dimensional plot with the particles'
  position will be drawn after each iteration. For high dimensional
  functions, only the first two dimensions of all the particles are
  plotted  

  By default `plot=FALSE`

- out.with.pbest:

  logical, indicates if the best parameter values for each particle and
  their goodness-of-fit will be included in the output of the
  algorithm  

  By default `out.with.pbest=FALSE`

- out.with.fit.iter:

  logical, indicates if the goodness-of-fit of each particle for each
  iteration will be included in the output of the algorithm  

  By default `out.with.fit.iter=FALSE`

- write2disk:

  logical, indicates if the output files will be written to the disk. By
  default `write2disk=TRUE`

- verbose:

  logical, indicates if progress messages are to be printed. By default
  `verbose=TRUE`

- REPORT:

  OPTIONAL. Used only when `verbose=TRUE`  

  The frequency of report messages printed to the screen. Default to
  every 100 iterations

- parallel:

  character, indicates how to parallelise ‘hydroPSO’ (to be precise,
  only the evaluation of the objective function `fn` is parallelised).
  Valid values are:  

  -)none: no parallelisation is made (this is the default value)  

  -)multicore: DEPRECATED!, since `multicore` package is not in CRAN
  anymore. Originally it was thought to carry out parallel computations
  for machines with multiple cores or CPUs. The evaluation of the
  objective function `fn` is done with the
  [`mclapply`](https://rdrr.io/r/parallel/mclapply.html) function of the
  parallel package. It requires POSIX-compliant OS (essentially anything
  but Windows)  

  -)parallel: parallel computations for network clusters or machines
  with multiple cores or CPUs. A ‘FORK’ cluster is created with the
  [`makeForkCluster`](https://rdrr.io/r/parallel/makeCluster.html)
  function.

  When `fn.name="hydromod"` the evaluation of the objective function
  `fn` is done with the
  [`clusterApply`](https://rdrr.io/r/parallel/clusterApply.html)
  function of the parallel package.

  When `fn.name!="hydromod"` the evaluation of the objective function
  `fn` is done with the
  [`parRapply`](https://rdrr.io/r/parallel/clusterApply.html) function
  of the parallel package.  

  -)parallelWin: parallel computations for network clusters or machines
  with multiple cores or CPUs (this is the only parallel implementation
  that works on Windows machines). A ‘PSOCK’ cluster is created with the
  [`makeCluster`](https://rdrr.io/r/parallel/makeCluster.html) function.

  When `fn.name="hydromod"` the evaluation of the objective function
  `fn` is done with the
  [`clusterApply`](https://rdrr.io/r/parallel/clusterApply.html)
  function of the parallel package.

  When `fn.name!="hydromod"` the evaluation of the objective function
  `fn` is done with the
  [`parRapply`](https://rdrr.io/r/parallel/clusterApply.html) function
  of the parallel package.

- par.nnodes:

  OPTIONAL. Used only when `parallel!='none'`  

  numeric, indicates the number of cores/CPUs to be used in the local
  multi-core machine, or the number of nodes to be used in the network
  cluster.  

  By default `par.nnodes` is set to the amount of cores detected by the
  function `detectCores()` (parallel package)

- par.pkgs:

  OPTIONAL. Used only when `parallel='parallelWin'`  

  list of package names (as characters) that need to be loaded on each
  node for allowing the objective function `fn` to be evaluated

- par.env:

  OPTIONAL. Used only when `parallel!='none'`  

  environment from which the objects required by the objective function
  `fn` are exported to the parallel workers. By default, the environment
  from which `hydroPSO` is called is used

- par.export:

  OPTIONAL. Used only when `parallel!='none'`  

  character vector with the names of additional objects to be exported
  from `par.env` to the parallel workers. By default, all functions in
  `par.env` are exported

## Value

A list, compatible with the output from
[`optim`](https://rdrr.io/r/stats/optim.html), with components:

- par:

  optimum parameter set found

- value:

  value of `fn` corresponding to `par`

- counts:

  three-element vector containing the total number of function calls,
  number of iterations, and number of regroupings

- convergence:

  integer code where `0` indicates that the algorithm terminated by
  reaching the absolute tolerance, otherwise:

  1

  :   relative tolerance reached

  2

  :   maximum number of (effective) function evaluations reached

  3

  :   maximum number of iterations reached

- message:

  character string giving human-friendly information about `convergence`

## References

Abdelaziz, Ramadan, and Zambrano-Bigiarini, Mauricio (2014). Particle
Swarm Optimization for inverse modeling of solute transport in fractured
gneiss aquifer. Journal of Contaminant Hydrology, 164, 285-298.
doi:10.1016/j.jconhyd.2014.06.003

Clerc, M. Standard Particle Swarm. 2012. (SPSO-2007, SPSO-2011).
<https://mat.uab.cat/~Alseda/MasterOpt/SPSO_descriptions.pdf>. Last
visited \[10-Jul-2024\]

Clerc, M. From Theory to Practice in Particle Swarm Optimization,
Handbook of Swarm Intelligence, Springer Berlin Heidelberg, 3-36, Eds:
Panigrahi, Bijaya Ketan, Shi, Yuhui, Lim, Meng-Hiot, Hiot, Lim Meng, and
Ong, Yew Soon, 2010, doi: 10.1007/978-3-642-17390-5_1

Clerc, M., Stagnation Analysis in Particle Swarm Optimisation or what
happens when nothing happens. Technical Report. 2006.
<https://hal.science/hal-00122031>

Clerc, M. Particle Swarm Optimization. ISTE, 2005

Clerc, M and J Kennedy. The particle swarm - explosion, stability, and
convergence in a multidimensional complex space. IEEE Transactions On
Evolutionary Computation, 6:58-73, 2002. doi:10.1109/4235.985692

Chatterjee, A. and Siarry, P. Nonlinear inertia weight variation for
dynamic adaptation in particle swarm optimization, Computers and
Operations Research, Volume 33, Issue 3, March 2006, Pages 859-871, ISSN
0305-0548, DOI: 10.1016/j.cor.2004.08.012

Eberhart, R.C.; Shi, Y.; Comparing inertia weights and constriction
factors in particle swarm optimization. Evolutionary Computation, 2000.
Proceedings of the 2000 Congress on , vol.1, no., pp.84-88 vol.1, 2000.
doi: 10.1109/CEC.2000.870279

Evers, G.I.; Ben Ghalia, M. Regrouping particle swarm optimization: A
new global optimization algorithm with improved performance consistency
across benchmarks. Systems, Man and Cybernetics, 2009. SMC 2009. IEEE
International Conference on , vol., no., pp.3901-3908, 11-14 Oct. 2009.
doi: 10.1109/ICSMC.2009.5346625

Huang, T.; Mohan, A.S.; , A hybrid boundary condition for robust
particle swarm optimization. Antennas and Wireless Propagation Letters,
IEEE , vol.4, no., pp. 112-117, 2005. doi: 10.1109/LAWP.2005.846166

Kennedy, J. and R. Eberhart. Particle Swarm Optimization. in proceedings
IEEE international conference on Neural networks. pages 1942-1948. 1995.
doi: 10.1109/ICNN.1995.488968

Kennedy, J.; Small worlds and mega-minds: effects of neighborhood
topology on particle swarm performance. Evolutionary Computation, 1999.
CEC 99. Proceedings of the 1999 Congress on , vol.3, no., pp.3 vol.
(xxxvii+2348), 1999. doi: 10.1109/CEC.1999.785509

Kennedy, J.; Mendes, R.. Population structure and particle swarm
performance. Evolutionary Computation, 2002. CEC '02. Proceedings of the
2002 Congress on , vol.2, no., pp.1671-1676, 2002. doi:
10.1109/CEC.2002.1004493

Kennedy, J.; Mendes, R.; , Neighborhood topologies in fully-informed and
best-of-neighborhood particle swarms. Soft Computing in Industrial
Applications, 2003. SMCia/03. Proceedings of the 2003 IEEE International
Workshop on , vol., no., pp. 45- 50, 23-25 June 2003. doi:
10.1109/SMCIA.2003.1231342

Kennedy, J. 2006. Swarm intelligence, in Handbook of Nature-Inspired and
Innovative Computing, edited by A. Zomaya, pp. 187-219, Springer US,
doi:10.1007/0-387-27705-6_6

Liu, B. and L. Wang, Y.-H. Jin, F. Tang, and D.-X. Huang. Improved
particle swarm optimization combined with chaos. Chaos, Solitons and
Fractals, vol. 25, no. 5, pp.1261-1271, Sep. 2005.
doi:10.1016/j.chaos.2004.11.095

Mendes, R.; Kennedy, J.; Neves, J. The fully informed particle swarm:
simpler, maybe better. Evolutionary Computation, IEEE Transactions on ,
vol.8, no.3, pp. 204-210, June 2004. doi: 10.1109/TEVC.2004.826074

Ratnaweera, A.; Halgamuge, S.K.; Watson, H.C. Self-organizing
hierarchical particle swarm optimizer with time-varying acceleration
coefficients. Evolutionary Computation, IEEE Transactions on , vol.8,
no.3, pp. 240- 255, June 2004. doi: 10.1109/TEVC.2004.826071

Robinson, J.; Rahmat-Samii, Y.; Particle swarm optimization in
electromagnetics. Antennas and Propagation, IEEE Transactions on ,
vol.52, no.2, pp. 397-407, Feb. 2004. doi: 10.1109/TAP.2004.823969

Shi, Y.; Eberhart, R. A modified particle swarm optimizer. Evolutionary
Computation Proceedings, 1998. IEEE World Congress on Computational
Intelligence. The 1998 IEEE International Conference on , vol., no.,
pp.69-73, 4-9 May 1998. doi: 10.1109/ICEC.1998.699146

Schor, D.; Kinsner, W.; Anderson, J.; A study of optimal topologies in
swarm intelligence. Electrical and Computer Engineering (CCECE), 2010
23rd Canadian Conference on , vol., no., pp.1-8, 2-5 May 2010. doi:
10.1109/CCECE.2010.5575132

Yong-Ling Zheng; Long-Hua Ma; Li-Yan Zhang; Ji-Xin Qian. On the
convergence analysis and parameter selection in particle swarm
optimization. Machine Learning and Cybernetics, 2003 International
Conference on , vol.3, no., pp. 1802-1807 Vol.3, 2-5 Nov. 2003. doi:
10.1109/ICMLC.2003.1259789

Zambrano-Bigiarini, M.; R. Rojas (2013), A model-independent Particle
Swarm Optimisation software for model calibration, Environmental
Modelling & Software, 43, 5-25, doi:10.1016/j.envsoft.2013.01.004

Zambrano-Bigiarini, M., M. Clerc, R. Rojas (2013), Standard Particle
Swarm Optimisation 2011 at CEC-2013: A baseline for future PSO
improvements, In Proceedings of 2013 IEEE Congress on Evolutionary
Computation (CEC'2013). doi:10.1109/CEC.2013.6557848

Zhao, B. An Improved Particle Swarm Optimization Algorithm for Global
Numerical Optimization. In Proceedings of International Conference on
Computational Science (1). 2006, 657-664

Lynn, N., Ali, M. Z., & Suganthan, P. N. (2018). Population topologies
for particle swarm optimization and differential evolution. Swarm and
evolutionary computation, 39, 24-35. doi: 10.1016/j.swevo.2017.11.002

## Author

Mauricio Zambrano-Bigiarini, <mzb.devel@gmail.com>

## Note

Note for [`optim`](https://rdrr.io/r/stats/optim.html) users:  

1\) In hydroPSO the length of `lower` and `upper` are used to define the
dimension of the solution space (not the length of `par`)  

2\) In hydroPSO, `par` may be omitted. If not omitted, the `m` parameter
sets provided by the user for `par` are used to overwrite the first `m`
parameter sets randomly defined according to the value of `Xini.type`  

## See also

[`optim`](https://rdrr.io/r/stats/optim.html)

## Examples

``` r
# Number of dimensions of the optimisation problem (for all the examples)
D <- 5

# Boundaries of the search space (Rastrigin function)
lower <- rep(-5.12, D)
upper <- rep(5.12, D)

# \donttest{
local({

################################ 
# Example 1. Basic use         #
################################ 

# Setting the seed (for reproducible results)         
set.seed(100)

# Basic use 1. Rastrigin function (non-linear and multi-modal with many local minima)
# Results are not saved to the hard disk, for faster execution ('write2disk=FALSE')
hydroPSO(fn=rastrigin, lower=lower, upper=upper, control=list(write2disk=FALSE) )

}) # local END
#>                                                                                 
#> ================================================================================
#> [                                Initialising  ...                             ]
#> ================================================================================
#>                                                                                 
#> [npart=40 ; maxit=1000 ; method=spso2011 ; topology=random ; boundary.wall=absorbing2011]
#>          
#> [ user-definitions in control: write2disk=FALSE ]
#>          
#>                                                                                 
#> ================================================================================
#> [                                 Running  PSO ...                             ]
#> ================================================================================
#>                                                                                 
#> iter: 100  Gbest: 2.992E+00  Gbest_rate:  0.08%  Iter_best_fit: 2.992E+00  nSwarm_Radius: 2.96E-02  |g-mean(p)|/mean(p): 69.36%
#>                                     |                                           
#> ================================================================================
#> [                          Creating the R output ...                           ]
#> ================================================================================
#> $par
#>       Param1       Param2       Param3       Param4       Param5 
#> 4.382399e-05 9.947626e-01 9.950853e-01 9.948407e-01 5.619973e-05 
#> 
#> $value
#> [1] 2.984892
#> 
#> $best.particle
#> [1] 5
#> 
#> $counts
#> function.calls     iterations    regroupings 
#>           5920            148              0 
#> 
#> $convergence
#> [1] 1
#> 
#> $message
#> [1] "Converged ('reltol' criterion)"
#> 
# }  # donttest END

# \donttest{
local({

# Setting the user temporal directory as working directory
oldwd <- getwd()      
on.exit(setwd(oldwd), add = TRUE) 
setwd(tempdir())

# Basic use 2. Rastrigin function (non-linear and multimodal with many local minima)
# Results are saved to the hard disk. Slower than before but results are kept for
# future inspection
hydroPSO(fn=rastrigin, lower=lower, upper=upper )

# Plotting the results, by default into the active graphic device
# 'MinMax="min"' indicates a minimisation problem
plot_results(MinMax="min") 

# Plotting the results into PNG files. 
plot_results(MinMax="min", do.png=TRUE)   
 
}) # local END    
#>                                                                                 
#> ================================================================================
#> [                                Initialising  ...                             ]
#> ================================================================================
#>                                                                                 
#> [npart=40 ; maxit=1000 ; method=spso2011 ; topology=random ; boundary.wall=absorbing2011]
#>                                                                                 
#> ================================================================================
#> [ Writing the 'PSO_logfile.txt' file ...                                       ]
#> ================================================================================
#>                                                                                 
#> ================================================================================
#> [                                 Running  PSO ...                             ]
#> ================================================================================
#>                                                                                 
#> iter: 100  Gbest: 3.980E+00  Gbest_rate:  0.00%  Iter_best_fit: 3.980E+00  nSwarm_Radius: 9.77E-04  |g-mean(p)|/mean(p): 59.45%
#>                            
#> [ Writing output files... ]
#>                            
#>                                     |                                           
#> ================================================================================
#> [                          Creating the R output ...                           ]
#> ================================================================================
#> [                                               ]
#> [         Reading output files ...              ]
#> [                                               ]
#>                                                      
#> [ Reading the file 'Particles.txt' ... ]
#> [ Total number of parameter sets: 6120 ]
#>                                                      
#> [ Reading the file 'Velocities.txt' ... ]
#> [ Total number of parameter sets: 6120 ]
#>                                                      
#> [ Reading the file 'Model_out.txt' ... ]
#> [ Total number of parameter sets: 6120 ]
#> [ Number of model outputs for each parameter set ('nsim'): 1 ]
#>                                                      
#> [ Reading the file 'ConvergenceMeasures.txt' ... ]
#> [ Total number of iterations: 153 ]
#>                                                      
#> [ Reading the file 'Particles_GofPerIter.txt' ... ]
#> [ Number of particles : 40 ]
#> [ Number of iterations: 153 ]
#> [                                               ]
#> [                  Plotting ...                 ]
#> [                                               ]
#> [ Plotting convergence measures' ... ]
#>                                                      
#> [ Plotting dotty plots for parameter values' ... ]
#> [ Plotting histograms for parameter values' ... ]
#> [ Plotting boxplots for parameter values' ... ]
#> [ Plotting empirical CDFs for parameter values' ... ]
#> [ Plotting parameter values vs Number of Model Evaluations' ... ]
#> [ Plotting 3D dotty plots for parameter values' ... ]
#> [ Plotting GoF for each particle vs Number of Model Evaluations' ... ]
#> [ Plotting velocity values vs Number of Model Evaluations' ...]
#> [ Plotting ECDFs of simulated quantiles vs observations' ... ]
#> [ Computing the ECDF for 'sim' , 1/1 => 100.00% ]
#> [                                               ]
#> [             Plots are finished !!             ]
#> [                                               ]
#> [                                               ]
#> [         Reading output files ...              ]
#> [                                               ]
#>                                                      
#> [ Reading the file 'Particles.txt' ... ]
#> [ Total number of parameter sets: 6120 ]
#>                                                      
#> [ Reading the file 'Velocities.txt' ... ]
#> [ Total number of parameter sets: 6120 ]
#>                                                      
#> [ Reading the file 'Model_out.txt' ... ]
#> [ Total number of parameter sets: 6120 ]
#> [ Number of model outputs for each parameter set ('nsim'): 1 ]
#>                                                      
#> [ Reading the file 'ConvergenceMeasures.txt' ... ]
#> [ Total number of iterations: 153 ]
#>                                                      
#> [ Reading the file 'Particles_GofPerIter.txt' ... ]
#> [ Number of particles : 40 ]
#> [ Number of iterations: 153 ]
#> [                                               ]
#> [                  Plotting ...                 ]
#> [                                               ]
#> [ Plotting convergence measures into 'ConvergenceMeasures.png' ... ]
#>                                                      
#> [ Plotting dotty plots for parameter values into 'Params_DottyPlots.png' ... ]
#> [ Plotting histograms for parameter values into 'Params_Histograms.png' ... ]
#> [ Plotting boxplots for parameter values into 'Params_Boxplots.png' ... ]
#> [ Plotting empirical CDFs for parameter values into 'Params_ECDFs.png' ... ]
#> [ Plotting parameter values vs Number of Model Evaluations into 'Params_ValuesPerRun.png' ... ]
#> [ Plotting 3D dotty plots for parameter values into 'Params_dp3d.png' ... ]
#> [ Plotting GoF for each particle vs Number of Model Evaluations into 'Particles_GofPerIter.png' ... ]
#> [ Plotting velocity values vs Number of Model Evaluations into 'Velocities_ValuePerRun.png' ...]
#> [ Plotting ECDFs of simulated quantiles vs observations into 'ModelOut_Quantiles.png' ... ]
#> [ Computing the ECDF for 'sim' , 1/1 => 100.00% ]
#> [                                               ]
#> [             Plots are finished !!             ]
#> [                                               ]
# } # donttest END


# \donttest{
local({

################################ 
# Example 2. More advanced use #
################################ 

# Defining the relative tolerance ('reltol'), the frequency of report messages 
# printed to the screen ('REPORT'), and no output files ('write2disk')
set.seed(100)
hydroPSO( fn=rastrigin, lower=lower, upper=upper,        
          control=list(reltol=1e-20, REPORT=10, write2disk=FALSE) )
        
        
################################### 
# Example 3. von Neumman Topology #
###################################

# Same as Example 2, but using a von Neumann topology ('topology="vonNeumann"')
set.seed(100)
hydroPSO(fn=rastrigin,lower=lower,upper=upper,
         control=list(topology="vonNeumann", reltol=1E-20, 
                      REPORT=50, write2disk=FALSE) ) 



################################ 
# Example 4. Regrouping        #
################################ 

# Same as Example 3 ('topology="vonNeumann"') but using regrouping ('use.RG')
set.seed(100)
hydroPSO(fn=rastrigin,lower=lower,upper=upper,
         control=list(topology="vonNeumann", reltol=1E-20, 
                      REPORT=50, write2disk=FALSE,
                      use.RG=TRUE,RG.thr=7e-2,RG.r=3,RG.miniter=50) )


################################ 
# Example 5. FIPS              #
################################ 

# Same as Example 3 ('topology="vonNeumann"') but using a fully informed 
# particle swarm (FIPS) variant ('method') with global best topology
set.seed(100)
hydroPSO(fn=rastrigin,lower=lower,upper=upper, method="fips",
         control=list(topology="gbest",reltol=1E-9,write2disk=FALSE) )


################################ 
# Example 6. normalisation     #
################################ 

# Same as Example 3 but parameter values are normalised to the [0,1] interval 
# during the optimisation. This option is recommended when the search space is 
# not an hypercube (not useful is this particular example)
set.seed(100)
hydroPSO(fn=rastrigin,lower=lower,upper=upper,
         control=list(topology="vonNeumann", reltol=1E-20, normalise=TRUE,
                      REPORT=50, write2disk=FALSE) ) 


################################ 
# Example 7. Asynchronus update#
################################ 

# Same as Example 3, but using asynchronus update of previus and local best 
# ('best.update'). Same global optimum but much slower....
set.seed(100)
hydroPSO(fn=rastrigin,lower=lower,upper=upper,
         control=list(topology="vonNeumann", reltol=1E-20, 
                      REPORT=50, write2disk=FALSE, best.update="async") ) 

}) # local END
#>                                                                                 
#> ================================================================================
#> [                                Initialising  ...                             ]
#> ================================================================================
#>                                                                                 
#> [npart=40 ; maxit=1000 ; method=spso2011 ; topology=random ; boundary.wall=absorbing2011]
#>          
#> [ user-definitions in control: reltol=1e-20 ; REPORT=10 ; write2disk=FALSE ]
#>          
#>                                                                                 
#> ================================================================================
#> [                                 Running  PSO ...                             ]
#> ================================================================================
#>                                                                                 
#> iter:  10  Gbest: 1.598E+01  Gbest_rate:  0.00%  Iter_best_fit: 3.071E+01  nSwarm_Radius: 1.94E-01  |g-mean(p)|/mean(p): 65.23%
#> iter:  20  Gbest: 1.268E+01  Gbest_rate:  0.00%  Iter_best_fit: 1.268E+01  nSwarm_Radius: 9.37E-02  |g-mean(p)|/mean(p): 61.74%
#> iter:  30  Gbest: 1.023E+01  Gbest_rate:  0.26%  Iter_best_fit: 1.023E+01  nSwarm_Radius: 5.99E-02  |g-mean(p)|/mean(p): 60.45%
#> iter:  40  Gbest: 5.684E+00  Gbest_rate:  0.00%  Iter_best_fit: 6.582E+00  nSwarm_Radius: 8.75E-02  |g-mean(p)|/mean(p): 70.25%
#> iter:  50  Gbest: 3.965E+00  Gbest_rate:  0.00%  Iter_best_fit: 5.651E+00  nSwarm_Radius: 7.67E-02  |g-mean(p)|/mean(p): 73.73%
#> iter:  60  Gbest: 3.654E+00  Gbest_rate:  0.00%  Iter_best_fit: 4.326E+00  nSwarm_Radius: 3.88E-02  |g-mean(p)|/mean(p): 72.32%
#> iter:  70  Gbest: 3.255E+00  Gbest_rate:  0.31%  Iter_best_fit: 3.255E+00  nSwarm_Radius: 3.43E-02  |g-mean(p)|/mean(p): 73.90%
#> iter:  80  Gbest: 3.185E+00  Gbest_rate:  0.01%  Iter_best_fit: 3.185E+00  nSwarm_Radius: 3.38E-02  |g-mean(p)|/mean(p): 72.15%
#> iter:  90  Gbest: 3.046E+00  Gbest_rate:  0.00%  Iter_best_fit: 3.052E+00  nSwarm_Radius: 3.19E-02  |g-mean(p)|/mean(p): 71.30%
#> iter: 100  Gbest: 2.992E+00  Gbest_rate:  0.08%  Iter_best_fit: 2.992E+00  nSwarm_Radius: 2.96E-02  |g-mean(p)|/mean(p): 69.36%
#> iter: 110  Gbest: 2.986E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.986E+00  nSwarm_Radius: 2.85E-02  |g-mean(p)|/mean(p): 67.85%
#> iter: 120  Gbest: 2.986E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.986E+00  nSwarm_Radius: 2.41E-02  |g-mean(p)|/mean(p): 65.21%
#> iter: 130  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 2.21E-02  |g-mean(p)|/mean(p): 64.82%
#> iter: 140  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 2.39E-02  |g-mean(p)|/mean(p): 64.79%
#> iter: 150  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 2.25E-02  |g-mean(p)|/mean(p): 64.24%
#> iter: 160  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 1.17E-02  |g-mean(p)|/mean(p): 61.53%
#> iter: 170  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 1.13E-02  |g-mean(p)|/mean(p): 61.02%
#> iter: 180  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 7.07E-03  |g-mean(p)|/mean(p): 60.96%
#> iter: 190  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 4.71E-03  |g-mean(p)|/mean(p): 57.38%
#> iter: 200  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 1.10E-03  |g-mean(p)|/mean(p): 56.23%
#> iter: 210  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 1.76E-04  |g-mean(p)|/mean(p): 55.88%
#> iter: 220  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 4.67E-05  |g-mean(p)|/mean(p): 55.88%
#> iter: 230  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 7.07E-06  |g-mean(p)|/mean(p): 55.66%
#> iter: 240  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 9.06E-07  |g-mean(p)|/mean(p): 55.40%
#> iter: 250  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 5.62E-07  |g-mean(p)|/mean(p): 55.39%
#> iter: 260  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 1.35E-07  |g-mean(p)|/mean(p): 54.99%
#> iter: 270  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 2.99E-08  |g-mean(p)|/mean(p): 54.14%
#> iter: 280  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 8.87E-09  |g-mean(p)|/mean(p): 52.66%
#> iter: 290  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 3.02E-09  |g-mean(p)|/mean(p): 52.23%
#> iter: 300  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 5.50E-10  |g-mean(p)|/mean(p): 52.21%
#> iter: 310  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 2.98E-10  |g-mean(p)|/mean(p): 52.20%
#> iter: 320  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 2.66E-10  |g-mean(p)|/mean(p): 52.20%
#> iter: 330  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 2.96E-10  |g-mean(p)|/mean(p): 52.20%
#> iter: 340  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 2.45E-10  |g-mean(p)|/mean(p): 52.20%
#> iter: 350  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 2.25E-10  |g-mean(p)|/mean(p): 52.20%
#> iter: 360  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 2.17E-10  |g-mean(p)|/mean(p): 52.20%
#> iter: 370  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 2.10E-10  |g-mean(p)|/mean(p): 52.20%
#> iter: 380  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 2.10E-10  |g-mean(p)|/mean(p): 52.20%
#> iter: 390  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 2.10E-10  |g-mean(p)|/mean(p): 52.20%
#> iter: 400  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 1.88E-10  |g-mean(p)|/mean(p): 52.20%
#> iter: 410  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 1.76E-10  |g-mean(p)|/mean(p): 52.20%
#> iter: 420  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 1.85E-10  |g-mean(p)|/mean(p): 52.20%
#> iter: 430  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 1.69E-10  |g-mean(p)|/mean(p): 52.20%
#> iter: 440  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 1.83E-10  |g-mean(p)|/mean(p): 52.20%
#> iter: 450  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 1.63E-10  |g-mean(p)|/mean(p): 52.20%
#> iter: 460  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 1.71E-10  |g-mean(p)|/mean(p): 51.80%
#> iter: 470  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 1.57E-10  |g-mean(p)|/mean(p): 51.80%
#> iter: 480  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 1.87E-10  |g-mean(p)|/mean(p): 51.80%
#> iter: 490  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 1.74E-10  |g-mean(p)|/mean(p): 51.80%
#> iter: 500  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 1.80E-10  |g-mean(p)|/mean(p): 51.70%
#> iter: 510  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 1.80E-10  |g-mean(p)|/mean(p): 51.29%
#> iter: 520  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 1.94E-10  |g-mean(p)|/mean(p): 51.28%
#> iter: 530  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 1.71E-10  |g-mean(p)|/mean(p): 49.88%
#> iter: 540  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 1.53E-10  |g-mean(p)|/mean(p): 49.88%
#> iter: 550  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 1.76E-10  |g-mean(p)|/mean(p): 49.88%
#> iter: 560  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 1.61E-10  |g-mean(p)|/mean(p): 49.88%
#> iter: 570  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 1.74E-10  |g-mean(p)|/mean(p): 49.15%
#> iter: 580  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 1.57E-10  |g-mean(p)|/mean(p): 46.52%
#> iter: 590  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 1.55E-10  |g-mean(p)|/mean(p): 46.20%
#> iter: 600  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 1.77E-10  |g-mean(p)|/mean(p): 46.07%
#> iter: 610  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 1.85E-10  |g-mean(p)|/mean(p): 45.46%
#> iter: 620  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 1.49E-10  |g-mean(p)|/mean(p): 45.46%
#> iter: 630  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 1.44E-10  |g-mean(p)|/mean(p): 45.46%
#> iter: 640  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 1.40E-10  |g-mean(p)|/mean(p): 45.46%
#> iter: 650  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 1.35E-10  |g-mean(p)|/mean(p): 45.46%
#> iter: 660  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 1.46E-10  |g-mean(p)|/mean(p): 45.46%
#> iter: 670  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 1.53E-10  |g-mean(p)|/mean(p): 45.46%
#> iter: 680  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 1.37E-10  |g-mean(p)|/mean(p): 45.46%
#> iter: 690  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 1.50E-10  |g-mean(p)|/mean(p): 45.46%
#> iter: 700  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 1.29E-10  |g-mean(p)|/mean(p): 45.46%
#> iter: 710  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 1.39E-10  |g-mean(p)|/mean(p): 44.90%
#> iter: 720  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 1.20E-10  |g-mean(p)|/mean(p): 44.65%
#> iter: 730  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 1.17E-10  |g-mean(p)|/mean(p): 44.65%
#> iter: 740  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 1.18E-10  |g-mean(p)|/mean(p): 44.38%
#> iter: 750  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 1.02E-10  |g-mean(p)|/mean(p): 44.29%
#> iter: 760  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 1.18E-10  |g-mean(p)|/mean(p): 44.29%
#> iter: 770  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 1.33E-10  |g-mean(p)|/mean(p): 44.29%
#> iter: 780  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 1.29E-10  |g-mean(p)|/mean(p): 44.29%
#> iter: 790  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 1.06E-10  |g-mean(p)|/mean(p): 44.26%
#> iter: 800  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 1.29E-10  |g-mean(p)|/mean(p): 44.26%
#> iter: 810  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 1.38E-10  |g-mean(p)|/mean(p): 44.07%
#> iter: 820  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 1.29E-10  |g-mean(p)|/mean(p): 43.82%
#> iter: 830  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 1.29E-10  |g-mean(p)|/mean(p): 43.67%
#> iter: 840  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 1.05E-10  |g-mean(p)|/mean(p): 42.03%
#> iter: 850  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 1.23E-10  |g-mean(p)|/mean(p): 41.96%
#> iter: 860  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 1.34E-10  |g-mean(p)|/mean(p): 41.95%
#> iter: 870  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 1.07E-10  |g-mean(p)|/mean(p): 41.95%
#> iter: 880  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 1.01E-10  |g-mean(p)|/mean(p): 41.95%
#> iter: 890  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 1.21E-10  |g-mean(p)|/mean(p): 41.95%
#> iter: 900  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 1.15E-10  |g-mean(p)|/mean(p): 41.95%
#> iter: 910  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 1.13E-10  |g-mean(p)|/mean(p): 41.95%
#> iter: 920  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 1.03E-10  |g-mean(p)|/mean(p): 41.95%
#> iter: 930  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 1.28E-10  |g-mean(p)|/mean(p): 41.95%
#> iter: 940  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 1.24E-10  |g-mean(p)|/mean(p): 41.95%
#> iter: 950  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 1.17E-10  |g-mean(p)|/mean(p): 38.59%
#> iter: 960  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 9.23E-11  |g-mean(p)|/mean(p): 38.46%
#> iter: 970  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 1.35E-10  |g-mean(p)|/mean(p): 38.43%
#> iter: 980  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 1.29E-10  |g-mean(p)|/mean(p): 34.78%
#> iter: 990  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 9.68E-11  |g-mean(p)|/mean(p): 33.46%
#> iter:1000  Gbest: 2.985E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.985E+00  nSwarm_Radius: 1.08E-10  |g-mean(p)|/mean(p): 33.46%
#>                                     |                                           
#> ================================================================================
#> [                          Creating the R output ...                           ]
#> ================================================================================
#>                                                                                 
#> ================================================================================
#> [                                Initialising  ...                             ]
#> ================================================================================
#>                                                                                 
#> [npart=40 ; maxit=1000 ; method=spso2011 ; topology=vonNeumann ; boundary.wall=absorbing2011]
#>          
#> [ user-definitions in control: topology=vonNeumann ; reltol=1e-20 ; REPORT=50 ; write2disk=FALSE ]
#>          
#>                                                                                 
#> ================================================================================
#> [                                 Running  PSO ...                             ]
#> ================================================================================
#>                                                                                 
#> iter:  50  Gbest: 3.010E+00  Gbest_rate:  0.00%  Iter_best_fit: 4.223E+00  nSwarm_Radius: 8.60E-02  |g-mean(p)|/mean(p): 80.20%
#> iter: 100  Gbest: 2.039E+00  Gbest_rate:  0.22%  Iter_best_fit: 2.039E+00  nSwarm_Radius: 8.64E-02  |g-mean(p)|/mean(p): 80.13%
#> iter: 150  Gbest: 1.990E+00  Gbest_rate:  0.00%  Iter_best_fit: 1.990E+00  nSwarm_Radius: 8.57E-02  |g-mean(p)|/mean(p): 76.31%
#> iter: 200  Gbest: 1.990E+00  Gbest_rate:  0.00%  Iter_best_fit: 1.990E+00  nSwarm_Radius: 9.05E-02  |g-mean(p)|/mean(p): 70.88%
#> iter: 250  Gbest: 1.990E+00  Gbest_rate:  0.00%  Iter_best_fit: 1.990E+00  nSwarm_Radius: 7.92E-02  |g-mean(p)|/mean(p): 69.64%
#> iter: 300  Gbest: 1.990E+00  Gbest_rate:  0.00%  Iter_best_fit: 1.990E+00  nSwarm_Radius: 7.20E-02  |g-mean(p)|/mean(p): 68.07%
#> iter: 350  Gbest: 1.990E+00  Gbest_rate:  0.00%  Iter_best_fit: 1.990E+00  nSwarm_Radius: 6.97E-02  |g-mean(p)|/mean(p): 66.11%
#> iter: 400  Gbest: 1.990E+00  Gbest_rate:  0.00%  Iter_best_fit: 1.990E+00  nSwarm_Radius: 7.25E-02  |g-mean(p)|/mean(p): 65.36%
#> iter: 450  Gbest: 1.990E+00  Gbest_rate:  0.00%  Iter_best_fit: 1.990E+00  nSwarm_Radius: 6.95E-02  |g-mean(p)|/mean(p): 65.06%
#> iter: 500  Gbest: 1.990E+00  Gbest_rate:  0.00%  Iter_best_fit: 1.990E+00  nSwarm_Radius: 6.41E-02  |g-mean(p)|/mean(p): 62.90%
#> iter: 550  Gbest: 1.990E+00  Gbest_rate:  0.00%  Iter_best_fit: 1.990E+00  nSwarm_Radius: 7.09E-02  |g-mean(p)|/mean(p): 61.12%
#> iter: 600  Gbest: 1.990E+00  Gbest_rate:  0.00%  Iter_best_fit: 1.990E+00  nSwarm_Radius: 6.68E-02  |g-mean(p)|/mean(p): 60.41%
#> iter: 650  Gbest: 1.990E+00  Gbest_rate:  0.00%  Iter_best_fit: 1.990E+00  nSwarm_Radius: 7.31E-02  |g-mean(p)|/mean(p): 60.21%
#> iter: 700  Gbest: 1.990E+00  Gbest_rate:  0.00%  Iter_best_fit: 1.990E+00  nSwarm_Radius: 7.03E-02  |g-mean(p)|/mean(p): 60.21%
#> iter: 750  Gbest: 1.990E+00  Gbest_rate:  0.00%  Iter_best_fit: 1.990E+00  nSwarm_Radius: 6.45E-02  |g-mean(p)|/mean(p): 59.14%
#> iter: 800  Gbest: 1.990E+00  Gbest_rate:  0.00%  Iter_best_fit: 1.990E+00  nSwarm_Radius: 8.17E-02  |g-mean(p)|/mean(p): 59.04%
#> iter: 850  Gbest: 1.990E+00  Gbest_rate:  0.00%  Iter_best_fit: 1.990E+00  nSwarm_Radius: 7.28E-02  |g-mean(p)|/mean(p): 59.04%
#> iter: 900  Gbest: 1.990E+00  Gbest_rate:  0.00%  Iter_best_fit: 1.990E+00  nSwarm_Radius: 6.97E-02  |g-mean(p)|/mean(p): 58.06%
#> iter: 950  Gbest: 1.990E+00  Gbest_rate:  0.00%  Iter_best_fit: 1.990E+00  nSwarm_Radius: 7.15E-02  |g-mean(p)|/mean(p): 57.86%
#> iter:1000  Gbest: 1.990E+00  Gbest_rate:  0.00%  Iter_best_fit: 1.990E+00  nSwarm_Radius: 6.30E-02  |g-mean(p)|/mean(p): 56.64%
#>                                     |                                           
#> ================================================================================
#> [                          Creating the R output ...                           ]
#> ================================================================================
#>                                                                                 
#> ================================================================================
#> [                                Initialising  ...                             ]
#> ================================================================================
#>                                                                                 
#> [npart=40 ; maxit=1000 ; method=spso2011 ; topology=vonNeumann ; boundary.wall=absorbing2011]
#>          
#> [ user-definitions in control: topology=vonNeumann ; reltol=1e-20 ; REPORT=50 ; write2disk=FALSE ; use.RG=TRUE ; RG.thr=0.07 ; RG.r=3 ; RG.miniter=50 ]
#>          
#>                                                                                 
#> ================================================================================
#> [                                 Running  PSO ...                             ]
#> ================================================================================
#>                                                                                 
#> iter:  50  Gbest: 3.010E+00  Gbest_rate:  0.00%  Iter_best_fit: 4.223E+00  nSwarm_Radius: 8.60E-02  |g-mean(p)|/mean(p): 80.20%
#> iter: 100  Gbest: 2.039E+00  Gbest_rate:  0.22%  Iter_best_fit: 2.039E+00  nSwarm_Radius: 8.64E-02  |g-mean(p)|/mean(p): 80.13%
#> [ Re-grouping particles in the swarm (iter: 111) ... ]
#> iter: 150  Gbest: 2.014E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.016E+00  nSwarm_Radius: 1.44E-01  |g-mean(p)|/mean(p): 92.01%
#> iter: 200  Gbest: 1.992E+00  Gbest_rate:  0.01%  Iter_best_fit: 1.992E+00  nSwarm_Radius: 1.21E-01  |g-mean(p)|/mean(p): 88.55%
#> iter: 250  Gbest: 1.990E+00  Gbest_rate:  0.00%  Iter_best_fit: 1.990E+00  nSwarm_Radius: 1.08E-01  |g-mean(p)|/mean(p): 87.07%
#> iter: 300  Gbest: 1.990E+00  Gbest_rate:  0.00%  Iter_best_fit: 1.990E+00  nSwarm_Radius: 1.07E-01  |g-mean(p)|/mean(p): 86.04%
#> iter: 350  Gbest: 1.990E+00  Gbest_rate:  0.00%  Iter_best_fit: 1.990E+00  nSwarm_Radius: 7.95E-02  |g-mean(p)|/mean(p): 85.69%
#> [ Re-grouping particles in the swarm (iter: 355) ... ]
#> iter: 400  Gbest: 1.990E+00  Gbest_rate:  0.00%  Iter_best_fit: 2.014E+00  nSwarm_Radius: 1.50E-01  |g-mean(p)|/mean(p): 92.31%
#> iter: 450  Gbest: 1.335E+00  Gbest_rate:  0.00%  Iter_best_fit: 1.335E+00  nSwarm_Radius: 1.21E-01  |g-mean(p)|/mean(p): 92.93%
#> iter: 500  Gbest: 1.184E+00  Gbest_rate:  0.01%  Iter_best_fit: 1.184E+00  nSwarm_Radius: 1.20E-01  |g-mean(p)|/mean(p): 93.24%
#> iter: 550  Gbest: 1.183E+00  Gbest_rate:  0.00%  Iter_best_fit: 1.183E+00  nSwarm_Radius: 1.20E-01  |g-mean(p)|/mean(p): 92.70%
#> iter: 600  Gbest: 1.031E+00  Gbest_rate:  0.04%  Iter_best_fit: 1.031E+00  nSwarm_Radius: 1.19E-01  |g-mean(p)|/mean(p): 93.09%
#> iter: 650  Gbest: 1.015E+00  Gbest_rate:  0.00%  Iter_best_fit: 1.015E+00  nSwarm_Radius: 1.19E-01  |g-mean(p)|/mean(p): 93.11%
#> iter: 700  Gbest: 1.015E+00  Gbest_rate:  0.00%  Iter_best_fit: 1.015E+00  nSwarm_Radius: 1.19E-01  |g-mean(p)|/mean(p): 92.79%
#> iter: 750  Gbest: 1.015E+00  Gbest_rate:  0.00%  Iter_best_fit: 1.015E+00  nSwarm_Radius: 1.10E-01  |g-mean(p)|/mean(p): 91.35%
#> [ Re-grouping particles in the swarm (iter: 786) ... ]
#> iter: 800  Gbest: 1.015E+00  Gbest_rate:  0.00%  Iter_best_fit: 3.360E+01  nSwarm_Radius: 1.93E-01  |g-mean(p)|/mean(p): 97.40%
#> iter: 850  Gbest: 9.966E-01  Gbest_rate:  0.00%  Iter_best_fit: 9.972E-01  nSwarm_Radius: 1.45E-01  |g-mean(p)|/mean(p): 95.59%
#> iter: 900  Gbest: 9.950E-01  Gbest_rate:  0.00%  Iter_best_fit: 9.950E-01  nSwarm_Radius: 1.36E-01  |g-mean(p)|/mean(p): 94.34%
#> iter: 950  Gbest: 9.950E-01  Gbest_rate:  0.00%  Iter_best_fit: 9.950E-01  nSwarm_Radius: 7.48E-02  |g-mean(p)|/mean(p): 93.16%
#> [ Re-grouping particles in the swarm (iter: 959) ... ]
#> iter:1000  Gbest: 9.950E-01  Gbest_rate:  0.00%  Iter_best_fit: 4.858E+00  nSwarm_Radius: 1.36E-01  |g-mean(p)|/mean(p): 96.25%
#>                                     |                                           
#> ================================================================================
#> [                          Creating the R output ...                           ]
#> ================================================================================
#>                                                                                 
#> ================================================================================
#> [                                Initialising  ...                             ]
#> ================================================================================
#>                                                                                 
#> [npart=40 ; maxit=1000 ; method=fips ; topology=gbest ; boundary.wall=absorbing2011]
#>          
#> [ user-definitions in control: topology=gbest ; reltol=1e-09 ; write2disk=FALSE ]
#>          
#>                                                                                 
#> ================================================================================
#> [                                 Running  PSO ...                             ]
#> ================================================================================
#>                                                                                 
#>                                     |                                           
#> ================================================================================
#> [                          Creating the R output ...                           ]
#> ================================================================================
#>                                                                                 
#> ================================================================================
#> [                                Initialising  ...                             ]
#> ================================================================================
#>                                                                                 
#> [npart=40 ; maxit=1000 ; method=spso2011 ; topology=vonNeumann ; boundary.wall=absorbing2011]
#>          
#> [ user-definitions in control: topology=vonNeumann ; reltol=1e-20 ; normalise=TRUE ; REPORT=50 ; write2disk=FALSE ]
#>          
#>                                                                                 
#> ================================================================================
#> [                                 Running  PSO ...                             ]
#> ================================================================================
#>                                                                                 
#> iter:  50  Gbest: 3.010E+00  Gbest_rate:  0.00%  Iter_best_fit: 4.223E+00  nSwarm_Radius: 8.60E-02  |g-mean(p)|/mean(p): 80.20%
#> iter: 100  Gbest: 2.039E+00  Gbest_rate:  0.22%  Iter_best_fit: 2.039E+00  nSwarm_Radius: 8.64E-02  |g-mean(p)|/mean(p): 80.13%
#> iter: 150  Gbest: 1.990E+00  Gbest_rate:  0.00%  Iter_best_fit: 1.990E+00  nSwarm_Radius: 8.57E-02  |g-mean(p)|/mean(p): 76.31%
#> iter: 200  Gbest: 1.990E+00  Gbest_rate:  0.00%  Iter_best_fit: 1.990E+00  nSwarm_Radius: 9.05E-02  |g-mean(p)|/mean(p): 70.88%
#> iter: 250  Gbest: 1.990E+00  Gbest_rate:  0.00%  Iter_best_fit: 1.990E+00  nSwarm_Radius: 7.92E-02  |g-mean(p)|/mean(p): 69.64%
#> iter: 300  Gbest: 1.990E+00  Gbest_rate:  0.00%  Iter_best_fit: 1.990E+00  nSwarm_Radius: 7.20E-02  |g-mean(p)|/mean(p): 68.07%
#> iter: 350  Gbest: 1.990E+00  Gbest_rate:  0.00%  Iter_best_fit: 1.990E+00  nSwarm_Radius: 6.97E-02  |g-mean(p)|/mean(p): 66.11%
#> iter: 400  Gbest: 1.990E+00  Gbest_rate:  0.00%  Iter_best_fit: 1.990E+00  nSwarm_Radius: 7.25E-02  |g-mean(p)|/mean(p): 65.36%
#> iter: 450  Gbest: 1.990E+00  Gbest_rate:  0.00%  Iter_best_fit: 1.990E+00  nSwarm_Radius: 6.95E-02  |g-mean(p)|/mean(p): 65.06%
#> iter: 500  Gbest: 1.990E+00  Gbest_rate:  0.00%  Iter_best_fit: 1.990E+00  nSwarm_Radius: 6.41E-02  |g-mean(p)|/mean(p): 62.90%
#> iter: 550  Gbest: 1.990E+00  Gbest_rate:  0.00%  Iter_best_fit: 1.990E+00  nSwarm_Radius: 7.09E-02  |g-mean(p)|/mean(p): 61.12%
#> iter: 600  Gbest: 1.990E+00  Gbest_rate:  0.00%  Iter_best_fit: 1.990E+00  nSwarm_Radius: 6.68E-02  |g-mean(p)|/mean(p): 60.41%
#> iter: 650  Gbest: 1.990E+00  Gbest_rate:  0.00%  Iter_best_fit: 1.990E+00  nSwarm_Radius: 7.31E-02  |g-mean(p)|/mean(p): 60.21%
#> iter: 700  Gbest: 1.990E+00  Gbest_rate:  0.00%  Iter_best_fit: 1.990E+00  nSwarm_Radius: 7.03E-02  |g-mean(p)|/mean(p): 60.21%
#> iter: 750  Gbest: 1.990E+00  Gbest_rate:  0.00%  Iter_best_fit: 1.990E+00  nSwarm_Radius: 6.45E-02  |g-mean(p)|/mean(p): 59.14%
#> iter: 800  Gbest: 1.990E+00  Gbest_rate:  0.00%  Iter_best_fit: 1.990E+00  nSwarm_Radius: 8.17E-02  |g-mean(p)|/mean(p): 59.04%
#> iter: 850  Gbest: 1.990E+00  Gbest_rate:  0.00%  Iter_best_fit: 1.990E+00  nSwarm_Radius: 7.28E-02  |g-mean(p)|/mean(p): 59.04%
#> iter: 900  Gbest: 1.990E+00  Gbest_rate:  0.00%  Iter_best_fit: 1.990E+00  nSwarm_Radius: 6.97E-02  |g-mean(p)|/mean(p): 58.06%
#> iter: 950  Gbest: 1.990E+00  Gbest_rate:  0.00%  Iter_best_fit: 1.990E+00  nSwarm_Radius: 7.15E-02  |g-mean(p)|/mean(p): 57.86%
#> iter:1000  Gbest: 1.990E+00  Gbest_rate:  0.00%  Iter_best_fit: 1.990E+00  nSwarm_Radius: 6.30E-02  |g-mean(p)|/mean(p): 56.64%
#>                                     |                                           
#> ================================================================================
#> [                          Creating the R output ...                           ]
#> ================================================================================
#>                                                                                 
#> ================================================================================
#> [                                Initialising  ...                             ]
#> ================================================================================
#>                                                                                 
#> [npart=40 ; maxit=1000 ; method=spso2011 ; topology=vonNeumann ; boundary.wall=absorbing2011]
#>          
#> [ user-definitions in control: topology=vonNeumann ; reltol=1e-20 ; REPORT=50 ; write2disk=FALSE ; best.update=async ]
#>          
#>                                                                                 
#> ================================================================================
#> [                                 Running  PSO ...                             ]
#> ================================================================================
#>                                                                                 
#> iter:  50  Gbest: 5.229E+00  Gbest_rate:  0.00%  Iter_best_fit: 5.339E+00  nSwarm_Radius: 1.14E-01  |g-mean(p)|/mean(p): 64.44%
#> iter: 100  Gbest: 2.069E+00  Gbest_rate:  0.02%  Iter_best_fit: 2.069E+00  nSwarm_Radius: 1.08E-01  |g-mean(p)|/mean(p): 78.97%
#> iter: 150  Gbest: 1.993E+00  Gbest_rate:  0.00%  Iter_best_fit: 1.993E+00  nSwarm_Radius: 1.06E-01  |g-mean(p)|/mean(p): 77.60%
#> iter: 200  Gbest: 1.991E+00  Gbest_rate:  0.00%  Iter_best_fit: 1.991E+00  nSwarm_Radius: 1.00E-01  |g-mean(p)|/mean(p): 72.19%
#> iter: 250  Gbest: 1.991E+00  Gbest_rate:  0.00%  Iter_best_fit: 1.991E+00  nSwarm_Radius: 1.02E-01  |g-mean(p)|/mean(p): 71.18%
#> iter: 300  Gbest: 1.991E+00  Gbest_rate:  0.00%  Iter_best_fit: 1.991E+00  nSwarm_Radius: 1.09E-01  |g-mean(p)|/mean(p): 68.70%
#> iter: 350  Gbest: 1.991E+00  Gbest_rate:  0.00%  Iter_best_fit: 1.991E+00  nSwarm_Radius: 1.08E-01  |g-mean(p)|/mean(p): 66.24%
#> iter: 400  Gbest: 1.991E+00  Gbest_rate:  0.00%  Iter_best_fit: 1.991E+00  nSwarm_Radius: 9.84E-02  |g-mean(p)|/mean(p): 63.03%
#> iter: 450  Gbest: 1.991E+00  Gbest_rate:  0.00%  Iter_best_fit: 1.991E+00  nSwarm_Radius: 1.09E-01  |g-mean(p)|/mean(p): 62.09%
#> iter: 500  Gbest: 1.991E+00  Gbest_rate:  0.00%  Iter_best_fit: 1.991E+00  nSwarm_Radius: 1.01E-01  |g-mean(p)|/mean(p): 61.98%
#> iter: 550  Gbest: 1.991E+00  Gbest_rate:  0.00%  Iter_best_fit: 1.991E+00  nSwarm_Radius: 1.10E-01  |g-mean(p)|/mean(p): 61.98%
#> iter: 600  Gbest: 1.991E+00  Gbest_rate:  0.00%  Iter_best_fit: 1.991E+00  nSwarm_Radius: 1.09E-01  |g-mean(p)|/mean(p): 61.76%
#> iter: 650  Gbest: 1.991E+00  Gbest_rate:  0.00%  Iter_best_fit: 1.991E+00  nSwarm_Radius: 1.10E-01  |g-mean(p)|/mean(p): 61.76%
#> iter: 700  Gbest: 1.991E+00  Gbest_rate:  0.00%  Iter_best_fit: 1.991E+00  nSwarm_Radius: 1.09E-01  |g-mean(p)|/mean(p): 61.76%
#> iter: 750  Gbest: 1.991E+00  Gbest_rate:  0.00%  Iter_best_fit: 1.991E+00  nSwarm_Radius: 1.08E-01  |g-mean(p)|/mean(p): 61.76%
#> iter: 800  Gbest: 1.991E+00  Gbest_rate:  0.00%  Iter_best_fit: 1.991E+00  nSwarm_Radius: 1.03E-01  |g-mean(p)|/mean(p): 61.35%
#> iter: 850  Gbest: 1.991E+00  Gbest_rate:  0.00%  Iter_best_fit: 1.991E+00  nSwarm_Radius: 9.78E-02  |g-mean(p)|/mean(p): 60.88%
#> iter: 900  Gbest: 1.991E+00  Gbest_rate:  0.00%  Iter_best_fit: 1.991E+00  nSwarm_Radius: 9.91E-02  |g-mean(p)|/mean(p): 59.46%
#> iter: 950  Gbest: 1.991E+00  Gbest_rate:  0.00%  Iter_best_fit: 1.991E+00  nSwarm_Radius: 1.02E-01  |g-mean(p)|/mean(p): 59.38%
#> iter:1000  Gbest: 1.991E+00  Gbest_rate:  0.00%  Iter_best_fit: 1.991E+00  nSwarm_Radius: 1.01E-01  |g-mean(p)|/mean(p): 59.18%
#>                                     |                                           
#> ================================================================================
#> [                          Creating the R output ...                           ]
#> ================================================================================
#> $par
#>        Param1        Param2        Param3        Param4        Param5 
#>  0.0005995376 -0.0007451089 -0.0014164096  0.9948278091 -0.9946553822 
#> 
#> $value
#> [1] 1.990519
#> 
#> $best.particle
#> [1] 28
#> 
#> $counts
#> function.calls     iterations    regroupings 
#>          40000           1000              0 
#> 
#> $convergence
#> [1] 3
#> 
#> $message
#> [1] "Maximum number of iterations reached"
#> 
# } # donttest END
```

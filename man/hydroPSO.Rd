%% Part of the hydroPSO R package, https://github.com/hzambran/hydroPSO
%%                                 http://cran.r-project.org/web/packages/hydroPSO
%%                                 http://www.rforge.net/hydroPSO/
%% Copyright 2011-2026 Mauricio Zambrano-Bigiarini & Rodrigo Rojas
%% Distributed under GPL 2 or later

\name{hydroPSO}
\alias{hydroPSO}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Enhanced Particle Swarm Optimisation algorithm
}
\description{
State-of-the-art version of the Particle Swarm Optimisation (PSO) algorithm (SPSO-2011 and SPSO-2007 capable). hydroPSO can be used as a replacement for \code{\link[stats]{optim}}, but its main focus is the calibration of environmental and other real-world model codes. Several fine-tuning options and PSO variants are available to customise the PSO engine to different calibration problems.
}
\usage{
hydroPSO(par, fn= "hydromod", ..., 
         method=c("spso2011", "spso2007", "ipso", "fips", "wfips", "canonical",
                  "spso2011-sa", "spso2011-de", "spso2011-cde",
                  "spso2011-dds", "spso2011-pdds"),
         lower=-Inf, upper=Inf, control=list(), 
         model.FUN=NULL, model.FUN.args=list(),
         change.type="repl", refValue=NULL )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{par}{
OPTIONAL. numeric with a first guess for the parameters to be optimised, with length equal to the dimension of the solution space \cr

All the particles are randomly initialised according to the value of \code{Xini.type}. If the user provides \env{m} parameter sets for \code{par}, they are used to overwrite the first \env{m} parameter sets randomly defined according to the value of \code{Xini.type}. If some elements in \code{par} are non finite (lower than \code{lower} or larger than \code{upper}) they are ignored.
}
  \item{fn}{
function or character with the name of a valid R function to be optimised (minimized or maximized). The character value \sQuote{hydromod} is used to specify that an R-external model code (i.e., an executable file that needs to be run from the system console) will be analised instead of an R function \cr

-) When \code{fn!='hydromod'}, the first argument of \code{fn} has to be a vector of parameters over which optimisation is going to take place. It should return a scalar result. When \code{fn!='hydromod'} the algorithm uses the value(s) returned by \code{fn} as both model output and its corresponding goodness-of-fit measure \cr

-) When \code{fn=='hydromod'} the algorithm will optimise the model defined by \code{model.FUN} and \code{model.args}, which are used to extract the values simulated by the model and to compute its corresponding goodness-of-fit measure
}
\item{\dots}{
OPTIONAL. Only used when \code{fn!='hydromod' & fn!='hydromodInR'}. \cr

further arguments to be passed to \code{fn}.
}
  \item{method}{
character, variant of the PSO algorithm to be used. By default \code{method='spso2011'}, while valid values are \sQuote{spso2011}, \sQuote{spso2007}, \sQuote{ipso}, \sQuote{fips}, \sQuote{wfips}, \sQuote{canonical}, \sQuote{spso2011-sa}, \sQuote{spso2011-de}, \sQuote{spso2011-cde}, \sQuote{spso2011-dds}, \sQuote{spso2011-pdds}:

\kbd{spso2011}: At each iteration particles are attracted to its own best-known \sQuote{personal} and to the best-known position in its \sQuote{local} neighbourhood, which depens on the value of \code{topology}. In addition, values of the PSO engine are set to the values defined in the Standard PSO 2011 (SPSO 2011, see Clerc 2012).

\kbd{spso2011-sa}: Event-driven Replica-Exchange Simulated Annealing followed by protected multi-basin swarm repopulation. SPSO-2011 remains the main search engine. Its finite personal-best memories populate a bounded quality-diversity archive in normalised parameter space. An SA event is eligible after \code{SA.stagnation} generations without global-best improvement and is launched when the normalised swarm radius is no larger than \code{SA.radius.threshold}. An emergency event is also launched after the larger of \code{SA.reheat} and four times the base stagnation interval even when the swarm remains dispersed. Setting \code{SA.stagnation=0} requests an event after every swarm batch while evaluation budget remains.

Each event selects up to \code{SA.replicas} archive entries by retaining the best solution first and then applying farthest-first basin selection. One persistent SA replica starts from each selected basin. Every one of the \code{SA.nsteps} sweeps generates one candidate per active replica using a fixed mixture of differential archive moves (probability 0.50), adaptive sparse Gaussian moves (probability 0.30), and heavy-tailed Cauchy moves (probability 0.20). The Gaussian kernel perturbs \code{SA.ndim} coordinates, while the differential and Cauchy kernels may move every non-fixed coordinate. All proposals are reflected at the parameter bounds.

For minimisation, \eqn{\Delta E=f(\widetilde{Z})-f(Z)}; for maximisation, \eqn{\Delta E=f(Z)-f(\widetilde{Z})}. A proposal with \eqn{\Delta E\le0} is always accepted, whereas a worse proposal follows the Metropolis rule

\deqn{P(\mathrm{accept})=\exp\left(-\Delta E/T_r\right),}{P(accept) = exp(-DeltaE/T[r]),}

using the SA principle of Metropolis et al. (1953) and Kirkpatrick et al. (1983). The first actual proposal batch calibrates a geometric cold-to-hot ladder from the median observed uphill energy. The cold and hot endpoints target uphill acceptance probabilities 0.20 and 0.80, respectively; a finite \code{SA.T0} overrides the automatically calibrated cold endpoint. All temperatures are multiplied by \code{SA.alpha} after each sweep. Per-replica coordinate scales use the diminishing adaptation of Corana et al. (1987) towards \code{SA.target.accept}. After \code{SA.reheat} proposals without a new event best, the initial ladder and scales are restored without discarding the current replica states.

Every \code{SA.swap.every} sweeps, alternating adjacent pairs attempt a replica exchange. For inverse temperatures \eqn{1/T_i} and \eqn{1/T_j}, energies \eqn{E_i} and \eqn{E_j}, the exchange probability is

\deqn{P(\mathrm{swap})=\min\left\{1,\exp\left[\left(T_i^{-1}-T_j^{-1}\right)\left(E_i-E_j\right)\right]\right\}.}{P(swap) = min(1, exp[(1/T[i]-1/T[j]) (E[i]-E[j])]).}

This replica-exchange mechanism follows Swendsen and Wang (1986) and Hukushima and Nemoto (1996): hot replicas cross objective barriers and accepted configurations can migrate towards colder searches. After an event, mutually distant accepted states may replace up to \code{SA.scout.fraction} of the weakest non-global-best particles, but only when the incoming state strictly improves the personal best it replaces. Their displaced personal bests are first retained in the archive; positions and personal-best memories are then reset to the superior new basins, with bounded non-zero SPSO velocities. For \code{SA.scout.generations} generations these scouts use only the protected scout group as informants, preventing immediate collapse back to the incumbent global-best basin. The original global-best particle is never selected for replacement. An event that transfers at least one competitive basin restores the base trigger interval; otherwise, the required stagnation interval doubles up to \code{SA.reheat}. A subsequent SPSO global-best improvement also restores the base interval.

Replica candidates in each sweep are evaluated as one batch with the requested serial, FORK, or PSOCK backend for ordinary R functions, R-internal models, and isolated R-external models. The total number of SA evaluations is limited by \code{SA.budget.fraction} and by \code{maxfn}. The method is intended for rugged multimodal objectives and does not imply a universal performance improvement; comparisons should use paired seeds and equal evaluation or wall-clock budgets.

\kbd{spso2011-de}: Informant-Driven DE-Mutation, a package-specific hybrid that embeds a Differential Evolution (DE)-style mutation within every SPSO-2011 particle update. It retains the adaptive random informant topology and computes the standard SPSO-2011 centre of gravity \eqn{G_i^{(t)}} from particle \eqn{i}'s current position, personal best, and best informant. The target particle is excluded from the donor pool. Two distinct personal-best positions are selected from its current informants whenever possible; a bounded per-particle archive containing only positions previously received from that particle's realised informant topology is used only when the current pool cannot supply a valid pair. With

\deqn{D_i^{(t)}=P_{r_1}^{(t)}-P_{r_2}^{(t)},\qquad \rho_i^{(t)}=\left\lVert G_i^{(t)}-X_i^{(t)}\right\rVert,}{D_i(t) = P_r1(t) - P_r2(t),  rho_i(t) = ||G_i(t) - X_i(t)||,}

the spherical draw is replaced by the radius-normalised mutant point

\deqn{M_i^{(t)}=G_i^{(t)}+F_i^{(t)}\rho_i^{(t)}\frac{D_i^{(t)}}{\left\lVert D_i^{(t)}\right\rVert}.}{M_i(t) = G_i(t) + F_i(t) rho_i(t) D_i(t) / ||D_i(t)||.}

Thus the informant difference supplies a correlation-sensitive direction, while the native SPSO-2011 radius controls its magnitude. If fewer than two distinct non-target memories are available, the donor difference is zero, or the SPSO radius is zero, the standard SPSO-2011 spherical draw is used for that update. The velocity is then updated as

\deqn{V_i^{(t+1)} = CF\left[wV_i^{(t)} + M_i^{(t)}-X_i^{(t)}\right],}{V_i(t+1) = CF [w V_i(t) + M_i(t) - X_i(t)],}

where \eqn{w} is the inertia weight and \eqn{CF} is the configured constriction multiplier (equal to one when \code{use.CF=FALSE}). This is followed by the same velocity clamping, position update, and boundary treatment used by the other SPSO-2011 methods.

The differential factors use success-history adaptation following the parameter-learning principles of JADE (Zhang and Sanderson, 2009) and SHADE (Tanabe and Fukunaga, 2013). Initially \eqn{\mu_F} is set to \code{DE.F}. Each particle factor is drawn from a positive Cauchy distribution centred at \eqn{\mu_F} with scale 0.1 and truncated above at 2. After the generated particles are evaluated, factors that produced a personal-best improvement are combined using the fitness-gain-weighted Lehmer mean

\deqn{L_F=\frac{\sum_k w_kF_k^2}{\sum_k w_kF_k},\qquad w_k=\frac{\Delta_k}{\sum_j\Delta_j},}{L_F = sum(w_k F_k^2) / sum(w_k F_k),  w_k = Delta_k / sum(Delta_j),}

where \eqn{\Delta_k>0} is the objective improvement. The updated memory is \eqn{\mu_F^{new}=(1-c)\mu_F+cL_F}, where \eqn{c} is \code{DE.c}. It remains unchanged when no DE-generated particle improves its personal best. Regrouping resets the factor mean and informant archives because the swarm search region has changed.

The method uses the differential mutation principle of Storn and Price (1997), the SPSO-2011 construction described by Zambrano-Bigiarini et al. (2013), and the population-topology rationale studied by Lynn et al. (2018). It does not run a separate DE population, binomial crossover, or sequential DE selection stage; fitness is evaluated in the next ordinary swarm batch. Therefore, it adds no objective-function evaluations. This method requires \code{topology='random'} and does not imply a convergence or performance guarantee.

\kbd{spso2011-cde}: Competitive Coevolutionary SPSO2011-SHADE. Unlike \code{spso2011-de}, this method does not embed a differential displacement inside SPSO geometry. Instead, every generation partitions one shared population between two complete and competing proposal operators. Particles assigned to SPSO use the unmodified SPSO-2011 random-informant and spherical update. Particles assigned to DE use mutation, binomial crossover, a global archive of successfully replaced parents, and greedy parent-trial selection. Exactly one candidate is evaluated per particle, so a generation still costs \code{npart} objective-function evaluations.

Outside emergency exploration, the DE mutant follows the JADE current-to-pbest/1 strategy (Zhang and Sanderson, 2009),

\deqn{M_i=X_i+F_i(P_i-X_i)+F_i(X_{r_1}-A_{r_2}),}{M[i] = X[i] + F[i] (P[i] - X[i]) + F[i] (X[r1] - A[r2]),}

where \eqn{P_i} is sampled from the best \code{CDE.p} fraction of personal-best positions, \eqn{X_{r_1}} is a distinct current-population donor, and \eqn{A_{r_2}} is selected from the remaining population or the archive. Binomial crossover with rate \eqn{CR_i} guarantees at least one mutant coordinate. Trial vectors are reflected at finite bounds. If global-best stagnation reaches \code{CDE.stag.iter} while mean relative coordinate spread is no larger than \code{CDE.diversity}, DE switches to exploratory DE/rand/1/bin and receives the maximum allowed population fraction.

Mutation factors and crossover rates use circular success-history memories of length \code{CDE.H}, following SHADE (Tanabe and Fukunaga, 2013). Each \eqn{F_i} is sampled from a positive Cauchy distribution and each \eqn{CR_i} from a bounded normal distribution. Strictly improving DE trials update one memory cell using improvement-weighted successful parameters and add their replaced parents to an archive bounded at \code{npart}. Rejected trials restore their parents before personal-best and informant updates.

The DE population fraction is adapted from exponentially smoothed, scale-normalised fitness gains produced by SPSO and DE. It is restricted to \code{[CDE.qmin,CDE.qmax]}, ensuring that both operators remain active. This is a single-objective probability-matching adaptation inspired by the recent-performance operator allocation of Li et al. (2014), rather than an implementation of their multiobjective bandit algorithm. Stagnating particles receive DE proposals first.

When \code{CDE.eigen=TRUE}, crossover competes in both the original coordinates and an eigen coordinate system obtained from the personal-best covariance matrix. The basis is refreshed every \code{CDE.eigen.freq} generations, and coordinate-system probabilities adapt from their recent gains, following the landscape-alignment rationale of Liu et al. (2019). This feature targets correlated and rotated parameter landscapes. The method requires \code{topology='random'}, \code{best.update='sync'}, and at least four particles. It does not imply a universal performance improvement.

\kbd{spso2011-dds}: Stagnation-triggered global-best DDS refinement combining the unmodified SPSO-2011 particle update of Clerc (2012) with the incumbent perturbation and greedy selection of Dynamically Dimensioned Search (DDS; Tolson and Shoemaker, 2007). SPSO-2011 first explores normally. After \code{DDS.stag.iter} consecutive completed swarm generations without improvement in the global best, DDS is activated for the rest of the run. This staged design follows the PSO-to-DDS rationale of Sagar et al. (2018), while retaining hydroPSO's single SPSO-2011 swarm. For DDS proposal \eqn{s}, each dimension is independently selected with probability

\deqn{P(s)=1-\frac{\log(s)}{\log(s_{\mathrm{max}})},}{P(s) = 1 - log(s) / log(smax),}

where \eqn{s_{\mathrm{max}}} is the number of iterations remaining when DDS is activated. At least one dimension is always selected. The proposal is generated around the current global best as

\deqn{\widetilde{G}_j=G_{\mathrm{best},j}+rR_jZ_j,\quad Z_j\sim N(0,1),}{Gnew_j = Gbest_j + r R_j Z_j,  Z_j ~ N(0,1),}

for selected dimensions only, where \eqn{r}=\code{DDS.r} and \eqn{R_j} is the search range of parameter \eqn{j}. Boundary violations are reflected into the feasible range. One DDS candidate is evaluated sequentially after each completed parallel swarm batch while DDS is active, provided that an evaluation remains under \code{maxfn}. The candidate replaces the corresponding personal-best and global-best memories only when it strictly improves the objective; rejected candidates cannot degrade the swarm state. Thus the SPSO velocities and their momentum are never masked. The method adds at most one objective-function evaluation per active iteration. It is a hydroPSO hybrid inspired by DDS and DOPS, not an exact implementation of the original single-solution DDS or multi-swarm DOPS algorithms.

\kbd{spso2011-pdds}: Parallel Multi-Basin DDS Portfolio combining complete SPSO-2011 generations with complete batches of parallel DDS candidates. It is intended for expensive, multimodal calibration problems where a single late DDS trajectory around the global best supplies too little independent search. The method first runs \code{PDDS.warmup} ordinary SPSO-2011 generations to construct a bounded archive of good parameter vectors. Archive entries are ranked by objective value and selected for diversity using root-mean-square distance in normalised parameter space, controlled by \code{PDDS.archive.size} and \code{PDDS.min.distance}.

After warm-up, every \code{PDDS.batch.every}-th batch is a pDDS batch; the intervening batches remain unmodified SPSO-2011 generations. With the defaults, three SPSO-2011 generations are followed by a repeating sequence of two SPSO batches and one pDDS batch. Each pDDS batch contains exactly \code{npart} candidates, so it replaces rather than supplements a swarm generation and consumes the same number of objective-function evaluations. Candidates are distributed round-robin among up to \code{PDDS.nchains} persistent archive-seeded chains. Chain perturbation scales cycle through \code{PDDS.r}; the default \code{c(0.05, 0.20, 0.50)} simultaneously supplies fine, standard DDS, and exploratory neighbourhoods.

Each chain uses its own repeating DDS epoch. For local proposal number \eqn{s} within an epoch of length \eqn{H}=\code{PDDS.epoch}, dimensions are selected with

\deqn{P(s)=1-\frac{\log(s)}{\log(H)},}{P(s) = 1 - log(s) / log(H),}

with at least one varying dimension selected. Selected coordinates receive Gaussian, parameter-range-scaled perturbations and are reflected at their bounds. At the end of a pDDS batch, only the best improving candidate from each chain is greedily accepted. Accepted chain solutions update the diversity archive and the best solutions replace up to \code{PDDS.migration.frac} of the weakest non-global-best swarm memories and current positions. Migrated particle velocities are set to zero. New SPSO archive elites may restart weaker chain incumbents, giving bidirectional information exchange between the portfolio and swarm.

This package-specific method is motivated by the multi-swarm-to-DDS strategy of Sagar et al. (2018), the original bounded greedy DDS of Tolson and Shoemaker (2007), and parallel multi-scale DDS knowledge sharing described by Asgari et al. (2023). It is not an exact implementation of DOPS or the Asgari et al. algorithm and does not imply a performance guarantee. Fair comparisons should use identical \code{maxfn} values because each reported iteration is one batch of \code{npart} evaluations.

\kbd{spso2007}: As in \code{method='spso2011'}, but with values of the PSO engine set to the values defined in the Standard PSO 2007 (SPSO 2007, see Clerc 2012).

\kbd{ipso}: at each iteration particles in the swarm are rearranged in descending order according to their goodness-of-fit and the best \code{ngbest} particles are used to modify particles' position and velocity (see Zhao, 2006). Each particle is connected to a neighbourhood of particles depending on the \code{topology} value.

\kbd{fips}: at each iteration ALL particles contribute to modify the particles' position and velocity (see Mendes et al., 2004). Each particle is connected to a neighbourhood of particles depending on the \code{topology} value.

\kbd{wfips}: same implementation as \kbd{fips} method, but the contribution of each particle is weighted according to their goodness-of-fit value (see Mendes et al., 2004).

\kbd{canonical}: It corresponds to the first formulation of the PSO algorithm, and it is included here for educational and comparative purposes only, due to several limitations described in literature (see Kennedy, 2006). 

At each iteration, particles are attracted to its own best-known \sQuote{personal} and to the best-known position in all the swarm (\sQuote{global}). The following \code{control} arguments are set when this method is selected: 

(i) \code{npart=40}, 

(ii) \code{topology='gbest'}, 

(iii) \code{Xini.type='random'}, 

(iv) \code{Vini.type='random2007'}, 

(v) \code{use.CF=TRUE}, 

(vi) \code{c1=2.05}, 

(vii) \code{c2=2.05}, 

(viii) \code{boundary.wall='absorbing2007'}, 

(ix) \code{lambda=1.0}
}
  \item{lower}{
numeric, lower boundary for each parameter. \cr

Note for \code{\link[stats]{optim}} users: in \kbd{hydroPSO} the length of \code{lower} and \code{upper} are used to defined the dimension of the solution space.
}
  \item{upper}{
numeric, upper boundary for each parameter. \cr

Note for \code{\link[stats]{optim}} users: in \kbd{hydroPSO} the length of \code{lower} and \code{upper} are used to defined the dimension of the solution space.
}
  \item{control}{
a list of control parameters. See \sQuote{Details}.
}
  \item{model.FUN}{
OPTIONAL. Used only when \code{fn='hydromod'}. \cr

character, valid R function representing the model code to be calibrated/optimised.
} 
  \item{model.FUN.args}{
OPTIONAL. Used only when \code{fn='hydromod'}. \cr

list with the arguments to be passed to \code{model.FUN}
} 
  \item{change.type}{
OPTIONAL. Used only when \code{fn='hydromodInR'}. Character of length 1 or length equal to the number of parameters, indicating how the PSO coordinates are converted to the parameter values passed to \code{model.FUN}. Valid values are \code{"repl"} for replacement, \code{"addi"} for additive changes, and \code{"mult"} for multiplicative changes. A single value is recycled to all parameters. The default \code{"repl"} preserves the previous behaviour.
}
  \item{refValue}{
OPTIONAL. Used only when \code{fn='hydromodInR'} and any element of \code{change.type} is \code{"addi"} or \code{"mult"}. Numeric reference value of length 1 or length equal to the number of parameters, used to compute the parameter values passed to \code{model.FUN}. Only parameters using \code{"addi"} or \code{"mult"} require and use their corresponding reference value.
}

}
\details{
By default the hydroPSO function performs minimization of \code{fn}, but it will maximize \code{fn} if \code{MinMax='max'} \cr

The default control arguments in hydroPSO implements the Standard PSO 2011 - SPSO2011 (see Clerc 2012; Clerc et al., 2010). At the same time, hydroPSO function provides options for clamping the maximal velocity, regrouping strategy when premature convergence is detected, time-variant acceleration coefficients, time-varying maximum velocity, (non-)linear / random / adaptive / best-ratio inertia weight definitions, random or LHS initialization of positions and velocities, synchronous or asynchronous update, 4 alternative neighbourhood topologies among others.

The \code{control} argument is a list that can supply any of the following components:

  \describe{    
    \item{drty.in}{
OPTIONAL. Used only when \code{fn='hydromod'} \cr
character, name of the directory storing the input files required for PSO, i.e. \sQuote{ParamRanges.txt} and \sQuote{ParamFiles.txt}
}
  \item{drty.out}{
character, path to the directory storing the output files generated by hydroPSO
}
  \item{param.ranges}{
OPTIONAL. Used only when \code{fn='hydromod'} \cr

character, name of the file defining the minimum and maximum boundary values for each one of the parameters to be calibrated
} 
  \item{digits}{
OPTIONAL. Used only when \code{write2disk=TRUE} \cr

numeric, number of significant digits used for writing the output files with scientific notation
}
  \item{MinMax}{
character, indicates whether a maximization or minimization problem needs to be solved. Valid values are in: \code{c('min', 'max')}. Default value is \kbd{min}
}
  \item{npart}{
numeric, number of particles in the swarm. By default \code{npart=NA}, which means that the swarm size depends on the value of \code{method}: \cr

when \code{method='spso2007'} \code{npart=ceiling(10+2*sqrt(n))}, or \code{npart=40} otherwise
}
  \item{maxit}{
numeric, maximum number of iterations. By default \code{maxit=1000}
}
  \item{maxfn}{
numeric, maximum number of function evaluations. Default value is \code{+Inf} \cr

When \code{fn=='hydromod'}, this stopping criterion uses the number of \emph{effective} function calls, i.e. those function calls with a finite output value
}
  \item{c1}{
numeric, cognitive acceleration coefficient. Encourages the exploitation of the solution space and reflects how much the particle is influenced by its own best-known position \cr

By default \code{c1= 0.5 + log(2)}
}
  \item{c2}{
numeric, social acceleration coefficient. Encourages the exploration of the current global best and reflects how much the particle is influenced by the best-known optimum of the swarm \cr

By default \code{c2= 0.5 + log(2)}
}
  \item{use.IW}{
logical, indicates if an inertia weight (\env{w}) will be used to avoid swarm explosion, i.e. particles flying around their best position without converging into it (see Shi and Eberhart, 1998) \cr

By default \code{use.IW=TRUE}
}
  \item{IW.w}{
OPTIONAL. Used only when \code{use.IW=TRUE} and \code{IW.type!='GLratio'} \cr

numeric, value of the inertia weight(s) (\env{w} or \env{[w.ini, w.fin]}). It can be a single number which is used for all iterations, or it can be a vector of length 2 with the initial and final values (in that order) that \env{w} will take along the iterations \cr

By default \code{IW.w=1/(2*log(2))}
}
  \item{use.CF}{
logical, indicates if the Clerc's Constriction Factor (see Clerc, 1999; Eberhart and Shi, 2000; Clerc and Kennedy, 2002) is used to avoid swarm explosion \cr

By default \code{use.CF=FALSE}
}
  \item{lambda}{
numeric in [0,1], represents a percentage to limit the maximum velocity (Vmax) for each dimension, which is computed as \code{vmax = lambda*(Xmax-Xmin)} \cr

By default \code{lambda=1}
}
  \item{abstol}{
numeric, absolute convergence tolerance. The algorithm stops if \code{gbest <= abstol} (minimisation problems) OR when \code{gbest >= abstol} (maximisation problems) \cr

By default it is set to \code{-Inf} or \code{+Inf} for minimisation or maximisation problems, respectively
}
  \item{reltol}{
numeric, relative convergence tolerance. The algorithm stops if the absolute difference between the best \sQuote{personal best} in the current iteration and the best \sQuote{personal best} in the previous iteration is less or equal to \code{reltol}. Defaults to \code{sqrt(.Machine$double.eps)}, typically, about 1e-8 \cr

If \code{reltol} is set to \code{0}, this stopping criterion is not used 
}
  \item{Xini.type}{
character, indicates how to initialise the particles' positions in the swarm within the ranges defined by \code{lower} and \code{upper}. Valid values are: \cr

-) \kbd{random}: random initialisation of positions within \code{lower} and \code{upper} \cr

-) \kbd{lhs}: Latin Hypercube initialisation of particle's positions, using \code{npart} number of strata to divide each parameter range. \bold{It requires the \pkg{lhs} package}. See \code{\link[lhs]{randomLHS}}. \cr

-) \kbd{sobol}: Quasi-random initialisation of particle's positions using uniform Sobol low discrepancy numbers within \code{lower} and \code{upper} with \code{npart} as the number of observations to extract. \bold{It requires the \pkg{randtoolbox} package}. See \code{\link[randtoolbox]{sobol}}. \cr

By default \code{Xini.type='random'}
}
  \item{Vini.type}{
character, indicates how to initialise the particles' velocities in the swarm. Valid values are: \cr

-) \kbd{random2011}: random initialisation of velocities within \code{lower-Xini} and \code{upper-Xini}, as defined in SPSO 2011 (\samp{Vini=U(lower-Xini, upper-Xini)}) (see Clerc, 2012, 2010) \cr

-) \kbd{lhs2011}: same as in \kbd{random2011}, but using a Latin Hypercube initialisation with \code{npart} number of strata instead of a random uniform distribution for each parameter. \bold{It requires the \pkg{lhs} package} \cr

-) \kbd{random2007}: random initialisation of velocities within \code{lower} and \code{upper} using the \sQuote{half-diff} method defined in SPSO 2007 (\samp{Vini=[U(lower, upper)-Xini]/2}) (see Clerc, 2012, 2010) \cr

-) \kbd{lhs2007}: same as in \kbd{random2007}, but using a Latin Hypercube initialisation with \code{npart} number of strata instead of a random uniform distribution for each parameter. \bold{It requires the \pkg{lhs} package} \cr

-) \kbd{zero}: all the particles are initialised with zero velocity \cr

By default \code{Vini.type=NA}, which means that \code{Vini.type} depends on the value of \code{method}: when \kbd{method='spso2007'} \code{Vini.type='random2007'}, or \code{Vini.type='random2011'} otherwise
}
  \item{best.update}{
character, indicates how (when) to update the global/neighbourhood and personal best. Valid values are: \cr

-)\kbd{sync}: the update is made synchronously, i.e. after computing the position and goodness-of-fit for ALL the particles in the swarm. This is the DEFAULT option\cr

-)\kbd{async}: the update is made asynchronously, i.e. after computing the position and goodness-of-fit for EACH individual particle in the swarm
}
  \item{random.update}{
OPTIONAL. Only used when \code{best.update='async'}\cr

logical, if \code{TRUE} the particles are processed in random order to update their personal best and the global/neighbourhood best \cr
By default \code{random.update=TRUE}
}
  \item{boundary.wall}{ 
character, indicates the type of boundary condition to be applied during optimisation. Valid values are: \code{NA}, \sQuote{absorbing2011}, \sQuote{absorbing2007}, \sQuote{reflecting}, \sQuote{damping}, \sQuote{invisible}: \cr

-) \kbd{NA}: The value of \code{boundary.wall} depends on the value defined for the \code{method}: when \kbd{method='spso2007'} \code{boundary.wall='absorbing2007'}, or \code{boundary.wall='absorbing2011'} otherwise. By default \code{boundary.wall=NA}. \cr

-) \kbd{absorbing2011}: This condition clamps a particle's position to the edge of the search space if it tries to leave, but it handles velocity flexibly to match its hypersphere trajectory updates. Instead of completely killing the particle's momentum, it often allows the internal velocity vector to persist or undergo geometric modification, enabling the particle to smoothly "slide" along the boundary during subsequent iterations. See more details in Clerc (2012). \cr

-) \kbd{absorbing2007}: Acting like a perfectly sticky wall, this method strictly clamps the particle's position to the violated boundary and forces its velocity in that specific dimension to exactly zero. The particle loses all momentum in that direction and will remain stuck at the edge until the gravitational pull of its personal or global best draws it back into the valid search space. See more details in Clerc (2012). \cr

-) \kbd{reflecting}: This approach treats the boundary as a perfectly elastic surface. When a particle overshoots the search space, its position is mirrored back inside by the exact distance it traveled out of bounds, and its velocity in that dimension is perfectly reversed. This conserves the particle's kinetic energy, encouraging aggressive and continuous exploration of the boundary regions without losing swarm momentum. See more details in Robinson and Rahmat-Samii (2004). \cr

-) \kbd{damping}: Functioning as an inelastic collision, the damping condition reflects the particle back into the search space but applies a random reduction factor to its reversed velocity. This hybrid approach allows the particle to bounce off the wall and continue exploring, but strategically bleeds off its kinetic energy to prevent the swarm from endlessly oscillating back and forth across the entire search space, thereby aiding convergence. See more details in Huang and Mohan (2005). \cr

-) \kbd{invisible}: Under this condition, the boundaries mathematically do not restrict movement, allowing particles to fly freely outside the search space with their position and velocity completely unchanged. However, while a particle is out of bounds, its fitness is not evaluated; it relies entirely on the algorithmic attraction of its previously established valid personal and global bests to eventually pull its trajectory back into the valid optimization space. See more details in Robinson and Rahmat-Samii (2004). \cr
 
Experience has shown that Clerc's constriction factor and the inertia weights do not always confine the particles within the solution space. To address this problem, Robinson and Rahmat-Samii (2004) and Huang and Mohan (2005) propose different boundary conditions, namely, \kbd{reflecting}, \kbd{damping}, \kbd{absorbing} and \kbd{invisible} to define how particles are treated when reaching the boundary of the searching space (see Robinson and Rahmat-Samii (2004) and Huang and Mohan (2005) for further details)
}
  \item{topology}{
character, indicates the neighbourhood topology used in hydroPSO. Valid values are in \code{c('random', 'gbest', 'lbest', 'vonNeumann')}: \cr

-) \kbd{random}: the random topology is a special case of \samp{lbest} where connections among particles are randomly modified after an iteration showing no improvement in the global best (see Clerc, 2005; Clerc, 2010) \cr

-) \kbd{gbest}: every particle is connected to each other and, hence the global best influences all particles in the swarm. This is also termed \samp{star} topology, and it is generally assumed to have a fast convergence but is more vulnerable to the attraction to sub-optimal solutions (see Kennedy, 1999; Kennedy and Mendes, 2002, Schor et al., 2010) \cr

-) \kbd{lbest}: each particle is connected to its \code{K} immediate neighbours only. This is also termed \samp{circles} or \samp{ring} topology, and generally the swarm will converge slower than the \kbd{gbest} topology but it is less vulnerable to sub-optimal solutions (see Kennedy, 1999; Kennedy and Mendes, 2002) \cr

-) \kbd{vonNeumann}: each particle is connected to its \code{K=4} immediate neighbours only. This topology is more densely connected than \samp{lbest} but less densely than \samp{gbest}, thus, showing some parallelism with \samp{lbest} but benefiting from a bigger neighbourhood (see Kennedy and Mendes, 2003) \cr

By default \code{topology='random'}.
}
  \item{K}{
OPTIONAL. Only used when \code{topology} is in \code{c(random, lbest, vonNeumann)} \cr

numeric, neighbourhood size, i.e. the number of informants for each particle (including the particle itself) to be considered in the computation of their personal best \cr

When \code{topology=lbest} \code{K} MUST BE an even number in order to consider the same amount of neighbours to the left and the right of each particle \cr

As special case, \code{K} could be equal to \code{npart}. By default \code{K=3}
}
  \item{iter.ini}{
OPTIONAL. Only used when \code{topology=='lbest'}. By default \code{iter.ini=0}. \cr

numeric, number of iterations for which the \kbd{gbest} topology will be used before using the \kbd{lbest} topology for the computation of the personal best of each particle\cr

This option aims at making faster the identification of the global zone of attraction.
}
  \item{ngbest}{
OPTIONAL. Only used when \code{method=='ipso'} \cr

numeric, number of particles considered in the computation of the global best \cr

By default \code{ngbest=4} (see Zhao, 2006)
}
  \item{SA.T0}{
OPTIONAL. Only used when \code{method='spso2011-sa'}. \code{NA} or a finite numeric value greater than zero defining the cold endpoint of the replica temperature ladder. With the default \code{SA.T0=NA}, the first actual proposal batch calibrates the cold and hot endpoints from the median uphill energy for target acceptance probabilities 0.20 and 0.80, respectively. No pilot evaluation is discarded.
}
  \item{SA.alpha}{
OPTIONAL. Only used when \code{method='spso2011-sa'}. Finite numeric cooling factor in \eqn{(0,1)} applied to every temperature after each replica sweep, preserving the ladder ratios. By default \code{SA.alpha=0.98}.
}
  \item{SA.scale}{
OPTIONAL. Only used when \code{method='spso2011-sa'}. Finite numeric value greater than zero defining the maximum initial proposal scale as a fraction of each parameter range. At an event it is reduced to the current relative swarm spread when smaller and adapted separately for each replica and coordinate. By default \code{SA.scale=0.05}.
}
  \item{SA.nsteps}{
OPTIONAL. Only used when \code{method='spso2011-sa'}. Positive integer number of replica-exchange sweeps attempted per qualifying event. Each full sweep evaluates one proposal per active replica. By default \code{SA.nsteps=4}, which permits both alternating adjacent-pair exchange rounds with the default \code{SA.swap.every=2} while preserving most evaluations for SPSO-2011.
}
  \item{SA.ndim}{
OPTIONAL. Only used when \code{method='spso2011-sa'}. Integer in \eqn{[1,n]} defining how many randomly selected parameter dimensions are perturbed in each SA transition. The default \code{SA.ndim=1} implements adaptive coordinate-wise local search and avoids simultaneous large moves in high dimension.
}
  \item{SA.stagnation}{
OPTIONAL. Only used when \code{method='spso2011-sa'}. Non-negative integer number of completed SPSO generations without a new global best required before a collapsed-swarm SA event. Unproductive events double this trigger interval up to \code{SA.reheat}; an SPSO improvement restores it. By default \code{SA.stagnation=5}; zero requests an event after every swarm batch while budget remains.
}
  \item{SA.reheat}{
OPTIONAL. Only used when \code{method='spso2011-sa'}. Positive integer number of proposals without a new event best after which the original temperature ladder and proposal scales are restored. Current replica states are retained. This value also caps the adaptive event-trigger interval and supplies the minimum emergency-stagnation interval. By default \code{SA.reheat=50}.
}
  \item{SA.target.accept}{
OPTIONAL. Only used when \code{method='spso2011-sa'}. Finite numeric target in \eqn{(0,1)} used by the diminishing per-coordinate step-size adaptation. By default \code{SA.target.accept=0.50}, following the approximately balanced acceptance objective of adaptive coordinate SA described by Corana et al. (1987).
}
  \item{SA.replicas}{
OPTIONAL. Only used when \code{method='spso2011-sa'}. \code{NA} or an integer in \eqn{[2,\code{npart}]} defining the maximum number of cold-to-hot replicas. With the default \code{NA}, serial runs use up to four replicas and parallel runs use up to six without exceeding \code{npart}; at least two replicas are retained even when only one worker is requested.
}
  \item{SA.swap.every}{
OPTIONAL. Only used when \code{method='spso2011-sa'}. Positive integer number of sweeps between adjacent replica-exchange attempts. Alternating odd and even pairs are used. By default \code{SA.swap.every=2}.
}
  \item{SA.scout.fraction}{
OPTIONAL. Only used when \code{method='spso2011-sa'}. Finite numeric value in \eqn{(0,1)} defining the maximum fraction of weak non-global-best particles repopulated with mutually distant accepted SA states that strictly improve their replacement targets. By default \code{SA.scout.fraction=0.10}.
}
  \item{SA.scout.generations}{
OPTIONAL. Only used when \code{method='spso2011-sa'}. Positive integer number of complete SPSO generations during which repopulated scouts use only other protected scouts as informants. By default \code{SA.scout.generations=3}.
}
  \item{SA.radius.threshold}{
OPTIONAL. Only used when \code{method='spso2011-sa'}. Finite numeric value in \eqn{(0,1]} defining the normalised swarm-radius threshold for a collapsed-swarm SA event. It also determines the preferred minimum archive separation as half this value. By default \code{SA.radius.threshold=0.03}.
}
  \item{SA.budget.fraction}{
OPTIONAL. Only used when \code{method='spso2011-sa'}. Finite numeric value in \eqn{(0,1]} limiting all SA proposal evaluations to this fraction of \code{maxfn}, when finite, or of \code{npart*maxit} otherwise. A partial final sweep is allowed, and SA does not consume more than the function-evaluation slots available at its event. By default \code{SA.budget.fraction=0.10}.
}
  \item{DE.F}{
OPTIONAL. Only used when \code{method='spso2011-de'}. Finite numeric value in \eqn{(0,2]} defining the initial success-history mean of the radius-normalised differential weight. Particle-specific factors are sampled adaptively around this mean. By default \code{DE.F=0.1}.
}
  \item{DE.c}{
OPTIONAL. Only used when \code{method='spso2011-de'}. Finite numeric learning rate in \eqn{(0,1]} used to update the differential-factor memory from the weighted Lehmer mean of successful factors. Larger values adapt more quickly but retain less history. By default \code{DE.c=0.1}.
}
  \item{DE.archive.size}{
OPTIONAL. Only used when \code{method='spso2011-de'}. Integer greater than or equal to 2 giving the maximum number of unique historical informant personal-best positions retained separately for each particle. The archive contains no global donors and is consulted only when the current random informant pool lacks two distinct non-target memories. By default \code{DE.archive.size=20}.
}
  \item{CDE.H}{
OPTIONAL. Only used when \code{method='spso2011-cde'}. Positive integer length of the circular SHADE success-history memories for the mutation factor and crossover rate. By default \code{CDE.H=10}.
}
  \item{CDE.p}{
OPTIONAL. Only used when \code{method='spso2011-cde'}. Finite numeric value in \eqn{(0,1]} defining the fraction of best personal-best positions eligible for current-to-pbest/1 mutation. By default \code{CDE.p=0.10}.
}
  \item{CDE.qmin, CDE.qmax}{
OPTIONAL. Only used when \code{method='spso2011-cde'}. Finite numeric lower and upper bounds satisfying \eqn{0 < qmin < qmax < 1} for the fraction of each population batch assigned to complete DE proposals. The complementary fraction receives unmodified SPSO-2011 proposals. By default \code{CDE.qmin=0.20} and \code{CDE.qmax=0.80}.
}
  \item{CDE.stag.iter}{
OPTIONAL. Only used when \code{method='spso2011-cde'}. Positive integer number of completed generations without global-best improvement required for emergency DE/rand/1 exploration when the population has also collapsed. By default \code{CDE.stag.iter=5}.
}
  \item{CDE.diversity}{
OPTIONAL. Only used when \code{method='spso2011-cde'}. Finite numeric value in \eqn{[0,1]} giving the maximum mean relative coordinate spread regarded as population collapse for emergency exploration. By default \code{CDE.diversity=0.05}.
}
  \item{CDE.eigen}{
OPTIONAL. Only used when \code{method='spso2011-cde'}. Logical indicating whether DE crossover may operate in an adaptively selected eigen coordinate system derived from the personal-best covariance matrix. By default \code{CDE.eigen=TRUE}.
}
  \item{CDE.eigen.freq}{
OPTIONAL. Only used when \code{method='spso2011-cde'}. Positive integer number of generations between eigen-basis refreshes. By default \code{CDE.eigen.freq=5}.
}
  \item{DDS.r}{
OPTIONAL. Only used when \code{method='spso2011-dds'}. Finite numeric value greater than zero defining the standard deviation of each selected Gaussian DDS perturbation as a fraction of that parameter's search range. Boundary violations are reflected. By default \code{DDS.r=0.20}, following the standard DDS perturbation magnitude used by Tolson and Shoemaker (2007).
}
  \item{DDS.stag.iter}{
OPTIONAL. Only used when \code{method='spso2011-dds'}. Positive integer giving the number of consecutive completed SPSO-2011 generations without global-best improvement required before DDS refinement is activated. Once activated, DDS remains active. By default \code{DDS.stag.iter=5}.
}
  \item{PDDS.warmup}{
OPTIONAL. Only used when \code{method='spso2011-pdds'}. Positive integer number of initial SPSO-2011 generations used to seed the quality-diversity archive before a pDDS batch may run. By default \code{PDDS.warmup=3}.
}
  \item{PDDS.batch.every}{
OPTIONAL. Only used when \code{method='spso2011-pdds'}. Integer greater than or equal to two defining the portfolio cycle after warm-up. Every \code{PDDS.batch.every}-th batch is pDDS and all intervening batches are SPSO-2011. By default \code{PDDS.batch.every=3}, giving two SPSO batches per pDDS batch.
}
  \item{PDDS.nchains}{
OPTIONAL. Only used when \code{method='spso2011-pdds'}. Integer in \eqn{[1,\code{npart}]} defining the maximum number of persistent, archive-seeded DDS chains. By default it is \code{min(npart, 5, max(2, floor(npart/8)))}.
}
  \item{PDDS.archive.size}{
OPTIONAL. Only used when \code{method='spso2011-pdds'}. Integer not smaller than \code{PDDS.nchains} defining the maximum number of quality-diverse solutions retained. By default it is \code{min(20, max(5, 2*n))}, where \eqn{n} is the parameter dimension.
}
  \item{PDDS.r}{
OPTIONAL. Only used when \code{method='spso2011-pdds'}. Numeric vector of positive, parameter-range-relative Gaussian perturbation scales assigned cyclically to the persistent chains. By default \code{PDDS.r=c(0.05, 0.20, 0.50)} supplies fine, standard DDS, and exploratory searches.
}
  \item{PDDS.epoch}{
OPTIONAL. Only used when \code{method='spso2011-pdds'}. Integer greater than or equal to two defining the number of proposals in each chain-local DDS dimension schedule before that schedule restarts. By default \code{PDDS.epoch=40}.
}
  \item{PDDS.migration.frac}{
OPTIONAL. Only used when \code{method='spso2011-pdds'}. Finite numeric value in \eqn{(0,1]} defining the maximum fraction of weak swarm particles replaced by improving DDS chain incumbents after one pDDS batch. Migrated velocities are reset to zero. By default \code{PDDS.migration.frac=0.20}.
}
  \item{PDDS.min.distance}{
OPTIONAL. Only used when \code{method='spso2011-pdds'}. Finite numeric value in \eqn{[0,1]} defining the minimum root-mean-square distance between preferred archive entries after normalising every parameter to its bounds. If too few separated entries exist, the best remaining solutions fill the archive so all requested chains can be seeded. By default \code{PDDS.min.distance=0.05}.
}
 \item{normalise}{
logical, indicates whether the parameter values have to be normalised to the [0,1] interval during the optimisation or not. \cr

This option appears in the C and Matlab version of SPSO-2011 (See https://www.particleswarm.info/standard_pso_2011_c.zip) and there it is recommended to use this option when the search space is not an hypercube. If the search space is an hypercube, it is better not normalise (there is a small difference between the position without any normalisation and the de-normalised one). By default \code{normalise=FALSE}
}
  \item{IW.type}{
OPTIONAL. Used only when \code{use.IW= TRUE} AND \code{length(IW.w)>1}\cr

character, defines how the inertia weight \env{w} will vary along iterations. Valid values are: \cr

-)\kbd{linear}: \env{w} varies linearly between the initial and final values specified in \code{IW.w} (see Shi and Eberhart, 1998; Zheng et al., 2003). This is the DEFAULT option \cr

-)\kbd{non-linear}: \env{w} varies non-linearly between the initial and final values specified in \code{IW.w} with exponential factor \kbd{IW.exp} (see Chatterjee and Siarry, 2006) \cr

-)\kbd{runif}: \env{w} is a uniform random variable in the range \env{[w.min, w.max]} specified in \code{IW.w}. It is a generalisation of the weight proposed in Eberhart and Shi (2001b) \cr

-)\kbd{aiwf}: adaptive inertia weight factor, where the inertia weight is varied adaptively depending on the goodness-of-fit values of the particles (see Liu et al., 2005) \cr

-)\kbd{GLratio}: \env{w} varies according to the ratio between the global best and the average of the particle's local best (see Arumugam and Rao, 2008) \cr

By default \code{IW.type='linear'}
}
  \item{IW.exp}{
OPTIONAL. Used only when \code{use.IW=TRUE} AND \code{IW.type='non-linear'} \cr

numeric, non-linear modulation index (see Chatterjee and Siarry, 2006) \cr

When \code{IW.type='linear'}, \code{IW.exp} is set to 1. By default \code{IW.exp=1}
}
 \item{use.TVc1}{
logical, indicates if the cognitive acceleration coefficient \code{c1} will have a time-varying value instead of a constant one provided by the user (see Ratnaweera et al. 2004). By default \code{use.TVc1=FALSE}
}
  \item{TVc1.type}{
character, required only when \code{use.TVc1 = TRUE}. Valid values are: \cr

-)\kbd{linear}: \env{c1} varies linearly between the initial and final values specified in \code{TVc1.rng} (see Ratnaweera et al., 2004) \cr

-)\kbd{non-linear}: \env{c1} varies non-linearly between the initial and final values specified in \code{TVc1.rng}. Proposed by the authors of hydroPSO taking into account the work of Chatterjee and Siarry (2006) for the inertia weight \cr

-)\kbd{GLratio}: \env{c1} varies according to the ratio between the global best and the average of the particle's local best (see Arumugam and Rao, 2008) \cr

By default \code{TVc1.type='linear'}
}
  \item{TVc1.rng}{
OPTIONAL. Used only when \code{use.TVc1=TRUE} AND \code{TVc1.type!='GLratio'} \cr

numeric, initial and final values for the cognitive acceleration coefficient \env{[c1.ini, c1.fin]} (in that order) along the iterations \cr

By default \code{TVc1.rng=c(1.28, 1.05)}
}
  \item{TVc1.exp}{
OPTIONAL. Used only when \code{use.TVc1= TRUE} AND \code{TVc1.type= 'non-linear'} \cr

numeric, non-linear modulation index \cr

When \code{TVc1.exp} is equal to 1, \code{TVc1} corresponds to the improvement proposed by Ratnaweera et al., (2004), whereas when \code{TVc1.exp} is different from one, no reference has been found in literature by the authors, but it was included as an option based on the work of Chatterjee and Siarry (2006) for the inertia weight \cr

When \code{TVc1.type='linear'}, \code{TVc1.exp} is automatically set to 1. By default \code{TVc1.exp=1}
}
 \item{use.TVc2}{
logical, indicates whether the social acceleration coefficient \code{c2} will have a time-varying value or a constant one provided by the user (see Ratnaweera et al. 2004). By default \code{use.TVc2=FALSE}
}
  \item{TVc2.type}{
character, required only when \code{use.TVc2=TRUE}. Valid values are: \cr

-)\kbd{linear}: \env{c2} varies linearly between the initial and final values specified in \code{TVc2.rng} (see Ratnaweera et al. 2004) \cr

-)\kbd{non-linear}: \env{c2} varies non-linearly between the initial and final values specified in \code{TVc2.rng}. Proposed by the authors of hydroPSO taking into account the work of Chatterjee and Siarry (2006) for the inertia weight \cr

By default \code{TVc2.type='linear'}
}
  \item{TVc2.rng}{
OPTIONAL. Used only when \code{use.TVc2=TRUE} \cr

numeric, initial and final values for the social acceleration coefficient \env{[c2.ini, c2.fin]} (in that order) along the iterations \cr

By default \code{TVc2.rng=c(1.05, 1.28)}
}
  \item{TVc2.exp}{
OPTIONAL. Used only when \code{use.TVc2= TRUE} AND \code{TVc2.type='non-linear'} \cr

numeric, non-linear modulation index \cr

When \code{TVc2.exp} is equal to 1, \code{TVc2} corresponds to the improvement proposed by Ratnaweera et al., 2004, whereas when \code{TVc2.exp} is different from one, no reference has been found in literature by the authors, but it was included as an option based on the work of Chatterjee and Siarry (2006) for the inertia weight \cr

When \code{TVc2.type= linear}, \code{TVc2.exp} is automatically set to 1. By default \code{TVc2.exp=1}
}
 \item{use.TVlambda}{
logical, indicates whether the percentage to limit the maximum velocity \code{lambda} will have a time-varying value or a constant value provided by the user. Proposed by the authors of hydroPSO based on the work of Chatterjee and Siarry (2006) for the inertia weight \cr

By default \code{use.TVlambda=FALSE}
}
  \item{TVlambda.type}{
character, required only when \code{use.TVlambda=TRUE}. Valid values are: \cr

-)\kbd{linear}: \env{TVvmax} varies linearly between the initial and final values specified in \code{TVlambda.rng} \cr

-)\kbd{non-linear}: \env{TVvmax} varies non-linearly between the initial and final values specified in \code{TVlambda.rng} \cr

By default \code{TVlambda.type='linear'}
}
  \item{TVlambda.rng}{
OPTIONAL. Used only when \code{use.TVlambda=TRUE} \cr

numeric, initial and final values for the percentage to limit the maximum velocity \env{[TVlambda.ini, TVlambda.fin]} (in that order) along the iterations \cr

By default \code{TVlambda.rng=c(1, 0.25)}
}
  \item{TVlambda.exp}{
OPTIONAL. only required when \code{use.TVlambda= TRUE} AND \code{TVlambda.type='non-linear'} \cr

numeric, non-linear modulation index \cr

When \code{TVlambda.type='linear'}, \code{TVlambda.exp} is automatically set to 1. By default \code{TVlambda.exp=1}
}
  \item{use.RG}{
logical, indicates if the swarm should be regrouped when premature convergence is detected. By default \code{use.RG=FALSE} \cr

When \code{use.RG=TRUE} the swarm is regrouped in a search space centred around the current global best. This updated search space is hoped to be both small enough for efficient search and large enough to allow the swarm to escape from stagnation (see Evers and Ghalia, 2009) \cr

There are 4 differences wrt Evers and Ghalia (2009): \cr

-) swarm radius: median is used instead of max \cr

-) computation of the new range of parameter space, which corresponds to the boundaries of the whole swarm at a given iteration, instead of the maximum values of \sQuote{abs(x-Gbest)} \cr

-) regrouping factor: \code{RG.r} instead of \sQuote{6/(5*ro)} \cr

-) velocity is re-initialized using \code{Vini.type} instead of using the formula proposed by Evers and Ghalia (2009) 
}
  \item{RG.thr}{
ONLY required when \code{use.RG=TRUE} \cr

numeric, positive number representing the \kbd{stagnation threshold} used to decide whether the swarm has to be regrouped or not. See Evers and Galia (2009) for further details \cr

Regrouping occurs when the \emph{normalised swarm radius} is less than \code{RG.thr}. By default \code{RG.thr=1E-5}
}
  \item{RG.r}{
ONLY required when \code{use.RG=TRUE}. \cr

numeric, positive number representing the \kbd{regrouping factor}, which is used to regroup the swarm in a search space centred around the current global best (see Evers and Galia, 2009 for further details). By default \code{RG.thr=2}
}
  \item{RG.miniter}{
ONLY required when \code{use.RG=TRUE} \cr

numeric, minimum number of iterations needed before each new regrouping. By default \code{RG.miniter=100}
}
%%  \item{use.DS}{
%%CPSO
%%}
  \item{plot}{
logical, indicates if a two-dimensional plot with the particles' position will be drawn after each iteration. For high dimensional functions, only the first two dimensions of all the particles are plotted \cr

By default \code{plot=FALSE}
}
  \item{out.with.pbest}{
logical, indicates if the best parameter values for each particle and their goodness-of-fit will be included in the output of the algorithm \cr

By default \code{out.with.pbest=FALSE}
}
  \item{out.with.fit.iter}{
logical, indicates if the goodness-of-fit of each particle for each iteration will be included in the output of the algorithm \cr

By default \code{out.with.fit.iter=FALSE}
}
  \item{write2disk}{
logical, indicates if the output files will be written to the disk. By default \code{write2disk=FALSE}. When \code{write2disk=TRUE}, \code{drty.out} must be provided explicitly.
}
  \item{verbose}{
logical, indicates if progress messages are to be printed. By default \code{verbose=TRUE}
} 
  \item{REPORT}{
OPTIONAL. Used only when \code{verbose=TRUE} \cr

The frequency of report messages printed to the screen. Default to every 100 iterations
} 
  \item{parallel}{
character, indicates how to parallelise \sQuote{hydroPSO} (to be precise, only the evaluation of the objective function \code{fn} is parallelised). Valid values are: \cr

-)\kbd{none}: no parallelisation is made (this is the default value) \cr

-)\kbd{multicore}: DEPRECATED. The former \pkg{multicore} package is no longer available from CRAN. For backward compatibility, \code{parallel="multicore"} is automatically changed to \code{parallel="parallel"} on Unix-like systems and to \code{parallel="parallelWin"} on Microsoft Windows. \cr

-)\kbd{parallel}: parallel computations using a \sQuote{FORK} cluster created with \code{\link[parallel]{makeForkCluster}}. FORK clusters are available only on Unix-like operating systems, such as GNU/Linux and macOS. They are not supported on Microsoft Windows; on Windows, \code{parallel="parallel"} is automatically changed to \code{parallel="parallelWin"}. \cr

-)\kbd{parallelWin}: parallel computations using a \sQuote{PSOCK} cluster created with \code{\link[parallel]{makeCluster}}. PSOCK clusters are the only parallel cluster option supported by \pkg{parallel} on Microsoft Windows, and they also work on Unix-like systems. \cr

For both \code{parallel="parallel"} and \code{parallel="parallelWin"}, \code{fn.name="hydromod"} and \code{fn.name="hydromodInR"} model runs are distributed one parameter set at a time with the \code{\link[parallel]{clusterApply}} function of the \pkg{parallel} package. Ordinary R objective functions are evaluated row-wise with the \code{\link[parallel]{parRapply}} function of the \pkg{parallel} package. \cr

With \code{method="spso2011-sa"}, both SPSO-2011 swarm batches and SA replica sweeps use the requested \code{parallel} backend for ordinary R objective functions, R-internal models selected with \code{fn="hydromodInR"}, and R-external models selected with \code{fn="hydromod"}. Each full SA sweep contains one candidate per active replica. External candidates use distinct worker model directories, and archive updates, replica exchange, Metropolis decisions, and scout repopulation run on the main process only after the candidate batch completes. The realised additional cost is reported in \code{SA.diagnostics$attempted} and is bounded by \code{SA.budget.fraction} and \code{maxfn}. \cr

With \code{method="spso2011-de"}, the DE mutation is part of each particle's position update and does not add objective-function evaluations. Ordinary R objective functions, R-internal models selected with \code{fn="hydromodInR"}, and R-external models selected with \code{fn="hydromod"} all use the requested \code{parallel} backend for each swarm evaluation batch. Informant-archive maintenance, mutation-factor adaptation, and mutation are computed on the main R process between batches because they modify shared swarm memory and do not run the model. R-external workers continue to use isolated copies of the model directory. \cr

With \code{method="spso2011-cde"}, every generation evaluates exactly one SPSO or DE candidate per particle as one \code{npart}-sized batch. The requested \code{parallel} backend is therefore used without extra sequential objective calls for ordinary R functions, R-internal models selected with \code{fn="hydromodInR"}, and R-external models selected with \code{fn="hydromod"}. Operator allocation, SHADE memories, greedy DE selection, archive maintenance, and eigen-basis adaptation run on the main R process after a batch completes. R-external candidates retain the standard isolated worker model directories. \cr

With \code{method="spso2011-dds"}, ordinary R objective functions, R-internal models selected with \code{fn="hydromodInR"}, and R-external models selected with \code{fn="hydromod"} use the requested \code{parallel} backend for every SPSO-2011 swarm batch. Once stagnation activates DDS, its single global-best proposal is evaluated sequentially on the main R process after each batch, adding at most one function call per active iteration and never exceeding an available \code{maxfn} slot. For an R-external model, the DDS evaluation reuses one isolated worker copy only after the parallel batch has completed, so it cannot collide with an active external-model evaluation. \cr

With \code{method="spso2011-pdds"}, both SPSO-2011 and pDDS batches contain exactly \code{npart} candidates and use the requested \code{parallel} backend. Ordinary R objectives are evaluated row-wise, R-internal model candidates are distributed one at a time, and R-external candidates use the same isolated worker directories as swarm particles. Archive updates, greedy chain selection, and migration occur on the main R process only after the full evaluation batch completes. Consequently, pDDS adds no sequential model calls and the total function-call accounting is identical to other fixed-size swarm methods for the same number of completed batches. \cr

On Unix-like systems, FORK clusters are usually faster than PSOCK clusters because workers inherit the parent R session and require less explicit object/package export. PSOCK clusters are more portable but usually have higher startup and data-transfer overhead, especially when large objects must be sent to workers.
} 
  \item{par.nnodes}{
OPTIONAL. Used only when \code{parallel!='none'} \cr

numeric, indicates the number of cores/CPUs to be used in the local multi-core machine, or the number of nodes to be used in the network cluster. \cr

By default \code{par.nnodes} is set to the amount of cores detected by the function \code{detectCores()} (\pkg{parallel} package)
} 
  \item{par.pkgs}{
OPTIONAL. Used only when \code{parallel='parallelWin'} \cr

list of package names (as characters) that need to be loaded on each node for allowing the objective function \code{fn} to be evaluated
} 
  \item{par.env}{
OPTIONAL. Used only when \code{parallel!='none'} \cr

environment from which the objects required by the objective function \code{fn} are exported to the parallel workers. By default, the environment from which \code{hydroPSO} is called is used
} 
  \item{par.export}{
OPTIONAL. Used only when \code{parallel!='none'} \cr

character vector with the names of additional objects to be exported from \code{par.env} to the parallel workers. By default, all functions in \code{par.env} are exported
} 
}
}

\value{
A list, compatible with the output from \code{\link[stats]{optim}}, with components:

\item{par}{
optimum parameter set found    
}
\item{value}{
value of \code{fn} corresponding to \code{par}
}
\item{counts}{
three-element vector containing the total number of function calls, number of iterations, and number of regroupings
}
\item{convergence}{
integer code where \code{0} indicates that the algorithm terminated by reaching the absolute tolerance, otherwise:

\describe{
 \item{1}{relative tolerance reached}
 \item{2}{maximum number of (effective) function evaluations reached}
 \item{3}{maximum number of iterations reached}
}
}
\item{message}{character string giving human-friendly information about \code{convergence}
}
\item{SA.diagnostics}{only returned when \code{method="spso2011-sa"}. A list containing proposal, acceptance, uphill-acceptance, invalid, global-improvement, activation, reheating, replica-swap, scout-injection, collapsed-event, emergency-event, productive-event, and unproductive-event counts. It also contains the cold-replica compatibility fields \code{initial.temperature}, \code{final.temperature}, \code{final.scale}, \code{current.par}, and \code{current.value}; the complete final temperature ladder, replica states, values, and scales; the quality-diversity archive; remaining scout-protection counters; and the realised SA evaluation budget. These diagnostics expose both search cost and information transfer.
}
\item{CDE.diagnostics}{only returned when \code{method="spso2011-cde"}. A list containing SPSO and DE evaluation counts, accepted DE trials, emergency DE/rand/1 evaluations, original- and eigen-coordinate evaluations, the final DE population fraction, diversity and stagnation state, operator and coordinate scores, final SHADE memories, and the global archive of replaced parents. These diagnostics expose the realised competitive allocation and adaptation state.
}
\item{PDDS.diagnostics}{only returned when \code{method="spso2011-pdds"}. A list containing the numbers of SPSO and pDDS batches, pDDS evaluations, improving chain updates and swarm migrations; the final archive size; and the final chain perturbation scales, parameter vectors and fitness values. These diagnostics expose the portfolio allocation and the realised DDS contribution.
}
}
\references{

\cite{Abdelaziz, Ramadan, and Zambrano-Bigiarini, Mauricio (2014). Particle Swarm Optimization for inverse modeling of solute transport in fractured gneiss aquifer. Journal of Contaminant Hydrology, 164, 285-298. doi:10.1016/j.jconhyd.2014.06.003}

\cite{Asgari, M.; Yang, W.; Lindsay, J.; Shao, H.; Liu, Y.; De Queiroga Miranda, R.; Mehri Dehnavi, M. (2023). Development of a knowledge-sharing parallel computing approach for calibrating distributed watershed hydrologic models. Environmental Modelling & Software, 164, 105708. doi:10.1016/j.envsoft.2023.105708}

\cite{Clerc, M. (2012). Standard Particle Swarm Optimisation: From 2006 to 2011. HAL open archive, hal-00764996. \url{https://hal.science/hal-00764996}}

\cite{Clerc, M. From Theory to Practice in Particle Swarm Optimization, Handbook of Swarm Intelligence, Springer Berlin Heidelberg, 3-36, Eds: Panigrahi, Bijaya Ketan, Shi, Yuhui, Lim, Meng-Hiot, Hiot, Lim Meng, and Ong, Yew Soon, 2010, doi: 10.1007/978-3-642-17390-5_1}

\cite{Clerc, M., Stagnation Analysis in Particle Swarm Optimisation or what happens when nothing happens. Technical Report. 2006. \url{https://hal.science/hal-00122031}}

\cite{Clerc, M. Particle Swarm Optimization. ISTE, 2005}

\cite{Corana, A.; Marchesi, M.; Martini, C.; Ridella, S. (1987). Minimizing multimodal functions of continuous variables with the simulated annealing algorithm. ACM Transactions on Mathematical Software, 13(3), 262-280. doi:10.1145/29380.29864}

\cite{Clerc, M and J Kennedy. The particle swarm - explosion, stability, and convergence in a multidimensional complex space. IEEE Transactions On Evolutionary Computation, 6:58-73, 2002. doi:10.1109/4235.985692}

\cite{Chatterjee, A. and Siarry, P. Nonlinear inertia weight variation for dynamic adaptation in particle swarm optimization, Computers and Operations Research, Volume 33, Issue 3, March 2006, Pages 859-871, ISSN 0305-0548, DOI: 10.1016/j.cor.2004.08.012}

\cite{Eberhart, R.C.; Shi, Y.; Comparing inertia weights and constriction factors in particle swarm optimization. Evolutionary Computation, 2000. Proceedings of the 2000 Congress on , vol.1, no., pp.84-88 vol.1, 2000. doi: 10.1109/CEC.2000.870279}

\cite{Evers, G.I.; Ben Ghalia, M. Regrouping particle swarm optimization: A new global optimization algorithm with improved performance consistency across benchmarks. Systems, Man and Cybernetics, 2009. SMC 2009. IEEE International Conference on , vol., no., pp.3901-3908, 11-14 Oct. 2009. doi: 10.1109/ICSMC.2009.5346625}

\cite{Huang, T.; Mohan, A.S.; , A hybrid boundary condition for robust particle swarm optimization. Antennas and Wireless Propagation Letters, IEEE , vol.4, no., pp. 112-117, 2005. doi: 10.1109/LAWP.2005.846166}

\cite{Hukushima, K.; Nemoto, K. (1996). Exchange Monte Carlo method and application to spin glass simulations. Journal of the Physical Society of Japan, 65(6), 1604-1608. doi:10.1143/JPSJ.65.1604}

\cite{Javidrad, F.; Nazari, M. (2017). A new hybrid particle swarm and simulated annealing stochastic optimization method. Applied Soft Computing, 60, 634-654. doi:10.1016/j.asoc.2017.07.023}

\cite{Kennedy, J. and R. Eberhart. Particle Swarm Optimization. in proceedings IEEE international conference on Neural networks. pages 1942-1948. 1995. doi: 10.1109/ICNN.1995.488968}
        
\cite{Kennedy, J.; Small worlds and mega-minds: effects of neighborhood topology on particle swarm performance. Evolutionary Computation, 1999. CEC 99. Proceedings of the 1999 Congress on , vol.3, no., pp.3 vol. (xxxvii+2348), 1999. doi: 10.1109/CEC.1999.785509}

\cite{Kennedy, J.; Mendes, R.. Population structure and particle swarm performance. Evolutionary Computation, 2002. CEC '02. Proceedings of the 2002 Congress on , vol.2, no., pp.1671-1676, 2002. doi: 10.1109/CEC.2002.1004493}

\cite{Kennedy, J.; Mendes, R.; , Neighborhood topologies in fully-informed and best-of-neighborhood particle swarms. Soft Computing in Industrial Applications, 2003. SMCia/03. Proceedings of the 2003 IEEE International Workshop on , vol., no., pp. 45- 50, 23-25 June 2003. doi: 10.1109/SMCIA.2003.1231342}

\cite{Kennedy, J. 2006. Swarm intelligence, in Handbook of Nature-Inspired and Innovative Computing, edited by A. Zomaya, pp. 187-219, Springer US, doi:10.1007/0-387-27705-6_6}

\cite{Kirkpatrick, S.; Gelatt, C. D.; Vecchi, M. P. (1983). Optimization by simulated annealing. Science, 220(4598), 671-680. doi:10.1126/science.220.4598.671}

\cite{Liu, B. and L. Wang, Y.-H. Jin, F. Tang, and D.-X. Huang. Improved particle swarm optimization combined with chaos. Chaos, Solitons and Fractals, vol. 25, no. 5, pp.1261-1271, Sep. 2005. doi:10.1016/j.chaos.2004.11.095}

\cite{Liu, Z.-Z.; Wang, Y.; Yang, S.; Tang, K. (2019). An adaptive framework to tune the coordinate systems in nature-inspired optimization algorithms. IEEE Transactions on Cybernetics, 49(4), 1403-1416. doi:10.1109/TCYB.2018.2802912}

\cite{Li, K.; Fialho, A.; Kwong, S.; Zhang, Q. (2014). Adaptive operator selection with bandits for a multiobjective evolutionary algorithm based on decomposition. IEEE Transactions on Evolutionary Computation, 18(1), 114-130. doi:10.1109/TEVC.2013.2239648}

\cite{Lynn, N.; Ali, M. Z.; Suganthan, P. N. (2018). Population topologies for particle swarm optimization and differential evolution. Swarm and Evolutionary Computation, 39, 24-35. doi:10.1016/j.swevo.2017.11.002}

\cite{Mendes, R.; Kennedy, J.; Neves, J. The fully informed particle swarm: simpler, maybe better. Evolutionary Computation, IEEE Transactions on , vol.8, no.3, pp. 204-210, June 2004. doi: 10.1109/TEVC.2004.826074}

\cite{Metropolis, N.; Rosenbluth, A. W.; Rosenbluth, M. N.; Teller, A. H.; Teller, E. (1953). Equation of state calculations by fast computing machines. The Journal of Chemical Physics, 21(6), 1087-1092. doi:10.1063/1.1699114}

\cite{Ratnaweera, A.; Halgamuge, S.K.; Watson, H.C. Self-organizing hierarchical particle swarm optimizer with time-varying acceleration coefficients. Evolutionary Computation, IEEE Transactions on , vol.8, no.3, pp. 240- 255, June 2004. doi: 10.1109/TEVC.2004.826071}

\cite{Robinson, J.; Rahmat-Samii, Y.; Particle swarm optimization in electromagnetics. Antennas and Propagation, IEEE Transactions on , vol.52, no.2, pp. 397-407, Feb. 2004. doi: 10.1109/TAP.2004.823969}

\cite{Sagar, A.; LeCover, R.; Shoemaker, C.; Varner, J. (2018). Dynamic Optimization with Particle Swarms (DOPS): a meta-heuristic for parameter estimation in biochemical models. BMC Systems Biology, 12, 87. doi:10.1186/s12918-018-0610-x}

\cite{Shi, Y.; Eberhart, R. A modified particle swarm optimizer. Evolutionary Computation Proceedings, 1998. IEEE World Congress on Computational Intelligence. The 1998 IEEE International Conference on , vol., no., pp.69-73, 4-9 May 1998. doi: 10.1109/ICEC.1998.699146}

\cite{Schor, D.; Kinsner, W.; Anderson, J.; A study of optimal topologies in swarm intelligence. Electrical and Computer Engineering (CCECE), 2010 23rd Canadian Conference on , vol., no., pp.1-8, 2-5 May 2010. doi: 10.1109/CCECE.2010.5575132}

\cite{Shieh, H.-L.; Kuo, C.-C.; Chiang, C.-M. (2011). Modified particle swarm optimization algorithm with simulated annealing behavior and its numerical verification. Applied Mathematics and Computation, 218(8), 4365-4383. doi:10.1016/j.amc.2011.10.012}

\cite{Swendsen, R. H.; Wang, J.-S. (1986). Replica Monte Carlo simulation of spin-glasses. Physical Review Letters, 57(21), 2607-2609. doi:10.1103/PhysRevLett.57.2607}

\cite{Storn, R.; Price, K. (1997). Differential Evolution - A simple and efficient heuristic for global optimization over continuous spaces. Journal of Global Optimization, 11(4), 341-359. doi:10.1023/A:1008202821328}

\cite{Tanabe, R.; Fukunaga, A. (2013). Success-history based parameter adaptation for Differential Evolution. In Proceedings of the 2013 IEEE Congress on Evolutionary Computation (CEC), 71-78. doi:10.1109/CEC.2013.6557555}

\cite{Tolson, B. A.; Shoemaker, C. A. (2007). Dynamically dimensioned search algorithm for computationally efficient watershed model calibration. Water Resources Research, 43, W01413. doi:10.1029/2005WR004723}

\cite{Yong-Ling Zheng; Long-Hua Ma; Li-Yan Zhang; Ji-Xin Qian. On the convergence analysis and parameter selection in particle swarm optimization. Machine Learning and Cybernetics, 2003 International Conference on , vol.3, no., pp. 1802-1807 Vol.3, 2-5 Nov. 2003. doi: 10.1109/ICMLC.2003.1259789}

\cite{Zambrano-Bigiarini, M.; R. Rojas (2013), A model-independent Particle Swarm Optimisation software for model calibration, Environmental Modelling & Software, 43, 5-25, doi:10.1016/j.envsoft.2013.01.004}

\cite{Zambrano-Bigiarini, M.; Clerc, M.; Rojas, R. (2013). Standard Particle Swarm Optimisation 2011 at CEC-2013: A baseline for future PSO improvements. In Proceedings of the 2013 IEEE Congress on Evolutionary Computation (CEC), 2337-2344. doi:10.1109/CEC.2013.6557848}

\cite{Zhang, J.; Sanderson, A. C. (2009). JADE: Adaptive Differential Evolution with Optional External Archive. IEEE Transactions on Evolutionary Computation, 13(5), 945-958. doi:10.1109/TEVC.2009.2014613}

\cite{Zhao, B.  An Improved Particle Swarm Optimization Algorithm for Global Numerical Optimization. In Proceedings of International Conference on Computational Science (1). 2006, 657-664}
}
\author{
Mauricio Zambrano-Bigiarini, \email{mzb.devel@gmail.com}
}
\note{
Note for \code{\link[stats]{optim}} users: \cr

1) In \kbd{hydroPSO} the length of \code{lower} and \code{upper} are used to define the dimension of the solution space (not the length of \code{par}) \cr

2) In \kbd{hydroPSO}, \code{par} may be omitted. If not omitted, the  \env{m} parameter sets provided by the user for \code{par} are used to overwrite the first \env{m} parameter sets randomly defined according to the value of \code{Xini.type} \cr
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[stats]{optim}}
}
\examples{
# Number of dimensions of the optimisation problem (for all the examples)
D <- 5

# Boundaries of the search space (Rastrigin function)
lower <- rep(-5.12, D)
upper <- rep(5.12, D)

\donttest{
local({

################################ 
# Example 1. Basic use         #
################################ 

# Setting the seed (for reproducible results)         
set.seed(100)

# Basic use 1. Rastrigin function (non-linear and multi-modal with many local minima)
# Results are not saved to the hard disk by default
hydroPSO(fn=rastrigin, lower=lower, upper=upper)

}) # local END
}  # donttest END

\donttest{
local({

# Setting the user temporal directory as working directory
oldwd <- getwd()      
on.exit(setwd(oldwd), add = TRUE) 
setwd(tempdir())

# Basic use 2. Rastrigin function (non-linear and multimodal with many local minima)
# Results are saved to the hard disk. Slower than before but results are kept for
# future inspection
pso.dir <- file.path(tempdir(), "PSO.out")
hydroPSO(fn=rastrigin, lower=lower, upper=upper,
         control=list(write2disk=TRUE, drty.out=pso.dir))

# Plotting the results, by default into the active graphic device
# 'MinMax="min"' indicates a minimisation problem
plot_results(drty.out=pso.dir, MinMax="min") 

# Plotting the results into PNG files. 
plot_results(drty.out=pso.dir, MinMax="min", do.png=TRUE)   
 
}) # local END    
} # donttest END


\donttest{
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
} # donttest END

}
\keyword{optimize}
\keyword{files}

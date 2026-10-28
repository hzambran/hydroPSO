test_that("hydroPSO exposes the requested optimisation methods", {
  methods <- eval(formals(hydroPSO)$method)

  expect_identical(methods,
                   c("spso2011", "spso2007", "ipso", "fips", "wfips",
                     "canonical", "spso2011-sa"))
  expect_error(hydroPSO(method="invalid"), "should be one of")
})

test_that("spso2011-sa adds one sequential SA evaluation per iteration", {
  sphere <- function(x) sum(x^2) # Provides a deterministic scalar minimisation objective.
  control <- list(npart=5, maxit=2, reltol=0, verbose=FALSE, REPORT=2,
                  SA.T0=1, SA.alpha=0.8, SA.scale=0.05)

  set.seed(42)
  out <- suppressMessages(
    hydroPSO(fn=sphere, method="spso2011-sa", lower=c(-1, -1), upper=c(1, 1),
             control=control)
  )

  expect_length(out$par, 2)
  expect_true(is.finite(out$value))
  expect_equal(unname(out$counts["iterations"]), 2)
  expect_equal(unname(out$counts["function.calls"]), 12)
})

test_that("spso2011-sa validates its SA controls", {
  sphere <- function(x) sum(x^2) # Provides the minimal objective needed to reach control validation.
  control <- list(npart=5, maxit=1, reltol=0, verbose=FALSE, REPORT=1)

  expect_error(
    hydroPSO(fn=sphere, method="spso2011-sa", lower=-1, upper=1,
             control=modifyList(control, list(SA.T0=0))),
    "control\\$SA.T0"
  )
  expect_error(
    hydroPSO(fn=sphere, method="spso2011-sa", lower=-1, upper=1,
             control=modifyList(control, list(SA.alpha=1))),
    "control\\$SA.alpha"
  )
  expect_error(
    hydroPSO(fn=sphere, method="spso2011-sa", lower=-1, upper=1,
             control=modifyList(control, list(SA.scale=0))),
    "control\\$SA.scale"
  )
})

test_that("spso2011-sa supports R-based hydrological models", {
  model_fun <- function(param.values, obs) {
    sim <- sum(param.values) # Provides a minimal model output for the wrapper contract.
    GoF <- sum((param.values - obs)^2) # Computes a scalar objective for optimisation.
    list(sim=sim, GoF=GoF)
  } # 'model_fun' end
  control <- list(npart=5, maxit=1, reltol=0, verbose=FALSE, REPORT=1,
                  SA.T0=1, SA.alpha=0.8, SA.scale=0.05)

  set.seed(7)
  out <- suppressMessages(
    hydroPSO(fn="hydromodInR", method="spso2011-sa", lower=c(a=0, b=0),
             upper=c(a=1, b=1), control=control, model.FUN=model_fun,
             model.FUN.args=list(obs=c(a=0.25, b=0.75)))
  )

  expect_true(is.finite(out$value))
  expect_equal(unname(out$counts["function.calls"]), 6)
})

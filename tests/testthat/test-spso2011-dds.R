test_that("DDS dimension masks follow the logarithmic proposal schedule", {
  set.seed(21)
  early <- hydroPSO:::dds.dimension.mask(n=8, t=1, tmax=100)
  late <- hydroPSO:::dds.dimension.mask(n=8, t=100, tmax=100)

  expect_true(all(early)) # A probability of one selects every early dimension.
  expect_equal(sum(late), 1) # The DDS fallback selects one final dimension.

  set.seed(22)
  expected <- stats::runif(8) < (1 - log(10)/log(100))
  set.seed(22)
  observed <- hydroPSO:::dds.dimension.mask(n=8, t=10, tmax=100)
  expect_identical(observed, expected) # The intermediate mask uses the requested formula.
})

test_that("DDS reflection keeps Gaussian proposals inside parameter bounds", {
  reflected <- hydroPSO:::dds.reflect(x=c(-2.5, 3.5, 4), lower=c(0, 0, 4),
                                      upper=c(1, 1, 4))

  expect_equal(reflected, c(0.5, 0.5, 4))
})

test_that("spso2011-dds is reproducible and activates after stagnation", {
  constant <- function(x) {
    1 # Forces stagnation after the first evaluated generation.
  } # 'constant' end
  control <- list(npart=5, maxit=3, reltol=0, verbose=FALSE, REPORT=3,
                  DDS.r=0.20, DDS.stag.iter=1, out.with.fit.iter=TRUE)

  set.seed(42)
  out1 <- suppressMessages(
    hydroPSO(fn=constant, method="spso2011-dds", lower=c(-1, -1), upper=c(1, 1),
             control=control)
  )
  set.seed(42)
  out2 <- suppressMessages(
    hydroPSO(fn=constant, method="spso2011-dds", lower=c(-1, -1), upper=c(1, 1),
             control=control)
  )

  expect_equal(out1$par, out2$par)
  expect_equal(out1$value, out2$value)
  expect_true(all(out1$par >= -1 & out1$par <= 1))
  expect_equal(dim(out1$part.fit.per.iter), c(3, 5))
  expect_equal(unname(out1$counts["function.calls"]), 17)
})

test_that("spso2011-dds validates its DDS controls", {
  sphere <- function(x) {
    sum(x^2) # Provides the minimal objective needed for control validation.
  } # 'sphere' end
  control <- list(npart=5, maxit=1, reltol=0, verbose=FALSE, REPORT=1)

  expect_error(
    hydroPSO(fn=sphere, method="spso2011-dds", lower=-1, upper=1,
             control=modifyList(control, list(DDS.r=0))),
    "control\\$DDS.r"
  )
  expect_error(
    hydroPSO(fn=sphere, method="spso2011-dds", lower=-1, upper=1,
             control=modifyList(control, list(DDS.stag.iter=1.5))),
    "control\\$DDS.stag.iter"
  )
})

test_that("spso2011-dds counts DDS proposals against maxfn", {
  constant <- function(x) {
    1 # Forces stagnation while keeping the objective finite.
  } # 'constant' end
  control <- list(npart=4, maxit=3, maxfn=8, reltol=0, verbose=FALSE,
                  REPORT=3, DDS.stag.iter=1)

  set.seed(43)
  out <- suppressMessages(
    hydroPSO(fn=constant, method="spso2011-dds", lower=c(-1, -1),
             upper=c(1, 1), control=control)
  )

  expect_equal(unname(out$counts["function.calls"]), 8)
})

test_that("spso2011-dds parallelizes ordinary R objective functions", {
  objective <- function(x) {
    1 # Forces one parent-process DDS proposal in generation two.
  } # 'objective' end
  control <- list(npart=4, maxit=2, reltol=0, verbose=FALSE, REPORT=2,
                  DDS.stag.iter=1,
                  parallel="parallelWin", par.nnodes=2,
                  par.env=environment())

  set.seed(101)
  out <- suppressMessages(
    hydroPSO(fn=objective, method="spso2011-dds", lower=c(-1, -1), upper=c(1, 1),
             control=control)
  )

  expect_true(is.finite(out$value))
  expect_equal(unname(out$counts["function.calls"]), 9)
})

test_that("spso2011-dds parallelizes R-internal models", {
  model_fun <- function(param.values, obs) {
    sim <- sum(param.values) # Produces a minimal deterministic model output.
    GoF <- 1 # Forces one parent-process DDS proposal in generation two.
    list(sim=sim, GoF=GoF)
  } # 'model_fun' end
  control <- list(npart=4, maxit=2, reltol=0, verbose=FALSE, REPORT=2,
                  DDS.stag.iter=1,
                  parallel="parallelWin", par.nnodes=2, par.env=environment())

  set.seed(102)
  out <- suppressMessages(
    hydroPSO(fn="hydromodInR", method="spso2011-dds", lower=c(a=0, b=0),
             upper=c(a=1, b=1), control=control, model.FUN=model_fun,
             model.FUN.args=list(obs=c(a=0.25, b=0.75)))
  )

  expect_true(is.finite(out$value))
  expect_equal(unname(out$counts["function.calls"]), 9)
})

test_that("spso2011-dds parallelizes R-external models in isolated directories", {
  fixture <- make_external_model_fixture() # Creates a temporary executable-model layout.
  on.exit(unlink(fixture$model.drty, recursive=TRUE), add=TRUE)
  read_output <- function(file) {
    value <- as.numeric(trimws(readLines(file, warn=FALSE)[1]))
    c(value, value + 1)
  } # 'read_output' end
  external_gof <- function(sim, obs) {
    1 # Forces one parent-process DDS proposal in generation two.
  } # 'external_gof' end
  control <- list(drty.in=fixture$drty.in, npart=4, maxit=2, reltol=0,
                  verbose=FALSE, REPORT=2, DDS.stag.iter=1, parallel="parallelWin",
                  par.nnodes=2, par.pkgs="hydroPSO", par.env=environment())

  set.seed(103)
  out <- suppressMessages(
    hydroPSO(fn="hydromod", method="spso2011-dds", control=control,
             model.FUN=hydromod,
             model.FUN.args=list(param.files=fixture$param.files,
                                 param.ranges=fixture$param.ranges,
                                 model.drty=fixture$model.drty,
                                 exe.fname=test_rscript(), exe.args=fixture$script,
                                 out.FUN=read_output,
                                 out.FUN.args=list(file="input.txt"),
                                 gof.FUN=external_gof, obs=c(0, 0),
                                 verbose=FALSE))
  )

  expect_true(is.finite(out$value))
  expect_equal(unname(out$counts["function.calls"]), 9)
  expect_false(dir.exists(file.path(fixture$model.drty, "parallel")))
})

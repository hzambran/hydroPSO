test_that("DDS velocity masks follow the logarithmic evaluation schedule", {
  set.seed(21)
  early <- hydroPSO:::dds.velocity.mask(n=8, t=1, tmax=100)
  late <- hydroPSO:::dds.velocity.mask(n=8, t=100, tmax=100)

  expect_true(all(early)) # A probability of one selects every early dimension.
  expect_equal(sum(late), 1) # The DDS fallback selects one final dimension.

  set.seed(22)
  expected <- stats::runif(8) < (1 - log(10)/log(100))
  set.seed(22)
  observed <- hydroPSO:::dds.velocity.mask(n=8, t=10, tmax=100)
  expect_identical(observed, expected) # The intermediate mask uses the requested formula.
})

test_that("spso2011-dds is reproducible and adds no function evaluations", {
  sphere <- function(x) sum(x^2) # Provides a deterministic scalar minimisation objective.
  control <- list(npart=5, maxit=3, reltol=0, verbose=FALSE, REPORT=3,
                  out.with.fit.iter=TRUE)

  set.seed(42)
  out1 <- suppressMessages(
    hydroPSO(fn=sphere, method="spso2011-dds", lower=c(-1, -1), upper=c(1, 1),
             control=control)
  )
  set.seed(42)
  out2 <- suppressMessages(
    hydroPSO(fn=sphere, method="spso2011-dds", lower=c(-1, -1), upper=c(1, 1),
             control=control)
  )

  expect_equal(out1$par, out2$par)
  expect_equal(out1$value, out2$value)
  expect_true(all(out1$par >= -1 & out1$par <= 1))
  expect_equal(dim(out1$part.fit.per.iter), c(3, 5))
  expect_equal(unname(out1$counts["function.calls"]), 15)
})

test_that("spso2011-dds parallelizes ordinary R objective functions", {
  target <- c(0.25, -0.50) # Defines worker-side data used by the objective closure.
  objective <- function(x) sum((x - target)^2) # Computes one scalar objective on each worker.
  control <- list(npart=4, maxit=2, reltol=0, verbose=FALSE, REPORT=2,
                  parallel="parallelWin", par.nnodes=2,
                  par.env=environment(), par.export="target")

  set.seed(101)
  out <- suppressMessages(
    hydroPSO(fn=objective, method="spso2011-dds", lower=c(-1, -1), upper=c(1, 1),
             control=control)
  )

  expect_true(is.finite(out$value))
  expect_equal(unname(out$counts["function.calls"]), 8)
})

test_that("spso2011-dds parallelizes R-internal models", {
  model_fun <- function(param.values, obs) {
    sim <- sum(param.values) # Produces a minimal deterministic model output.
    GoF <- sum((param.values - obs)^2) # Computes the calibration objective.
    list(sim=sim, GoF=GoF)
  } # 'model_fun' end
  control <- list(npart=4, maxit=2, reltol=0, verbose=FALSE, REPORT=2,
                  parallel="parallelWin", par.nnodes=2, par.env=environment())

  set.seed(102)
  out <- suppressMessages(
    hydroPSO(fn="hydromodInR", method="spso2011-dds", lower=c(a=0, b=0),
             upper=c(a=1, b=1), control=control, model.FUN=model_fun,
             model.FUN.args=list(obs=c(a=0.25, b=0.75)))
  )

  expect_true(is.finite(out$value))
  expect_equal(unname(out$counts["function.calls"]), 8)
})

test_that("spso2011-dds parallelizes R-external models in isolated directories", {
  fixture <- make_external_model_fixture() # Creates a temporary executable-model layout.
  on.exit(unlink(fixture$model.drty, recursive=TRUE), add=TRUE)
  read_output <- function(file) {
    value <- as.numeric(trimws(readLines(file, warn=FALSE)[1]))
    c(value, value + 1)
  } # 'read_output' end
  external_gof <- function(sim, obs) {
    sum((sim - obs)^2)
  } # 'external_gof' end
  control <- list(drty.in=fixture$drty.in, npart=4, maxit=2, reltol=0,
                  verbose=FALSE, REPORT=2, parallel="parallelWin",
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
  expect_equal(unname(out$counts["function.calls"]), 8)
  expect_false(dir.exists(file.path(fixture$model.drty, "parallel")))
})

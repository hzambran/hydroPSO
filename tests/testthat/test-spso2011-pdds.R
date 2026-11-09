test_that("pDDS archives retain quality and multiple basins", {
  candidates <- rbind(c(0.10, 0.10), c(0.11, 0.11), c(0.90, 0.90),
                      c(0.50, 0.50))
  archive <- hydroPSO:::pdds.update.archive(
    archive.X=NULL, archive.fit=numeric(), candidates=candidates,
    fitness=c(1, 2, 3, 4), MinMax="min", max.size=3,
    min.distance=0.20, x.MinMax=cbind(c(0, 0), c(1, 1))
  )

  expect_equal(archive$fit, c(1, 3, 4))
  expect_equal(nrow(archive$X), 3)
})

test_that("pDDS batches use local schedules and reflected multi-scale proposals", {
  set.seed(31)
  generated <- hydroPSO:::pdds.generate.batch(
    chain.X=rbind(c(0.25, 0.25), c(0.75, 0.75)), chain.evals=c(0L, 0L),
    chain.r=c(0.05, 0.50), npart=6, epoch=10,
    x.MinMax=cbind(c(0, 0), c(1, 1))
  )

  expect_equal(generated$chain, c(1L, 2L, 1L, 2L, 1L, 2L))
  expect_equal(generated$evals, c(3, 3))
  expect_true(all(generated$X >= 0 & generated$X <= 1))
})

test_that("spso2011-pdds is reproducible and preserves batch accounting", {
  sphere <- function(x) {
    sum(x^2) # Provides a deterministic scalar minimisation objective.
  } # 'sphere' end
  control <- list(npart=6, maxit=6, reltol=0, verbose=FALSE, REPORT=6,
                  PDDS.warmup=1, PDDS.batch.every=2, PDDS.nchains=2,
                  out.with.fit.iter=TRUE)

  set.seed(1)
  out1 <- suppressMessages(
    hydroPSO(fn=sphere, method="spso2011-pdds", lower=rep(-5, 3),
             upper=rep(5, 3), control=control)
  )
  set.seed(1)
  out2 <- suppressMessages(
    hydroPSO(fn=sphere, method="spso2011-pdds", lower=rep(-5, 3),
             upper=rep(5, 3), control=control)
  )

  expect_equal(out1$par, out2$par)
  expect_equal(out1$value, out2$value)
  expect_equal(unname(out1$counts["function.calls"]), 36)
  expect_equal(unname(out1$counts["iterations"]), 6)
  expect_equal(out1$PDDS.diagnostics$swarm.batches, 4)
  expect_equal(out1$PDDS.diagnostics$dds.batches, 2)
  expect_equal(out1$PDDS.diagnostics$dds.evaluations, 12)
  expect_equal(dim(out1$part.fit.per.iter), c(6, 6))
  expect_true(out1$PDDS.diagnostics$chain.improvements > 0)
  expect_true(out1$PDDS.diagnostics$migrations > 0)
})

test_that("spso2011-pdds validates its portfolio controls", {
  sphere <- function(x) {
    sum(x^2) # Provides the minimal objective needed for control validation.
  } # 'sphere' end
  control <- list(npart=5, maxit=1, reltol=0, verbose=FALSE, REPORT=1)

  invalid <- list(PDDS.warmup=0, PDDS.batch.every=1, PDDS.nchains=6,
                  PDDS.archive.size=1, PDDS.r=0, PDDS.epoch=1,
                  PDDS.migration.frac=0, PDDS.min.distance=2)
  for (argument in names(invalid)) {
    expect_error(
      hydroPSO(fn=sphere, method="spso2011-pdds", lower=-1, upper=1,
               control=modifyList(control, setNames(list(invalid[[argument]]),
                                                    argument))),
      paste0("control\\$", argument), fixed=FALSE
    )
  } # FOR argument end
})

test_that("spso2011-pdds parallelizes ordinary R objective batches", {
  target <- c(0.25, -0.50) # Defines worker-side data used by the objective closure.
  objective <- function(x) {
    sum((x - target)^2) # Evaluates both SPSO and pDDS candidates on workers.
  } # 'objective' end
  control <- list(npart=4, maxit=3, reltol=0, verbose=FALSE, REPORT=3,
                  PDDS.warmup=1, PDDS.batch.every=2, PDDS.nchains=2,
                  parallel="parallelWin", par.nnodes=2,
                  par.env=environment(), par.export="target")

  set.seed(101)
  out <- suppressMessages(
    hydroPSO(fn=objective, method="spso2011-pdds", lower=c(-1, -1),
             upper=c(1, 1), control=control)
  )

  expect_true(is.finite(out$value))
  expect_equal(unname(out$counts["function.calls"]), 12)
  expect_equal(out$PDDS.diagnostics$dds.batches, 1)
})

test_that("spso2011-pdds parallelizes R-internal model batches", {
  model_fun <- function(param.values, obs) {
    sim <- sum(param.values) # Produces a minimal deterministic model output.
    GoF <- sum((param.values - obs)^2) # Computes the calibration objective.
    list(sim=sim, GoF=GoF)
  } # 'model_fun' end
  control <- list(npart=4, maxit=3, reltol=0, verbose=FALSE, REPORT=3,
                  PDDS.warmup=1, PDDS.batch.every=2, PDDS.nchains=2,
                  parallel="parallelWin", par.nnodes=2, par.env=environment())

  set.seed(102)
  out <- suppressMessages(
    hydroPSO(fn="hydromodInR", method="spso2011-pdds", lower=c(a=0, b=0),
             upper=c(a=1, b=1), control=control, model.FUN=model_fun,
             model.FUN.args=list(obs=c(a=0.25, b=0.75)))
  )

  expect_true(is.finite(out$value))
  expect_equal(unname(out$counts["function.calls"]), 12)
  expect_equal(out$PDDS.diagnostics$dds.evaluations, 4)
})

test_that("spso2011-pdds parallelizes isolated R-external model batches", {
  fixture <- make_external_model_fixture() # Creates a temporary executable-model layout.
  on.exit(unlink(fixture$model.drty, recursive=TRUE), add=TRUE)
  read_output <- function(file) {
    value <- as.numeric(trimws(readLines(file, warn=FALSE)[1]))
    c(value, value + 1)
  } # 'read_output' end
  external_gof <- function(sim, obs) {
    sum((sim - obs)^2) # Computes a scalar objective for each isolated model run.
  } # 'external_gof' end
  control <- list(drty.in=fixture$drty.in, npart=4, maxit=3, reltol=0,
                  verbose=FALSE, REPORT=3, PDDS.warmup=1,
                  PDDS.batch.every=2, PDDS.nchains=2,
                  parallel="parallelWin", par.nnodes=2,
                  par.pkgs="hydroPSO", par.env=environment())

  set.seed(103)
  out <- suppressMessages(
    hydroPSO(fn="hydromod", method="spso2011-pdds", control=control,
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
  expect_equal(unname(out$counts["function.calls"]), 12)
  expect_equal(out$PDDS.diagnostics$dds.batches, 1)
  expect_false(dir.exists(file.path(fixture$model.drty, "parallel")))
})

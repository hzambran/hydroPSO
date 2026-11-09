test_that("competitive DE helpers implement selection, archive, and SHADE memory", {
  expect_equal(hydroPSO:::cde.is.better(c(1, 3), c(2, 2), "min"),
               c(TRUE, FALSE))
  expect_equal(hydroPSO:::cde.fitness.gain(c(1, 3), c(2, 2), "min"),
               c(0.5, 0))

  archive <- hydroPSO:::cde.update.archive(
    archive=matrix(c(0, 0), nrow=1),
    parents=rbind(c(1, 1), c(2, 2)), max.size=2)
  expect_equal(nrow(archive), 2)
  expect_true(all(is.finite(archive)))

  memory <- hydroPSO:::cde.update.memories(
    M.F=rep(0.5, 3), M.CR=rep(0.5, 3), pointer=1L,
    F=c(0.2, 0.8), CR=c(0.3, 0.9), gain=c(1, 2),
    successful=c(TRUE, TRUE))
  weights <- c(1, 2)/3
  expected.F <- sum(weights*c(0.2, 0.8)^2)/sum(weights*c(0.2, 0.8))

  expect_equal(memory$M.F[1], expected.F)
  expect_equal(memory$M.CR[1], sum(weights*c(0.3, 0.9)))
  expect_equal(memory$pointer, 2L)
  expect_equal(memory$n.success, 2L)
})

test_that("competitive DE trials use complete mutation and crossover", {
  X <- rbind(c(0.1, 0.2), c(0.8, 0.2), c(0.2, 0.8), c(0.8, 0.8))
  Pbest <- X
  pbest.fit <- rowSums(X^2)
  basis <- hydroPSO:::cde.coordinate.basis(Pbest)

  set.seed(21)
  exploit <- hydroPSO:::cde.trial.vector(
    part.index=1, X=X, Pbest=Pbest, pbest.fit=pbest.fit,
    archive=matrix(numeric(), nrow=0, ncol=2), F=0.5, CR=0.8,
    p=0.5, MinMax="min", lower=c(0, 0), upper=c(1, 1),
    strategy="current-to-pbest", basis=basis, use.eigen=TRUE)
  set.seed(22)
  explore <- hydroPSO:::cde.trial.vector(
    part.index=1, X=X, Pbest=Pbest, pbest.fit=pbest.fit,
    archive=matrix(numeric(), nrow=0, ncol=2), F=0.5, CR=0.8,
    p=0.5, MinMax="min", lower=c(0, 0), upper=c(1, 1),
    strategy="rand1", basis=basis, use.eigen=FALSE)

  expect_equal(exploit$strategy, "current-to-pbest")
  expect_equal(exploit$coordinate, "eigen")
  expect_equal(explore$strategy, "rand1")
  expect_equal(explore$coordinate, "original")
  expect_true(all(exploit$point >= 0 & exploit$point <= 1))
  expect_true(all(explore$point >= 0 & explore$point <= 1))
})

test_that("spso2011-cde is reproducible and keeps the swarm evaluation budget", {
  sphere <- function(x) sum(x^2) # Provides a deterministic scalar objective.
  control <- list(npart=8, maxit=4, reltol=0, verbose=FALSE, REPORT=4,
                  out.with.fit.iter=TRUE)

  set.seed(31)
  out1 <- suppressMessages(
    hydroPSO(fn=sphere, method="spso2011-cde", lower=rep(-2, 3),
             upper=rep(2, 3), control=control)
  )
  set.seed(31)
  out2 <- suppressMessages(
    hydroPSO(fn=sphere, method="spso2011-cde", lower=rep(-2, 3),
             upper=rep(2, 3), control=control)
  )

  expect_equal(out1$par, out2$par)
  expect_equal(out1$value, out2$value)
  expect_equal(unname(out1$counts["function.calls"]), 32)
  expect_equal(dim(out1$part.fit.per.iter), c(4, 8))
  expect_true(all(out1$par >= -2 & out1$par <= 2))
  expect_equal(out1$CDE.diagnostics$pso.evaluated +
                 out1$CDE.diagnostics$de.evaluated, 24)
  expect_gt(out1$CDE.diagnostics$pso.evaluated, 0)
  expect_gt(out1$CDE.diagnostics$de.evaluated, 0)
  expect_lte(out1$CDE.diagnostics$de.accepted,
             out1$CDE.diagnostics$de.evaluated)
  expect_true(all(out1$CDE.diagnostics$F.memory > 0 &
                    out1$CDE.diagnostics$F.memory <= 1))
  expect_true(all(out1$CDE.diagnostics$CR.memory >= 0 &
                    out1$CDE.diagnostics$CR.memory <= 1))
})

test_that("spso2011-cde validates its method-specific controls", {
  sphere <- function(x) sum(x^2) # Provides the minimal validation objective.
  control <- list(npart=5, maxit=1, reltol=0, verbose=FALSE, REPORT=1)

  expect_error(
    hydroPSO(fn=sphere, method="spso2011-cde", lower=-1, upper=1,
             control=modifyList(control, list(CDE.H=0))),
    "control\\$CDE.H"
  )
  expect_error(
    hydroPSO(fn=sphere, method="spso2011-cde", lower=-1, upper=1,
             control=modifyList(control, list(CDE.p=0))),
    "control\\$CDE.p"
  )
  expect_error(
    hydroPSO(fn=sphere, method="spso2011-cde", lower=-1, upper=1,
             control=modifyList(control, list(CDE.qmin=0.8, CDE.qmax=0.2))),
    "CDE.qmin"
  )
  expect_error(
    hydroPSO(fn=sphere, method="spso2011-cde", lower=-1, upper=1,
             control=modifyList(control, list(best.update="async"))),
    "best.update=sync"
  )
  expect_error(
    hydroPSO(fn=sphere, method="spso2011-cde", lower=-1, upper=1,
             control=modifyList(control, list(topology="gbest"))),
    "requires 'control\\$topology=random'"
  )
})

test_that("spso2011-cde parallelizes ordinary R objective functions", {
  target <- c(0.25, -0.50) # Defines worker-side closure data.
  objective <- function(x) sum((x-target)^2) # Computes one worker objective.
  control <- list(npart=4, maxit=2, reltol=0, verbose=FALSE, REPORT=2,
                  parallel="parallelWin", par.nnodes=2,
                  par.env=environment(), par.export="target")

  set.seed(41)
  out <- suppressMessages(
    hydroPSO(fn=objective, method="spso2011-cde", lower=c(-1, -1),
             upper=c(1, 1), control=control)
  )

  expect_true(is.finite(out$value))
  expect_equal(unname(out$counts["function.calls"]), 8)
  expect_equal(out$CDE.diagnostics$pso.evaluated +
                 out$CDE.diagnostics$de.evaluated, 4)
})

test_that("spso2011-cde parallelizes R-internal models", {
  model_fun <- function(param.values, obs) {
    sim <- sum(param.values) # Produces a minimal deterministic model output.
    GoF <- sum((param.values-obs)^2) # Computes the calibration objective.
    list(sim=sim, GoF=GoF)
  } # 'model_fun' end
  control <- list(npart=4, maxit=2, reltol=0, verbose=FALSE, REPORT=2,
                  parallel="parallelWin", par.nnodes=2,
                  par.env=environment())

  set.seed(42)
  out <- suppressMessages(
    hydroPSO(fn="hydromodInR", method="spso2011-cde", lower=c(a=0, b=0),
             upper=c(a=1, b=1), control=control, model.FUN=model_fun,
             model.FUN.args=list(obs=c(a=0.25, b=0.75)))
  )

  expect_true(is.finite(out$value))
  expect_equal(unname(out$counts["function.calls"]), 8)
})

test_that("spso2011-cde parallelizes R-external models in isolated directories", {
  fixture <- make_external_model_fixture() # Creates a temporary executable-model layout.
  on.exit(unlink(fixture$model.drty, recursive=TRUE), add=TRUE)
  read_output <- function(file) {
    value <- as.numeric(trimws(readLines(file, warn=FALSE)[1]))
    c(value, value+1)
  } # 'read_output' end
  external_gof <- function(sim, obs) {
    sum((sim-obs)^2)
  } # 'external_gof' end
  control <- list(drty.in=fixture$drty.in, npart=4, maxit=2, reltol=0,
                  verbose=FALSE, REPORT=2, parallel="parallelWin",
                  par.nnodes=2, par.pkgs="hydroPSO", par.env=environment())

  set.seed(43)
  out <- suppressMessages(
    hydroPSO(fn="hydromod", method="spso2011-cde", control=control,
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

test_that("informant DE mutation excludes the target and uses SPSO radius", {
  x <- c(0, 0) # Defines the current particle position.
  G <- c(3, 4) # Gives a known SPSO-2011 radius of five.
  Pbest <- rbind(c(100, 100), c(1, 0), c(0, 1), c(-100, -100)) # Separates target, donors, and non-informant.
  F <- 0.5 # Requests a differential displacement of half the SPSO radius.

  set.seed(11)
  out <- hydroPSO:::informant.de.mutation(x=x, G=G, Pbest=Pbest,
                                          part.index=1,
                                          informants=c(1, 2, 3, NA), F=F)
  direction <- (Pbest[2, ]-Pbest[3, ]) / sqrt(2) # Computes the only valid donor direction.
  valid <- rbind(G + F*5*direction, G - F*5*direction)

  expect_true(out$used)
  expect_true(any(apply(valid, 1, function(z) isTRUE(all.equal(out$point, z)))))
  expect_equal(sqrt(sum((out$point-G)^2)), F*5)

  set.seed(12)
  fallback <- hydroPSO:::informant.de.mutation(x=x, G=G, Pbest=Pbest,
                                               part.index=1,
                                               informants=c(1, 2, NA), F=F)
  expect_false(fallback$used)
  expect_lte(sqrt(sum((fallback$point-G)^2)), 5)

  set.seed(14)
  archived <- hydroPSO:::informant.de.mutation(x=x, G=G, Pbest=Pbest,
                                               part.index=1,
                                               informants=c(1, 2, NA),
                                               archive=matrix(c(0, 1), nrow=1), F=F)
  expect_true(archived$used)
})

test_that("informant archive is bounded and contains only non-target informants", {
  archive <- matrix(c(9, 9), nrow=1) # Supplies one historical informant position.
  Pbest <- rbind(c(100, 100), c(1, 0), c(0, 1), c(-100, -100))

  updated <- hydroPSO:::update.informant.archive(archive=archive, Pbest=Pbest,
                                                 informants=c(1, 2, 3, NA),
                                                 part.index=1, max.size=2)

  expect_equal(unname(updated), Pbest[c(2, 3), , drop=FALSE])
  expect_false(any(apply(updated, 1, function(z) identical(as.numeric(z), Pbest[1, ]))))
})

test_that("DE success history uses a fitness-weighted Lehmer mean", {
  F <- c(0.2, 0.8, NA_real_) # Associates two valid factors and one non-DE candidate.
  pbest.fit <- c(5, 5, 5) # Defines the fitness before evaluating the new batch.
  fitness <- c(4, 3, 1) # Makes the first two DE candidates improve by one and two.
  weights <- c(1, 2)/3
  expected <- sum(weights*c(0.2, 0.8)^2) / sum(weights*c(0.2, 0.8))

  out <- hydroPSO:::update.de.success.history(mu=0.5, F=F, fitness=fitness,
                                              pbest.fit=pbest.fit,
                                              MinMax="min", c=1)

  expect_equal(out$mu, expected)
  expect_equal(out$n.success, 2L)

  unchanged <- hydroPSO:::update.de.success.history(mu=0.5, F=F,
                                                     fitness=c(6, 7, 8),
                                                     pbest.fit=pbest.fit,
                                                     MinMax="min", c=0.1)
  expect_equal(unchanged, list(mu=0.5, n.success=0L))
})

test_that("adaptive DE factors are positive and bounded", {
  set.seed(13)
  values <- replicate(100, hydroPSO:::sample.de.F(mu=0.5))

  expect_true(all(is.finite(values)))
  expect_true(all(values > 0 & values <= 2))
})

test_that("spso2011-de is reproducible and adds no function evaluations", {
  sphere <- function(x) sum(x^2) # Provides a deterministic scalar minimisation objective.
  control <- list(npart=5, maxit=3, reltol=0, verbose=FALSE, REPORT=3,
                  out.with.fit.iter=TRUE)

  set.seed(42)
  out1 <- suppressMessages(
    hydroPSO(fn=sphere, method="spso2011-de", lower=c(-1, -1), upper=c(1, 1),
             control=control)
  )
  set.seed(42)
  out2 <- suppressMessages(
    hydroPSO(fn=sphere, method="spso2011-de", lower=c(-1, -1), upper=c(1, 1),
             control=control)
  )
  set.seed(42)
  out3 <- suppressMessages(
    hydroPSO(fn=sphere, method="spso2011-de", lower=c(-1, -1), upper=c(1, 1),
             control=modifyList(control, list(DE.F=0.1)))
  )

  expect_equal(out1$par, out2$par)
  expect_equal(out1$value, out2$value)
  expect_equal(out1$par, out3$par)
  expect_equal(out1$value, out3$value)
  expect_true(all(out1$par >= -1 & out1$par <= 1))
  expect_equal(dim(out1$part.fit.per.iter), c(3, 5))
  expect_equal(unname(out1$counts["function.calls"]), 15)
})

test_that("spso2011-de validates its method-specific controls", {
  sphere <- function(x) sum(x^2) # Provides the minimal objective needed for validation.
  control <- list(npart=5, maxit=1, reltol=0, verbose=FALSE, REPORT=1)

  expect_error(
    hydroPSO(fn=sphere, method="spso2011-de", lower=-1, upper=1,
             control=modifyList(control, list(DE.F=0))),
    "control\\$DE.F"
  )
  expect_error(
    hydroPSO(fn=sphere, method="spso2011-de", lower=-1, upper=1,
             control=modifyList(control, list(DE.c=0))),
    "control\\$DE.c"
  )
  expect_error(
    hydroPSO(fn=sphere, method="spso2011-de", lower=-1, upper=1,
             control=modifyList(control, list(DE.archive.size=2.5))),
    "control\\$DE.archive.size"
  )
  expect_error(
    hydroPSO(fn=sphere, method="spso2011-de", lower=-1, upper=1,
             control=modifyList(control, list(topology="gbest"))),
    "requires 'control\\$topology=random'"
  )
})

test_that("spso2011-de parallelizes ordinary R objective functions", {
  target <- c(0.25, -0.50) # Defines worker-side data used by the objective closure.
  objective <- function(x) sum((x - target)^2) # Computes one scalar objective on each worker.
  control <- list(npart=4, maxit=2, reltol=0, verbose=FALSE, REPORT=2,
                  DE.F=0.5, parallel="parallelWin", par.nnodes=2,
                  par.env=environment(), par.export="target")

  set.seed(101)
  out <- suppressMessages(
    hydroPSO(fn=objective, method="spso2011-de", lower=c(-1, -1), upper=c(1, 1),
             control=control)
  )

  expect_true(is.finite(out$value))
  expect_equal(unname(out$counts["function.calls"]), 8)
})

test_that("spso2011-de parallelizes R-internal models", {
  model_fun <- function(param.values, obs) {
    sim <- sum(param.values) # Produces a minimal deterministic model output.
    GoF <- sum((param.values - obs)^2) # Computes the calibration objective.
    list(sim=sim, GoF=GoF)
  } # 'model_fun' end
  control <- list(npart=4, maxit=2, reltol=0, verbose=FALSE, REPORT=2,
                  DE.F=0.5, parallel="parallelWin", par.nnodes=2,
                  par.env=environment())

  set.seed(102)
  out <- suppressMessages(
    hydroPSO(fn="hydromodInR", method="spso2011-de", lower=c(a=0, b=0),
             upper=c(a=1, b=1), control=control, model.FUN=model_fun,
             model.FUN.args=list(obs=c(a=0.25, b=0.75)))
  )

  expect_true(is.finite(out$value))
  expect_equal(unname(out$counts["function.calls"]), 8)
})

test_that("spso2011-de parallelizes R-external models in isolated directories", {
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
                  verbose=FALSE, REPORT=2, DE.F=0.5, parallel="parallelWin",
                  par.nnodes=2, par.pkgs="hydroPSO", par.env=environment())

  set.seed(103)
  out <- suppressMessages(
    hydroPSO(fn="hydromod", method="spso2011-de", control=control,
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

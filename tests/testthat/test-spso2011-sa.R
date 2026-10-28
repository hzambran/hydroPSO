test_that("hydroPSO exposes the requested optimisation methods", {
  methods <- eval(formals(hydroPSO)$method)

  expect_identical(methods,
                   c("spso2011", "spso2007", "ipso", "fips", "wfips",
                     "canonical", "spso2011-sa", "spso2011-de",
                     "spso2011-dds"))
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

test_that("spso2011-sa uses PSOCK workers for ordinary R objectives", {
  pid.dir <- tempfile("spso2011-sa-r-pids-")
  dir.create(pid.dir)
  logged_sphere <- function(x, pid.dir) {
    file.create(file.path(pid.dir, paste0("pid-", Sys.getpid()))) # Records the evaluating process.
    sum(x^2)
  } # 'logged_sphere' end
  control <- list(npart=3, maxit=1, reltol=0, verbose=FALSE, REPORT=1,
                  SA.T0=1, SA.alpha=0.8, SA.scale=0.05,
                  parallel="parallelWin", par.nnodes=2,
                  par.env=environment(), par.export="logged_sphere")

  set.seed(11)
  out <- suppressMessages(
    hydroPSO(fn=logged_sphere, pid.dir=pid.dir, method="spso2011-sa",
             lower=c(-1, -1), upper=c(1, 1), control=control)
  )

  pids <- sub("^pid-", "", basename(list.files(pid.dir)))
  expect_true(as.character(Sys.getpid()) %in% pids)
  expect_true(any(pids != as.character(Sys.getpid())))
  expect_equal(unname(out$counts["function.calls"]), 4)
})

test_that("spso2011-sa uses PSOCK workers for R-internal models", {
  pid.dir <- tempfile("spso2011-sa-internal-pids-")
  dir.create(pid.dir)
  logged_model <- function(param.values, obs, pid.dir) {
    file.create(file.path(pid.dir, paste0("pid-", Sys.getpid()))) # Records the evaluating process.
    sim <- sum(param.values) # Provides a minimal model output for the wrapper contract.
    GoF <- sum((param.values - obs)^2) # Computes a scalar objective for optimisation.
    list(sim=sim, GoF=GoF)
  } # 'logged_model' end
  control <- list(npart=3, maxit=1, reltol=0, verbose=FALSE, REPORT=1,
                  SA.T0=1, SA.alpha=0.8, SA.scale=0.05,
                  parallel="parallelWin", par.nnodes=2,
                  par.pkgs="hydroPSO", par.env=environment(),
                  par.export="logged_model")

  set.seed(12)
  out <- suppressMessages(
    hydroPSO(fn="hydromodInR", method="spso2011-sa", lower=c(a=0, b=0),
             upper=c(a=1, b=1), control=control, model.FUN=logged_model,
             model.FUN.args=list(obs=c(a=0.25, b=0.75), pid.dir=pid.dir))
  )

  pids <- sub("^pid-", "", basename(list.files(pid.dir)))
  expect_true(as.character(Sys.getpid()) %in% pids)
  expect_true(any(pids != as.character(Sys.getpid())))
  expect_equal(unname(out$counts["function.calls"]), 4)
})

test_that("spso2011-sa uses isolated PSOCK workers for R-external models", {
  fixture <- make_external_model_fixture()
  pid.dir <- tempfile("spso2011-sa-external-pids-")
  dir.create(pid.dir)
  logged_reader <- function(file, pid.dir) {
    file.create(file.path(pid.dir, paste0("pid-", Sys.getpid()))) # Records the evaluating process.
    value <- as.numeric(trimws(readLines(file, warn=FALSE)[1]))
    c(value, value + 1)
  } # 'logged_reader' end
  logged_gof <- function(sim, obs) {
    sum((sim - obs)^2)
  } # 'logged_gof' end
  control <- list(drty.in=fixture$drty.in, npart=3, maxit=1, reltol=0,
                  verbose=FALSE, REPORT=1, SA.T0=1, SA.alpha=0.8,
                  SA.scale=0.05, parallel="parallelWin", par.nnodes=2,
                  par.pkgs="hydroPSO", par.env=environment(),
                  par.export=c("logged_reader", "logged_gof"))

  set.seed(13)
  out <- suppressMessages(
    hydroPSO(fn="hydromod", method="spso2011-sa", lower=0, upper=10,
             control=control, model.FUN=hydromod,
             model.FUN.args=list(param.files=fixture$param.files,
                                 param.ranges=fixture$param.ranges,
                                 model.drty=fixture$model.drty,
                                 exe.fname=test_rscript(), exe.args=fixture$script,
                                 out.FUN=logged_reader,
                                 out.FUN.args=list(file="input.txt", pid.dir=pid.dir),
                                 gof.FUN=logged_gof, obs=c(0, 0), verbose=FALSE))
  )

  pids <- sub("^pid-", "", basename(list.files(pid.dir)))
  expect_true(as.character(Sys.getpid()) %in% pids)
  expect_true(any(pids != as.character(Sys.getpid())))
  expect_equal(unname(out$counts["function.calls"]), 4)
  expect_false(dir.exists(file.path(fixture$model.drty, "parallel")))
})

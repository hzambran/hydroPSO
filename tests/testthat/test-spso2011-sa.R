test_that("hydroPSO exposes the requested optimisation methods", {
  methods <- eval(formals(hydroPSO)$method)

  expect_identical(methods,
                   c("spso2011", "spso2007", "ipso", "fips", "wfips",
                     "canonical", "spso2011-sa", "spso2011-de",
                     "spso2011-dds"))
  expect_error(hydroPSO(method="invalid"), "should be one of")
})

test_that("spso2011-sa runs a persistent adaptive chain after stagnation", {
  sphere <- function(x) sum(x^2) # Provides a deterministic scalar minimisation objective.
  control <- list(npart=5, maxit=2, reltol=0, verbose=FALSE, REPORT=2,
                  SA.T0=1, SA.alpha=0.8, SA.scale=0.05,
                  SA.nsteps=2, SA.stagnation=0)

  set.seed(42)
  out <- suppressMessages(
    hydroPSO(fn=sphere, method="spso2011-sa", lower=c(-1, -1), upper=c(1, 1),
             control=control)
  )

  expect_length(out$par, 2)
  expect_true(is.finite(out$value))
  expect_equal(unname(out$counts["iterations"]), 2)
  expect_equal(unname(out$counts["function.calls"]), 14)
  expect_equal(out$SA.diagnostics$attempted, 4)
  expect_true(out$SA.diagnostics$accepted <= out$SA.diagnostics$attempted)
  expect_true(is.finite(out$SA.diagnostics$initial.temperature))
  expect_length(out$SA.diagnostics$final.scale, 2)
  expect_named(out$SA.diagnostics$final.scale, names(out$par))
  expect_true(all(out$SA.diagnostics$current.par >= -1 &
                  out$SA.diagnostics$current.par <= 1))
})

test_that("spso2011-sa adapts and reheats accepted coordinate proposals", {
  constant <- function(x) 0 # Makes every finite reflected proposal acceptable.
  control <- list(npart=3, maxit=1, reltol=0, verbose=FALSE, REPORT=1,
                  SA.T0=1, SA.alpha=0.9, SA.scale=0.05, SA.nsteps=3,
                  SA.ndim=1, SA.stagnation=0, SA.reheat=2,
                  SA.target.accept=0.5)

  set.seed(24)
  out <- suppressMessages(
    hydroPSO(fn=constant, method="spso2011-sa", lower=c(-1, -1),
             upper=c(1, 1), control=control)
  )

  expect_equal(out$SA.diagnostics$attempted, 3)
  expect_equal(out$SA.diagnostics$accepted, 3)
  expect_equal(out$SA.diagnostics$reheats, 1)
  expect_true(any(out$SA.diagnostics$final.scale != control$SA.scale))
  expect_true(all(out$SA.diagnostics$current.par >= -1 &
                  out$SA.diagnostics$current.par <= 1))
})

test_that("spso2011-sa automatically scales temperature to the objective", {
  sphere <- function(x, multiplier) multiplier*sum(x^2) # Changes only the objective scale.
  control <- list(npart=5, maxit=1, reltol=0, verbose=FALSE, REPORT=1,
                  SA.T0=NA_real_, SA.nsteps=1, SA.stagnation=0)

  set.seed(29)
  base <- suppressMessages(
    hydroPSO(fn=sphere, multiplier=1, method="spso2011-sa", lower=c(-1, -1),
             upper=c(1, 1), control=control)
  )
  set.seed(29)
  scaled <- suppressMessages(
    hydroPSO(fn=sphere, multiplier=1000, method="spso2011-sa",
             lower=c(-1, -1), upper=c(1, 1), control=control)
  )

  expect_equal(scaled$SA.diagnostics$initial.temperature /
               base$SA.diagnostics$initial.temperature, 1000, tolerance=1E-10)
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
  expect_error(
    hydroPSO(fn=sphere, method="spso2011-sa", lower=-1, upper=1,
             control=modifyList(control, list(SA.nsteps=0))),
    "control\\$SA.nsteps"
  )
  expect_error(
    hydroPSO(fn=sphere, method="spso2011-sa", lower=-1, upper=1,
             control=modifyList(control, list(SA.ndim=2))),
    "control\\$SA.ndim"
  )
  expect_error(
    hydroPSO(fn=sphere, method="spso2011-sa", lower=-1, upper=1,
             control=modifyList(control, list(SA.stagnation=-1))),
    "control\\$SA.stagnation"
  )
  expect_error(
    hydroPSO(fn=sphere, method="spso2011-sa", lower=-1, upper=1,
             control=modifyList(control, list(SA.reheat=0))),
    "control\\$SA.reheat"
  )
  expect_error(
    hydroPSO(fn=sphere, method="spso2011-sa", lower=-1, upper=1,
             control=modifyList(control, list(SA.target.accept=1))),
    "control\\$SA.target.accept"
  )
})

test_that("spso2011-sa supports R-based hydrological models", {
  model_fun <- function(param.values, obs) {
    sim <- sum(param.values) # Provides a minimal model output for the wrapper contract.
    GoF <- sum((param.values - obs)^2) # Computes a scalar objective for optimisation.
    list(sim=sim, GoF=GoF)
  } # 'model_fun' end
  control <- list(npart=5, maxit=1, reltol=0, verbose=FALSE, REPORT=1,
                  SA.T0=1, SA.alpha=0.8, SA.scale=0.05,
                  SA.nsteps=1, SA.stagnation=0)

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
                  SA.nsteps=1, SA.stagnation=0,
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
                  SA.nsteps=1, SA.stagnation=0,
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
                  SA.scale=0.05, SA.nsteps=1, SA.stagnation=0,
                  parallel="parallelWin", par.nnodes=2,
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

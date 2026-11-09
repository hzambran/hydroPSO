test_that("hydroPSO exposes the requested optimisation methods", {
  methods <- eval(formals(hydroPSO)$method)

  expect_identical(methods,
                   c("spso2011", "spso2007", "ipso", "fips", "wfips",
                     "canonical", "spso2011-sa", "spso2011-de",
                     "spso2011-cde", "spso2011-dds", "spso2011-pdds"))
  expect_error(hydroPSO(method="invalid"), "should be one of")
})

test_that("spso2011-sa evaluates and exchanges replica batches", {
  constant <- function(x) 0 # Accepts every finite proposal so event counts are deterministic.
  control <- list(npart=6, maxit=2, reltol=0, verbose=FALSE, REPORT=2,
                  SA.T0=1, SA.alpha=0.98, SA.scale=0.05,
                  SA.nsteps=2, SA.stagnation=0, SA.replicas=3,
                  SA.swap.every=1, SA.scout.fraction=0.34,
                  SA.scout.generations=4, SA.budget.fraction=1)

  set.seed(42)
  out <- suppressMessages(
    hydroPSO(fn=constant, method="spso2011-sa", lower=c(-1, -1), upper=c(1, 1),
             control=control)
  )

  expect_length(out$par, 2)
  expect_equal(out$value, 0)
  expect_equal(unname(out$counts["iterations"]), 2)
  expect_equal(unname(out$counts["function.calls"]), 24)
  expect_equal(out$SA.diagnostics$attempted, 12)
  expect_equal(out$SA.diagnostics$accepted, 12)
  expect_equal(out$SA.diagnostics$activations, 2)
  expect_equal(out$SA.diagnostics$swap.attempted, 4)
  expect_equal(out$SA.diagnostics$swap.accepted, 4)
  expect_equal(out$SA.diagnostics$scouts.injected, 0)
  expect_true(is.finite(out$SA.diagnostics$initial.temperature))
  expect_length(out$SA.diagnostics$final.scale, 2)
  expect_named(out$SA.diagnostics$final.scale, names(out$par))
  expect_equal(dim(out$SA.diagnostics$replica.par), c(3, 2))
  expect_equal(dim(out$SA.diagnostics$replica.scale), c(3, 2))
  expect_length(out$SA.diagnostics$temperature.ladder, 3)
  expect_true(all(out$SA.diagnostics$current.par >= -1 &
                  out$SA.diagnostics$current.par <= 1))
})

test_that("spso2011-sa defaults to a compact four-sweep event", {
  constant <- function(x) 0 # Keeps the event active for all default sweeps.

  set.seed(43)
  out <- suppressMessages(
    hydroPSO(fn=constant, method="spso2011-sa", lower=c(-1, -1), upper=c(1, 1),
             control=list(npart=6, maxit=1, maxfn=100, reltol=0,
                          verbose=FALSE, REPORT=1,
                          SA.stagnation=0, SA.replicas=2,
                          SA.budget.fraction=1))
  )

  expect_equal(out$SA.diagnostics$attempted, 8)
})

test_that("spso2011-sa repopulates only competitive protected scouts", {
  sphere <- function(x) sum(x^2) # Allows accepted replica states to improve weak memories.

  set.seed(1)
  out <- suppressMessages(
    hydroPSO(fn=sphere, method="spso2011-sa", lower=c(-1, -1), upper=c(1, 1),
             control=list(npart=6, maxit=2, reltol=0, verbose=FALSE, REPORT=2,
                          SA.stagnation=0, SA.replicas=3, SA.nsteps=2,
                          SA.swap.every=1, SA.scout.fraction=0.34,
                          SA.scout.generations=4, SA.budget.fraction=1))
  )

  expect_gt(out$SA.diagnostics$scouts.injected, 0)
  expect_true(any(out$SA.diagnostics$scout.remaining > 0))
  expect_equal(out$SA.diagnostics$productive.events, 2)
})

test_that("spso2011-sa adapts and reheats its replica ladder", {
  constant <- function(x) 0 # Makes every finite reflected proposal acceptable.
  control <- list(npart=6, maxit=1, reltol=0, verbose=FALSE, REPORT=1,
                  SA.T0=1, SA.alpha=0.9, SA.scale=0.05, SA.nsteps=3,
                  SA.ndim=1, SA.stagnation=0, SA.reheat=2,
                  SA.target.accept=0.5, SA.replicas=2,
                  SA.swap.every=1, SA.budget.fraction=1)

  set.seed(24)
  out <- suppressMessages(
    hydroPSO(fn=constant, method="spso2011-sa", lower=c(-1, -1),
             upper=c(1, 1), control=control)
  )

  expect_equal(out$SA.diagnostics$attempted, 6)
  expect_equal(out$SA.diagnostics$accepted, 6)
  expect_equal(out$SA.diagnostics$reheats, 2)
  expect_equal(out$SA.diagnostics$swap.attempted, 2)
  expect_true(any(out$SA.diagnostics$final.scale != control$SA.scale))
  expect_true(all(out$SA.diagnostics$current.par >= -1 &
                  out$SA.diagnostics$current.par <= 1))
})

test_that("spso2011-sa automatically scales temperature to the objective", {
  sphere <- function(x, multiplier) multiplier*sum(x^2) # Changes only the objective scale.
  control <- list(npart=5, maxit=1, reltol=0, verbose=FALSE, REPORT=1,
                  SA.T0=NA_real_, SA.nsteps=1, SA.stagnation=0,
                  SA.replicas=2, SA.budget.fraction=1)

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
  expect_error(
    hydroPSO(fn=sphere, method="spso2011-sa", lower=-1, upper=1,
             control=modifyList(control, list(SA.replicas=1))),
    "control\\$SA.replicas"
  )
  expect_error(
    hydroPSO(fn=sphere, method="spso2011-sa", lower=-1, upper=1,
             control=modifyList(control, list(SA.swap.every=0))),
    "control\\$SA.swap.every"
  )
  expect_error(
    hydroPSO(fn=sphere, method="spso2011-sa", lower=-1, upper=1,
             control=modifyList(control, list(SA.scout.fraction=1))),
    "control\\$SA.scout.fraction"
  )
  expect_error(
    hydroPSO(fn=sphere, method="spso2011-sa", lower=-1, upper=1,
             control=modifyList(control, list(SA.scout.generations=0))),
    "control\\$SA.scout.generations"
  )
  expect_error(
    hydroPSO(fn=sphere, method="spso2011-sa", lower=-1, upper=1,
             control=modifyList(control, list(SA.radius.threshold=0))),
    "control\\$SA.radius.threshold"
  )
  expect_error(
    hydroPSO(fn=sphere, method="spso2011-sa", lower=-1, upper=1,
             control=modifyList(control, list(SA.budget.fraction=0))),
    "control\\$SA.budget.fraction"
  )
})

test_that("spso2011-sa uses emergency events and enforces its evaluation share", {
  constant <- function(x) 0 # Produces stagnation without forcing geometric collapse.

  set.seed(31)
  emergency <- suppressMessages(
    hydroPSO(fn=constant, method="spso2011-sa", lower=c(-1, -1), upper=c(1, 1),
             control=list(npart=6, maxit=5, reltol=0, verbose=FALSE, REPORT=5,
                          SA.stagnation=1, SA.radius.threshold=1E-12,
                          SA.reheat=2, SA.replicas=2, SA.nsteps=1,
                          SA.budget.fraction=1))
  )

  expect_equal(emergency$SA.diagnostics$activations, 1)
  expect_equal(emergency$SA.diagnostics$emergency.events, 1)
  expect_equal(emergency$SA.diagnostics$collapsed.events, 0)

  set.seed(32)
  budgeted <- suppressMessages(
    hydroPSO(fn=constant, method="spso2011-sa", lower=c(-1, -1), upper=c(1, 1),
             control=list(npart=10, maxit=10, reltol=0, verbose=FALSE, REPORT=10,
                          SA.stagnation=0, SA.replicas=3, SA.nsteps=10,
                          SA.budget.fraction=0.15))
  )

  expect_equal(budgeted$SA.diagnostics$evaluation.budget, 15)
  expect_equal(budgeted$SA.diagnostics$attempted, 15)
})

test_that("spso2011-sa supports R-based hydrological models", {
  model_fun <- function(param.values, obs) {
    sim <- sum(param.values) # Provides a minimal model output for the wrapper contract.
    GoF <- sum((param.values - obs)^2) # Computes a scalar objective for optimisation.
    list(sim=sim, GoF=GoF)
  } # 'model_fun' end
  control <- list(npart=5, maxit=1, reltol=0, verbose=FALSE, REPORT=1,
                  SA.T0=1, SA.alpha=0.8, SA.scale=0.05,
                  SA.nsteps=1, SA.stagnation=0, SA.replicas=2,
                  SA.budget.fraction=1)

  set.seed(7)
  out <- suppressMessages(
    hydroPSO(fn="hydromodInR", method="spso2011-sa", lower=c(a=0, b=0),
             upper=c(a=1, b=1), control=control, model.FUN=model_fun,
             model.FUN.args=list(obs=c(a=0.25, b=0.75)))
  )

  expect_true(is.finite(out$value))
  expect_equal(unname(out$counts["function.calls"]), 7)
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
                  SA.nsteps=1, SA.stagnation=0, SA.replicas=2,
                  SA.budget.fraction=1,
                  parallel="parallelWin", par.nnodes=2,
                  par.env=environment(), par.export="logged_sphere")

  set.seed(11)
  out <- suppressMessages(
    hydroPSO(fn=logged_sphere, pid.dir=pid.dir, method="spso2011-sa",
             lower=c(-1, -1), upper=c(1, 1), control=control)
  )

  pids <- sub("^pid-", "", basename(list.files(pid.dir)))
  expect_false(as.character(Sys.getpid()) %in% pids)
  expect_true(any(pids != as.character(Sys.getpid())))
  expect_equal(length(pids), 2)
  expect_equal(unname(out$counts["function.calls"]), 5)
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
                  SA.nsteps=1, SA.stagnation=0, SA.replicas=2,
                  SA.budget.fraction=1,
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
  expect_equal(unname(out$counts["function.calls"]), 5)
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
                  SA.replicas=2, SA.budget.fraction=1,
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
  expect_equal(unname(out$counts["function.calls"]), 5)
  expect_false(dir.exists(file.path(fixture$model.drty, "parallel")))
})

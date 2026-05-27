test_that("hydroPSO hydromodInR change.type='repl' preserves current behaviour", {
  model_fun <- function(param.values, obs) {
    sim <- c(sum(param.values), sum(param.values) + 1)
    GoF <- sum((param.values - obs)^2)
    list(sim=sim, GoF=GoF)
  }

  control <- list(maxit=1, npart=5, write2disk=FALSE, verbose=FALSE, REPORT=1,
                  drty.out=tempdir())
  lower <- c(a=0, b=0)
  upper <- c(a=1, b=1)
  model_args <- list(obs=c(a=0.25, b=0.75))

  set.seed(123)
  out.default <- suppressMessages(
    hydroPSO(fn="hydromodInR", lower=lower, upper=upper, control=control,
             model.FUN=model_fun, model.FUN.args=model_args)
  )

  set.seed(123)
  out.repl <- suppressMessages(
    hydroPSO(fn="hydromodInR", lower=lower, upper=upper, control=control,
             model.FUN=model_fun, model.FUN.args=model_args,
             change.type="repl")
  )

  expect_equal(out.repl, out.default)
})

test_that("verification hydromodInR change.type='repl' passes numeric parameter vectors", {
  model_fun <- function(param.values, obs) {
    expect_true(is.numeric(param.values))
    expect_null(dim(param.values))
    sim <- c(sum(param.values), sum(param.values) + 1)
    GoF <- sum((param.values - obs)^2)
    list(sim=sim, GoF=GoF)
  }

  params <- data.frame(a=c(0.2, 0.4), b=c(0.6, 0.8))
  control <- list(drty.out=file.path(tempdir(), "verification-repl"),
                  write2disk=TRUE, verbose=FALSE, parallel="none")

  out <- suppressMessages(
    verification(fn="hydromodInR", par=params, control=control,
                 model.FUN=model_fun, model.FUN.args=list(obs=c(a=0.25, b=0.75)),
                 change.type="repl")
  )

  expect_equal(length(out$gofs), nrow(params))
})

test_that(".changeParamValues supports one change type per parameter", {
  change_param_values <- getFromNamespace(".changeParamValues", "hydroPSO")
  x <- matrix(c(1, 2, 3, 4,
                5, 6, 7, 8), nrow=2, byrow=TRUE)
  colnames(x) <- c("a", "b", "c", "d")
  expected <- matrix(c(1, 12, 30, 3,
                       5, 16, 70, 7), nrow=2, byrow=TRUE)
  colnames(expected) <- colnames(x)

  out <- change_param_values(x,
                             change.type=c("repl", "addi", "mult", "addi"),
                             refValue=c(NA, 10, 10, -1))

  expect_equal(out, expected)
})

test_that("verification hydromodInR applies mixed per-parameter change types", {
  seen <- new.env(parent=emptyenv())
  seen$param.values <- NULL
  model_fun <- function(param.values, obs) {
    seen$param.values <- param.values
    sim <- c(sum(param.values), sum(param.values) + 1)
    GoF <- sum((param.values - obs)^2)
    list(sim=sim, GoF=GoF)
  }

  params <- data.frame(a=1, b=2, c=3, d=4)
  control <- list(drty.out=file.path(tempdir(), "verification-mixed"),
                  write2disk=TRUE, verbose=FALSE, parallel="none")

  suppressMessages(
    verification(fn="hydromodInR", par=params, control=control,
                 model.FUN=model_fun, model.FUN.args=list(obs=c(a=1, b=12, c=30, d=3)),
                 change.type=c("repl", "addi", "mult", "addi"),
                 refValue=c(NA, 10, 10, -1))
  )

  expect_equal(seen$param.values, c(a=1, b=12, c=30, d=3))
})

test_that(".changeParamValues rejects change.type lengths other than one or nparam", {
  change_param_values <- getFromNamespace(".changeParamValues", "hydroPSO")
  x <- matrix(1:8, nrow=2)

  expect_error(change_param_values(x, change.type=c("repl", "addi", "mult"),
                                   refValue=1),
               "length\\(change.type\\)")
})

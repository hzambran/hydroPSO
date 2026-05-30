test_that("verification hydromod resolves parameter metadata files from drty.in", {
  model.drty <- tempfile("verification-hydromod-model-")
  drty.in <- file.path(model.drty, "PSO.in")
  dir.create(drty.in, recursive=TRUE)

  writeLines("      0.00", file.path(model.drty, "input.txt"))
  writeLines(
    c("ParameterNmbr ParameterName MinValue MaxValue",
      "1 P1 0 10"),
    file.path(drty.in, "ParamRanges.txt")
  )
  writeLines(
    c("ParameterNmbr ParameterName Filename Row.Number Col.Start Col.End Decimals",
      "1 P1 input.txt 1 1 10 2"),
    file.path(drty.in, "ParamFiles.txt")
  )

  exe.fname <- file.path(model.drty, "run.model")
  writeLines(c("#!/bin/sh", "exit 0"), exe.fname)
  Sys.chmod(exe.fname, mode="0755")

  read_external_output <- function(file) {
    value <- as.numeric(trimws(readLines(file, warn=FALSE)[1]))
    c(value, value + 1)
  }
  sum_gof <- function(sim, obs) {
    sum(sim - obs)
  }

  out <- suppressMessages(
    verification(
      fn="hydromod",
      par=matrix(c(1.25, 2.50), ncol=1),
      model.FUN=hydromod,
      model.FUN.args=list(
        param.files="ParamFiles.txt",
        param.ranges="ParamRanges.txt",
        model.drty=model.drty,
        exe.fname=exe.fname,
        out.FUN=read_external_output,
        out.FUN.args=list(file=file.path(model.drty, "input.txt")),
        gof.FUN=sum_gof,
        obs=c(0, 0),
        verbose=FALSE
      ),
      control=list(drty.in=drty.in,
                   drty.out=file.path(tempdir(), "verification-hydromod-paths"),
                   verbose=FALSE,
                   write2disk=TRUE)
    )
  )

  expect_equal(as.numeric(out$gofs), c(3.5, 6))
  expect_equal(nrow(out$model.values), 2)
})

test_that("verification hydromod socket workers export helper functions from par.env", {
  fixture <- make_external_model_fixture()
  par.env <- new.env(parent=globalenv())
  par.env$local_read_model_output <- function(file) {
    value <- as.numeric(trimws(readLines(file, warn=FALSE)[1]))
    c(value, value + 1)
  }
  par.env$local_gof <- function(sim, obs) {
    sum(sim - obs)
  }

  out <- suppressMessages(
    verification(
      fn="hydromod",
      par=matrix(c(1.25, 2.50), ncol=1),
      model.FUN=hydromod,
      model.FUN.args=list(
        param.files=fixture$param.files,
        param.ranges=fixture$param.ranges,
        model.drty=fixture$model.drty,
        exe.fname=test_rscript(),
        exe.args=fixture$script,
        out.FUN="local_read_model_output",
        out.FUN.args=list(file="input.txt"),
        gof.FUN="local_gof",
        obs=c(0, 0),
        verbose=FALSE
      ),
      control=list(drty.out=tempfile("verification-socket-"),
                   verbose=FALSE,
                   write2disk=TRUE,
                   parallel="parallelWin",
                   par.nnodes=2,
                   par.pkgs="hydroPSO",
                   par.env=par.env)
    )
  )

  expect_equal(as.numeric(out$gofs), c(3.5, 6))
  expect_equal(nrow(out$model.values), 2)
})

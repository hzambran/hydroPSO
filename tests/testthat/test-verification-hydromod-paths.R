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

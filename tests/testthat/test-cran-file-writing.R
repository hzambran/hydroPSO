test_that("hydroPSO does not write by default and requires explicit output path", {
  oldwd <- getwd()
  tmp <- tempfile("hydropso-cran-")
  dir.create(tmp)
  on.exit(setwd(oldwd), add=TRUE)
  on.exit(unlink(tmp, recursive=TRUE), add=TRUE)
  setwd(tmp)

  expect_error(
    suppressMessages(
      hydroPSO(fn=sphere, lower=-1, upper=1,
               control=list(maxit=1, npart=3, REPORT=1, verbose=FALSE))
    ),
    NA
  )
  expect_false(file.exists("PSO.out"))

  expect_error(
    suppressMessages(
      hydroPSO(fn=sphere, lower=-1, upper=1,
               control=list(maxit=1, npart=3, REPORT=1,
                            write2disk=TRUE, verbose=FALSE))
    ),
    "drty.out"
  )
})

test_that("verification does not write by default and requires explicit output path", {
  oldwd <- getwd()
  tmp <- tempfile("verification-cran-")
  dir.create(tmp)
  on.exit(setwd(oldwd), add=TRUE)
  on.exit(unlink(tmp, recursive=TRUE), add=TRUE)
  setwd(tmp)

  objective <- function(x) sum(x)

  out <- suppressMessages(
    verification(fn=objective, par=matrix(c(1, 2), ncol=1),
                 control=list(verbose=FALSE))
  )
  expect_false(file.exists("verification"))
  expect_s3_class(out$model.values, "data.frame")

  expect_error(
    suppressMessages(
      verification(fn=objective, par=matrix(c(1, 2), ncol=1),
                   control=list(write2disk=TRUE, verbose=FALSE))
    ),
    "drty.out"
  )
})

test_that("lhoat does not write by default and requires explicit output path", {
  skip_if_not_installed("lhs")

  oldwd <- getwd()
  tmp <- tempfile("lhoat-cran-")
  dir.create(tmp)
  on.exit(setwd(oldwd), add=TRUE)
  on.exit(unlink(tmp, recursive=TRUE), add=TRUE)
  setwd(tmp)

  objective <- function(x) sum(x)

  expect_error(
    suppressWarnings(
      suppressMessages(
      lhoat(fn=objective, lower=c(0, 0), upper=c(1, 1),
            control=list(N=1, verbose=FALSE))
      )
    ),
    NA
  )
  expect_false(file.exists("LH_OAT"))

  expect_error(
    suppressWarnings(
      suppressMessages(
      lhoat(fn=objective, lower=c(0, 0), upper=c(1, 1),
            control=list(N=1, write2disk=TRUE, verbose=FALSE))
      )
    ),
    "drty.out"
  )
})

test_that("PEST converters require explicit output paths", {
  fixture <- make_external_model_fixture()
  pso.out <- file.path(fixture$model.drty, "PSO.out")
  dir.create(pso.out)
  writeLines("1.0 2.0", file.path(pso.out, "Observations.txt"))

  expect_error(
    suppressMessages(
      hydroPSO2pest(
        param.files=fixture$param.files,
        param.ranges=fixture$param.ranges,
        observations.fname=file.path(pso.out, "Observations.txt"),
        exe.fname="model.exe",
        drty.model=fixture$model.drty,
        verbose=FALSE
      )
    ),
    "pst.fname"
  )

  pst.fname <- file.path(fixture$model.drty, "model.pst")
  writeLines(
    c("pcf",
      "* control data",
      "restart estimation",
      "1 1 1 0 1",
      "1 1",
      "* parameter groups",
      "P1 relative 0.01 0.0 switch 2.0 parabolic",
      "* parameter data",
      "P1 none relative 1.0 0.0 10.0 P1 1.0 0.0 1",
      "* observation groups",
      "group1",
      "* observation data",
      "obs1 1.0 1.0 group1",
      "* model command line",
      "model.exe",
      "* model input/output",
      "model.tpl model.in",
      "model.ins model.out"),
    pst.fname
  )

  expect_error(
    suppressMessages(
      pest2hydroPSO(
        pst.fname=pst.fname,
        drty.pest=fixture$model.drty,
        drty.model=fixture$model.drty,
        verbose=FALSE
      )
    ),
    "drty.out"
  )
})

test_that("hydromod requires explicit model directory before writing model files", {
  fixture <- make_external_model_fixture()

  expect_error(
    suppressMessages(
      hydromod(
        param.values=c(P1=1),
        param.files=fixture$param.files,
        param.ranges=fixture$param.ranges,
        exe.fname=fixture$exe.fname,
        out.FUN=fixture$out.FUN,
        out.FUN.args=list(file=fixture$input),
        gof.FUN=fixture$gof.FUN,
        obs=c(0, 1),
        verbose=FALSE
      )
    ),
    "model.drty"
  )
})

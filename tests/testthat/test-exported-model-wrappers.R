test_that("hydromodInR.eval and hydromodInR evaluate a selected particle", {
  particles <- matrix(c(1, 2,
                        3, 4), ncol=2, byrow=TRUE)
  colnames(particles) <- c("a", "b")
  args <- list(obs=c(a=1, b=1))

  out.eval <- hydromodInR.eval(2, particles, test_model_fun, args)
  out.wrapper <- hydromodInR(2, particles, test_model_fun, args)

  expect_equal(out.eval$GoF, 13)
  expect_equal(out.eval$sim, c(7, 8))
  expect_equal(out.wrapper, out.eval)
})

test_that("hydromodInR accepts a single named vector of parameters", {
  particles <- matrix(c(a=3, b=4), nrow=1)
  args <- list(obs=c(a=1, b=1))

  out.matrix <- hydromodInR(1, particles, test_model_fun, args)
  out.vector <- hydromodInR(Particles=c(a=3, b=4),
                            model.FUN=test_model_fun,
                            model.FUN.args=args)
  out.direct <- hydromodInR(c(a=3, b=4),
                            model.FUN=test_model_fun,
                            model.FUN.args=args)

  expect_equal(out.vector, out.matrix)
  expect_equal(out.direct, out.matrix)
  expect_error(hydromodInR(Particles=c(3, 4),
                           model.FUN=test_model_fun,
                           model.FUN.args=args),
               "must be named")
})

test_that("hydromod modifies external input files and returns sim and GoF", {
  fixture <- make_external_model_fixture()

  out <- suppressMessages(
    hydromod(
      param.values=1.25,
      param.files=fixture$param.files,
      param.ranges=fixture$param.ranges,
      model.drty=fixture$model.drty,
      exe.fname=test_rscript(),
      exe.args=fixture$script,
      out.FUN=test_read_model_output,
      out.FUN.args=list(file=fixture$input),
      gof.FUN=test_gof,
      obs=c(0, 0),
      verbose=FALSE
    )
  )

  expect_equal(out$sim, c(1.25, 2.25))
  expect_equal(out$GoF, 3.5)
})

test_that("hydromod verbose message accepts a GoF function object", {
  fixture <- make_external_model_fixture()

  expect_message(
    hydromod(
      param.values=1.25,
      param.files=fixture$param.files,
      param.ranges=fixture$param.ranges,
      model.drty=fixture$model.drty,
      exe.fname=test_rscript(),
      exe.args=fixture$script,
      out.FUN=test_read_model_output,
      out.FUN.args=list(file=fixture$input),
      gof.FUN=test_gof,
      obs=c(0, 0),
      verbose=TRUE
    ),
    "\\[test_gof= 3.5\\]"
  )
})

test_that("lhoat runs a small one-at-a-time sensitivity analysis", {
  set.seed(123)

  out <- suppressMessages(
    lhoat(
      fn=sphere,
      lower=c(0, 0),
      upper=c(1, 1),
      control=list(N=2, write2disk=FALSE, verbose=FALSE, REPORT=1,
                   drty.out=tempfile("lhoat-"))
    )
  )

  expect_named(out, c("ParameterSets", "Ranking"))
  expect_equal(ncol(out$ParameterSets), 2)
  expect_equal(nrow(out$Ranking), 2)
})

test_that("hydroPSO2pest creates PEST control and template files", {
  fixture <- make_external_model_fixture()
  pso.out <- file.path(fixture$model.drty, "PSO.out")
  dir.create(pso.out)
  writeLines("1.0 2.0", file.path(pso.out, "Observations.txt"))
  pst.fname <- file.path(fixture$model.drty, "model.pst")

  suppressMessages(
    hydroPSO2pest(
      param.files=fixture$param.files,
      param.ranges=fixture$param.ranges,
      observations.fname=file.path(pso.out, "Observations.txt"),
      exe.fname="model.exe",
      drty.model=fixture$model.drty,
      pst.fname=pst.fname,
      verbose=FALSE
    )
  )

  expect_true(file.exists(pst.fname))
  expect_true(file.exists(file.path(fixture$model.drty, "file001.tpl")))
  expect_true(file.exists(file.path(fixture$model.drty, "ModelOut.ins")))
})

test_that("pest2hydroPSO imports a minimal PEST control file", {
  fixture <- make_external_model_fixture()
  pst.fname <- file.path(fixture$model.drty, "model.pst")
  drty.out <- file.path(fixture$model.drty, "PSO.from.pest")

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
  writeLines(
    c("ptf #",
      "# P1      #"),
    file.path(fixture$model.drty, "model.tpl")
  )

  suppressMessages(
    pest2hydroPSO(
      pst.fname=pst.fname,
      drty.pest=fixture$model.drty,
      drty.model=fixture$model.drty,
      drty.out=drty.out,
      verbose=FALSE
    )
  )

  expect_true(file.exists(file.path(drty.out, "ParamRanges.txt")))
  expect_true(file.exists(file.path(drty.out, "ParamFiles.txt")))
  expect_true(file.exists(file.path(drty.out, "PEST2hydroPSO_OBS.txt")))
})

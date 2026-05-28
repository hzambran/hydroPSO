test_that("read_best reads the best parameter set file", {
  drty.out <- make_pso_results_fixture()

  out <- suppressMessages(read_best(file.path(drty.out, "BestParameterSet.txt"),
                                    verbose=FALSE))

  expect_named(out, c("best.part.number", "best.param.values", "best.param.gof"))
  expect_equal(ncol(out$best.param.values), 2)
  expect_true(is.numeric(out$best.param.gof))
})

test_that("read_convergence reads convergence measures without plotting", {
  drty.out <- make_pso_results_fixture()

  out <- suppressMessages(read_convergence(file.path(drty.out, "ConvergenceMeasures.txt"),
                                           MinMax="min", plot=FALSE,
                                           verbose=FALSE))

  expect_s3_class(out, "data.frame")
  expect_true(all(c("Iter", "Gbest", "normSwarmRadius") %in% names(out)))
})

test_that("read_GofPerParticle reads particle GoF values without plotting", {
  drty.out <- make_pso_results_fixture()

  out <- suppressMessages(read_GofPerParticle(file.path(drty.out,
                                                        "Particles_GofPerIter.txt"),
                                              plot=FALSE, verbose=FALSE))

  expect_s3_class(out, "data.frame")
  expect_equal(ncol(out), 3)
})

test_that("read_out reads analytical model output values", {
  drty.out <- make_pso_results_fixture()

  out <- suppressMessages(read_out(file.path(drty.out, "Model_out.txt"),
                                   MinMax="min", plot=FALSE, verbose=FALSE))

  expect_named(out, c("model.values", "model.gofs", "model.best", "model.obs"))
  expect_equal(length(out$model.values), 3)
  expect_true(is.numeric(out$model.best))
})

test_that("read_params generic and default method read parameter columns", {
  drty.out <- make_pso_results_fixture()
  file <- file.path(drty.out, "Particles.txt")

  out.generic <- suppressMessages(read_params(file, param.cols=3:4, of.col=2,
                                              plot=FALSE, verbose=FALSE))
  out.default <- suppressMessages(read_params.default(file, param.cols=3:4,
                                                      of.col=2, plot=FALSE,
                                                      verbose=FALSE))

  expect_named(out.generic, c("params", "gofs"))
  expect_equal(out.generic, out.default)
  expect_equal(ncol(out.generic$params), 2)
})

test_that("read_particles and read_velocities read swarm files", {
  drty.out <- make_pso_results_fixture()

  particles <- suppressMessages(read_particles(file.path(drty.out, "Particles.txt"),
                                               MinMax="min", plot=FALSE,
                                               verbose=FALSE))
  velocities <- suppressMessages(read_velocities(file.path(drty.out, "Velocities.txt"),
                                                 MinMax="min", plot=FALSE,
                                                 verbose=FALSE))

  expect_named(particles, c("part.params", "part.gofs", "best.param", "best.gof"))
  expect_named(velocities, c("velocities", "gofs", "best.velocity",
                             "best.gof"))
  expect_equal(ncol(particles$part.params), 2)
  expect_equal(ncol(velocities$velocities), 2)
})

test_that("read_results aggregates the standard hydroPSO output files", {
  drty.out <- make_pso_results_fixture()

  out <- suppressMessages(read_results(drty.out, MinMax="min", verbose=FALSE))

  expect_true(all(c("best.param", "best.gof", "params", "gofs",
                    "velocities", "model.values", "convergence.measures",
                    "part.GofPerIter") %in% names(out)))
  expect_equal(ncol(out$params), 2)
})

test_that("read.ParameterRanges reads hydroPSO range files", {
  fixture <- make_external_model_fixture()

  ranges <- suppressMessages(read.ParameterRanges(fixture$param.ranges,
                                                  verbose=FALSE))
  ranges.full <- suppressMessages(read.ParameterRanges(fixture$param.ranges,
                                                       flag.full=TRUE,
                                                       verbose=FALSE))

  expect_equal(unname(as.matrix(ranges)), matrix(c(0, 10), nrow=1))
  expect_true(all(c("ParameterName", "MinValue", "MaxValue") %in%
                    names(ranges.full)))
})

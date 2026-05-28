test_that("all exported functions have explicit test coverage", {
  expect_setequal(sort(getNamespaceExports("hydroPSO")), sort(tested_exports))
})

test_that("standard analytical benchmark functions return known optima", {
  expect_equal(sphere(c(1, 2, 3)), 14)
  expect_equal(rosenbrock(c(1, 1)), 0)
  expect_equal(rastrigin(c(0, 0)), 0)
  expect_equal(rastrigrin(c(0, 0)), 0)
  expect_equal(griewank(c(0, 0)), 0)
  expect_equal(ackley(c(0, 0)), 0, tolerance=1e-8)
  expect_equal(schafferF6(c(0, 0)), 0)
  expect_equal(schwefel(rep(420.9687, 2)), 0, tolerance=1e-3)
})

test_that("shifted analytical benchmark functions use the supplied origin", {
  x <- c(1, 2)
  expect_equal(ssphere(x, o=x), -450)
  expect_equal(sgriewank(x, o=x), -180)
  expect_equal(sackley(x, o=x), -140)
  expect_equal(srastrigin(x, o=x), -330)
  expect_equal(sschwefel1_2(x, o=x), -450)
  expect_true(is.finite(srosenbrock(x, o=x)))
})

test_that("rLHS samples within parameter ranges", {
  set.seed(123)
  ranges <- matrix(c(-1, 1, 10, 20), ncol=2, byrow=TRUE)
  colnames(ranges) <- c("MinValue", "MaxValue")

  out <- rLHS(5, ranges)

  expect_equal(dim(out), c(5, 2))
  expect_true(all(out[, 1] >= -1 & out[, 1] <= 1))
  expect_true(all(out[, 2] >= 10 & out[, 2] <= 20))
})

test_that("wquantile computes column and row quantiles", {
  x <- matrix(1:12, nrow=4)
  colnames(x) <- c("a", "b", "c")

  col.q <- suppressMessages(wquantile(x, probs=c(0.25, 0.5, 0.75),
                                      verbose=FALSE))
  row.q <- suppressMessages(wquantile(x, probs=c(0.25, 0.5, 0.75),
                                      byrow=TRUE, verbose=FALSE))

  expect_equal(dim(col.q), c(3, 3))
  expect_equal(dim(row.q), c(4, 3))
  expect_equal(as.numeric(col.q[, "50%"]), as.numeric(apply(x, 2, median)))
  expect_equal(as.numeric(row.q[, "50%"]), as.numeric(apply(x, 1, median)))
})

test_that("params2ecdf generic and S3 methods compute one ECDF per parameter", {
  params <- test_params()
  params.df <- as.data.frame(params)

  out.generic <- with_test_device(
    suppressMessages(params2ecdf(params, plot=FALSE, verbose=FALSE))
  )
  out.default <- with_test_device(
    suppressMessages(params2ecdf.default(params, param.names=colnames(params),
                                         plot=FALSE, verbose=FALSE))
  )
  out.matrix <- with_test_device(
    suppressMessages(params2ecdf.matrix(params, plot=FALSE, verbose=FALSE))
  )
  out.data.frame <- with_test_device(
    suppressMessages(params2ecdf.data.frame(params.df, plot=FALSE,
                                            verbose=FALSE))
  )

  expect_length(out.generic, ncol(params))
  expect_equal(names(out.generic), colnames(params))
  expect_equal(out.default, out.matrix)
  expect_equal(out.data.frame, out.matrix)
})

test_that("quant2ecdf generic and S3 methods compute desired quantile ECDFs", {
  sim <- matrix(1:12, nrow=4)
  sim.df <- as.data.frame(sim)
  quantiles.desired <- c(0.25, 0.5, 0.75)
  quantiles.labels <- c("Q25", "Q50", "Q75")

  out.generic <- quiet_test_output(
    suppressMessages(quant2ecdf(sim, plot=FALSE, verbose=FALSE,
                                quantiles.desired=quantiles.desired,
                                quantiles.labels=quantiles.labels))
  )
  out.default <- quiet_test_output(
    suppressMessages(quant2ecdf.default(sim, plot=FALSE, verbose=FALSE,
                                        quantiles.desired=quantiles.desired,
                                        quantiles.labels=quantiles.labels))
  )
  out.matrix <- quiet_test_output(
    suppressMessages(quant2ecdf.matrix(sim, plot=FALSE, verbose=FALSE,
                                       quantiles.desired=quantiles.desired,
                                       quantiles.labels=quantiles.labels))
  )
  out.data.frame <- quiet_test_output(
    suppressMessages(quant2ecdf.data.frame(sim.df, plot=FALSE, verbose=FALSE,
                                           quantiles.desired=quantiles.desired,
                                           quantiles.labels=quantiles.labels))
  )

  expect_equal(names(out.generic), quantiles.labels)
  expect_equal(out.default, out.matrix)
  expect_equal(out.data.frame, out.matrix)
})

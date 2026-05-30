test_that("parallel backend resolver preserves Unix FORK requests", {
  expect_identical(
    hydroPSO:::.resolveParallelBackend("parallel", os.type="unix"),
    "parallel"
  )
})

test_that("parallel backend resolver maps Windows FORK requests to PSOCK", {
  expect_warning(
    out <- hydroPSO:::.resolveParallelBackend("parallel", os.type="windows"),
    "not supported on Windows"
  )
  expect_identical(out, "parallelWin")
})

test_that("parallel backend resolver handles deprecated multicore backend", {
  expect_warning(
    out <- hydroPSO:::.resolveParallelBackend("multicore", os.type="unix"),
    "multicore"
  )
  expect_identical(out, "parallel")

  expect_warning(
    out <- hydroPSO:::.resolveParallelBackend("multicore", os.type="windows"),
    "multicore"
  )
  expect_identical(out, "parallelWin")
})

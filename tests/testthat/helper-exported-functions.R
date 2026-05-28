tested_exports <- c(
  "ackley", "griewank", "hydromod", "hydromodInR", "hydromodInR.eval",
  "hydroPSO", "hydroPSO2pest", "lhoat", "params2ecdf",
  "params2ecdf.data.frame", "params2ecdf.default", "params2ecdf.matrix",
  "pest2hydroPSO", "plot_2parOF", "plot_convergence",
  "plot_GofPerParticle", "plot_NparOF", "plot_out", "plot_params",
  "plot_params.data.frame", "plot_params.default", "plot_params.matrix",
  "plot_ParamsPerIter", "plot_ParamsPerIter.data.frame",
  "plot_ParamsPerIter.matrix", "plot_particles", "plot_results",
  "quant2ecdf", "quant2ecdf.data.frame", "quant2ecdf.default",
  "quant2ecdf.matrix", "rastrigin", "rastrigrin", "read_best",
  "read_convergence", "read_GofPerParticle", "read_out", "read_params",
  "read_params.default", "read_particles", "read_results",
  "read_velocities", "read.ParameterRanges", "rLHS", "rosenbrock",
  "sackley", "schafferF6", "schwefel", "sgriewank", "sphere",
  "srastrigin", "srosenbrock", "sschwefel1_2", "ssphere",
  "verification", "wquantile"
)

test_model_fun <- function(param.values, obs) {
  sim <- c(sum(param.values), sum(param.values) + 1)
  GoF <- sum((param.values - obs)^2)
  list(sim=sim, GoF=GoF)
}

test_gof <- function(sim, obs) {
  sum(sim - obs)
}

test_read_model_output <- function(file) {
  value <- as.numeric(trimws(readLines(file, warn=FALSE)[1]))
  c(value, value + 1)
}

test_rscript <- function() {
  file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
}

make_external_model_fixture <- function() {
  model.drty <- tempfile("hydropso-external-model-")
  drty.in <- file.path(model.drty, "PSO.in")
  dir.create(drty.in, recursive=TRUE)

  writeLines("      0.00", file.path(model.drty, "input.txt"))
  writeLines("quit(status=0)", file.path(model.drty, "noop.R"))
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

  list(
    model.drty=model.drty,
    drty.in=drty.in,
    input=file.path(model.drty, "input.txt"),
    script=file.path(model.drty, "noop.R"),
    param.ranges=file.path(drty.in, "ParamRanges.txt"),
    param.files=file.path(drty.in, "ParamFiles.txt")
  )
}

make_pso_results_fixture <- function() {
  drty.out <- tempfile("hydropso-results-")
  set.seed(246)
  suppressMessages(
    hydroPSO(
      fn=sphere,
      lower=c(Param1=-1, Param2=-1),
      upper=c(Param1=1, Param2=1),
      control=list(maxit=1, npart=3, write2disk=TRUE, verbose=FALSE,
                   REPORT=1, drty.out=drty.out)
    )
  )
  drty.out
}

with_test_device <- function(expr) {
  fname <- tempfile(fileext=".png")
  grDevices::png(fname, width=900, height=700, res=90)
  on.exit(grDevices::dev.off(), add=TRUE)
  value <- force(expr)
  invisible(value)
}

quiet_test_output <- function(expr) {
  value <- NULL
  utils::capture.output(value <- force(expr))
  value
}

test_params <- function() {
  params <- matrix(c(0.10, 0.30, 0.50,
                     0.20, 0.40, 0.60,
                     0.30, 0.50, 0.70,
                     0.40, 0.60, 0.80),
                   ncol=3, byrow=TRUE)
  colnames(params) <- c("alpha", "beta", "gamma")
  params
}

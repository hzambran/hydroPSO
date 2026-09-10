test_that("plot_convergence draws convergence measures", {
  convergence <- data.frame(
    Iter=1:3,
    Gbest=c(3, 2, 1),
    GbestRate=c(0, 50, 100),
    IterBestFit=c(3, 2, 1),
    normSwarmRadius=c(1, 0.6, 0.2),
    GbestPbestRatio=c(1, 2, 3)
  )

  expect_error(
    suppressMessages(
      plot_convergence(convergence, verbose=FALSE, do.png=TRUE,
                       png.fname=tempfile(fileext=".png"))
    ),
    NA
  )
})

test_that("plot_GofPerParticle draws one-panel and multi-panel GoF plots", {
  gofs <- data.frame(Part1=c(3, 2, 1), Part2=c(4, 3, 2))

  expect_error(
    suppressMessages(
      plot_GofPerParticle(gofs, ptype="one", verbose=FALSE, do.png=TRUE,
                          png.fname=tempfile(fileext=".png"))
    ),
    NA
  )
  expect_error(
    suppressMessages(
      plot_GofPerParticle(gofs, ptype="many", verbose=FALSE, do.png=TRUE,
                          png.fname=tempfile(fileext=".png"))
    ),
    NA
  )
})

test_that("plot_params generic and S3 methods draw parameter histograms", {
  params <- test_params()
  params.df <- as.data.frame(params)
  gofs <- c(3, 2, 1, 4)

  expect_error(
    suppressMessages(plot_params(params, gofs=gofs, ptype="histogram",
                                 verbose=FALSE, do.png=TRUE,
                                 png.fname=tempfile(fileext=".png"))),
    NA
  )
  expect_error(
    suppressMessages(plot_params.default(params, gofs=gofs, ptype="histogram",
                                         param.names=colnames(params),
                                         verbose=FALSE, do.png=TRUE,
                                         png.fname=tempfile(fileext=".png"))),
    NA
  )
  expect_error(
    suppressMessages(plot_params.matrix(params, gofs=gofs, ptype="histogram",
                                        verbose=FALSE, do.png=TRUE,
                                        png.fname=tempfile(fileext=".png"))),
    NA
  )
  expect_error(
    suppressMessages(plot_params.data.frame(params.df, gofs=gofs,
                                           ptype="histogram", verbose=FALSE,
                                           do.png=TRUE,
                                           png.fname=tempfile(fileext=".png"))),
    NA
  )
})

test_that("plot_ParamsPerIter generic and S3 methods draw trajectories", {
  params <- test_params()
  params.df <- as.data.frame(params)

  expect_error(
    suppressMessages(plot_ParamsPerIter(params, verbose=FALSE, do.png=TRUE,
                                        png.fname=tempfile(fileext=".png"))),
    NA
  )
  expect_error(
    suppressMessages(plot_ParamsPerIter.matrix(params, verbose=FALSE,
                                               do.png=TRUE,
                                               png.fname=tempfile(fileext=".png"))),
    NA
  )
  expect_error(
    suppressMessages(plot_ParamsPerIter.data.frame(params.df, verbose=FALSE,
                                                   do.png=TRUE,
                                                   png.fname=tempfile(fileext=".png"))),
    NA
  )
})

test_that("plot_out draws simulated versus observed values", {
  expect_error(
    suppressMessages(
      plot_out(sim=c(1, 2, 3), obs=c(1.1, 1.9, 3.2), ptype="corr",
               verbose=FALSE, do.png=TRUE, png.fname=tempfile(fileext=".png"))
    ),
    NA
  )
})

test_that("plot_2parOF and plot_NparOF draw parameter-response surfaces", {
  params <- as.data.frame(test_params())
  gofs <- c(4, 3, 2, 1)

  out <- NULL
  with_test_device({
    out <- plot_2parOF(params=params, gofs=gofs, p1.name="alpha",
                       p2.name="beta", MinMax="min")
    print(out)
  })
  expect_s3_class(out, "trellis")

  expect_error(
    suppressMessages(
      with_test_device(
        plot_NparOF(params=params, gofs=gofs, MinMax="min", verbose=FALSE)
      )
    ),
    NA
  )
})

test_that("plot_particles draws the standard particle diagnostics", {
  params <- test_params()
  gofs <- c(4, 3, 2, 1)

  expect_error(
    suppressWarnings(
      suppressMessages(
        plot_particles(
          params=params,
          gofs=gofs,
          MinMax="min",
          verbose=FALSE,
          do.png=TRUE,
          dotty.png.fname=tempfile(fileext=".png"),
          hist.png.fname=tempfile(fileext=".png"),
          bxp.png.fname=tempfile(fileext=".png"),
          ecdf.png.fname=tempfile(fileext=".png"),
          runs.png.fname=tempfile(fileext=".png"),
          dp3d.png.fname=tempfile(fileext=".png"),
          pairs.png.fname=tempfile(fileext=".png")
        )
      )
    ),
    NA
  )
})

test_that("plot_results draws diagnostics from a hydroPSO output directory", {
  drty.out <- make_pso_results_fixture()

  expect_error(
    suppressWarnings(
      suppressMessages(
        plot_results(
          drty.out=drty.out,
          MinMax="min",
          do.png=TRUE,
          verbose=FALSE,
          dotty.png.fname=tempfile(fileext=".png"),
          hist.png.fname=tempfile(fileext=".png"),
          bxp.png.fname=tempfile(fileext=".png"),
          ecdf.png.fname=tempfile(fileext=".png"),
          pruns.png.fname=tempfile(fileext=".png"),
          dp3d.png.fname=tempfile(fileext=".png"),
          pairs.png.fname=tempfile(fileext=".png"),
          part.png.fname=tempfile(fileext=".png"),
          vruns.png.fname=tempfile(fileext=".png"),
          modelout.best.png.fname=tempfile(fileext=".png"),
          modelout.quant.png.fname=tempfile(fileext=".png"),
          conv.png.fname=tempfile(fileext=".png")
        )
      )
    ),
    NA
  )
})

test_that("plot_results skips model-output plots for incompatible observations", {
  drty.out <- make_pso_results_fixture()
  writeLines(c("1 1", "2 2"), file.path(drty.out, "Observations.txt"))
  modelout.best <- tempfile(fileext=".png")
  modelout.quant <- tempfile(fileext=".png")

  expect_error(
    suppressMessages(plot_results(drty.out=drty.out, MinMax="min",
                                  verbose=FALSE, do.png=TRUE,
                                  skip.incompatible.obs=FALSE)),
    "length\\(obs\\) != ncol\\(sims\\)"
  )

  expect_warning(
    suppressMessages(
      plot_results(
        drty.out=drty.out,
        MinMax="min",
        do.png=TRUE,
        verbose=FALSE,
        skip.incompatible.obs=TRUE,
        dotty.png.fname=tempfile(fileext=".png"),
        hist.png.fname=tempfile(fileext=".png"),
        bxp.png.fname=tempfile(fileext=".png"),
        ecdf.png.fname=tempfile(fileext=".png"),
        pruns.png.fname=tempfile(fileext=".png"),
        dp3d.png.fname=tempfile(fileext=".png"),
        pairs.png.fname=tempfile(fileext=".png"),
        part.png.fname=tempfile(fileext=".png"),
        vruns.png.fname=tempfile(fileext=".png"),
        modelout.best.png.fname=modelout.best,
        modelout.quant.png.fname=modelout.quant,
        conv.png.fname=tempfile(fileext=".png")
      )
    ),
    "Skipping observation-dependent plots"
  )
  expect_false(file.exists(modelout.best))
  expect_false(file.exists(modelout.quant))
})

test_that("plot_verification_results draws diagnostics from verification files", {
  drty.out <- tempfile("hydropso-verification-plots-")
  params <- test_params()

  suppressMessages(
    verification(fn="hydromodInR", par=params,
                 control=list(drty.out=drty.out, MinMax="min",
                              write2disk=TRUE, verbose=FALSE),
                 model.FUN=test_model_fun,
                 model.FUN.args=list(obs=c(0, 0, 0)))
  )

  expect_error(
    suppressWarnings(
      suppressMessages(
        plot_verification_results(
          drty.out=drty.out,
          MinMax="min",
          obs=c(0, 1),
          do.png=TRUE,
          verbose=FALSE,
          dotty.png.fname=tempfile(fileext=".png"),
          hist.png.fname=tempfile(fileext=".png"),
          bxp.png.fname=tempfile(fileext=".png"),
          ecdf.png.fname=tempfile(fileext=".png"),
          pruns.png.fname=tempfile(fileext=".png"),
          dp3d.png.fname=tempfile(fileext=".png"),
          pairs.png.fname=tempfile(fileext=".png"),
          modelout.best.png.fname=tempfile(fileext=".png"),
          modelout.quant.png.fname=tempfile(fileext=".png")
        )
      )
    ),
    NA
  )
})

test_that("plot_verification_results skips incompatible observations", {
  drty.out <- tempfile("hydropso-verification-incompatible-")
  params <- test_params()
  modelout.best <- tempfile(fileext=".png")
  modelout.quant <- tempfile(fileext=".png")

  suppressMessages(
    verification(fn="hydromodInR", par=params,
                 control=list(drty.out=drty.out, MinMax="min",
                              write2disk=TRUE, verbose=FALSE),
                 model.FUN=test_model_fun,
                 model.FUN.args=list(obs=c(0, 0, 0)))
  )

  expect_error(
    suppressMessages(
      plot_verification_results(drty.out=drty.out, MinMax="min",
                                obs=c(1, 2, 3), verbose=FALSE,
                                do.png=TRUE)
    ),
    "length\\(obs\\) != ncol\\(sims\\)"
  )

  expect_warning(
    suppressMessages(
      plot_verification_results(
        drty.out=drty.out,
        MinMax="min",
        obs=c(1, 2, 3),
        do.png=TRUE,
        verbose=FALSE,
        skip.incompatible.obs=TRUE,
        dotty.png.fname=tempfile(fileext=".png"),
        hist.png.fname=tempfile(fileext=".png"),
        bxp.png.fname=tempfile(fileext=".png"),
        ecdf.png.fname=tempfile(fileext=".png"),
        pruns.png.fname=tempfile(fileext=".png"),
        dp3d.png.fname=tempfile(fileext=".png"),
        pairs.png.fname=tempfile(fileext=".png"),
        modelout.best.png.fname=modelout.best,
        modelout.quant.png.fname=modelout.quant
      )
    ),
    "Skipping observation-dependent plots"
  )
  expect_false(file.exists(modelout.best))
  expect_false(file.exists(modelout.quant))
})

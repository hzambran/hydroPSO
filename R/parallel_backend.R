# File parallel_backend.R
# Part of the hydroPSO R package, https://github.com/hzambran/hydroPSO
#                                 http://cran.r-project.org/web/packages/hydroPSO
#                                 http://www.rforge.net/hydroPSO/
# Copyright 2010-2026 Mauricio Zambrano-Bigiarini & Rodrigo Rojas
# Distributed under GPL 2 or later

.resolveParallelBackend <- function(parallel, os.type=.Platform$OS.type) {

  if (parallel == "multicore") {
    parallel <- ifelse(os.type == "windows", "parallelWin", "parallel")
    warning("[ Package 'multicore' is not available anymore in CRAN. It was changed to parallel='", parallel, "' ]",
            call.=FALSE)
    return(parallel)
  } # IF end

  if ( (os.type == "windows") & (parallel == "parallel") ) {
    warning("[ parallel='parallel' uses FORK clusters, which are not supported on Windows. It was changed to parallel='parallelWin' ]",
            call.=FALSE)
    parallel <- "parallelWin"
  } # IF end

  parallel

} # '.resolveParallelBackend' END

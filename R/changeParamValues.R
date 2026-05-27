# Part of the hydroPSO package, https://github.com/hzambran/hydroPSO
# Copyright 2010-2026 Mauricio Zambrano-Bigiarini & Rodrigo Rojas
# Distributed under GPL 2 or later

################################################################################
#                           .changeParamValues                                 #
################################################################################
# Purpose    : Apply replacement, additive, or multiplicative parameter changes #
#              before evaluating an R-based model.                             #
################################################################################
.changeParamValues <- function(x, change.type="repl", refValue=NULL) { # Defines the internal parameter-transformation helper.
  change.type <- match.arg(change.type, choices=c("repl", "addi", "mult"), several.ok=TRUE) # Selects valid parameter-change type(s).
  is.vector.x <- is.null(dim(x)) # Records whether the input was provided as a vector.
  out <- if (is.vector.x) matrix(x, nrow=1) else as.matrix(x) # Uses a matrix internally so vector and matrix inputs share the same code.
  if (!is.numeric(out)) stop("Invalid argument: 'x' must be numeric") # Ensures the parameter values are numeric.
  nparam <- ncol(out) # Counts the number of model parameters.
  if (length(change.type) == 1L) change.type <- rep(change.type, nparam) # Recycles one change type to all parameters.
  if (length(change.type) != nparam) stop("Invalid argument: 'length(change.type)' must be 1 or equal to the number of parameters") # Checks the change-type length.
  if (!any(change.type %in% c("addi", "mult"))) refValue <- NULL # Ignores reference values when all parameters use replacement.
  if (any(change.type %in% c("addi", "mult")) && is.null(refValue)) stop("Missing argument: 'refValue' is required when any 'change.type' is in c('addi', 'mult')") # Requires a reference value for additive and multiplicative changes.
  if (!is.null(refValue) && !is.numeric(refValue)) stop("Invalid argument: 'refValue' must be numeric") # Ensures arithmetic can be applied safely.
  if (length(refValue) == 1L) refValue <- rep(refValue, nparam) # Recycles a scalar reference value to all parameters.
  if (!is.null(refValue) && length(refValue) != nparam) stop("Invalid argument: 'length(refValue)' must be 1 or equal to the number of parameters") # Checks the reference-value length.
  i.addi <- change.type == "addi" # Identifies parameters with additive changes.
  if (any(i.addi)) out[, i.addi] <- sweep(out[, i.addi, drop=FALSE], 2, refValue[i.addi], "+") # Applies additive changes to selected parameters.
  i.mult <- change.type == "mult" # Identifies parameters with multiplicative changes.
  if (any(i.mult)) out[, i.mult] <- sweep(out[, i.mult, drop=FALSE], 2, refValue[i.mult], "*") # Applies multiplicative changes to selected parameters.
  if (is.vector.x) out <- as.numeric(out[1,]) # Restores vector output when the input was a vector.
  if (is.vector.x) names(out) <- names(x) # Preserves vector names when they were provided.
  return(out) # Returns transformed model parameter values.
} # '.changeParamValues' END

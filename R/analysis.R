# -------- #
# ANALYSIS #
# -------- #

#' Evaluate a core collection using the specified objective.
#'
#' @param core Can be either a core collection reference (\code{chcore}) or a
#'   numeric or character vector indicating the indices or names, respectively,
#'   of the individuals in the evaluated core.
#' @param data Core Hunter data (\code{chdata}).
#' @param objective Objective function (\code{chobj}) used to evaluate the core.
#'
#' @return Value of the core when evaluated with the chosen objective (numeric).
#'
#' @seealso \code{\link{coreHunterData}}, \code{\link{objective}}
#'
#' @import rJava
#' @export
evaluateCore <- function(core, data, objective){
  UseMethod("analyzeCore")
}

#' @export
evaluateCore.chcore <- function(core, data, objective){
  evaluateCore(core$sel, data, objective)
}

#' @export
evaluateCore.character <- function(core, data, objective){
  # TODO: convert names to indices and call method below
  # ...
}

#' @export
evaluateCore.numeric <- function(core, data, objective){

  # check arguments
  if(!is(data, 'chdata')){
    stop("Argument 'data' should be of class 'chdata' (see function 'coreHunterData').")
  }
  if(!is(objective, 'chobj')){
    stop("Argument 'objective' should be of class 'chobj' (see function 'objective').")
  }

  # convert objective to Java
  j.objective <- ch.objectives(list(objective))[[1]]

  # evaluate core
  api <- ch.api()
  j.core <- as.integer(core)
  j.data <- data$java
  value <- api$evaluateCore(j.core, j.data, j.objective)

  return(value)

}

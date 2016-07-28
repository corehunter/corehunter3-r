# ------------- #
# CORE SAMPLING #
# ------------- #

#' Sample a core collection from the given data.
#'
#' @param data Core Hunter data (\code{chdata}) containing genotypes,
#'   phenotypes and/or a precomputed distance matrix. Can also be an
#'   object of class \code{chdist}, \code{chgeno} or \code{chpheno}
#'   if only one type of data is provided.
#' @param obj Objective or list of objectives (\code{chobj}).
#'   If no objectives are specified Core Hunter maximizes a weighted
#'   index including the default entry-to-nearest-entry distance
#'   (\code{EN}) for each available data type, with equal weight.
#'   For genotyes, the Modified Roger's distance (\code{MR}) is
#'   used. For phenotypes, Gower's distance (\code{GD}) is applied.
#' @param size Desired core subset size (numeric). If larger than one the value
#'   is used as the absolute core size after rounding. Else it is used as the
#'   sampling rate and multiplied with the dataset size to determine the size of
#'   the core. The default sampling rate is 0.2.
#' @param mode Execution mode (\code{default} or \code{fast}). In default mode,
#'   Core Hunter uses an advanced parallel tempering search algorithm and terminates
#'   when no improvement is found for 5 seconds. In fast mode, a simple stochastic
#'   hill-climbing algorithm is applied and Core Hunter terminates as soon as no
#'   improvement is made for 1 second. Stop conditions can be overriden with
#'   arguments \code{time} and \code{impr.time}.
#' @param normalize If \code{TRUE} (default) the applied objectives in a multi-objective
#'   configuration (two or more objectives) are automatically normalized prior to execution.
#'   Normalization requires a short preliminary search per objective. If a \code{time}
#'   limit is set, at most 20% of the time is spent on normalization. Similarly, when limiting the
#'   time without finding an improvement (\code{impr.time}) during execution, a five times
#'   lower limit is imposed on each preliminary search for the purpose of normalization.
#' @param time Absolute runtime limit in seconds. Not used by default. If used
#'   it should be a strictly positive value and is rounded to the nearest integer.
#' @param impr.time Maximum time without improvement in seconds. When set to
#'   \code{NA} a default value is set depending on the execution \code{mode}.
#'   If set to another value it should be strictly positive and is rounded
#'   to the nearest integer.
#' @param indices If \code{TRUE} the result contains the indices instead of unique
#'   identifiers (default) of the selected individuals.
#' @param verbose If \code{TRUE} search progress messages are printed to the console.
#'   Defaults to \code{FALSE}.
#'
#' @return Core subset (\code{chcore}). It has an element \code{sel}
#'  which is a character or numeric vector containing the ids or indices,
#'  respectively, of the selected individuals (see argument \code{indices}).
#'  In addition the result has one or more elements that indicate the value
#'  of each objective function that was included in the optimization.
#'
#' @examples
#' data <- exampleData()
#'
#' # default size, fast mode, maximize entry-to-nearest-entry Modified Rogers distance
#' obj <- objective("EN", "MR")
#' sampleCore(data, mode = "f", obj)
#'
#' \dontrun{
#' # absolute size
#' sampleCore(data, obj, size = 25)
#' # relative size
#' sampleCore(data, obj, size = 0.1)
#'
#' # other objective: minimize accession-to-nearest-entry precomputed distance
#' sampleCore(data, obj = objective(type = "AN", measure = "PD"))
#' # multiple objectives (equal weight)
#' sampleCore(data, obj = list(
#'  objective("EN", "PD"),
#'  objective("AN", "GD")
#' ))
#' # multiple objectives (custom weight)
#' sampleCore(data, obj = list(
#'  objective("EN", "PD", weight = 0.3),
#'  objective("AN", "GD", weight = 0.7)
#' ))
#'
#' # custom stop conditions
#' sampleCore(data, obj, time = 5, impr.time = 2)
#'
#' # print progress messages
#' sampleCore(data, obj, verbose = TRUE)
#' }
#'
#' @seealso \code{\link{coreHunterData}}, \code{\link{objective}}
#'
#' @import rJava naturalsort
#' @importFrom methods is
#' @importFrom utils capture.output
#' @export
sampleCore <- function(data, obj, size = 0.2, mode = c("default", "fast"), normalize = TRUE,
                       time = NA, impr.time = NA, indices = FALSE, verbose = FALSE){

  # wrap and check data class
  data <- wrapData(data)
  if(!is(data, "chdata")){
    stop("Argument 'data' should be of class 'chdata' (see function 'coreHunterData').")
  }

  # set and check size
  if(!is.numeric(size)){
    stop("Core 'size' should be numeric.")
  }
  n <- data$size
  if(size > 0 && size < 1){
    size <- size * n
  }
  size <- round(size)
  if(size < 2 || size >= n){
    stop(sprintf("Core 'size' should be >= 2 and < %d (dataset size). Got: %d.", n, size))
  }

  # chek mode and stop conditions
  mode <- match.arg(mode)
  if(!is.na(time)){
    if(!is.numeric(time)){
      stop("Time limit should be numeric.")
    }
    time <- as.integer(round(time))
    if(time <= 0){
      stop("Time limit should positive a number (seconds).")
    }
  }
  if(!is.na(impr.time)){
    if(!is.numeric(impr.time)){
      stop("Maximum time without improvement should be numeric.")
    }
    impr.time <- as.integer(round(impr.time))
    if(impr.time <= 0){
      stop("Maximum time without improvement should positive a number (seconds).")
    }
  }

  # check logicals
  if(!is.logical(indices)){
    stop("Argument 'indices' should be a logical.")
  }
  if(!is.logical(verbose)){
    stop("Argument 'verbose' should be a logical.")
  }

  api <- ch.api()

  # set default objectives or check given objectives
  j.data <- data$java
  if(missing(obj)){
    # set default objectives
    obj <- api$createDefaultObjectives(j.data)
    obj <- lapply(obj, function(o){
      objective(
        type = o$getObjectiveType()$getAbbreviation(),
        measure = o$getMeasure()$getAbbreviation(),
        weight = o$getWeight()
      )
    })
  }
  # wrap single objective in list
  if(is(obj, 'chobj')){
    obj <- list(obj)
  }
  # check objectives
  if(!all(sapply(obj, is, 'chobj'))){
    stop("Objectives should be of class 'chobj'.")
  }
  if(length(unique(obj)) != length(obj)){
    stop("Duplicate objectives.")
  }

  # convert objectives to Java objects
  j.obj <- ch.objectives(obj)

  # create Core Hunter arguments
  j.size <- as.integer(size)
  j.obj.array <- .jarray(j.obj, contents.class = ch.obj()@name)
  j.args <- api$createArguments(j.data, j.size, j.obj.array, normalize)

  # run Core Hunter
  if(is.na(time)){
    time <- as.integer(-1)
  }
  if(is.na(impr.time)){
    impr.time <- as.integer(-1)
  }
  sel <- api$sampleCore(j.args, mode, time, impr.time, !verbose)
  if(indices){
    sel <- toRIndices(sel)
  } else {
    # convert indices to ids
    sel <- api$getIdsFromIndices(j.data, .jarray(sel))
  }
  # sort selection
  sel <- naturalsort(sel)

  # wrap result
  core <-list(
    sel = sel
  )
  # add objective function values
  for(o in obj){
    value <- evaluateCore(sel, data, o)
    if(is.null(o$meas)){
      core[[o$type]] <- value
    } else {
      if(is.null(core[[o$type]])){
        core[[o$type]] <- list()
      }
      core[[o$type]][[o$meas]] <- value
    }
  }
  # set class and return
  class(core) <- c("chcore", class(core))
  return(core)

}

# ---------- #
# OBJECTIVES #
# ---------- #

#' Create Core Hunter objective.
#'
#' The following optimization objectives are supported by Core Hunter:
#' \describe{
#'  \item{\code{EN}}{
#'    Average entry-to-nearest-entry distance (default). Maximizes the average distance
#'    between each selected individual and the closest other selected item
#'    in the core. Favors diverse cores in which each individual is sufficiently
#'    different from the most similar other selected item (low redundancy).
#'    Multiple distance measures are provided to be used with this objective (see below).
#'  }
#'  \item{\code{AN}}{
#'    Average accession-to-nearest-entry distance. Minimizes the average distance
#'    between each individual (from the full dataset) and the closest selected item
#'    in the core (which can be the individual itself). Favors representative cores
#'    in which all items from the original dataset are represented by similar individuals
#'    in the selected subset. Multiple distance measures are provided to be used with this
#'    objective (see below).
#'  }
#'  \item{\code{EE}}{
#'    Average entry-to-entry distance. Maximizes the average distance between
#'    each pair of selected individuals in the core. This objective is related to
#'    the entry-to-nearest-entry (EN) distance but less effectively avoids redundant,
#'    similar individuals in the core. In general, use of \code{EN} is preferred.
#'    Multiple distance measures are provided to be used with this objective (see below).
#'  }
#'  \item{\code{SH}}{
#'    Shannon's allelic diversity index. Maximizes the entropy, as used in information
#'    theory, of the selected core. Independently takes into account all allele frequencies,
#'    regardless of the locus (marker) where to which the allele belongs. Requires genotypes.
#'  }
#'  \item{\code{HE}}{
#'    Expected proportion of heterozygous loci. Maximizes the expected proportion of heterzygous
#'    loci in offspring produced from random crossings within the selected core. In contrast to
#'    Shannon's index (\code{SH}) this objective treats each marker (locus) with equal importance,
#'    regardless of the number of possible alleles for that marker. Requires genotypes.
#'  }
#'  \item{\code{CV}}{
#'    Allele coverage. Maximizes the proportion of alleles observed in the full dataset that are
#'    retained in the selected core. Requires genotoypes.
#'  }
#' }
#' The first three objective types (\code{EN}, \code{AN} and \code{EE}) aggregate pairwise distances
#' between individuals. These distances can be computed using various measures:
#' \describe{
#' \item{\code{MR}}{
#'    Modified Rogers distance (default). Requires genotypes.
#'  }
#'  \item{\code{CE}}{
#'    Cavalli-Sforza and Edwards distance. Requires genotypes.
#'  }
#'  \item{\code{GD}}{
#'    Gower distance. Requires phenotypes.
#'  }
#'  \item{\code{PD}}{
#'    Precomputed distances. Uses the precomputed distance matrix of the dataset.
#'  }
#' }
#'
#' @param type Objective type, one of \code{EN} (default), \code{AN}, \code{EE},
#'   \code{SH}, \code{HE} or \code{CV} (see description). The former three
#'   objectives are distance based and require to choose a distance
#'   \code{measure}. By default, Modified Roger's distance is used,
#'   computed from the genotypes.
#' @param measure Distance measure used to compute the distance between two
#'   individuals, one of \code{MR} (default), \code{CE}, \code{GD} or \code{PD}
#'   (see description). Ignored when \code{type} is \code{SH}, \code{HE} or
#'   \code{CV}.
#' @param weight Weight assigned to the objective, when maximizing a weighted
#'   index. Defaults to 1.0.
#'
#' @return Core Hunter objective of class \code{chobj} with elements
#' \describe{
#'  \item{\code{type}}{Objective type.}
#'  \item{\code{weight}}{Assigned weight.}
#'  \item{\code{meas}}{Distance measure (if applicable).}
#' }
#'
#' @examples
#' objective()
#' objective(meas = "PD")
#' objective("EE", "GD")
#' objective("HE")
#'
#' @export
objective <- function(type = c("EN", "AN", "EE", "SH", "HE", "CV"),
                      measure = c("MR", "CE", "GD", "PD"), weight = 1.0){
  # check arguments
  type <- match.arg(type)
  measure <- match.arg(measure)
  if(!is.numeric(weight) || weight < 0.0){
    stop("Objective 'weight' should be a positive number.")
  }
  # create objective
  obj <- list(
    type = type,
    weight = weight
  )
  if(type %in% c("EE", "EN", "AN")){
    obj$meas <- measure
  }
  class(obj) <- c("chobj", class(obj))
  return(obj)
}

#' @export
print.chobj <- function(x, ...){
  prefix <- "Core Hunter objective"
  if(!is.null(x$meas)){
    cat(sprintf("%s: %s (measure = %s, weight = %.2f)", prefix, x$type, x$meas, x$weight))
  } else {
    cat(sprintf("%s: %s (weight = %.2f)", prefix, x$type, x$weight))
  }
}








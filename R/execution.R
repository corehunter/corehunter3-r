# ------------- #
# CORE SAMPLING #
# ------------- #

#' Sample a core collection from the given data.
#'
#' @param data Core Hunter data (\code{chdata}) containing genotypes,
#'   phenotypes and/or a precomputed distance matrix.
#' @param size Desired core subset size (numeric). If larger than one the value
#'   is used as the absolute core size after rounding. Else it is used as the
#'   sampling rate and multiplied with the dataset size to determine the size of
#'   the core. The default sampling rate is 0.2.
#' @param obj Objective or list of objectives (\code{chobj}).
#'   If no objectives are specified and the data contains
#'   only genotypes, phenotypes or precomputed distances, the average
#'   entry-to-nearest entry distance (\code{EN}) is maximized, using
#'   Modified Rogers distance (\code{MR}) for genotypes and Gower
#'   distance (\code{GD}) for phenotypes, respectively.
#' @param indices If \code{TRUE} the result contains the indices instead of names
#'   (default) of the selected individuals.
#'
#' @return Core subset (\code{chcore}). It has an element \code{sel}
#'  which is a character or numeric vector containing the names or indices,
#'  respectively, of the selected individuals (see argument \code{indices}).
#'  In addition the result has one or more elements that indicate the value
#'  of each objective function that was included in the optimization.
#'
#' @seealso \code{\link{coreHunterData}}, \code{\link{objective}}
#'
#' @import rJava
#' @export
sampleCore <- function(data, size = 0.2, obj, indices = FALSE){

  # check data class
  if(!is(data, "chdata")){
    stop("Argument 'data' should be of class 'chdata' (see function 'coreHunterData').")
  }

  # set and check size
  if(!is.numeric(size)){
    stop("Core 'size' should be numeric.")
  }
  n <- getSize(data)
  if(size > 0 && size < 1){
    size <- size * n
  }
  size <- round(size)
  if(size < 2 || size >= n){
    stop(sprintf("Core 'size' should be >= 2 and < %d (dataset size). Got: %d.", n, size))
  }

  # set default objectives or check given objectives
  j.data <- data$java
  if(missing(obj)){
    # check: only genotypes, phenotypes or distances provided
    if(sum(j.data$hasGenotypes(), j.data$hasPhenotypes(), j.data$hasDistances()) != 1){
      stop("Default objective applicable only when data contains only genotypes, phenotypes
            or precomputed distances. Please specify at least one objective.")
    }
    # set default objective
    meas <- "PD"
    if(j.data$hasGenotypes()){
      meas <- "MR"
    }
    if(j.data$hasPhenotypes()){
      meas <- "GD"
    }
    obj <- objective(type = "EN", measure = meas)
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
  api <- ch.api()
  j.size <- as.integer(size)
  j.obj.array <- .jarray(j.obj, contents.class = ch.obj()@name)
  j.args <- api$createArguments(j.data, j.size, j.obj.array)

  # run Core Hunter
  sel <- api$sampleCore(j.args)
  # convert indices to names if requested
  if(!indices){
    sel <- api$getIdsFromIndices(j.data, .jarray(sel))
  }

  # wrap result
  # TODO: add objective function value(s)
  core <-list(
    sel = sel
  )
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
#'  \item{\code{PD}}{
#'    Precomputed distances (default). Uses the precomputed distance matrix of the dataset.
#'  }
#'  \item{\code{GD}}{
#'    Gower distance. Requires phenotypes.
#'  }
#'  \item{\code{MR}}{
#'    Modified Rogers distance. Requires genotypes.
#'  }
#'  \item{\code{CE}}{
#'    Cavalli-Sforza and Edwards distance. Requires genotypes.
#'  }
#' }
#'
#' @param type Objective type, one of \code{EN} (default), \code{AN}, \code{EE},
#'   \code{SH}, \code{HE} or \code{CV} (see description). The former three
#'   objectives are distance based and require to choose a distance
#'   \code{measure}. By default, the precomputed distance matrix is used (if
#'   available).
#' @param measure Distance measure used to compute the distance between two
#'   individuals, one of \code{PD} (default), \code{GD}, \code{MR} or \code{CE}
#'   (see description). Ignored when \code{type} is \code{SH}, \code{HE} or
#'   \code{CV}.
#' @param weight Weight assigned to the objective, when maximizing a weighted
#'   index. Defaults to 1.0.
#'
#' @return Core Hunter objective (\code{chobj}).
#'
#' @export
objective <- function(type = c("EN", "AN", "EE", "SH", "HE", "CV"),
                      measure = c("PD", "GD", "MR", "CE"), weight = 1.0){
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








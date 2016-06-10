# ------------ #
# EXAMPLE DATA #
# ------------ #

#' Read example data for 100 individuals.
#'
#' Reads a precomputed distance matrix from \code{extdata/distances.csv}.
#'
#' @source ...
#'
#' @return data of class \code{chdata}
#' @export
exampleData <- function(){
  getFile <- function(file){
    system.file("extdata", file, package = "corehunter")
  }
  # TODO include genotypes and phenotypes
  coreHunterData(
    distances = distances(file = getFile("distances.csv"))
  )
}

# ---------------- #
# CORE HUNTER DATA #
# ---------------- #

#' Initialize Core Hunter data.
#'
#' The data may contain genotypes, phenotypes and/or a precomputed distance matrix.
#'
#' @param genotypes Genetic marker data (\code{chgeno}).
#' @param phenotypes Phenotypic trait data (\code{chpheno}).
#' @param distances Precomputed distance matrix (\code{chdist}).
#'
#' @return Core Hunter data (\code{chdata}) with elements \code{dist},
#'  \code{geno} and/or \code{pheno}, depending on which data is provided,
#'  as well as an element \code{java} that holds the Java version of the
#'  data object.
#'
#' @examples
#' dist.file <- system.file("extdata", "distances.csv", package = "corehunter")
#' chData <- coreHunterData(distances = distances(file = dist.file))
#' chData
#'
#' @seealso \code{\link{genotypes}}, \code{\link{phenotypes}}, \code{\link{distances}}
#'
#' @export
coreHunterData <- function(distances, genotypes, phenotypes){

  # check arguments
  if(!missing(distances) && !is(distances, "chdist")){
    stop("Argument 'distances' should contain Core Hunter distance matrix data of class 'chdist'.")
  }
  if(!missing(genotypes) && !is(genotypes, "chgeno")){
    stop("Argument 'genotypes' should contain Core Hunter genotype data of class 'chgeno'.")
  }
  if(!missing(phenotypes) && !is(phenotypes, "chpheno")){
    stop("Argument 'phenotypes' should contain Core Hunter phenotype data of class 'chpheno'.")
  }
  if(missing(genotypes) && missing(phenotypes) && missing(distances)){
    stop("Please specify at least one type of data (genotypes, phenotypes and/or distances).")
  }

  # create data
  j.dist <- .jnull(ch.distances())
  j.geno <- .jnull(ch.genotypes())
  j.pheno <- .jnull(ch.phenotypes())
  if(!missing(distances)){
    j.dist <- distances$java
  }
  if(!missing(genotypes)){
    j.geno <- genotypes$java
  }
  if(!missing(phenotypes)){
    j.pheno <- phenotypes$java
  }
  java.obj <- new(ch.data(), j.geno, j.pheno, j.dist)

  # create R object
  data <- list(
    java = java.obj
  )
  if(!missing(distances)){
    data$dist <- distances
  }
  if(!missing(genotypes)){
    data$geno <- genotypes
  }
  if(!missing(phenotypes)){
    data$pheno <- phenotypes
  }
  class(data) <- c("chdata", class(data))

  return(data)

}

#' @export
print.chdata <- function(x, ...){
  data <- x$java
  available <- c()
  if(data$hasGenotypes()){
    available <- c(available, "genotypes")
  }
  if(data$hasPhenotypes()){
    available <- c(available, "phenotypes")
  }
  if(data$hasDistances()){
    available <- c(available, "precomputed distances")
  }
  if(length(available) > 1){
    available <- paste(paste(available[1:(length(available)-1)], collapse = ", "), tail(available, n = 1), sep = " & ")
  }
  cat(sprintf("Core Hunter data containing %s for %d individuals.", available, data$getSize()))
}

# -------------------- #
# DISTANCE MATRIX DATA #
# -------------------- #

#' Create distances data from matrix or file.
#'
#' Specify either a symmetric distance matrix or the file from which to read the matrix.
#' See \url{www.corehunter.org} for documentation and examples of the distance matrix
#' file format used by Core Hunter.
#'
#' @param data Symmetric distance matrix. Unique row and column names are required,
#'  should be the same and are used as item ids. Can be a \code{numeric} matrix or a data frame.
#'  The data frame may optionally include a first column \code{NAME} (\code{character})
#'  used to assign a name to some or all individuals. The remaining columns should
#'  be \code{numeric}.
#' @param file File from which to read the distance matrix.
#'
#' @return Distance matrix data of class \code{chdist} with elements
#' \describe{
#'  \item{\code{data}}{
#'    Distance matrix. Data frame or \code{numeric} matrix, as provided.
#'    Data frame if read from a file.
#'  }
#'  \item{\code{file}}{Path of file from which data was read (if applicable).}
#'  \item{\code{java}}{Java version of the data object.}
#' }
#'
#' @examples
#' # read from file
#'
#' dist.file <- system.file("extdata", "distances.csv", package = "corehunter")
#' dist <- distances(file = dist.file)
#'
#' # create from distance matrix
#'
#' m <- matrix(runif(100), nrow = 10, ncol = 10)
#' diag(m) <- 0
#' # make symmetric
#' m[lower.tri(m)] <- t(m)[lower.tri(m)]
#' # set headers
#' rownames(m) <- colnames(m) <- paste("i", 1:10, sep = "-")
#'
#' dist <- distances(m)
#' dist
#'
#' @import rJava
#' @export
distances <- function(data, file){

  # check input
  if(missing(data) && missing(file)){
    stop("Please specify matrix or file.")
  }
  if(!missing(data) && !missing(file)){
    stop("Please specify either matrix or file, not both.")
  }

  api <- ch.api()

  if(!missing(file)){

    # read from file

    # check file path
    if(!is.character(file)){
      stop("Argument 'file' should be a file path (character).")
    }
    if(!file.exists(file)){
      stop("File 'file' does not exist.")
    }

    # create Java object from file
    java.obj <- api$readDistanceMatrixData(file)
    # read matrix as data frame
    data <- read.csv(file, row.names = 1, check.names = FALSE, as.is = TRUE)

  } else {

    # in memory

    # check type
    if(!is.data.frame(data) && !is.matrix(data)){
      stop("Argument 'matrix' should be a matrix or a data frame.")
    }

    # extract matrix
    names <- NULL
    if(is.matrix(data)){
      matrix <- data
    } else {
      # strip names if provided and convert to matrix
      names <- data$NAME
      if(!is.null(names) && !is.character(names)){
        stop("Column NAME should be of class 'character'.")
      }
      data.without.names <- data
      data.without.names$NAME <- NULL
      matrix <- as.matrix(data.without.names)
    }

    # check matrix
    if(!is.numeric(matrix)){
      stop("Distance matrix should be numeric")
    }
    if(is.null(rownames(matrix)) || is.null(colnames(matrix))){
      stop("Row and column names are required.")
    }
    if(!isSymmetric(matrix)){
      stop("Distance matrix should be symmetric.")
    }

    j.matrix <- .jarray(matrix, dispatch = TRUE)
    j.ids <- .jarray(rownames(data))
    j.names <- .jnull("[S")
    if(!is.null(names)){
      j.names <- .jarray(names)
    }
    java.obj <- api$createDistanceMatrixData(j.matrix, j.ids, j.names)

  }

  # create R object
  dist <- list(
    data = data,
    java = java.obj
  )
  if(!missing(file)){
    dist$file = file
  }
  class(dist) <- c("chdist", "chdata", class(dist))

  return(dist)

}

#' @export
print.chdist <- function(x, ...){
  cat(sprintf("Precomputed distance matrix for %d individuals.", getSize(x)))
}

# ------------- #
# GENOTYPE DATA #
# ------------- #

#' Read genotype data from file.
#'
#' See \url{www.corehunter.org} for documentation and examples of the different
#' genotype data formats supported by Core Hunter.
#'
#' @param file File containing the genotype data.
#' @param format Genotype data format, one of \code{default}, \code{biparental} or \code{frequency}.
#'
#' @return Genotype data of class \code{chdist} with elements
#' \describe{
#'  \item{\code{data}}{Genotypes (data frame).}
#'  \item{\code{file}}{Path of file from which data was read (if applicable).}
#'  \item{\code{java}}{Java version of the data object.}
#' }
#'
#' @examples
#' geno.file <- system.file("extdata", "genotypes.csv", package = "corehunter")
#' geno <- genotypes(geno.file)
#'
#' @import rJava
#' @export
genotypes <- function(file, format = c("default", "biparental", "frequency")){

  # check input
  if(missing(file)){
    stop("File path is required.")
  }
  format <- match.arg(format)

  api <- ch.api()

  # read from file

  # check file path
  if(!is.character(file)){
    stop("Argument 'file' should be a file path (character).")
  }
  if(!file.exists(file)){
    stop("File 'file' does not exist.")
  }

  # read from file
  java.obj <- api$readGenotypeData(file, format)
  # read raw data
  data <- read.csv(file, row.names = 1, check.names = FALSE, as.is = TRUE)

  # create R object
  geno <- list(
    data = data,
    file = file,
    java = java.obj
  )
  class(geno) <- c("chgeno", "chdata", class(geno))

  return(geno)

}

#' @export
print.chgeno <- function(x, ...){
  cat(sprintf("Genotypes for %d individuals.", getSize(x)))
}

# -------------- #
# PHENOTYPE DATA #
# -------------- #

#' Read phenotype data from file.
#'
#' See \url{www.corehunter.org} for documentation and examples
#' of the phenotype data format used by Core Hunter.
#'
#' @param file File containing the phenotype data.
#'
#' @return Phenotype data of class \code{chpheno} with elements
#' \describe{
#'  \item{\code{data}}{Phenotypes (data frame).}
#'  \item{\code{file}}{Path of file from which data was read (if applicable).}
#'  \item{\code{java}}{Java version of the data object.}
#' }
#'
#' @examples
#' pheno.file <- system.file("extdata", "phenotypes.csv", package = "corehunter")
#' pheno <- phenotypes(pheno.file)
#'
#' @import rJava
#' @export
phenotypes <- function(file){

  # check input
  if(missing(file)){
    stop("File path is required.")
  }

  api <- ch.api()

  # read from file

  # check file path
  if(!is.character(file)){
    stop("Argument 'file' should be a file path (character).")
  }
  if(!file.exists(file)){
    stop("File 'file' does not exist.")
  }

  # read from file
  java.obj <- api$readPhenotypeData(file)
  # read raw data
  data <- read.csv(file, row.names = 1, check.names = FALSE, as.is = TRUE)

  # create R object
  pheno <- list(
    data = data,
    file = file,
    java = java.obj
  )
  class(pheno) <- c("chpheno", "chdata", class(pheno))

  return(pheno)

}

#' @export
print.chpheno <- function(x, ...){
  cat(sprintf("Phenotypes for %d individuals.", getSize(x)))
}

# ------- #
# GENERAL #
# ------- #

#' @export
getSize.chdata <- function(data){
  data$java$getSize()
}

# ---------- #
# S3 METHODS #
# ---------- #

#' Retrieve dataset size.
#'
#' @param data data object
#'
#' @examples
#' data <- exampleData()
#' getSize(data)
#'
#' @return dataset size
#' @export
getSize <- function(data){
  UseMethod("getSize")
}

# ----------------- #
# PRIVATE UTILITIES #
# ----------------- #

# Wrap distances, genotypes or phenotypes in Core Hunter data.
# If the given data does not match any of these three classes
# it is returned unchanged.
wrapData <- function(data){
  if(is(data, "chdist")){
    data <- coreHunterData(distances = data)
  }
  if(is(data, "chgeno")){
    data <- coreHunterData(genotypes = data)
  }
  if(is(data, "chpheno")){
    data <- coreHunterData(phenotypes = data)
  }
  return(data)
}








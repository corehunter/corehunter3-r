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
#' @return Core Hunter data (\code{chdata}).
#'
#' @seealso \code{\link{genotypes}}, \code{\link{phenotypes}}, \code{\link{distances}}
#'
#' @export
coreHunterData <- function(genotypes, phenotypes, distances){

  # check arguments
  if(!missing(genotypes) && !is(genotypes, "chgeno")){
    stop("Argument 'genotypes' should contain Core Hunter genotype data of class 'chgeno'.")
  }
  if(!missing(phenotypes) && !is(phenotypes, "chpheno")){
    stop("Argument 'phenotypes' should contain Core Hunter phenotype data of class 'chpheno'.")
  }
  if(!missing(distances) && !is(distances, "chdist")){
    stop("Argument 'distances' should contain Core Hunter distance matrix data of class 'chdist'.")
  }
  if(missing(genotypes) && missing(phenotypes) && missing(distances)){
    stop("Please specify at least one type of data (genotypes, phenotypes and/or distances).")
  }

  # create data
  j.geno <- .jnull(ch.genotypes())
  j.pheno <- .jnull(ch.phenotypes())
  j.dist <- .jnull(ch.distances())
  if(!missing(genotypes)){
    j.geno <- genotypes$java
  }
  if(!missing(phenotypes)){
    j.pheno <- phenotypes$java
  }
  if(!missing(distances)){
    j.dist <- distances$java
  }
  java.obj <- new(ch.data(), j.geno, j.pheno, j.dist)

  # create R object
  data <- list(
    java = java.obj
  )
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

#' @export
getDistanceMatrix.chdata <- function(data){
  if(!data$java$hasDistances()){
    stop("No distances in given Core Hunter data.")
  }
  distances2matrix(data$java$getDistancesData())
}

# -------------------- #
# DISTANCE MATRIX DATA #
# -------------------- #

#' Create distances data from matrix or file.
#'
#' Specify either a symmetric distance matrix or the file from which to read the matrix.
#'
#' @param matrix Symmetric distance matrix. Row and column names are required and used as item ids.
#' @param file File from which to read the distance matrix.
#'
#' @return distance matrix data (\code{chdist}).
#'
#' @import rJava
#' @export
distances <- function(matrix, file){

  # check input
  if(missing(matrix) && missing(file)){
    stop("Please specify matrix or file.")
  }
  if(!missing(matrix) && !missing(file)){
    stop("Please specify either matrix or file, not both.")
  }

  api <- ch.api()

  if(!missing(file)){

    ##################
    # read from file #
    ##################

    # check file path
    if(!is.character(file)){
      stop("Argument 'file' should be a file path (character).")
    }
    if(!file.exists(file)){
      stop("File 'file' does not exist.")
    }

    # read from file
    java.obj <- api$readDistanceMatrixData(file)

  } else {

    #############
    # in memory #
    #############

    if(!is.matrix(matrix) || !is.numeric(matrix)){
      stop("Argument 'matrix' should be a numeric matrix.")
    }
    if(is.null(rownames(matrix)) || is.null(colnames(matrix))){
      stop("Row and column names are required.")
    }
    if(!isSymmetric(matrix)){
      stop("Distance matrix should be symmetric.")
    }

    j.matrix <- .jarray(matrix, dispatch = TRUE)
    j.ids <- .jarray(rownames(matrix))
    j.names <- .jnull("[S") # no explicit names assigned
    java.obj <- api$createDistanceMatrixData(j.matrix, j.ids, j.names)

  }

  # create R object
  file <- ifelse(missing(file), NA, file)
  distances <- list(
    file = file,
    java = java.obj
  )
  class(distances) <- c("chdist", "chdata", class(distances))

  return(distances)

}

#' @export
getDistanceMatrix.chdist <- function(data){
  distances2matrix(data$java)
}

#' @export
print.chdist <- function(x, ...){
  cat(sprintf("Precomputed distance matrix for %d individuals.", getSize(x)))
}

# ------------- #
# GENOTYPE DATA #
# ------------- #

#' Create genotype data.
#'
#' To do.
#'
#' @import rJava
#' @export
genotypes <- function(){

}

# -------------- #
# PHENOTYPE DATA #
# -------------- #

#' Create phenotype data.
#'
#' To do.
#'
#' @import rJava
#' @export
phenotypes <- function(){

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

#' Retrieve precomputed distance matrix.
#'
#' @param data data object containing distances
#'
#' @return distance matrix (numeric)
#' @export
getDistanceMatrix <- function(data){
  UseMethod("getDistanceMatrix")
}

#' Retrieve dataset size.
#'
#' @param data data object
#'
#' @return dataset size
#' @export
getSize <- function(data){
  UseMethod("getSize")
}

# ----------------- #
# PRIVATE UTILITIES #
# ----------------- #

distances2matrix <- function(java.obj){
  api <- ch.api()
  # extract matrix from java object
  matrix <- .jevalArray(api$getDistanceMatrix(java.obj), simplify = TRUE)
  # set headers
  colnames(matrix) <- rownames(matrix) <- api$getIds(java.obj)
  return(matrix)
}









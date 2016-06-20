# ------------ #
# EXAMPLE DATA #
# ------------ #

#' Example dataset with 100 individuals.
#'
#' Reads a precomputed distance matrix from \code{"extdata/distances.csv"},
#' genotypes from \code{"extdata/genotypes.csv"} and phenotypes from
#' \code{"extdata/phenotypes.csv"}.
#'
#' Genotypes and phenotypes are taken from the PowerCore project and, although
#' this is not really the case, it is assumed here that they both describe
#' the same population. Only the first 100 individuals are included.
#' The distance matrix is computed from the phenotypic traits (Gower distance).
#'
#' @source \url{http://bioinformatics.oxfordjournals.org/content/23/16/2155.long}
#'
#' @return data of class \code{chdata}
#' @export
exampleData <- function(){
  getFile <- function(file){
    system.file("extdata", file, package = "corehunter")
  }
  coreHunterData(
    distances = distances(file = getFile("distances.csv")),
    genotypes = genotypes(file = getFile("genotypes.csv")),
    phenotypes = phenotypes(file = getFile("phenotypes.csv"))
  )
}

# ---------------- #
# CORE HUNTER DATA #
# ---------------- #

#' Initialize Core Hunter data.
#'
#' The data may contain genotypes, phenotypes and/or a precomputed distance matrix.
#' All provided data should describe the same individuals which is verified through
#' the item ids and names.
#'
#' @param genotypes Genetic marker data (\code{chgeno}).
#' @param phenotypes Phenotypic trait data (\code{chpheno}).
#' @param distances Precomputed distance matrix (\code{chdist}).
#'
#' @return Core Hunter data (\code{chdata}) with elements
#' \describe{
#'  \item{\code{dist}}{Distance data of class \code{chdist} if included.}
#'  \item{\code{geno}}{Genotype data of class \code{chgeno} if included.}
#'  \item{\code{pheno}}{Phenotype data of class \code{chpheno} if included.}
#'  \item{\code{size}}{Number of individuals in the dataset.}
#'  \item{\code{ids}}{Unique item identifiers.}
#'  \item{\code{names}}{Item names. Names of individuals to which no explicit name
#'    has been assigned are equal to the unique \code{ids}.}
#'  \item{\code{java}}{Java version of the data object.}
#' }
#'
#' @return Core Hunter data of class \code{chdata}.
#'
#' @examples
#' dist.file <- system.file("extdata", "distances.csv", package = "corehunter")
#' geno.file <- system.file("extdata", "genotypes.csv", package = "corehunter")
#' pheno.file <- system.file("extdata", "phenotypes.csv", package = "corehunter")
#' chData <- coreHunterData(
#'   distances(file = dist.file),
#'   genotypes(file = geno.file),
#'   phenotypes(file = pheno.file)
#' )
#'
#' @seealso \code{\link{genotypes}}, \code{\link{phenotypes}}, \code{\link{distances}}
#'
#' @import rJava
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
  j.data <- new(ch.data(), j.geno, j.pheno, j.dist)

  # create R object
  data <- list(
    java = j.data
  )
  size.ids.names <- c("size", "ids", "names")
  if(!missing(distances)){
    data$dist <- distances
    data[size.ids.names] <- distances[size.ids.names]
  }
  if(!missing(genotypes)){
    data$geno <- genotypes
    data[size.ids.names] <- genotypes[size.ids.names]
  }
  if(!missing(phenotypes)){
    data$pheno <- phenotypes
    data[size.ids.names] <- phenotypes[size.ids.names]
  }
  # set class
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
  cat(sprintf("Core Hunter data containing %s for %d individuals.", available, x$size))
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
#' @param data Symmetric distance matrix. Unique row and column headers are required,
#'  should be the same and are used as item ids. Can be a \code{numeric} matrix or a data frame.
#'  The data frame may optionally include a first column \code{NAME} (\code{character})
#'  used to assign names to some or all individuals. The remaining columns should
#'  be \code{numeric}.
#' @param file File from which to read the distance matrix.
#'
#' @return Distance matrix data of class \code{chdist} with elements
#' \describe{
#'  \item{\code{data}}{Distance matrix (\code{numeric} matrix).}
#'  \item{\code{size}}{Number of individuals in the dataset.}
#'  \item{\code{ids}}{Unique item identifiers.}
#'  \item{\code{names}}{Item names. Names of individuals to which no explicit name
#'    has been assigned are equal to the unique \code{ids}.}
#'  \item{\code{file}}{Path of file from which data was read (if applicable).}
#'  \item{\code{java}}{Java version of the data object.}
#' }
#'
#' @examples
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
#'
#' # read from file
#'
#' dist.file <- system.file("extdata", "distances.csv", package = "corehunter")
#' dist <- distances(file = dist.file)
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

  extract.matrix <- function(data){
    # discard names (if set)
    data$NAME <- NULL
    # extract matrix
    matrix <- as.matrix(data)
    # check matrix
    check.matrix(matrix)
    return(matrix)
  }

  check.matrix <- function(matrix){
    if(!is.numeric(matrix)){
      stop("Distance matrix should be numeric")
    }
    if(is.null(rownames(matrix)) || is.null(colnames(matrix))){
      stop("Row and column names are required.")
    }
    if(!isSymmetric(matrix)){
      stop("Distance matrix should be symmetric.")
    }
  }

  if(!missing(file)){

    # read from file

    # check file path
    if(!is.character(file)){
      stop("Argument 'file' should be a file path (character).")
    }
    if(!file.exists(file)){
      stop("File 'file' does not exist.")
    }
    file <- normalizePath(file)

    # create Java object from file
    j.data <- api$readDistanceMatrixData(file)
    # read matrix as data frame
    data <- read.autodelim(file)
    # extract distance matrix
    matrix <- extract.matrix(data)

  } else {

    # in memory

    # check type
    if(!is.data.frame(data) && !is.matrix(data)){
      stop("Argument 'matrix' should be a matrix or a data frame.")
    }

    # extract matrix
    if(is.matrix(data)){
      names <- as.character(rep(NA, nrow(data)))
      matrix <- data
      check.matrix(matrix)
    } else {
      # extract names and convert to matrix
      names <- extract.names(data)
      matrix <- extract.matrix(data)
    }

    j.matrix <- .jarray(matrix, dispatch = TRUE)
    j.ids <- .jarray(rownames(data))
    j.names <- .jarray(names)
    j.data <- api$createDistanceMatrixData(j.matrix, j.ids, j.names)

  }

  # obtain ids and names from Java object
  ids <- api$getIds(j.data)
  names <- api$getNames(j.data)

  # create R object
  dist <- list(
    data = matrix,
    size = j.data$getSize(),
    ids = ids,
    names = names,
    java = j.data
  )
  if(!missing(file)){
    dist$file = file
  }
  class(dist) <- c("chdist", "chdata", class(dist))

  return(dist)

}

#' @export
print.chdist <- function(x, ...){
  cat(sprintf("Precomputed distance matrix for %d individuals.", x$size))
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
#'  \item{\code{size}}{Number of individuals in the dataset.}
#'  \item{\code{ids}}{Unique item identifiers.}
#'  \item{\code{names}}{Item names. Names of individuals to which no explicit name
#'    has been assigned are equal to the unique \code{ids}.}
#'  \item{\code{alleles}}{List of character vectors with allele names per marker.}
#'  \item{\code{file}}{Path of file from which data was read (if applicable).}
#'  \item{\code{java}}{Java version of the data object.}
#' }
#'
#' @examples
#' geno.file <- system.file("extdata", "genotypes.csv", package = "corehunter")
#' geno <- genotypes(geno.file)
#'
#' geno.file.biparental <- system.file("extdata", "genotypes-biparental.csv", package = "corehunter")
#' geno.biparental <- genotypes(geno.file.biparental, format = "biparental")
#'
#' geno.file.freq <- system.file("extdata", "genotypes-frequency.csv", package = "corehunter")
#' geno.freq <- genotypes(geno.file.freq, format = "frequency")
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
  file <- normalizePath(file)

  # read from file
  j.data <- api$readGenotypeData(file, format)
  # read raw data
  data <- read.autodelim(file)
  # drop names
  data$NAME <- NULL
  # clean frequency format
  if(format == "frequency"){
    # drop allele name row
    data <- data[rownames(data) != "ALLELE",]
    # convert to numeric
    for(col in colnames(data)){
      data[[col]] <- as.numeric(data[[col]])
    }
  }

  # obtain ids, names and allele names from Java object
  ids <- api$getIds(j.data)
  names <- api$getNames(j.data)
  alleles <- lapply(.jevalArray(api$getAlleles(j.data)), .jevalArray)

  # create R object
  geno <- list(
    data = data,
    size = j.data$getSize(),
    ids = ids,
    names = names,
    alleles  = alleles,
    file = file,
    java = j.data
  )
  class(geno) <- c("chgeno", "chdata", class(geno))

  return(geno)

}

#' @export
print.chgeno <- function(x, ...){
  cat(sprintf("Genotypes for %d individuals (%d markers).", x$size, x$java$getNumberOfMarkers()))
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
#'  \item{\code{size}}{Number of individuals in the dataset.}
#'  \item{\code{ids}}{Unique item identifiers.}
#'  \item{\code{names}}{Item names. Names of individuals to which no explicit name
#'    has been assigned are equal to the unique \code{ids}.}
#'  \item{\code{types}}{Variable types and encodings.}
#'  \item{\code{ranges}}{Ranges of numeric variables (interval/ratio).
#'    \code{NA} for other variables (nominal/ordinal).}
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
  file <- normalizePath(file)

  # read from file
  j.data <- api$readPhenotypeData(file)
  # read raw data
  data <- read.autodelim(file)
  # drop names
  data$NAME <- NULL
  # extract variable types and encodings
  if("TYPE" %in% rownames(data)){
    types <- data["TYPE",]
  } else {
    types <- rep("NS", ncol(data))
  }
  # drop type, min, max
  data <- data[!(rownames(data) %in% c("TYPE", "MIN", "MAX")), ]
  # convert columns accordingly
  for(c in 1:ncol(data)){
    data[[c]] <- convert.column(data[[c]], types[c])
  }

  # obtain ids, names and variable ranges from Java object
  ids <- api$getIds(j.data)
  names <- api$getNames(j.data)
  ranges <- .jevalArray(api$getRanges(j.data), simplify = TRUE)

  # create R object
  pheno <- list(
    data = data,
    size = j.data$getSize(),
    ids = ids,
    names = names,
    types = types,
    ranges = ranges,
    file = file,
    java = j.data
  )
  class(pheno) <- c("chpheno", "chdata", class(pheno))

  return(pheno)

}

convert.column <- function(col, type){
  enc <- substr(type, 2, 2)
  type <- substr(type, 1, 1)
  # default encoding if not specified
  if(enc == ""){
    enc <- switch(type, "N" = "S", "O" = "I", "I" = "I", "R" = "D")
  }
  # convert to proper encoding
  if(enc == "B"){
    # cfr. Java: boolean
    col <- as.logical(col)
  } else if(enc %in% c("T", "I", "L", "R")){
    # cfr. Java: short, integer, long, big integer
    col <- as.integer(col)
  } else if(enc %in% c("F", "D", "M")){
    # cfr. Java: float, double, big decimal
    col <- as.numeric(col)
  } else if(enc == "S"){
    # cfr. Java: string
    col <- as.character(col)
  } else if(enc == "A"){
    # cfr. Java: date
    col <- as.Date(col, format = "%Y%m%d%H%M%S%z")
  } else {
    stop(sprintf("Unsupported variable encoding '%s'.", enc))
  }
  # convert to proper type
  if(type == "N" && enc != "B"){
    col <- as.factor(col)
  } else if(type == "O"){
    col <- as.ordered(col)
  }
  return(col)
}

#' @export
print.chpheno <- function(x, ...){
  cat(sprintf("Phenotypes for %d individuals (%d traits).", x$size , x$java$getFeatures()$size()))
}

# --- #
# I/O #
# --- #

#' Read delimited file.
#'
#' Delegates to \code{\link{read.delim}} where the separator is inferred from the file extension (CSV or TXT).
#' For CSV files the delimiter is set to \code{","} while for TXT file \code{"\t"} is used. Also sets
#' some default argument values as used by Core Hunter.
#'
#' @param file File path.
#' @param ... Further arguments to be passed to  \code{\link{read.delim}}.
#' @inheritParams utils::read.table
#'
#' @return Data frame.
#' @export
read.autodelim <- function(file, row.names = 1, check.names = FALSE, stringsAsFactors = FALSE, strip.white = TRUE, ...){
  sep <- switch(tolower(tools::file_ext(file)),
                "csv" = ",",
                "txt" = "\t")
  read.delim(file, sep = sep,
             row.names = row.names, check.names = check.names,
             stringsAsFactors = stringsAsFactors, strip.white = strip.white,
             ...)
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

# Extract NAME column from data frame. If no NAME column is included
# the row names (unique ids) are used as names.
extract.names <- function(data){
  names <- data$NAME
  if(is.null(names)){
    names <- rep(NA, nrow(data))
  }
  names <- as.character(names)
  # replace blank names with NAs
  names[names == ""] <- NA
  return(names)
}






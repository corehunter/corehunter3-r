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

#' Create Core Hunter distance data from matrix or file.
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
    data <- subset(data, rownames(data) != "ALLELE")
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

#' Create Core Hunter phenotype data from data frame or file.
#'
#' Specify either a data frame containing the phenotypic trait observations
#' or a file from which to read the data. See \url{www.corehunter.org} for
#' documentation and examples of the phenotype data format used by Core Hunter.
#'
#' @param data Data frame containing one row per individual and one column per trait.
#'   Unique row and column names are required and used as item and trait ids, respectively.
#'   The data frame may optionally include a first column \code{NAME} (\code{character})
#'   used to assign names to some or all individuals.
#'
#' @param types Variable types (optional).
#'   Vector of characters of length one or two.
#'   Ignored when reading from file.
#'
#'   The first letter indicates the scale type and should be one of \code{N} (nominal),
#'   \code{O} (ordinal), \code{I} (interval) or \code{R} (ratio).
#'   Note that in Core Hunter the levels of ordinal variables are ordered naturally and
#'   it is assumed that each level occurs at least once in the data.
#'
#'   The second letter optionally indicates the variable encoding (in Java) and should
#'   be one of \code{B} (boolean), \code{T} (short), \code{I} (integer), \code{L} (long),
#'   \code{R} (big integer), \code{F} (float), \code{D} (double), \code{M} (big decimal),
#'   \code{A} (date) or \code{S} (string). The default encoding is \code{S} (string)
#'   for nominal variables, \code{I} (integer) for ordinal and interval variables
#'   and \code{D} (double) for ratio variables. Interval and ratio variables are
#'   limited to numeric encodings.
#'
#'   If no explicit variable types are specified these are automatically inferred from
#'   the data frame column types and classes, whenever possible. Columns of type
#'   \code{character} are treated as nominal string encoded variables (\code{N}).
#'   Unordered \code{factor} columns are converted to \code{character} and also
#'   treated as string encoded nominals. Ordered factors are converted to
#'   integer encoded interval variables (\code{I}) as described below.
#'   Columns of type \code{logical} are taken to be assymetric binary variables (\code{NB}).
#'   Finally, \code{integer} and more broadly \code{numeric} columns are treated as integer
#'   encoded interval variables (\code{I}) and double encoded ratio variables (\code{R}),
#'   respectively.
#'
#'   Ordinal variables of class \code{ordered} are converted to integers respecting
#'   the order and range of the factor levels and subsequently treated as integer
#'   encoded interval variables (\code{I}). This conversion allows to model the
#'   full range of factor levels also when some might not occur in the data. For other
#'   ordinal variables it is assumed that each value occurs at least once and that
#'   values follow the natural ordering of the chosen data type (in Java).
#'
#'   If explicit types are given for some variables others can still be automatically inferred
#'   by setting their type to \code{NA}.
#'
#' @param min Minimum values of interval or ratio variables (optional).
#'   Numeric vector. Ignored when reading from file.
#'   If undefined for some variables the respective minimum is inferred from the data.
#'   If the data exceeds the minimum it is also updated accordingly.
#'   For nominal and ordinal variables just put \code{NA}.
#' @param max Maximum values of interval or ratio variables (optional).
#'   Numeric vector. Ignored when reading from file.
#'   If undefined for some variables the respective maximum is inferred from the data.
#'   If the data exceeds the maximum it is also updated accordingly.
#'   For nominal and ordinal variables just put \code{NA}.
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
#'  \item{\code{ranges}}{Variable ranges, when applicable (\code{NA} elsewhere).}
#'  \item{\code{file}}{Path of file from which data was read (if applicable).}
#'  \item{\code{java}}{Java version of the data object.}
#' }
#'
#' @examples
#' # read from file
#' pheno.file <- system.file("extdata", "phenotypes.csv", package = "corehunter")
#' pheno <- phenotypes(file = pheno.file)
#'
#' # create from data frame
#' # TODO ...
#'
#' @import rJava
#' @export
phenotypes <- function(data, types, min, max, file){

  # check input
  if(missing(data) && missing(file)){
    stop("Data frame or file path is required.")
  }
  if(!missing(data) && !missing(file)){
    stop("Please specify either data frame or file, not both.")
  }

  # utility function
  read.raw.data <- function(file){
    # read raw data
    data <- read.autodelim(file)
    # drop names
    data$NAME <- NULL
    # extract variable types and encodings
    if("TYPE" %in% rownames(data)){
      types <- as.character(data["TYPE",])
    } else {
      stop("Variable types are required.")
    }
    # drop type, min, max
    data <- subset(data, !(rownames(data) %in% c("TYPE", "MIN", "MAX")))
    # convert columns accordingly
    for(c in 1:ncol(data)){
      data[[c]] <- convert.column(data[[c]], types[c])
    }
    # prepare and return result
    result <- list(data = data, types = types)
    return(result)
  }

  api <- ch.api()

  if(missing(file)){

    # write data to file to be read in Java

    # check data type
    if(!is.data.frame(data)){
      stop("Argument 'data' should be a data frame.")
    }

    # check row and column names
    if(is.null(rownames(data)) || is.null(colnames(data))){
      stop("Unique row and column names are required.")
    }

    # extract item names
    names <- data$NAME
    if(!is.null(names) && !is.character(names)){
      stop("Item names should be of type 'character'.")
    }
    data$NAME <- NULL

    # check and/or infer types and ignore bounds for non-numeric variables
    if(missing(types)){
      types <- rep(NA, ncol(data))
    } else if(length(types) != ncol(data)){
      stop("Number of variable types does not correspond to number of data columns.")
    }
    if(missing(min)){
      min = as.numeric(rep(NA, length(types)))
    }
    if(missing(max)){
      max = as.numeric(rep(NA, length(types)))
    }
    for(t in 1:length(types)){
      if(!is.na(types[t])){
        if(!is.character(types[t]) || (nchar(types[t]) != 1 && nchar(types[t]) != 2)){
          stop("Types should consist of one or two characters.")
        }
        if(!is.numeric(data[[t]])){
          min[t] <- max[t] <- NA
        }
      } else {
        # infer type
        col <- data[[t]]
        if(is.character(col)){
          # character treated as nominal string
          types[t] <- "N"
          min[t] <- max[t] <- NA
        } else if(is.factor(col)){
          if(!is.ordered(col)){
            # unordered factor treated as nominal string
            types[t] <- "N"
            min[t] <- max[t] <- NA
          } else {
            # ordered factor: will be converted to integer (see below)
            types[t] <- "O"
          }
        } else if(is.logical(col)){
          types[t] <- "NB"
          min[t] <- max[t] <- NA
        } else if(is.integer(col)){
          types[t] <- "I"
        } else if(is.numeric(col)){
          types[t] <- "R"
        } else {
          stop("Could not automatically infer variable type.")
        }
      }
      # convert ordinal columns of class ordered to integer (interval)
      if(substr(types[t], 1, 1) == "O" && is.ordered(data[[t]])){
        orig.col <- data[[t]]
        data[[t]] <- as.integer(orig.col)
        types[t] <- "I"
        min[t] <- 1
        max[t] <- length(levels(orig.col))
      }
    }

    # convert all columns to characters
    ids <- rownames(data)
    data <- data.frame(lapply(data, as.character), stringsAsFactors = FALSE, check.names = FALSE)
    rownames(data) <- ids

    # add min and max rows (bottom to top)
    if(!is.numeric(max)){
      stop("Maximum values should be numeric.")
    }
    if(length(max) != ncol(data)){
      stop("Number of maximum values does not correspond to number of data columns.")
    }
    data <- rbind(MAX = max, data)
    if(!is.numeric(min)){
      stop("Maximum values should be numeric.")
    }
    if(length(min) != ncol(data)){
      stop("Number of minimum values does not correspond to number of data columns.")
    }
    data <- rbind(MIN = min, data)
    # add type row
    data <- rbind(TYPE = types, data)

    # reinsert names if set
    if(!is.null(names)){
      names <- c(rep("", sum(rownames(data) %in% c("TYPE", "MIN", "MAX"))), names)
      data <- cbind(NAME = names, data)
    }
    # make row headers first column (ID)
    data <- cbind(ID = rownames(data), data)

    # write temporary file
    tmp <- tempfile(fileext = ".csv")
    write.csv(data, file = tmp, quote = F, row.names = F, na = "")

    # read data into Core Hunter from file
    j.data <- api$readPhenotypeData(tmp)
    # read raw data
    raw.data <- read.raw.data(tmp)

    # remove temporary file
    file.remove(tmp)

  } else {

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
    raw.data <- read.raw.data(file)

  }

  # obtain ids, names and variable ranges from Java object
  ids <- api$getIds(j.data)
  names <- api$getNames(j.data)
  ranges <- .jevalArray(api$getRanges(j.data), simplify = TRUE)

  # create R object
  pheno <- list(
    data = raw.data$data,
    size = j.data$getSize(),
    ids = ids,
    names = names,
    types = raw.data$types,
    ranges = ranges,
    java = j.data
  )
  if(!missing(file)){
    pheno$file <- file
  }
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






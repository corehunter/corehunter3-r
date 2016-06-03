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
#' @import rJava
#' @export
ch.distances <- function(matrix = NA, file = NA){

  # check input
  if(all(is.na(c(matrix, file)))){
    stop("Please specify matrix or file.")
  }
  if(all(!is.na(c(matrix, file)))){
    stop("Please specify either matrix or file, not both.")
  }

  api <- ch.api()

  if(!is.na(file)){

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

    # extract matrix from java object
    matrix <- .jevalArray(api$getDistanceMatrix(java.obj), simplify = TRUE)
    # set headers
    colnames(matrix) <- rownames(matrix) <- api$getIds(java.obj)

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
  distances <- list(
    file = file,
    matrix = matrix,
    java = java.obj
  )
  class(distances) <- c("chdist", "chdata", class(distances))

  return(distances)

}

# ----------------- #
# GENERAL FUNCTIONS #
# ----------------- #

#' @export
print.chdata <- function(x, ...){
  # exclude java object from print
  chdata.stripped <- x[names(x) != "java"]
  print(chdata.stripped)
}













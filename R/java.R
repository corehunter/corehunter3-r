#' @import rJava
ch.api <- function(){
  J("org.corehunter.API")
}

#' @import rJava
ch.executor <- function(){
  new(J("org.corehunter.CoreHunter"))
}

#' @import rJava
ch.data <- function(){
  J("org.corehunter.data.CoreHunterData")
}
#' @import rJava
ch.distances <- function(){
  J("org.corehunter.data.DistanceMatrixData")
}
#' @import rJava
ch.genotypes <- function(){
  J("org.corehunter.data.GenotypeData")
}
#' @import rJava
ch.phenotypes <- function(){
  J("uno.informatics.data.dataset.FeatureData")
}

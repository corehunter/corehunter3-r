.jinit(Sys.glob("../../inst/java/*.jar"))

# --------- #
# TEST DATA #
# --------- #

getFile <- function(file){
  system.file("extdata", file, package = "corehunter")
}

testData <- function(){
  coreHunterData(
    distances = distanceData(),
    genotypes = genotypeData()
  )
}
distanceData <- function(){
  distances(file = distanceFile())
}
genotypeData <- function(format = c("default", "biparental", "frequency")){
  genotypes(file = genotypeFile(format))
}

distanceFile <- function(){
  getFile("distances.csv")
}
readDistanceMatrix <- function(){
  file <- distanceFile()
  matrix <- read.csv(file, row.names = 1, check.names = F, as.is = T)
  return(matrix)
}

genotypeFile <- function(format = c("default", "biparental", "frequency")){
  format <- match.arg(format)
  file <- switch(format,
    "default" = "genotypes.csv",
    "biparental" = "genotypes-biparental.csv",
    "frequency" = "genotypes-frequency.csv"
  )
  getFile(file)
}

# ----------------- #
# UTILITY FUNCTIONS #
# ----------------- #

testSampleCore <- function(...){
  sampleCore(..., mode = "f", time = 1)
}

.jinit(Sys.glob("../../inst/java/*.jar"))

testData <- function(){
  coreHunterData(
    distances = distanceData()
  )
}
distanceData <- function(){
  distances(file = distanceMatrixFile())
}

distanceMatrixFile <- function(){
  system.file("extdata", "distances.csv", package = "corehunter")
}
readDistanceMatrix <- function(){
  file <- distanceMatrixFile()
  matrix <- as.matrix(read.csv(file, row.names = 1, header = T))
  colnames(matrix) <- rownames(matrix)
  return(matrix)
}

testSampleCore <- function(...){
  corehunter::sampleCore(..., mode = "f", time = 1, silent = TRUE)
}

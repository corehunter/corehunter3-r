.jinit(Sys.glob("../../inst/java/*.jar"))

# --------- #
# TEST DATA #
# --------- #

getFile <- function(file){
  normalizePath(system.file("extdata", file, package = "corehunter"))
}

testData <- function(){
  coreHunterData(
    distances = distanceData(),
    genotypes = genotypeData(),
    phenotypes = phenotypeData()
  )
}
distanceData <- function(dataset = c("default", "small")){
  dataset <- match.arg(dataset)
  if(dataset == "small"){
    distances(file = "data/distances-small.txt")
  } else {
    distances(file = distanceFile())
  }
}
genotypeData <- function(dataset = c("default", "small"), format = c("default", "biparental", "frequency")){
  dataset <- match.arg(dataset)
  if(dataset == "small"){
    genotypes(file = "data/genotypes-small.csv")
  } else {
    genotypes(file = genotypeFile(format), format = format)
  }
}
phenotypeData <- function(dataset = c("default", "small")){
  dataset <- match.arg(dataset)
  if(dataset == "small"){
    phenotypes(file = "data/phenotypes-small.csv")
  } else {
    phenotypes(file = phenotypeFile())
  }
}

distanceFile <- function(){
  getFile("distances.csv")
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

phenotypeFile <- function(){
  getFile("phenotypes.csv")
}

getIds <- function(dataset = c("default", "small")){
  dataset <- match.arg(dataset)
  if(dataset == "default"){
    ids <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12",
             "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23",
             "48", "49", "50", "51", "56", "57", "58", "59", "65", "66", "67",
             "68", "69", "70", "71", "72", "73", "74", "75", "76", "77", "78",
             "79", "80", "81", "82", "83", "84", "85", "86", "87", "88", "89",
             "119", "120", "121", "122", "123", "124", "125", "126", "127",
             "128", "133", "134", "166", "167", "168", "182", "183", "184",
             "185", "186", "187", "188", "189", "190", "191", "192", "193",
             "194", "195", "198", "201", "299", "300", "301", "302", "303",
             "304", "305", "306", "316", "317", "318", "351", "353")
  } else {
    ids <- c("Alice", "Dave", "Bob-1", "Bob-2", "Carol")
  }
  return(ids)
}

getNames <- function(dataset = c("default", "small")){
  dataset <- match.arg(dataset)
  if(dataset == "default"){
    names <- c("Bred_0002", "Bred_0004", "Bred_0005", "Bred_0006", "Bred_0007",
               "Bred_0009", "Bred_0010", "Bred_0011", "Bred_0012", "Bred_0013",
               "Bred_0015", "Bred_0017", "Bred_0020", "Bred_0023", "Bred_0027",
               "Bred_0028", "Bred_0030", "Bred_0031", "Bred_0033", "Bred_0035",
               "Bred_0039", "Bred_0040", "Bred_0041", "Bred_0042", "Bred_0043",
               "Bred_0044", "Bred_0045", "Bred_0047", "Bred_0048", "Bred_0051",
               "Bred_0053", "Bred_0054", "Bred_0055", "Bred_0057", "Bred_0058",
               "Bred_0061", "Bred_0062", "Bred_0063", "Bred_0064", "Bred_0065",
               "Bred_0066", "Bred_0067", "Bred_0068", "Bred_0070", "Bred_0072",
               "Bred_0074", "Bred_0075", "Bred_0076", "Bred_0077", "Bred_0078",
               "Bred_0079", "Bred_0080", "Bred_0083", "Bred_0084", "Bred_0085",
               "Bred_0087", "Bred_0088", "Bred_0091", "Bred_0096", "Bred_0099",
               "Bred_0100", "Bred_0101", "Bred_0102", "Bred_0103", "Bred_0105",
               "Bred_0106", "Bred_0107", "Bred_0108", "Bred_0111", "Bred_0112",
               "Bred_0113", "Bred_0114", "Bred_0115", "Bred_0116", "Bred_0117",
               "Bred_0118", "Bred_0119", "Bred_0121", "Bred_0123", "Bred_0125",
               "Bred_0129", "Bred_0132", "Bred_0133", "Bred_0134", "Bred_0135",
               "Bred_0137", "Bred_0140", "Bred_0141", "Bred_0142", "Bred_0143",
               "Bred_0145", "Bred_0146", "Bred_0147", "Bred_0149", "Bred_0152",
               "Bred_0154", "Bred_0156", "Bred_0157", "Bred_0158", "Bred_0159")
  } else {
    names <- c("Alice", "Dave", "Bob", "Bob", "Carol")
  }
  return(names)
}

getRanges <- function(dataset = c("default", "small")){
  dataset <- match.arg(dataset)
  if(dataset == "default"){
    ranges <- c(rep(NA, 28), c(14, 36, 14, 31, 81, 74, 14, 20, 4, 2, 35))
  } else {
    ranges <- c(NA, NA, 10, 2.0, NA)
  }
  return(ranges)
}

# ----------------- #
# UTILITY FUNCTIONS #
# ----------------- #

testSampleCore <- function(...){
  sampleCore(..., mode = "f", time = 1)
}

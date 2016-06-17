# --- #
# API #
# --- #

ch.api <- function(){
  J("org.corehunter.API")
}

# ---- #
# DATA #
# ---- #

ch.data <- function(){
  J("org.corehunter.data.CoreHunterData")
}
ch.distances <- function(){
  J("org.corehunter.data.DistanceMatrixData")
}
ch.genotypes <- function(){
  J("org.corehunter.data.GenotypeData")
}
ch.phenotypes <- function(){
  J("uno.informatics.data.dataset.FeatureData")
}

# ---------- #
# OBJECTIVES #
# ---------- #

ch.obj <- function(){
  J("org.corehunter.CoreHunterObjective")
}

ch.objectives <- function(objectives){
  api <- ch.api()
  j.objectives <- lapply(objectives, function(obj){
    api$createObjective(obj$type, obj$meas, obj$weight)
  })
  return(j.objectives)
}

# ---------------- #
# INDEX CONVERSION #
# ---------------- #

toJavaIndices <- function(indices){
  as.integer(indices-1)
}

toRIndices <- function(indices){
  indices + 1
}



#############
# GENOTYPES #
#############

# read raw data
markers.raw <- read.csv("data-raw/DATA_SET-5_Markers_Cycle-0.csv")

# convert to 0/1/2
markers.raw <- markers.raw + 1

# generate names with random gaps
n <- nrow(markers.raw)
m <- ncol(markers.raw)
names <- paste("Bred", sprintf("%04d", sort(sample(2*n, size = n))), sep = "_")

# compose biparental data frame with names and allele scores
biparental <- data.frame(NAME = names, markers.raw)

# write biparental genotype file
biparental.out <- data.frame(ID = rownames(markers.raw), biparental)
file <- "inst/extdata/genotypes-biparental.csv"
write.csv(biparental.out, file = file, row.names = FALSE, quote = FALSE)
chGeno <- genotypes(file = file, format = "bi")

# infer frequencies and compose default format
freqs <- default <- matrix(NA, nrow = n, ncol = 2*m)
colnames(freqs) <- colnames(default) <- rep(chGeno$markers, each = 2)
for(i in 1:n){
  for(j in 1:m){
    score <- markers.raw[i, j]
    if(score == 0){
      freqs[i, 2*j-1]   <- 1.0
      freqs[i, 2*j]     <- 0.0
      default[i, 2*j-1] <- "A"
      default[i, 2*j]   <- "A"
    } else if(score == 2){
      freqs[i, 2*j-1] <- 0.0
      freqs[i, 2*j]   <- 1.0
      default[i, 2*j-1] <- "B"
      default[i, 2*j]   <- "B"
    } else {
      freqs[i, 2*j-1] <- 0.5
      freqs[i, 2*j] <-   0.5
      if(runif(1) > 0.5){
        default[i, 2*j-1] <- "A"
        default[i, 2*j]   <- "B"
      } else {
        default[i, 2*j-1] <- "B"
        default[i, 2*j]   <- "A"
      }
    }
  }
}
freqs <- data.frame(NAME = names, freqs)
default <- data.frame(NAME = names, default)

# write frequency file
freqs.out <- data.frame(ID = rownames(freqs), freqs)
file <- "inst/extdata/genotypes-frequency.csv"
write.csv(freqs.out, file = file, row.names = FALSE, quote = FALSE)
chGeno2 <- genotypes(file = "inst/extdata/genotypes-frequency.csv", format = "freq")

# write default format
default.out <- data.frame(ID = rownames(default), default)
file <- "inst/extdata/genotypes.csv"
write.csv(default.out, file = file, row.names = FALSE, quote = FALSE)
chGeno3 <- genotypes(file = "inst/extdata/genotypes.csv")

##############
# PHENOTYPES #
##############

# read raw data
pheno.raw <- read.csv("data-raw/DATA_SET-5_Phenotypic-Means_Cycle-0.csv")
pheno.raw$entry <- NULL

# add names
pheno <- data.frame(NAME = names, pheno.raw, stringsAsFactors = FALSE)

# write phenotype data
pheno.out <- rbind(
  c("TYPE", "", rep("RD", 4)),
  data.frame(ID = rownames(pheno), pheno, stringsAsFactors = FALSE)
)
file <- "inst/extdata/phenotypes.csv"
write.csv(pheno.out, file = file, row.names = FALSE, quote = FALSE)
chPheno <- phenotypes(file = "inst/extdata/phenotypes.csv")

#############
# DISTANCES #
#############

# precompute Modified Rogers' distances
freqs <- chGeno2$data
dist <- matrix(NA, nrow = n, ncol = n)
for(i in 1:n){
  for(j in 1:n){
    dist[i,j] <- 1/sqrt(2*m) * sqrt(sum((freqs[i,] - freqs[j,])^2))
  }
}

# set row and column names (ids)
rownames(dist) <- colnames(dist) <- chGeno2$ids

# add names
dist <- data.frame(NAME = names, dist, check.names = FALSE)

# write distance file
dist.out <- data.frame(ID = rownames(dist), dist, check.names = FALSE)
file <- "inst/extdata/distances.csv"
write.csv(dist.out, file = file, row.names = FALSE, quote = FALSE)
chDist <- distances(file = "inst/extdata/distances.csv")









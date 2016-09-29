#' Core Hunter 3
#'
#' A fast and flexible core subset selection tool.
#'
#' @examples
#' # sample core based on genetic marker data
#' geno.file <- system.file("extdata", "genotypes.csv", package = "corehunter")
#' geno <- genotypes(file = geno.file)
#' sampleCore(geno)
#'
#' # sample core based on phenotypic traits
#' pheno.file <- system.file("extdata", "phenotypes.csv", package = "corehunter")
#' pheno <- phenotypes(file = pheno.file)
#' sampleCore(pheno)
#'
#' # sample core based on precomputed distance matrix
#' dist.file <- system.file("extdata", "distances.csv", package = "corehunter")
#' dist <- distances(file = dist.file)
#' sampleCore(dist)
#'
#' # sample core from genotypes with custom objective (allelic richness)
#' sampleCore(geno, obj = objective("HE"))
#'
#' # sample core based on both genotypes and phenotypes
#' geno.pheno <- coreHunterData(geno, pheno)
#' sampleCore(geno.pheno)
#'
#' @seealso \code{\link{coreHunterData}}, \code{\link{genotypes}},
#'  \code{\link{phenotypes}}, \code{\link{distances}},
#'  \code{\link{sampleCore}}, \code{\link{evaluateCore}},
#'  \code{\link{objective}}
#'
#' @docType package
#' @name corehunter
NULL

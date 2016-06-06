#' @import rJava
.onLoad <- function(libname, pkgname) {
  print("--- onLoad {")
  .jpackage(pkgname, lib.loc = libname)
  print(dir(recursive = T))
  print(.jclassPath())
  print("--- onLoad }")
}

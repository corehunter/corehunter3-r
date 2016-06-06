#' @import rJava
.onLoad <- function(libname, pkgname) {
  .jpackage(pkgname, lib.loc = libname)
  print(dir(recursive = T))
  print(.jclassPath())
}

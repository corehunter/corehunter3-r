#' @import rJava
ch.api <- function(){
  J("org.corehunter.API")
}

#' @import rJava
ch.executor <- function(){
  new(J("org.corehunter.CoreHunter"))
}

#' Chaos01
#' 
#' Computes and visualize the results of the 0-1 test for chaos proposed
#' by Gottwald and Melbourne (2004) <DOI:10.1137/080718851>. The algorithm is
#' available in parallel for the independent values of parameter c. Additionally,
#' fast RQA is added to distinguish chaos from noise.
#' 
#' @docType package
#' @author Tomas Martinovic
#' @name Chaos01

.onUnload <- function (libpath) {
  #' @keywords internal
  library.dynam.unload("Chaos01", libpath)
}
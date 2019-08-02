plot.chaos01.rqa.sequence <- function(x, plotvar = c("RR", "DET"), type = NULL, ...){
    #' Plot the results for the sequence of eps values.
    #'
    #' This function plot the selected variables of RQA as a sequence for the different values of epsilon.
    #' @param x the object of "rqa.sequence" class, produced by rqa.seq function.
    #' @param plotvar vector/list of strings of variables which should be plotted.
    #' \itemize{
    #'     \item "TR"    - Total number of recurrence points
    #'     \item "DLRP"  - Recurrence points on the diagonal lines of length of length >= lmin
    #'     \item "DLC"   - Count of diagonal lines of length of length >= lmin
    #'     \item "VLRP"  - Recurrence points on the vertical lines of length of length >= lmin
    #'     \item "VLC"   - Count of vertical lines of length of length >= lmin
    #'     \item "RR"    - Recurrence rate
    #'     \item "DET"   - Determinism, count recurrence points in diagonal lines of length >= lmin 
    #'     \item "RATIO" - DET/RR
    #'     \item "AVG"   - average length of diagonal lines of length >= lmin
    #'     \item "MAX"   - maximal length of diagonal lines of length >= lmin
    #'     \item "LAM"   - Laminarity, VLRP/TR
    #'     \item "TT"    - Trapping time, average length of vertical lines of length >= lmin
    #'     \item "MAX_V" - maximal length of vertical lines of length >= lmin
    #'     \item "DIV"   - Divergence, 1/MAX
    #' }
    #' 
    #' Default = c("RR", DET"). 
    #' @param type string what type of plot should be drawn: see\code{\link[graphics]{plot}}
    #' @param ... arguments to be passed as graphical parameters.
    #' @importFrom graphics plot
    #' @keywords plot rqa threshold
    #' @method plot chaos01.rqa.sequence
    #' @export
    #' @seealso \code{\link{rqa.seq}}, \code{\link{fast.rqa}}
    #' @examples
    #' vec.x <- gen.logistic(mu = 3.55, iter = 2000)
    #' 
    #' x.range <- diff(range(vec.x))
    #' 
    #' from = 0.01 * x.range
    #' by   = 0.1 * x.range
    #' 
    #' # Output for each value of c
    #' res <- rqa.seq(vec.x, from = from, to = x.range, by = by, TS = vec.x, dim = 3, lag = 10)
    #'
    #' plotvar <- c("RR", "DET", "RATIO", "LAM")
    #'
    #' par(mfrow = c(2,2))
    #' plot(res, plotvar = plotvar)
    #' @references
    #' N. Marwan; M. C. Romano; M. Thiel; J. Kurths (2007). "Recurrence Plots for the Analysis of Complex Systems". Physics Reports. 438 (5-6): 237. Bibcode:2007PhR...438..237M. doi:10.1016/j.physrep.2006.11.001.
    
  eps <- unlist(lapply(x, function(y)y$settings$eps))
 
  if(is.null(type)){type = "l"}
  
  for(plots in plotvar)
    switch(plots,
           "RR"    = {RR     <- unlist(lapply(x, function(y)y$RQA$RR))
                      plot(eps, RR, type = type, ...)},
           "RATIO" = {RATIO  <- unlist(lapply(x, function(y)y$RQA$RATIO))
                      plot(eps, RATIO, type = type, ...)},
           "DET"   = {DET    <- unlist(lapply(x, function(y)y$RQA$DET))
                      plot(eps, DET, type = type, ...)},
           "AVG"   = {AVG    <- unlist(lapply(x, function(y)y$RQA$AVG))
                      plot(eps, AVG, type = type, ...)},
           "MAX"   = {MAX    <- unlist(lapply(x, function(y)y$RQA$MAX))
                      plot(eps, MAX, type = type, ...)},
           "LAM"   = {LAM    <- unlist(lapply(x, function(y)y$RQA$LAM))
                      plot(eps, LAM, type = type, ...)},
           "TT"    = {TT     <- unlist(lapply(x, function(y)y$RQA$TT))
                      plot(eps, TT, type = type, ...)},
           "DIV"   = {DIV    <- unlist(lapply(x, function(y)y$RQA$DIV))
                      plot(eps, DIV, type = type, ...)},
           "TR"    = {TR     <- unlist(lapply(x, function(y)y$RQA$TR))
                      plot(eps, TR, type = type, ...)},
           "DLRP"  = {DLRP   <- unlist(lapply(x, function(y)y$RQA$DLRP))
                      plot(eps, DLRP, type = type, ...)},
           "DLC"   = {DLC    <- unlist(lapply(x, function(y)y$RQA$DLC))
                      plot(eps, DLC, type = type, ...)},
           "VLRP"  = {VLRP   <- unlist(lapply(x, function(y)y$RQA$VLRP))
                      plot(eps, VLRP, type = type, ...)},
           "VLC"   = {VLC    <- unlist(lapply(x, function(y)y$RQA$VLC))
                      plot(eps, VLC, type = type, ...)},
           "MAX_V" = {MAX_V  <- unlist(lapply(x, function(y)y$RQA$MAX_V))
                      plot(eps, MAX_V, type = type, ...)}
           )
  
}
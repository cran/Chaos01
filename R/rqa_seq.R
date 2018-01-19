rqa.seq <- function(from, to = NULL, by, TS, dim = 2, lag = 1, theta = 1, lmin = 3, use.by = TRUE, length.out = 100, include.TS = FALSE){
  #' Function to compute diagonal RQA measures for given time series and sequence of thresholds.
  #'
  #' This function is a wrapper for the rqa function to compute RQA for a sequence of thresholds. 
  #' It computes results of the RQA from the numeric vector (time series) for a sequence of thresholds 
  #' given by standard parameter of the seq() function.
  #' @param from double, smallest value of epsilon (threshold to be used for the computation of the rqa)
  #' @param to double, largest value of epsilon, passed to the "to" parameter of seq(). If NULL, it is set to diff(range(TS)), which is maximum possible distance in TS. Default is NULL.
  #' @param by double, increment of the sequence of threshold values, passed to the "by" parameter of seq().
  #' @param TS the input vector, This should be a numeric vector. (e.g. ts object is also accepted)
  #' @param dim integer, embedding dimension. Default is 2.
  #' @param lag integer, embedding lag/delay. Default is 1.
  #' @param theta integer, Theiler window, number of diagonal lines which should be skipped from the main diagonal. 
  #'  \itemize{
  #'    \item 0 - include main diagonal/LOS into computation
  #'    \item 1 - do not include main diagonal.
  #'    \item 2 - skip main diagonal and 1 diagonal closest to main diagonal.
  #'    \item 3 - etc.
  #'  }
  #' 
  #' Default is 1.
  #' @param lmin integer, minimal length of line to be considered for recurrence line. Default is 3.
  #' @param use.by logical, indicate whether to use by statement, or length.out statement. If TRUE "by" is used. Default is TRUE.
  #' @param length.out integer, desired number of computation of rqa, passed to the "length.out" parameter of seq(). Used if "use.by = FALSE"
  #' as an alternative to creating the sequence of threshold values.
  #' @param include.TS logical, if TRUE input time series will be added to the list of outputs. Default is FALSE.
  #' @seealso \code{\link{fast.rqa}}, \code{\link{plot.chaos01.rqa.sequence}}
  #' @keywords determinism, test, rqa
  #' @export
  #' @examples
  #' vec.x <- gen.logistic(mu = 3.55, iter = 2000)
  #' 
  #' x.range <- diff(range(vec.x))
  #' 
  #' from = 0.01 * x.range
  #' by   = 0.1 * x.range
  #' 
  #' #Output for each value of eps
  #' res <- rqa.seq(vec.x, from = from, to = x.range, by = by, TS = vec.x, dim = 3, lag = 10)
  #' 
  #' \dontrun{
  #' #It is a good idea to get a grasp on how RQA develop for different colored noise.
  #' if(requireNamespace(tuneR)){
  #' pink  <- tuneR::noise(kind = "pink", duration = 1000)@left
  #' red   <- tuneR::noise(kind = "red", duration = 1000)@left
  #' power <- tuneR::noise(kind = "power", duration = 1000)@left
  #' white <- tuneR::noise(kind = "white", duration = 1000)@left
  #' 
  #' start <- 0.001 * diff(range(TS))
  #' end   <- 1.0   * diff(range(TS))
  #' step  <- 0.01  * diff(range(TS))
  #'
  #' rqa.pink  <- Chaos01::rqa.seq(start, end, step, pink, dim, lag, theta, lmin)
  #' rqa.red   <- Chaos01::rqa.seq(start, end, step, red, dim, lag, theta, lmin)
  #' rqa.power <- Chaos01::rqa.seq(start, end, step, power, dim, lag, theta, lmin)
  #' rqa.white <- Chaos01::rqa.seq(start, end, step, white, dim, lag, theta, lmin)
  #' 
  #' plotvar <- c("RR", "RATIO", "DET", "LAM", "AVG", "TT", "MAX", "MAX_V")
  #' 
  #' par(mfrow = c(4,2))
  #' plot(rqa.pink, plotvar)
  #' plot(rqa.red, plotvar)
  #' plot(rqa.power, plotvar)
  #' plot(rqa.white, plotvar)
  #' }
  #' }
  #' @references
  #' Marwan; M. C. Romano; M. Thiel; J. Kurths (2007). "Recurrence Plots for the Analysis of Complex Systems". Physics Reports. 438 (5-6): 237. Bibcode:2007PhR...438..237M. doi:10.1016/j.physrep.2006.11.001.
  #' 
  #' Zbilut, J.; Webber C., L. (2006). "Recurrence Quantification Analysis". Wiley Encylopedia of Biomedical Engineering, SN: 9780471740360, doi: 10.1002/9780471740360.ebs1355
  #' @details
  #' RQA analysis tool is included in this package because '0-1 test for chaos' can determine whether the dynamics of the system is chaotic or regular, but cannot distinguish between chaotic and random dynamics.
  #' 
  #' It should be possible to determine whether the system is deterministic or not, based on the evolution of RQA measure with increasing thresholds 'eps'. For this it is necessary to compute RQA many times and therefore this
  #' fast version of RQA computation is provided. Function rqa.seq is wrapper for the \code{\link{fast.rqa}} function and computes RQA for a sequence of 'eps' values.
  #' 
  #' Results of this function can be easily visualized by the plot function. See \code{\link{plot.chaos01.rqa.sequence}} for more information.
  #' 
  #' Usually, RQA is computed from a state-space reconstruction of the dynamical system. In this case Takens embedding is used [3]. It is necessary to set two parameters for Takens embedding: embeding dimension and delay time.
  #' If You have no prior knowledge about the system, it is possible to estimate best values for these parameters according to the first minimal value of the mutual information of the time series and the minimal value of the false 
  #' nearest neighbour algorithm. These routines can be found in e.g. 'nonlinearTseries' package and 'fNonlinear' package. 
  #' @return
  #' Returns "chaos01.rqa.seq" object, as a list of "chaos01.rqa" objects for every "eps" given by the input parameters.
  #' 

  #================= Check Input =====================
    
  if(!(is.numeric(TS) || is.integer(TS))){
    stop("Error: 'TS' is not a numeric.")
  }
  
  if(!(is.numeric(dim) || is.integer(dim))){
    stop("Error: 'dim' is not an integer.")
  }  
  
  if(!(is.numeric(lag) || is.integer(lag))){
    stop("Error: 'lag' is not an integer.")
  }
  
  if(!(is.numeric(theta) || is.integer(theta))){
    stop("Error: 'theta' is not an integer.")
  }
  
  if(!(is.numeric(lmin) || is.integer(lmin))){
    stop("Error: 'lmin' is not an integer.")
  }
  
  if(!is.logical(use.by)){
    stop("Error: 'use.by' is not an integer")
  }
  
  if(!is.logical(include.TS)){
    stop("Error: 'include.TS' is not an integer")
  }
  
  #================= Create sequence of threshold values =====================
  
  if(is.null(to)){to = diff(range(TS))}
  
  if(use.by){
    eps.seq <- seq(from = from, to = to, by = by)
  } else{
    eps.seq <- seq(from = from, to = to, length.out = length.out)
  }
  
  #================= Compute RQA for all the thresholds =====================
  
  RQA <- lapply(eps.seq, function(eps){
      Chaos01::fast.rqa(TS, dim, lag, eps, theta, lmin, include.TS)
    }
  )
  
  class(RQA) <- "chaos01.rqa.sequence"
  
  return(RQA)
} 
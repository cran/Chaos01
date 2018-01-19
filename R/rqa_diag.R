fast.rqa <- function(TS, dim = 2, lag = 1, eps, theta = 1, lmin = 3, include.TS = FALSE){
  #' Function to compute diagonal RQA measures for given time series
  #'
  #' This function computes results of the RQA from the numeric vector (time series).
  #' @useDynLib Chaos01, diag_rqa_max
  #' @param TS the input vector, This should be a numeric vector. (e.g. ts object is also accepted)
  #' @param dim integer, embedding dimension. See details for more information. Default is 2.
  #' @param lag integer, embedding lag/delay. See details for more information. Default is 1.
  #' @param eps double, threshold/neighbourhood size.
  #' @param theta integer, Theiler window, number of diagonal lines which should be skipped from the main diagonal. 
  #'  \itemize{
  #'    \item 0 - include main diagonal/LOS into computation
  #'    \item 1 - do not include main diagonal.
  #'    \item 2 - skip main diagonal and 1 diagonal closest to main diagonal.
  #'    \item 3 - etc.
  #'  }
  #'  Default is 1.
  #' @param lmin integer, minimal length of line to be considered for recurrence line. Default is 3
  #' @param include.TS logical, if TRUE input time series will be added to the list of outputs. Default is FALSE.
  #' @seealso \code{\link{rqa.seq}}, \code{\link{plot.chaos01.rqa.sequence}}, \code{\link{summary.chaos01.rqa}}
  #' @keywords determinism, test, rqa
  #' @export
  #' @examples
  #' vec.x <- gen.logistic(mu = 3.55, iter = 2000)
  #' 
  #' res <- fast.rqa(vec.x, dim = 3, lag = 10, eps = 0.3)
  #' summary(res)
  #' @references
  #' [1] Marwan; M. C. Romano; M. Thiel; J. Kurths (2007). "Recurrence Plots for the Analysis of Complex Systems". Physics Reports. 438 (5-6): 237. Bibcode:2007PhR...438..237M. doi:10.1016/j.physrep.2006.11.001.
  #' 
  #' [2] Zbilut, J.; Webber C., L. (2006). "Recurrence Quantification Analysis". Wiley Encylopedia of Biomedical Engineering, SN: 9780471740360, doi: 10.1002/9780471740360.ebs1355
  #' 
  #' [3] F. Takens (1981). "Detecting strange attractors in turbulence". In D. A. Rand and L.-S. Young. Dynamical Systems and Turbulence, Lecture Notes in Mathematics, vol. 898. Springer-Verlag. pp. 366–381.
  #' @details
  #' RQA analysis tool is included in this package because '0-1 test for chaos' can determine whether the dynamics of the system is chaotic or regular, but cannot distinguish between chaotic and random dynamics.
  #' 
  #' It should be possible to determine whether the system si deterministic or not based on the evolution of RQA measure with increasing thresholds 'eps'. For this it is necessary to compute RQA many times and therefore this
  #' fast version of RQA computation is provided. To further improve workflow in examining the system \code{\link{rqa.seq}} is provided to compute RQA for sequence of 'eps' values and resulting object can be easily visualized by the plot function.
  #' 
  #' This version of RQA is based on the optimized algorithms for RQA computation given at \url{https://code.it4i.cz/ADAS/RQA_HPC}. Main difference is in reduction of the memory complexity by not storing histogram. Due to this Shannon entropy is not computed,
  #' but the algorithm is faster. Additionally, distance metric is set to the maximum distance. This is due to the fact, that for eps = diff(range(TS)), all the points will be counted as the recurrences. This fact is used when
  #' studying the characteristics of the time series dependent on the 'eps' value using the \code{\link{rqa.seq}} function.
  #' 
  #' Usually, RQA is computed from a state-space reconstruction of the dynamical system. In this case Takens embedding is used [3]. It is necessary to set two parameters for Takens embedding: embeding dimension and delay time.
  #' If You have no prior knowledge about the system, it is possible to estimate best values for these parameters according to the first minimal value of the mutual information of the time series and the minimal value of the false 
  #' nearest neighbour algorithm. These routines can be found in e.g. 'nonlinearTseries' package and 'fNonlinear' package.
  #' 
  #' There are other ways how to test whether the data have non-linear characteristics, have stochastic nature, or are just colored noise. To this end You can use tests included in 'nonlinearTseries' package or 'fNonlinear' package. 'nonlinearTseries' package also include
  #' RQA function, which stores more results, but are significantly slower and memory expensive, especially for the longer time series. Similar test could be found in other packages focused on nonlinear time series analysis.
  #' @return
  #' Returns "chaos01.rqa" object (to differentiate from the 'rqa' object given by the 'nonlinearTseries' package), which contains list of RQA results and list of settings. Additionaly, if include.TS = TRUE, it adds input time series to the end of the list.
  #' \itemize{
  #'     \item "RR"    - Recurrence rate
  #'     \item "DET"   - Determinism, count recurrence points in diagonal lines of length >= lmin 
  #'     \item "RATIO" - DET/RR
  #'     \item "AVG"   - average length of diagonal lines of length >= lmin
  #'     \item "MAX"   - maximal length of diagonal lines of length >= lmin
  #'     \item "DIV"   - Divergence, 1/MAX
  #'     \item "LAM"   - Laminarity, VLRP/TR
  #'     \item "TT"    - Trapping time, average length of vertical lines of length >= lmin
  #'     \item "MAX_V" - maximal length of vertical lines of length >= lmin
  #'     \item "TR"    - Total number of recurrence points
  #'     \item "DLRP"  - Recurrence points on the diagonal lines of length of length >= lmin
  #'     \item "DLC"   - Count of diagonal lines of length of length >= lmin
  #'     \item "VLRP"  - Recurrence points on the vertical lines of length of length >= lmin
  #'     \item "VLC"   - Count of vertical lines of length of length >= lmin
  #' }
  #================= Check Input =====================
  
  if(!(is.numeric(TS) || is.integer(TS))){
    stop("Error: 'TS' is not a numeric.")
  }
  
  if(!(is.numeric(eps) || is.integer(eps))){
    stop("Error: 'eps' is not a numeric.")
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

  #================= Create sequence of threshold values =====================
  
  rmax <- length(TS) - ((dim-1) * lag)
  RQA  <- rep(-1,14)
  
  res <- .C("diag_rqa_max",
            TS = as.double(TS),
            RQA = as.double(RQA),
            R_theta = as.integer(theta),
            R_rmax = as.integer(rmax),
            R_dim = as.integer(dim),
            R_lag = as.integer(lag),
            R_eps = as.double(eps),
            R_lmin = as.integer(lmin)
  )
  
  RQA <- list(RR    = res$RQA[1],
              DET   = res$RQA[2],
              RATIO = res$RQA[3],
              AVG   = res$RQA[4],
              MAX   = res$RQA[5],
              DIV   = res$RQA[6],
              LAM   = res$RQA[7],
              TT    = res$RQA[8],
              MAX_V = res$RQA[9],
              TR    = res$RQA[10],
              DLRP  = res$RQA[11],
              DLC   = res$RQA[12],
              VLRP  = res$RQA[13],
              VLC   = res$RQA[14])
  
  results <- list()
  results[[1]] <- RQA
  settings <- c(dim, lag, eps, theta, lmin)
  results[[2]] <- list(dim = dim,
                       lag = lag,
                       eps = eps,
                       theta = theta,
                       lmin  = lmin)
  names(results) <- c("RQA", "settings")
  
  if(include.TS){
    results[[3]] <- TS
    names(results) <- c("RQA", "settings", "TS")
  }
  
  class(results) <- "chaos01.rqa"
  
  return(results)
}

summary.chaos01.rqa <- function(object, ...){
  #' Print all the settings and results of the RQA computation.
  #'
  #' This function prints structured results of the RQA computation.
  #' @param object the object of "chaos01.rqa" class, produced by fast.rqa function.
  #' @param ... further arguments passed to or from other methods.
  #' @keywords summary print rqa 
  #' @method summary chaos01.rqa
  #' @export
  #' @seealso \code{\link{rqa.seq}}, \code{\link{fast.rqa}}
  #' @examples
  #' vec.x <- gen.logistic(mu = 3.55, iter = 2000)
  #' 
  #' res <- fast.rqa(vec.x, dim = 3, lag = 10, eps = 0.3)
  #' summary(res)
  #' @references
  #' N. Marwan; M. C. Romano; M. Thiel; J. Kurths (2007). "Recurrence Plots for the Analysis of Complex Systems". Physics Reports. 438 (5-6): 237. Bibcode:2007PhR...438..237M. doi:10.1016/j.physrep.2006.11.001.
  
  cat("
SETTINGS
---------------------------------------------------
Embedding dimension : ", object$settings$dim,"
Delay time          : ", object$settings$lag,"
Thresholds (eps)    : ", object$settings$eps,"
Theiler window      : ", object$settings$theta,"
Minimal line length : ", object$settings$lmin,"
      
RECURRENCE PLOT STATISTICS
---------------------------------------------------
Total recurrences : ", object$RQA$TR,"
Recurrences on diagonal lines : ", object$RQA$DLRP,"
Number of diagonal lines      : ", object$RQA$DLC,"
Recurrences on vertical lines : ", object$RQA$VLRP,"
Number of vertical lines      : ", object$RQA$VLC,"

RQA MEASURES
---------------------------------------------------
Recurrence rate (RR)         : ", object$RQA$RR,"
Determinism (DET)            : ", object$RQA$DET,"
RATIO - DET/RR               : ", object$RQA$RATIO,"
Average diagonal line length : ", object$RQA$AVG,"
Maximal diagonal line length : ", object$RQA$MAX,"
Divergence (1/MAX)           : ", object$RQA$DIV,"
Laminarity                   : ", object$RQA$LAM,"
Trapping time                : ", object$RQA$TT,"
Maximal vertical line length : ", object$RQA$MAX_V)
  
}
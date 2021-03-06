testChaos01 <- function(TS, c.rep = 100, alpha = 0, out = FALSE, c.int = c(pi/5, 4*pi/5), c.gen = "random", par = "seq", num.threads = NA, include.TS = FALSE, approach = "cor", window.size = NA, control.set.point = NA, threshold = NA){

  #' Function to compute 0-1 test for chaos
  #'
  #' This function computes results of the 0-1 test for chaos from the numeric vector (time series).
  #' @param TS the input vector. This should be a numeric vector.
  #' @param c.rep integer, defines how many different parameters "c" should be used for the computation. Default is 100.
  #' @param alpha numeric, the noise dampening parameter. If 0, no noise dampening is done. For more details see the Gottwald and Melbourne (2004). This variable is not used in "bounding box" and "centre of gravity" approaches. Default is 0.
  #' @param out logical, if TRUE return the list of class "chaos01.res". This list contain lists of "chaos01" list with values of pc, qc, Mc, Dc, Kc and c. These can be then easily plotted by the plot function. Default is FALSE.
  #' @param c.int set the interval from which the parameter "c" should be chosen. The input is numeric vector. The minimal and maximal value in the vector is then chosen as the minimum and maximum for the generation of parameters "c". Generally it is not needed to change this value. Default is c(pi/5, 4*pi/5).
  #' @param c.gen character string, which defines how the parameter "c" should be generated from the interval defined by c.int.
  #'  \itemize{
  #'    \item "random" - draws from uniform distribution
  #'    \item "equal" - equidistant distribution
  #'  }
  #'  Default is "random". Note: If there is unrecognized input, it will use default.
  #' @param par character string, determine whether make the computation for every parameter "c" sequentially or in parallel. Parallelization is provided by the package "parallel" for one machine, or the cluster option using package "Rmpi" is available.
  #' \itemize{
  #' \item "seq" - sequential run
  #' \item "parallel" - parallel on one machine
  #' \item "MPI" - run parallel using Rmpi. }
  #' When the work with MPI is finished, the Rmpi::mpi.finalize(), must be used to properly close the MPI. After that command, it is impossible to run MPI until restarting the R session, therefore use it after all runs of the test.
  #'
  #' Default is "seq".
  #' Note: If there is unrecognized input, it will use default.
  #' @param num.threads integer, number of threads use for the computation. When the computation is sequential, this is ignored. Default is NA.
  #' @param include.TS logical, if TRUE and out is TRUE input time series will be added to the list of outputs. Default is FALSE.
  #' @param approach character string, determine which computational approach to use.
  #' \itemize{
  #' \item "cor" - mean square displacement method with correlation proposed by Gottwald and Melbourne.
  #' \item "bb"  - bounding box method proposed by Martinovic.
  #' \item "cog" - centre of gravity method proposed by Martinovic.
  #' }
  #' @param window.size integer, window size used for the bounding box or the centre of gravity computation.
  #' @param control.set.point integer, point where the bounding box size should be checked, when the approach = "bb".
  #' @param threshold numeric, a threshold value for the bounding box computation and centre of gravity computation. For more information see Martinovic (2019).
  #' @seealso \code{\link{plot.chaos01}}, \code{\link{plot.chaos01.res}}, \code{\link{getVal}}
  #' @keywords test
  #' @concept chaos
  #' @export
  #' @examples
  #' TS <- gen.logistic(mu = 3.55, iter = 2000)
  #'
  #' # The median of Kc
  #' res <- testChaos01(TS)
  #' print(res)
  #'
  #' # Output for each value of c
  #' res2 <- testChaos01(TS, out = TRUE)
  #'
  #' summary(res2[[1]])
  #' head(res2[[1]]$pc)
  #' print(res2[[1]]$Kc)
  #'
  #' class(res2)
  #' class(res2[[1]])
  #'
  #' \dontrun{
  #' # Introducing noise
  #' TS2 <- TS + runif(2000, 0, 0.1)
  #'
  #' res.orig <- testChaos01(TS2, alpha = 0)
  #' res.damp <- testChaos01(TS2, alpha = 2.5)
  #'
  #' sprintf(Original test result %s\n Dampened test result %s, res.orig, res.damp)
  #'
  #' # Parallel
  #' res <- testChaos01(TS, par = "parallel", num.treads = 2)
  #'
  #' # Parallel cluster, remember to initialize and finalize the MPI
  #' # Needs to be run inside a script and run from a command line interface (CLI)
  #' # e.g. mpirun Rscript test_chaos01_mpi.R
  #' pbdMPI::init()
  #' res <- testChaos01(TS, par = "MPI", num.treads = 2)
  #' pbdMPI::finalize()
  #' 
  #'
  #' # Different interval for generating c
  #' res <- testChaos01(TS, c.int = c(0, pi))
  #' }
  #' @references
  #' Gottwald G.A. and Melbourne I. (2004) On the implementation of the 0-1 Test for Chaos, SIAM J. Appl. Dyn. Syst., 8(1), 129–145.
  #'
  #' Gottwald G.A. and Melbourne I. (2009) On the validity of the 0-1 Test for Chaos, Nonlinearity, 22, 6
  #'
  #' Martinovic T. (2019)  Alternative approaches of evaluating the 0-1 test for chaos, Int. J. Comput. Math.
  #' @details
  #' Note that this test does not work in several cases.
  #' \itemize{
  #' \item In general, time series have to approach steady state, that is it`s attractor. E.g. if the time series corresponds
  #' to homoclinic trajectory, the output of the test should not be close to 0 or 1.
  #' \item The time series contain too much noise. See examples.
  #' \item The time series is behaving like a white noise.
  #' }
  #' In these cases you may receive unclear results, or in special cases the false results.
  #' You can find more on the validity of the test in Gottwald and Melbourne (2009).
  #' @return
  #' A numeric from the interval (0,1). 0 stands for the regular dynamics and 1 for the chaotic dynamics. If the parameter out = TRUE, the output is list of list of all the computed variables. This is mainly for research and testing purposes.

  #================= Check Input =====================

  if(!(is.numeric(TS) || is.integer(TS))){
    stop("Error: 'TS' is not a numeric.")
  }

  if(!(is.logical(out))){
    stop("Error: 'out' is not a logical.")
  }
  
  if(!(is.logical(include.TS))){
    stop("Error: 'out' is not a logical.")
  }
  
  if(!(is.numeric(alpha) || is.integer(alpha))){
    stop("Error: 'alpha' is not a numeric.")
  }

  if(!requireNamespace("parallel", quietly = TRUE) & par == "parallel"){
    warning("No package parallel found. Switching to sequential computation.")
    par <- "seq"
  }
  if(!requireNamespace("pbdMPI", quietly = TRUE) & par == "MPI"){
    warning("No package pbdMPI found. Switching sequential computation.")
    par <- "seq"
  }

  if((par %in% c("MPI", "parallel") & !(is.numeric(num.threads) || is.integer(num.threads)))){
    stop("Error: Parameter num.threads should be integer, for the parallel computation.")
  }
  
  if(requireNamespace("pbdMPI", quietly = TRUE) & par == "MPI"){
    if(pbdMPI::is.finalized()){
      stop("MPI was finalized by previous run/process. \nNew run of the test in MPI would result in R crash.\nYou need to restart R for another run.")
    }
  }
  
  #================= Generate vector of values c to be used =====================
  
  switch(as.character(c.gen),
         equal = {
           c <- seq(from = min(c.int), to = max(c.int), length.out = c.rep)
         },
          {
           c <- stats::runif(c.rep, min = min(c.int), max = max(c.int))
         }
  )

  switch(as.character(par),

  #================= Multicore parallel, one machine =====================

    parallel = {
      no_cores <- parallel::detectCores() - 1
      num.threads <- min(num.threads, no_cores)

      cl <- parallel::makeCluster(num.threads, type = "PSOCK")
      if(out){
            res <- parallel::parLapplyLB(cl,
                                   c,
                                   K.c.computation,
                                   TS,
                                   alpha,
                                   out,
                                   include.TS,
                                   approach,
                                   window.size,
                                   control.set.point,
                                   threshold)
          class(res) <- "chaos01.res"
      } else{
        Kc <- unlist(parallel::parLapplyLB(cl,
                                 c,
                                 K.c.computation,
                                 TS,
                                 alpha,
                                 out,
                                 include.TS,
                                 approach,
                                 window.size,
                                 control.set.point,
                                 threshold))
      }
      parallel::stopCluster(cl)
      if(out){return(res)
      } else {return(stats::median(Kc))}
    },

  #================= MPI, cluster parallel =====================

    MPI = {
          if(out){
            res <- pbdMPI::pbdLapply(c,
                                     K.c.computation,
                                     TS,
                                     alpha,
                                     out,
                                     include.TS,
                                     approach,
                                     window.size,
                                     control.set.point,
                                     threshold,
                                     pbd.mode = "spmd")
            class(res) <- "chaos01.res"
          } else{
            Kc <- unlist(pbdMPI::pbdLapply(c,
                                           K.c.computation,
                                           TS,
                                           alpha,
                                           out,
                                           include.TS,
                                           approach,
                                           window.size,
                                           control.set.point,
                                           threshold,
                                           pbd.mode = "spmd"))
          }

         if(out){return(res)
           } else {return(stats::median(Kc))}
    },

  #================= Sequential =====================

    {
      if(out){
        res <- lapply(c,
                      K.c.computation,
                      TS,
                      alpha,
                      out,
                      include.TS,
                      approach,
                      window.size,
                      control.set.point,
                      threshold)
        class(res) <- "chaos01.res"
        return(res)
      } else{
        K.c <- unlist(lapply(c,
                             K.c.computation,
                             TS,
                             alpha,
                             out,
                             include.TS,
                             approach,
                             window.size,
                             control.set.point,
                             threshold))

        return(stats::median(K.c))
      }
    }
  )
}

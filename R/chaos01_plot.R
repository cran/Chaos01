plot.chaos01 <- function(x, ...){
  #' Plot the additional results of 0-1 test for chaos.
  #'
  #' This function plot the Pc to Qc plot and Mc/Dc plot as described in Gottwald and Melbourne (2004).
  #' @param x the object of "chaos01" class, produced by test.chaos01 function when parameter Val = "TRUE". Subset the output of the function to get the results for the concrete c. See the example.
  #' @param ... arguments to be passed as graphical parameters.
  #' @keywords plot test chaos
  #' @method plot chaos01
  #' @export
  #' @seealso \code{\link{testChaos01}}, \code{\link{plot.chaos01.res}}
  #' @examples
  #' vec.x <- gen.logistic(mu = 3.55, iter = 2000)
  #'
  #' #Output for each value of c
  #' res2 <- testChaos01(vec.x, out = TRUE)
  #'
  #' plot(res2[[1]])
  #' @references
  #' Gottwald G.A. and Melbourne I. (2004) On the implementation of the 0-1 Test for Chaos, SIAM J. Appl. Dyn. Syst., 8(1), 129–145.

  graphics::plot(x$pc, x$qc,
       type = "l",
       main = paste("0-1 test for Chaos. P_c to Q_c plot. c = ", round(x$c, 4), sep=""),
       xlab = "P_c",
       ylab = "Q_c",
       ...)

  graphics::plot(1:length(x$Mc), x$Mc,
       type = "l",
       main = paste("0-1 test for Chaos. M_c and D_c plot. c = ", round(x$c, 4), sep=""),
       xlab = "Time",
       ylab = "M_c/D_c",
       col = 4,
       ...)
  graphics::lines(1:length(x$Dc), x$Dc,
        col = 2)

}

plot.chaos01.res <- function(x, ...){

  #' Plot Kc based on c
  #'
  #' This function plot results Kc dependent on the value of parameter c as described in Gottwald and Melbourne (2004).
  #' @param x the object of "chaos01.res" class, produced by test.chaos01 function when parameter Val = "TRUE".
  #' @param ... arguments to be passed as graphical parameters.
  #' @keywords plot test chaos
  #' @export
  #' @method plot chaos01.res
  #' @seealso \code{\link{testChaos01}}, \code{\link{plot.chaos01}}
  #' @examples
  #' vec.x <- gen.logistic(mu = 3.55, iter = 2000)
  #'
  #' #Output for each value of c
  #' res2 <- testChaos01(vec.x, out = TRUE)
  #'
  #' plot(res2)
  #' @references
  #' Gottwald G.A. and Melbourne I. (2004) On the implementation of the 0-1 Test for Chaos, SIAM J. Appl. Dyn. Syst., 8(1), 129–145.

  temp <- data.frame(
    c = unlist(x)[names(unlist(x))=="c"],
    kc = unlist(x)[names(unlist(x))=="Kc"])
  temp <- temp[order(temp$c),]

  graphics::plot(temp$c, temp$kc,
       type="l",
       main = "0-1 test for Chaos. K_c to c plot.",
       xlab = "c",
       ylab = "K_c",
       ...)
}

plot.chaos01 <- function(x, plotvar = c("PQ", "MD"), mdcol = NULL, col = 1, main = NULL, xlab = NULL, ylab = NULL, type = NULL, ylim = NULL, ...){
  #' Plot the additional results of 0-1 test for chaos.
  #'
  #' This function plot the Pc to Qc plot and Mc/Dc plot as described in Gottwald and Melbourne (2004).
  #' @param x the object of "chaos01" class, produced by testChaos01 function when parameter out = "TRUE". Subset the output of the function to get the results for the concrete c. See the example.
  #' @param plotvar list/vector define what should be plotted.
  #' \itemize{
  #'     \item c("PQ", "MD") - both plots Pc - Qc, and Mc/Dc should be plotted.
  #'     \item "PQ"          - only Pc-Qc should be plotted.
  #'     \item "MD"          - only Mc/Dc plot should be plotted.
  #' }
  #' Default is c("PQ", "MD").
  #' @param mdcol vector of length 2 or NULL. 
  #' If NULL colors in MD plot will be the same as in 'col' argument. If vector of length 2, first color stands for the Mc line and second color for the Dc line.
  #' \itemize{
  #'     \item NULL   - use color defined in 'col' argument.
  #'     \item c(4,3) - use blue for the Mc line and green for the Dc line.
  #'     \item c("firebrick", "cadetblue") - use of color names is also possible.
  #'     }
  #'     
  #' When used, it overrides 'col' argument. It is possible to set colors as numbers, or by the string name. 
  #'  
  #' Default is NULL.
  #' @param col color for the lines in plots as defined in plot(). Default is 1.
  #' @param main string an overall title for the plot: see \code{\link[graphics]{title}}
  #' @param xlab string a title for the x axis: see \code{\link[graphics]{title}}
  #' @param ylab string a title for the y axis: see \code{\link[graphics]{title}}
  #' @param ylim numeric vectors of length 2, giving the x and y coordinates ranges: see \code{\link[graphics]{plot.window}}
  #' @param type string what type of plot should be drawn: see \code{\link[graphics]{plot}}
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
  #' plot(res2[[1]], plotvar = c("PQ", "MD"), mdcol = c(4,3))
  #' @details 
  #' When plotvar = c("PQ", "MD"), or plotvar = c("MD", "PQ") the settings for main, xlab, ylab, ylim, would affect both plots, what does not make sense in most cases.
  #' To prevent this, setting of main, xlab, ylab and ylim only affects the first figure and second is set to default values for the given figure.
  #' @references
  #' Gottwald G.A. and Melbourne I. (2004) On the implementation of the 0-1 Test for Chaos, SIAM J. Appl. Dyn. Syst., 8(1), 129–145.

  cont.main <- 0
  cont.xlab <- 0
  cont.ylab <- 0
  cont.ylim <- 0
  
  for(y in plotvar)
    switch(y,
           "PQ" = {
                    if(is.null(main) || is.null(cont.main)){
                      main <- paste("0-1 test for Chaos. P_c to Q_c plot. c = ", round(x$c, 4), sep="")
                      cont.main <- NULL
                    } else{
                      cont.main <- NULL
                    }
                    if(is.null(xlab) || is.null(cont.xlab)){
                      xlab <- "P_c"
                      cont.xlab <- NULL
                    } else{
                      cont.xlab <- NULL
                    }
                    if(is.null(ylab) || is.null(cont.ylab)){
                      ylab <- "Q_c"
                      cont.ylab <- NULL
                    } else{
                      cont.ylab <- NULL
                    }
                    if(is.null(type)){
                      type <- "l"
                    }
                    if(is.null(ylim) || is.null(cont.ylim)){
                      ylim <- range(x$qc)
                      cont.ylim <- NULL
                    } else{
                      cont.ylim <- NULL
                    }
                    graphics::plot(x$pc, x$qc,
                         main = main,
                         xlab = xlab,
                         ylab = ylab,
                         type = type,
                         ylim = ylim,
                         ...)
           },
           "MD" = {
                    if(is.null(main) || is.null(cont.main)){
                      main <- paste("0-1 test for Chaos. M_c and D_c plot. c = ", round(x$c, 4), sep="")
                      cont.main <- NULL
                    } else{
                      cont.main <- NULL
                    }
                    if(is.null(xlab) || is.null(cont.xlab)){
                      xlab <- "Time"
                      cont.xlab <- NULL
                    } else{
                      cont.xlab <- NULL
                    }
                    if(is.null(ylab) || is.null(cont.ylab)){
                      ylab <- "M_c/D_c"
                      cont.ylab <- NULL
                    } else{
                      cont.ylab <- NULL
                    }
                    if(is.null(type)){
                      type <- "l"
                    }
                    if(is.null(ylim) || is.null(cont.ylim)){
                      ylim <- range(c(x$Mc, x$Dc))
                      cont.ylim <- NULL
                    } else{
                      cont.ylim <- NULL
                    }
                    if(is.null(mdcol)){
                      col1 <- col
                      col2 <- col
                    } else
                    {
                      col1 <- mdcol[1]
                      col2 <- mdcol[2]
                    }
                    graphics::plot(1:length(x$Mc), x$Mc,
                         main = main,
                         xlab = xlab,
                         ylab = ylab,
                         type = type,
                         ylim = ylim,
                         col = col1,
                         ...)
                    
                    graphics::lines(1:length(x$Dc), x$Dc,
                          col = col2)
           }
    )

}

plot.chaos01.res <- function(x, main = NULL, xlab = NULL, ylab = NULL, type = NULL, ...){

  #' Plot Kc based on c
  #'
  #' This function plot results Kc dependent on the value of parameter c as described in Gottwald and Melbourne (2004).
  #' @param x the object of "chaos01.res" class, produced by testChaos01 function when parameter out = "TRUE".
  #' @param main string an overall title for the plot: see \code{\link[graphics]{title}}
  #' @param xlab string a title for the x axis: see \code{\link[graphics]{title}}
  #' @param ylab string a title for the y axis: see \code{\link[graphics]{title}}
  #' @param type string what type of plot should be drawn: see \code{\link[graphics]{plot}}
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
    c = sapply(x, function(y)y$c),
    kc = sapply(x, function(y)y$Kc))
  temp <- temp[order(temp$c),]

  if(is.null(main)){
    main = "0-1 test for Chaos. K_c to c plot."
  }
  if(is.null(xlab)){
    xlab = "c"
  }
  if(is.null(ylab)){
    ylab = "K_c"
  }
  if(is.null(type)){
    type = "l"
  }
  
  graphics::plot(temp$c, temp$kc,
       main = main,
       xlab = xlab,
       ylab = ylab,
       type = type,
       ...)
}

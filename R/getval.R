getVal <- function(x, vars = "both"){
  #' Get the vector of Kc/c values from the chaos01.res object. 
  #'
  #' This function allows easy extraction of Kc/c values from the chaos01.res object.
  #' @param x the object of "chaos01.res" class, produced by testChaos01 function when parameter out = "TRUE". Subset the output of the function to get the results for the concrete c. See the example.
  #' @param vars list/vector define what should be plotted.
  #' \itemize{
  #'     \item "both" - both variables "Kc" and "c" will be returned in data.frame
  #'     \item "Kc"   - vector of "Kc" values will be returned
  #'     \item "c"    - vector of "c" values will be returned
  #' }
  #' Default is "both").
  #' @keywords results test chaos
  #' @export
  #' @seealso \code{\link{testChaos01}}
  #' @examples
  #' vec.x <- gen.logistic(mu = 3.55, iter = 2000)
  #'
  #' #Kc for each value of c
  #' res2 <- testChaos01(vec.x, out = TRUE)
  #'
  #' results <- getVal(res2, vars = "both")
  #' print(head(results))
  #' 
  #' #Get results of 0-1 test for Chaos when out = TRUE
  #' K <- median(getVal(res2, vars = "Kc"))
  #' @references
  #' Gottwald G.A. and Melbourne I. (2004) On the implementation of the 0-1 Test for Chaos, SIAM J. Appl. Dyn. Syst., 8(1), 129–145.
  #' @return
  #' Vector of Kc or c values, or data.frame including both vectors if vars = "both".
  
  
  
  if(class(x) != "chaos01.res"){
    stop("Input variable is not of class 'chaos01.res' (list of results of class 'chaos01').")
  }
  
  switch(vars,
    "Kc" = return(sapply(x, function(y)y$Kc)),
    "c"  = return(sapply(x, function(y)y$c)),
    "both" = {
      kc <- sapply(x, function(y)y$Kc)
      c  <- sapply(x, function(y)y$c)
      return(data.frame(c = c, kc = kc))
    }
  )
}

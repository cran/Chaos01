gen.logistic <- function(mu, iter = 5000, x0 = 0.0001){
  #' Logistic map
  #'
  #' Generate iterations of the logistic map defined as x[t+1] = mu * x[t] * (1 - x[t]).
  #' @param mu parameter of the logistic function. mu should be from the interval (0,4).
  #' @param iter number of iterations of the logistic function. Default is 5000.
  #' @param x0 the initial value of the series. Should be from the interval (0,1). Default is 0.0001.
  #' @keywords logistic map
  #' @export
  #' @examples
  #' vec.x <- gen.logistic(mu = 3.55, iter = 200)
  #' plot(vec.x, type = "l")
  #' @return numeric vector with the iterations of the logistic map.

  if(mu > 4 || mu < 0){
    stop("mu should be from the interval (0,4).", call. = F)
  }

  if(x0 > 1 || x0 < 0){
    stop("x0 should be from the interval (0,1).", call. = F)
  }

  x <- rep(NA, iter)
  x[1] <- x0
  for(k in 2:iter){
    x[k] <- mu*(x[k-1])*(1-x[k-1])
  }
  return(x)
}

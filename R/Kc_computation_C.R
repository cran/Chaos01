K.c.computation <- function(c, TS, alpha = 0, out = FALSE, include.TS = FALSE, approach = "cor", window.size = NA, control.set.point = NA, threshold = NA){
  #' @keywords internal

  if(approach == "cor"){
    res <- K.c.cor(c, TS, alpha, out, include.TS)
  } else if (approach == "bb"){
    res <- K.c.bb(c, TS,window.size, control.set.point, threshold, out, include.TS)
  } else if (approach == "cog"){
    res <- K.c.cog(c, TS, window.size, threshold, out, include.TS)
  } else {
    stop("Variable 'approach' had unknown value.\nPlease choose from 'cor', 'bb', or 'cog'.")
  }

  return(res)
}

K.c.cor <- function(c, TS, alpha = 0, out = FALSE, include.TS = FALSE){
  #' @useDynLib Chaos01, compute_kc
  #' @keywords internal
  
  N <- length(TS)
  vec.n.cut <- 1:(N/10)
  mean.x2 <- mean(TS)^2
  pc <- rep(0, N)
  qc <- rep(0, N)
  Mc <- rep(0, N/10)
  Dc <- rep(0, N/10)
  
  res <- .C("compute_kc",
            TS = as.double(TS),
            c = as.double(c),
            N = as.integer(N),
            mean_x2 = as.double(mean.x2),
            pc = as.double(pc),
            qc = as.double(qc),
            Mc = as.double(Mc),
            Dc = as.double(Dc),
            alpha = as.double(alpha)
  )
  
  K.c.cor <- abs(stats::cor(vec.n.cut, res$Dc))
  
  if(out){
    if(include.TS){
      res <- list(pc=res$pc, qc=res$qc, Mc=res$Mc, Dc=res$Dc, Kc=K.c.cor, c=c, TS=TS)
    } else {
      res <- list(pc=res$pc, qc=res$qc, Mc=res$Mc, Dc=res$Dc, Kc=K.c.cor, c=c) 
    }
    class(res) <- "chaos01"
    return(res)
  } else{
    return(K.c.cor)
  }
}

K.c.bb <- function(c, TS, window.size, control.set.point, threshold = 0.995, out = FALSE, include.TS = FALSE){
  #' @keywords internal

  if(is.na(threshold)){
    threshold <- 0.995
    warning("There was no threshold given.\nIt was set to default value for bounding box computation: 0.995.")
  }
  
  if(!(is.numeric(threshold) || is.integer(threshold))){
    stop("Error: 'threshold' is not a numeric.")
  }
  
  if(!(is.numeric(window.size) || is.integer(window.size))){
    stop("Error: 'window.size' is not a numeric.")
  }
  
  if(!(is.numeric(control.set.point) || is.integer(control.set.point))){
    stop("Error: 'control.set.point' is not a numeric.")
  }
  
  if(window.size > length(TS)){
    stop("Error: 'window.size' is greater than the length of the input time series.")
  }
  
  if(control.set.point > length(TS)){
    stop("Error: 'control.set.point' is greater than the length of the input time series.")
  }
  
  if(control.set.point < window.size){
    stop("Error: 'control.set.point' is smaller than the 'window.size'. Please set higher 'control.set.point'.")
  }
  
  pc <- cumsum(TS * cos(1:length(TS) * c))
  qc <- cumsum(TS * sin(1:length(TS) * c))
  maxpc <- cummax(pc)
  minpc <- cummin(pc)
  maxqc <- cummax(qc)
  minqc <- cummin(qc)
    
  sizepc <- max(pc[(control.set.point - window.size):control.set.point]) - min(pc[(control.set.point - window.size):control.set.point])
  sizeqc <- max(qc[(control.set.point - window.size):control.set.point]) - min(qc[(control.set.point - window.size):control.set.point])
    
  sizepc2 <- maxpc[length(TS)] - minpc[length(TS)] 
  sizeqc2 <- maxqc[length(TS)] - minqc[length(TS)]
    
  Kc <- ((sizepc * sizeqc) / (sizepc2 * sizeqc2)) < threshold
  
  bb <- list(maxpc = maxpc,
             minpc = minpc,
             maxqc = maxqc,
             minqc = minqc)
  
  if(out){
    if(include.TS){
      res <- list(pc = pc, qc = qc, Kc = Kc, c = c, bb = bb, TS = TS)
    } else {
      res <- list(pc = pc, qc = qc, Kc = Kc, c = c, bb = bb) 
    }
    class(res) <- "chaos01"
    return(res)
  } else {
    return(Kc)
  }
}


K.c.cog <- function(c, TS, window.size, threshold = 4.5, out = FALSE, include.TS = FALSE){
  #' @useDynLib Chaos01, myrollmean
  #' @keywords internal
  
  if(is.na(threshold)){
    threshold <- 4.5
    warning("There was no threshold given.\nIt was set to default value for centre of gravity computation: 4.5.")
  }
  
  if(!(is.numeric(threshold) || is.integer(threshold))){
    stop("Error: 'threshold' is not a numeric.")
  }
  
  if(!(is.numeric(window.size) || is.integer(window.size))){
    stop("Error: 'window.size' is not a numeric.")
  }
  
  if(window.size > length(TS)){
    stop("Error: 'window.size' is greater than the length of the input time series.")
  }
  
  length.res <- length(TS)

  pc <- cumsum(TS * cos(1:length.res * c))
  qc <- cumsum(TS * sin(1:length.res * c))
    
  rollpccog <- c(rep(NA, (window.size - 1)), myrollmean_c(pc, window.size))
  rollqccog <- c(rep(NA, (window.size - 1)), myrollmean_c(qc, window.size))
    
  sizepc <- max(pc[1:window.size]) - min(pc[1:window.size])
  sizeqc <- max(qc[1:window.size]) - min(qc[1:window.size])
    
  res <- sum((rollpccog[(2 * window.size):length.res] - rollpccog[window.size:(length.res - window.size)])^2 + 
                 (rollqccog[(2 * window.size):length.res] - rollqccog[window.size:(length.res - window.size)])^2
               > (sizepc / (window.size/threshold))^2 + (sizeqc / (window.size/threshold))^2)
  Kc <- res/(length(TS) - (2 * window.size - 1))

  cog <- list(cog.pc = rollpccog,
              cog.qc = rollqccog)
  
  if(out){
    if(include.TS){
      res <- list(pc = pc, qc = qc, Kc = Kc, c = c, cog = cog, TS = TS)
    } else {
      res <- list(pc = pc, qc = qc, Kc = Kc, c = c, cog = cog) 
    }
    class(res) <- "chaos01"
    return(res)
  } else {
    return(Kc)
  }
}

myrollmean_c <- function(x, width){
  n <- length(x)
  out <- rep(0, n)
  res <- .C("myrollmean",
            TS = as.double(x),
            width = as.integer(width),
            N = as.integer(n),
            out = as.double(out))
  return(res$out)
}

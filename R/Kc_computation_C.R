K.c.computation <- function(c, TS, alpha=0, out = FALSE, include.TS = FALSE){
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

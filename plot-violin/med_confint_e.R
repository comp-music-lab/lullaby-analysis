# exact confidence intervals for medians
med_confint <- function(x, al, verbose) {
  q <- 1 - al
  n <- length(x)
  
  k <- 0:n
  cdf <- pbinom(q = k, size = n, p = 0.5)
  
  r <- k[max(which(cdf < q/2))]
  s <- k[min(which(cdf > (1 - q/2)))]

  if (r == 0) {
    r <- 1
    
    if (verbose == TRUE) {
      cat(sprintf("Data below %3.1f%% quantile does not exist so the smallest data is used insted\n",
                  100*q/2))
    }
  }
  
  x <- sort(x)
  
  y <- median(x)
  ymin <- x[r]
  ymax <- x[s]
  
  if (verbose == TRUE) {
    cat(sprintf("median = %3.3f, %3.1f%% quantile = %3.3f (%dth data), %3.1f%% quantile = %3.3f (%dth data)\n",
                y, 100*(1 - q/2), ymax, s, 100*q/2, ymin, r))  
  }
  
  y.df <- data.frame(y, ymin, ymax)
  
  return(y.df)
}
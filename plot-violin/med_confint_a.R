# approximate confidence intervals for medians
med_confint <- function(x, al, verbose) {
  q <- 1 - al
  n <- length(x)
  
  C <- qnorm(1 - q/2, mean = 0, sd = 1)
  
  r <- round(n/2 - C*sqrt(n)/2)
  s <- round(1 + n/2 + C*sqrt(n)/2)
  
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
rosenthal <- function(stat, se, alpha = 0.05) {

  k <- length(stat) ## how many studies you have
  Nr <- sum( stat/se )^2 / qnorm(1 - alpha)^2 - k
  rule <- 5 * k + 10
  res <- c(Nr, rule)
  names(res) <- c('fail-safe Nr', 'rule')
  res
  
}

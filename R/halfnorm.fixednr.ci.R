halfnorm.fixednr.ci <- function(stat, se, alpha = 0.05, type = "dist", B = 1000) {

z <- stat/se
k <- length(stat)
za <- qnorm(1 - alpha)
Nr <- sum( z )^2 / za - k
za2 <- za^2
f <- (k - 1) / k

if ( type == "dist" ) {
  m1 <- sqrt(2 / pi)    ;    s1 <- 1 - 2/pi
  variance <- 2 * k^2 * s1 * ( 2 * k * m1^2 + s1 ) / za^4
  ci <- c( Nr - qnorm(1 - alpha/2) * sqrt(variance), Nr + qnorm(1 - alpha/2) * sqrt(variance) )

} else if ( type == "mom" ) {
  m2 <- mean(z)    ;     s2 <- f * var(z)
  variance <- 2 * k^2 * s2 * ( 2 * k * m2^2 + s2 ) / za^4
  ci <- c( Nr - qnorm(1 - alpha/2) * sqrt(variance), Nr + qnorm(1 - alpha/2) * sqrt(variance) )

} else if ( type == "boot" ) {
  statb <- numeric(B)
  for (j in 1:B) {
    nu <- sample(1:k, k, replace = TRUE)
    statb[j] <- sum(z[1:nu])^2 / za2 - k
  }
  variance <- var(statb)
  ci <- c( Nr - qnorm(1 - alpha/2) * sqrt(variance), Nr + qnorm(1 - alpha/2) * sqrt(variance) )

} else if ( type == "all" ) {

  m1 <- sqrt(2 / pi)    ;    s1 <- 1 - 2/pi
  vardist <- 2 * k^2 * s1 * ( 2 * k * m1^2 + s1 ) / za^4
  cidist <- c( Nr - qnorm(1 - alpha/2) * sqrt(vardist), Nr + qnorm(1 - alpha/2) * sqrt(vardist) )

  m2 <- mean(z)    ;    s2 <- f * var(z)
  varmom <- 2 * k^2 * s2 * ( 2 * k * m2^2 + s2 ) / za^4
  cimom <- c( Nr - qnorm(1 - alpha/2) * sqrt(varmom), Nr + qnorm(1 - alpha/2) * sqrt(varmom) )

  statb <- numeric(B)
  for (j in 1:B) {
    nu <- sample(1:k, k, replace = TRUE)
    statb[j] <- sum(z[1:nu])^2 / za2 - k
  }
  varboot <- var(statb)
  ciboot <- c( Nr - qnorm(1 - alpha/2) * sqrt(varboot), Nr + qnorm(1 - alpha/2) * sqrt(varboot) )

  variance <- c(vardist, varmom, varboot)
  names(variance) <- c( "distributional variace", "moments variance", "bootstrap variance" )
  ci <- rbind(cidist, cimom, ciboot)
  rownames(ci) <- c( "distribution based", "moments based", "bootstrap based" )
}

list(Nr = Nr, variance = variance, ci = ci )

}

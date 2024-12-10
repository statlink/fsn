halfnorm.randomnr.ci <- function(stat, se, alpha = 0.05, type = "dist") {

  z <- stat/se
  k <- length(stat)
  za <- qnorm(1 - alpha)
  Nr <- sum( z )^2 / za - k
  za4 <- za^4
  f <- (k - 1) / k
  lam <- k

  if ( type == "dist" ) {
    m1 <- sqrt(2 / pi)    ;    s1 <- 1 - 2/pi
    a1 <- ( ( 4 * lam^3 + 6 * lam^2 + lam ) * m1^4 + (4 * lam^3 + 16 * lam^2 + 6 * lam) * m1^2 * s1 ) / za4
    a2 <- ( 2 * lam^2 + 3 * lam ) * s1^4  / za4 - 2 * ( ( 2 * lam^2 + lam ) * m1^2 + lam * s1 )/ za^2 + lam
    variance <- a1 + a2
    ci <- c( Nr - qnorm(1 - alpha/2) * sqrt(variance), Nr + qnorm(1 - alpha/2) * sqrt(variance) )

  } else if ( type == "mom" ) {
    m2 <- mean(z)     ;    s2 <- f * var(z)
    a1 <- ( ( 4 * lam^3 + 6 * lam^2 + lam ) * m2^4 + (4 * lam^3 + 16 * lam^2 + 6 * lam) * m2^2 * s2 ) / za4
    a2 <- ( 2 * lam^2 + 3 * lam ) * s2^4  / za4 - 2 * ( ( 2 * lam^2 + lam ) * m2^2 + lam * s2 )/ za^2 + lam
    variance <- a1 + a2
    ci <- c( Nr - qnorm(1 - alpha/2) * sqrt(variance), Nr + qnorm(1 - alpha/2) * sqrt(variance) )

  } else if ( type == "both" ) {

    m1 <- sqrt(2 / pi)    ;    s1 <- 1 - 2/pi
    a1 <- ( ( 4 * lam^3 + 6 * lam^2 + lam ) * m1^4 + (4 * lam^3 + 16 * lam^2 + 6 * lam) * m1^2 * s1 ) / za4
    a2 <- ( 2 * lam^2 + 3 * lam ) * s1^4  / za4 - 2 * ( ( 2 * lam^2 + lam ) * m1^2 + lam * s1 )/ za^2 + lam
    vardist <- a1 + a2
    cidist <- c( Nr - qnorm(1 - alpha/2) * sqrt(vardist), Nr + qnorm(1 - alpha/2) * sqrt(vardist) )

    m2 <- mean(z)     ;    s2 <- f * var(z)
    a1 <- ( ( 4 * lam^3 + 6 * lam^2 + lam ) * m2^4 + (4 * lam^3 + 16 * lam^2 + 6 * lam) * m2^2 * s2 ) / za4
    a2 <- ( 2 * lam^2 + 3 * lam ) * s2^4  / za4 - 2 * ( ( 2 * lam^2 + lam ) * m2^2 + lam * s2 )/ za^2 + lam
    varmom <- a1 + a2
    cimom <- c( Nr - qnorm(1 - alpha/2) * sqrt(varmom), Nr + qnorm(1 - alpha/2) * sqrt(varmom) )

    variance <- c(vardist, varmom)
    names(variance) <- c( "distributional variace", "moments variance" )
    ci <- rbind(cidist, cimom)
    rownames(ci) <- c( "distribution based", "moments based")
  }

  list(Nr = Nr, variance = variance, ci = ci )

}

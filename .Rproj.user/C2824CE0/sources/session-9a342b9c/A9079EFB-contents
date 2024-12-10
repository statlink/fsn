truncnorm.nr.density <- function(nr, k, alpha = 0.05) {

  mu <- k * sqrt(2 / pi) ## mu parameter, NOT the mean of Nr
  s2 <- k * ( 1 - 2 / pi)  ## sigma squared parameter, NOT the variace of Nr
  za <- qnorm(1 - alpha)
  lambda <- ( mu - za * sqrt(k) ) / sqrt(s2)
  if ( nr >= 0 ) {
    f1 <- za / ( 2 * pnorm(lambda) * sqrt(2 * pi * s2 * (nr + k)) )
    f2 <- exp( -0.5 / s2 * ( za * sqrt(nr + k) - mu)^2 )
    f <- f1 * f2
  } else f <- NA
  f
  
}

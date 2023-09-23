den.plot <- function(k, max_k = 20 * k, dist = "truncnorm") {

  if ( dist == "truncnorm" ) {
    nr <- den <- seq(-k + 1, max_k)
    for ( i in 1:length(nr) )  den[i] <- fsn::truncnorm.nr.density(nr[i], k = k)
    plot( nr, den, type = 'l', lty = 2, lwd = 2, ylab = 'Density of Nr',
         xlab = paste('Number of studies =', k, sep = " "), cex.lab = 1.2, cex.axis = 1.2 )
    abline(v = 0, lty = 3)

  } else if ( dist == "foldnorm" ) {
    nr <- den <- seq(-k + 1, max_k)
    for ( i in 1:length(nr) )  den[i] <- fsn::foldnorm.nr.density(nr[i], k = k)
    plot( nr, den, type = 'l', lty = 2, lwd = 2, ylab = 'Density of Nr',
         xlab = paste('Number of studies =', k, sep = " "), cex.lab = 1.2, cex.axis = 1.2 )
    abline(v = 0, lty = 3)

  } else if ( dist == "both" ) {
    nr <- den1 <- den2 <- seq(-k + 1, max_k)
    for ( i in 1:length(nr) ) {
      den1[i] <- fsn::truncnorm.nr.density(nr[i], k = k)
      den2[i] <- fsn::foldnorm.nr.density(nr[i], k = k)
    }
    plot( nr, den1, type = 'l', lty = 2, lwd = 2, ylab = 'Density of Nr',
         xlab = paste('Number of studies =', k, sep = " "), cex.lab = 1.2, cex.axis = 1.2  )
    lines(nr, den2)
    abline(v = 0, lty = 3)
  }

}
